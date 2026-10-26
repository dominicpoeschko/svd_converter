#include "svd/chip.hpp"

#include "svd/interrupts.hpp"
#include "svd/naming.hpp"
#include "svd/read.hpp"
#include "svd/semantics.hpp"

#include <cstddef>
#include <format>
#include <iterator>
#include <print>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

namespace Svd {
namespace {
    // Before any peripheral is copied into its derived instances: the copies carry the result.
    void analyzeAll(RawDevice&            raw,
                    ConvertOptions const& options) {
        for(auto& peripheral : raw.chip.peripherals) { analyze(peripheral, options); }
        for(auto& modeClusters : raw.modeClusters) {
            for(auto& mode : modeClusters.modes) { analyze(mode, options); }
        }
    }

    // Each SVD peripheral once, a mode as `<peripheral>.<mode>`.
    void collectFindings(RawDevice const&               raw,
                         std::vector<WriteOnlyFinding>& findings) {
        for(auto const& peripheral : raw.chip.peripherals) {
            writeOnlyFindings(peripheral.name, peripheral, findings);
        }
        for(auto const& modeClusters : raw.modeClusters) {
            for(auto const& mode : modeClusters.modes) {
                writeOnlyFindings(modeClusters.parent + "." + mode.name, mode, findings);
            }
        }
    }

    // A peripheral others derive from becomes an instance group (TC3 <- TC4: TC, instances 3 and 4);
    // derived ones with a stem of their own become groups of their own with the same registers (ST:
    // UART4 <- USART3); groups whose names clash are named after their members (TIM1_8).
    void groupDerived(std::vector<Peripheral>& peripherals,
                      std::vector<DerivedRef>& derived) {
        std::vector<MergedGroup> merged;
        std::vector<Peripheral>  otherStems;
        for(std::size_t i = 0; i < peripherals.size(); ++i) {
            auto& peripheralRef = peripherals[i];
            auto  groups        = groupInstances(peripheralRef.name,
                                                 peripheralRef.baseAddresses.front().address,
                                                 derived);
            if(groups.empty()) { continue; }

            peripheralRef.name          = groups.front().stem;
            peripheralRef.type          = RepeatType::cluster;
            peripheralRef.baseAddresses = groups.front().instances;

            for(auto& group : groups | std::views::drop(1)) {
                Peripheral copy    = peripheralRef;
                copy.name          = group.stem;
                copy.baseAddresses = std::move(group.instances);
                otherStems.push_back(std::move(copy));
            }
            merged.push_back({.index = i, .instances = peripheralRef.baseAddresses});
        }
        for(auto& peripheralRef : otherStems) {
            merged.push_back(
              {.index = peripherals.size(), .instances = peripheralRef.baseAddresses});
            peripherals.push_back(std::move(peripheralRef));
        }
        nameClashingGroups(peripherals, merged);
    }

    // Each mode of a peripheral becomes the peripheral <PERIPHERAL>_<MODE>; peripherals derived from
    // the parent are the modes' further instances.
    void splitModeClusters(std::vector<Peripheral>&   peripherals,
                           std::vector<ModeClusters>& modeClusters,
                           std::vector<DerivedRef>&   derived) {
        for(auto& clusterRef : modeClusters) {
            auto&                    clusterPeripherals = clusterRef.modes;
            auto const&              clusterName        = clusterRef.parent;
            std::string              newName            = clusterName;
            RepeatType               newType            = RepeatType::normal;
            std::vector<AddressType> newAddr;

            auto groups = groupInstances(
              clusterName,
              clusterPeripherals.empty() ? 0
                                         : clusterPeripherals.front().baseAddresses.front().address,
              derived);
            if(!groups.empty()) {
                newName = groups.front().stem;
                newType = RepeatType::cluster;
                // The cluster peripherals carry the base's address as instance 0 already.
                auto const& instances = groups.front().instances;
                newAddr.assign(std::next(instances.begin()), instances.end());
                for(auto& peripheralRef : clusterPeripherals) {
                    peripheralRef.baseAddresses.front().index = instances.front().index;
                }
                for(auto const& group : groups | std::views::drop(1)) {
                    std::print(stderr,
                               "derived peripherals of cluster {} named {}* are not supported\n",
                               clusterName,
                               group.stem);
                }
            }

            for(auto& peripheralRef : clusterPeripherals) {
                peripheralRef.name = std::format("{}_{}", newName, peripheralRef.name);
                peripheralRef.type = newType;
                std::ranges::copy(newAddr, std::back_inserter(peripheralRef.baseAddresses));
                peripherals.push_back(std::move(peripheralRef));
            }
        }
    }

    void reportUngrouped(std::vector<DerivedRef> const& derived) {
        for(auto const& derivedRef : derived) {
            if(!derivedRef.processed) {
                std::print(stderr, "have not found derived peripheral {}\n", derivedRef.name);
            }
        }
    }
}   // namespace

Chip chipFromSVD(pugi::xml_node const&          device,
                 ConvertOptions const&          options,
                 std::vector<WriteOnlyFinding>* findings) {
    auto raw = readDevice(device);
    analyzeAll(raw, options);
    if(findings != nullptr) { collectFindings(raw, *findings); }
    auto& chip = raw.chip;
    groupDerived(chip.peripherals, raw.derived);
    splitModeClusters(chip.peripherals, raw.modeClusters, raw.derived);
    reportUngrouped(raw.derived);
    stripPeripheralPrefixes(chip.peripherals);
    renameGroupMembers(chip.peripherals);
    dedupeInterrupts(chip.interrupts);
    return std::move(chip);
}

Chip chipFromSVD(pugi::xml_node const& device) {
    return chipFromSVD(device,
                       ConvertOptions{.guard              = WriteOnlyGuard::off,
                                      .writeOnlyMask      = false,
                                      .writeOnlyRegisters = WriteOnlyRegisters::off},
                       nullptr);
}

}   // namespace Svd
