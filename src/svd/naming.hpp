#pragma once

// Names: what an SVD name becomes in C++ (sanitizeName, removePercentS), descriptions on one line,
// and the passes that rename a chip's peripherals and registers after reading - derived peripherals
// grouped into instance groups and the names of clashing groups, the peripheral prefix stripped from
// register, field and value names, a register named like its group.

#include "svd/model.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace Svd {

// The name without its `[%s]`, `_%s` or `%s` (the first of them found, in that order).
[[nodiscard]] std::string removePercentS(std::string name) noexcept;

// A C++ identifier: punctuation becomes `_` (with `full`, `[`, `]` and `%` too), a keyword or a name
// that starts with a digit gets a leading `_`, `%s` goes, no `__`.
[[nodiscard]] std::string sanitizeName(std::string name,
                                       bool        full = false);

// One line: line breaks, tabs (also written as `\n`...) and double spaces become one space, no
// space at either end.
[[nodiscard]] std::string sanitizeDescription(std::string description) noexcept;

struct InstanceName {
    std::string   stem;
    std::uint64_t index;
};

// The instance in a peripheral's name: trailing digits (USART3 -> USART, 3), or with
// letterInstances one trailing capital (GPIOC -> GPIO, 2). nullopt for a name with neither.
[[nodiscard]] std::optional<InstanceName> splitInstanceName(std::string_view name,
                                                            bool             letterInstances);

struct InstanceGroup {
    std::string              stem;
    std::vector<AddressType> instances;
};

// The base peripheral and the ones derived from it, as instances grouped by name stem; the base's
// group first, its own instance first. Each instance number comes from the peripheral's own name
// (TC3 <- TC4 is TC 3 and 4; GPIOF <- GPIOC is GPIO 5 and 2); a base without one is instance 0
// (AC <- AC1). A derived peripheral with a stem of its own makes a group of its own (ST: UART4 <-
// USART3). Empty when nothing derives from base. Marks the members it took as processed, and those
// it leaves out (it says so on stderr).
[[nodiscard]] std::vector<InstanceGroup> groupInstances(std::string const&       baseName,
                                                        std::uint64_t            baseAddress,
                                                        std::vector<DerivedRef>& derived);

// A peripheral that instance groups were merged into (its index in the chip's peripherals) and its
// instances.
struct MergedGroup {
    std::size_t              index;
    std::vector<AddressType> instances;
};

// ST: TIM1 <- TIM8 and TIM3 <- TIM4 both have the stem TIM, with different registers; such groups
// are named after their members instead: TIM1_8, TIM3_4.
void nameClashingGroups(std::vector<Peripheral>&        peripherals,
                        std::vector<MergedGroup> const& merged);

// `<PERIPHERAL>_` in front of a register, field or value name goes (RTC_1 keeps `_1`).
void stripPeripheralPrefixes(std::vector<Peripheral>& peripherals);

// A register named like its register group would be a member named like its class (`GROUP<n>::GROUP`,
// which no compiler accepts once the group is instantiated): it gets a `_` suffix (E51 EVSYS CHANNEL[n]
// holds a register CHANNEL -> CHANNEL_). Compared as the template spells both, upper case.
void renameGroupMembers(std::vector<Peripheral>& peripherals);

}   // namespace Svd
