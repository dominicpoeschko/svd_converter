#include "svd/read.hpp"

#include "svd/naming.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <print>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace Svd {

template<SvdValue T>
T fromSVDString(std::string_view svdString) {
    if constexpr(std::is_same_v<T, std::uint64_t>) {
        std::size_t       position = 0;
        std::string const str{svdString};
        auto              value = std::stoull(str, &position, 0);
        if(position == 0) {
            throw std::runtime_error(std::format("no valid number {}", svdString));
        }
        return value;
    } else if constexpr(std::is_same_v<T, std::string>) {
        return std::string{svdString};
    } else if constexpr(std::is_same_v<T, BitRange>) {
        if(svdString.empty()) {
            throw std::runtime_error(std::format("wrong bitRange {}", svdString));
        }
        if(svdString.front() != '[' || svdString.back() != ']') {
            throw std::runtime_error(std::format("wrong bitRange {}", svdString));
        }
        auto const colonPos = svdString.find_first_of(':');
        if(colonPos == std::string::npos || colonPos != svdString.find_last_of(':')) {
            throw std::runtime_error(std::format("wrong bitRange {}", svdString));
        }
        auto msbString = svdString.substr(1, colonPos);
        auto lsbString = svdString.substr(colonPos + 1, svdString.size() - 1);

        auto msb = fromSVDString<std::uint64_t>(msbString);
        auto lsb = fromSVDString<std::uint64_t>(lsbString);
        if(lsb > msb) { throw std::runtime_error(std::format("wrong bitRange {}", svdString)); }
        return BitRange{.start = lsb, .stop = msb};
    } else if constexpr(std::is_same_v<T, DataType>) {
        auto size = fromSVDString<std::uint64_t>(svdString);
        switch(size) {
        case 8:  return DataType::u8;
        case 16: return DataType::u16;
        case 32: return DataType::u32;
        case 64: return DataType::u64;
        default: throw std::runtime_error(std::format("wrong size {} {}", size, svdString));
        }
    } else if constexpr(std::is_same_v<T, Access>) {
        if(svdString == "read-only") { return Access::readOnly; }
        if(svdString == "write-only") { return Access::writeOnly; }
        if(svdString == "read-write") { return Access::readWrite; }
        if(svdString == "writeOnce") { return Access::writeOnce; }
        // CMSIS-SVD.xsd accessType spells it read-writeOnce (it was read as "read-WriteOnce", which no SVD uses)
        if(svdString == "read-writeOnce") { return Access::readWriteOnce; }
        throw std::runtime_error(std::format("bad access {}", svdString));
    } else if constexpr(std::is_same_v<T, ModifiedWriteValues>) {
        if(svdString.empty()) { return ModifiedWriteValues::empty; }
        if(svdString == "oneToClear") { return ModifiedWriteValues::oneToClear; }
        if(svdString == "oneToSet") { return ModifiedWriteValues::oneToSet; }
        if(svdString == "oneToToggle") { return ModifiedWriteValues::oneToToggle; }
        if(svdString == "zeroToClear") { return ModifiedWriteValues::zeroToClear; }
        if(svdString == "zeroToSet") { return ModifiedWriteValues::zeroToSet; }
        if(svdString == "zeroToToggle") { return ModifiedWriteValues::zeroToToggle; }
        if(svdString == "clear") { return ModifiedWriteValues::clear; }
        if(svdString == "set") { return ModifiedWriteValues::set; }
        if(svdString == "modify") { return ModifiedWriteValues::modify; }
        throw std::runtime_error(std::format("bad modifiedWriteValues {}", svdString));
    } else {
        static_assert(std::is_same_v<T, ReadAction>);
        if(svdString.empty()) { return ReadAction::empty; }
        if(svdString == "clear") { return ReadAction::clear; }
        if(svdString == "set") { return ReadAction::set; }
        if(svdString == "modify") { return ReadAction::modify; }
        if(svdString == "modifyExternal") { return ReadAction::modifyExternal; }
        throw std::runtime_error(std::format("bad ReadAction {}", svdString));
    }
}

template std::uint64_t       fromSVDString<std::uint64_t>(std::string_view);
template std::string         fromSVDString<std::string>(std::string_view);
template BitRange            fromSVDString<BitRange>(std::string_view);
template DataType            fromSVDString<DataType>(std::string_view);
template Access              fromSVDString<Access>(std::string_view);
template ModifiedWriteValues fromSVDString<ModifiedWriteValues>(std::string_view);
template ReadAction          fromSVDString<ReadAction>(std::string_view);

namespace {
    template<typename T>
    auto getDefaultSVD(pugi::xml_node const& node,
                       std::string_view      name,
                       T const&              defaultValue) -> T {
        auto const xmlValue = node.child(name);
        if(xmlValue.empty()) { return defaultValue; }
        return fromSVDString<T>(xmlValue.text().as_string());
    }

    template<typename T>
    auto getOptionalSVD(pugi::xml_node const& node,
                        std::string_view      name) -> std::optional<T> {
        auto const xmlValue = node.child(name);
        if(xmlValue.empty()) { return std::nullopt; }
        return fromSVDString<T>(xmlValue.text().as_string());
    }

    template<typename T>
    auto getCheckedSVD(pugi::xml_node const& node,
                       std::string_view      name,
                       std::string_view      msg = "") -> T {
        auto const xmlValue = node.child(name);
        if(xmlValue.empty()) {
            throw std::runtime_error(std::string{name} + " not found in " + std::string{msg});
        }
        return fromSVDString<T>(xmlValue.text().as_string());
    }

    // A register's or field's <dim> and <dimIncrement>; both 0 without <dim>.
    struct Dim {
        std::uint64_t count     = 0;
        std::uint64_t increment = 0;
    };

    Dim readDim(pugi::xml_node const& node,
                std::string_view      what) {
        if(node.child("dim").empty()) { return {}; }
        Dim const dim{.count     = getCheckedSVD<std::uint64_t>(node, "dim", what),
                      .increment = getCheckedSVD<std::uint64_t>(node, "dimIncrement", what)};
        if(dim.count == 0) { throw std::runtime_error("register dim should not be 0"); }
        return dim;
    }

    // The register properties an SVD passes down (its registerPropertiesGroup): device -> peripheral
    // -> cluster (a mode cluster, a dim cluster) -> register, each level overriding them with its own
    // <size>, <access>, <resetValue> and <resetMask>.
    struct RegisterProperties {
        DataType      size       = DataType::u32;
        Access        access     = Access::readWrite;
        std::uint64_t resetValue = 0;
        std::uint64_t resetMask  = ~std::uint64_t{0};
    };

    RegisterProperties readProperties(pugi::xml_node const&     node,
                                      RegisterProperties const& inherited) {
        return {.size       = getDefaultSVD(node, "size", inherited.size),
                .access     = getDefaultSVD(node, "access", inherited.access),
                .resetValue = getDefaultSVD(node, "resetValue", inherited.resetValue),
                .resetMask  = getDefaultSVD(node, "resetMask", inherited.resetMask)};
    }

    // What a register passes on to its fields.
    struct FieldDefaults {
        Access              access;
        DataType            dataType;
        std::uint64_t       resetValue;
        std::uint64_t       resetMask;
        ModifiedWriteValues modifiedWriteValues;
        ReadAction          readAction;
        bool                reportResets = true;   // say a reset value no enum value matches
    };

    Value valueFromSVD(pugi::xml_node const& value) {
        Value enumValue;
        auto  maybeName = getOptionalSVD<std::string>(value, "name");
        enumValue.description
          = sanitizeDescription(getDefaultSVD(value, "description", std::string{}));
        enumValue.value = getCheckedSVD<std::uint64_t>(value, "value");
        if(maybeName) {
            enumValue.name = sanitizeName(*maybeName);
        } else {
            if(!enumValue.description.empty()) {
                enumValue.name = sanitizeName(enumValue.description, true);
            } else {
                enumValue.name = "_" + std::to_string(enumValue.value);
            }
        }
        return enumValue;
    }

    // `where`: the register as the SVD names it, `<cluster or peripheral>::<register>`, for the
    // reset value message.
    Field fieldFromSVD(pugi::xml_node const& field,
                       FieldDefaults const&  defaults,
                       std::string_view      where) {
        Field fieldResult;
        fieldResult.name = sanitizeName(getCheckedSVD<std::string>(field, "name", "Field"));
        fieldResult.description
          = sanitizeDescription(getDefaultSVD(field, "description", std::string{}));

        auto bitRange = getOptionalSVD<BitRange>(field, "bitRange");
        if(bitRange) {
            fieldResult.startBit = bitRange->start;
            fieldResult.stopBit  = bitRange->stop;
        } else {
            fieldResult.startBit = getCheckedSVD<std::uint64_t>(field, "bitOffset", "Field");
            fieldResult.stopBit
              = getCheckedSVD<std::uint64_t>(field, "bitWidth", "Field") + fieldResult.startBit - 1;
        }

        fieldResult.access   = getDefaultSVD(field, "access", defaults.access);
        fieldResult.dataType = defaults.dataType;
        fieldResult.resetValue
          = makeResetValue(defaults.resetValue, fieldResult.startBit, fieldResult.stopBit);
        fieldResult.type = FieldType::normal;
        fieldResult.modifiedWriteValues
          = getDefaultSVD(field, "modifiedWriteValues", defaults.modifiedWriteValues);
        fieldResult.readAction = getDefaultSVD(field, "readAction", defaults.readAction);
        // A writeConstraint range of one value: every write of the register must carry it (a key).
        if(auto const range = field.child("writeConstraint").child("range"); !range.empty()) {
            fieldResult.key = getCheckedSVD<std::uint64_t>(range, "minimum", "writeConstraint")
                           == getCheckedSVD<std::uint64_t>(range, "maximum", "writeConstraint");
        }
        // `<!-- Kvasir: write-only accepted: ... -->`: seen only when the document was loaded with
        // pugi::parse_comments (main.cpp does)
        for(auto const& node : field.children()) {
            if(node.type() == pugi::node_comment
               && std::string_view{node.value()}.contains("Kvasir: write-only accepted"))
            {
                fieldResult.writeOnlyAccepted = true;
            }
        }

        auto const dim           = readDim(field, "Field");
        fieldResult.dim          = dim.count;
        fieldResult.dimIncrement = dim.increment;
        if(fieldResult.dim != 0) {
            fieldResult.repType = RepeatType::cluster;
            fieldResult.name    = removePercentS(fieldResult.name);
        }

        auto values = field.child("enumeratedValues");
        if(values.empty()) { return fieldResult; }

        fieldResult.type = FieldType::typeEnum;
        for(auto const& enumValueNode : values.children("enumeratedValue")) {
            fieldResult.values.emplace_back(valueFromSVD(enumValueNode));
        }

        // A write-only field has no value to read back after reset (the RP2350 POWMAN PASSWORD key
        // reads 0, its one enumerated value is 0x5AFE), and one outside <resetMask> no reset value:
        // nothing to check. Otherwise a value matches when it equals the reset value in every bit but
        // the field's bits outside <resetMask> (a value wider than the field never matches).
        if(!hasResetValue(fieldResult, defaults.resetMask)) { return fieldResult; }
        std::uint64_t const undefined
          = fieldResult.startBit < 64
            ? ~(defaults.resetMask >> fieldResult.startBit)
                & maskFromRange(fieldResult.stopBit - fieldResult.startBit, 0)
            : 0;
        bool found = fieldResult.access == Access::writeOnly;
        for(auto const& enumValue : fieldResult.values) {
            if(((enumValue.value ^ fieldResult.resetValue) & ~undefined) == 0) {
                found = true;
                break;
            }
        }
        if(!found && defaults.reportResets) {
            std::print(stderr, "no valid ResetValue in {}::{}\n", where, fieldResult.name);
        }
        return fieldResult;
    }

    // `<!-- Kvasir: write key NAME[hi:lo] = 0xVALUE: <why, datasheet line> -->` inside a <register>: every write
    // of the register must carry VALUE in bits hi:lo, but the SVD cannot name them as a field because read-only fields
    // sit there (an SVD forbids overlapping fields; RP2350 POWMAN TIMER, SEQ_CFG, CHIP_RESET: "Protected registers
    // obviously don't have writeable fields in the top 16 bits, however they may have read-only fields in that range",
    // RP2350 datasheet 6.4). The converter adds the key as a write-only field of its own with one enumerated value, like
    // a field with a one-value writeConstraint (POWMAN VREG's PASSWORD): Register::Semantics::mustSupply then makes
    // every write name it, and the read-only fields keep their reads. Throws on a marker it cannot read.
    std::optional<Field> writeKeyFromComment(pugi::xml_node const& reg,
                                             DataType              dataType) {
        static constexpr std::string_view marker = "Kvasir: write key ";
        for(auto const& node : reg.children()) {
            if(node.type() != pugi::node_comment) { continue; }
            std::string_view const text{node.value()};
            auto const             at = text.find(marker);
            if(at == std::string_view::npos) { continue; }
            auto const rest    = text.substr(at + marker.size());
            auto const open    = rest.find('[');
            auto const colon   = rest.find(':', open);
            auto const close   = rest.find(']', colon);
            auto const equals  = rest.find('=', close);
            auto const valueAt = rest.find_first_not_of(' ', equals + 1);
            auto const valueTo = rest.find_first_of(": \n", valueAt);
            if(open == std::string_view::npos || colon == std::string_view::npos
               || close == std::string_view::npos || equals == std::string_view::npos
               || valueAt == std::string_view::npos)
            {
                throw std::runtime_error(
                  std::format("register {}: cannot read the write key marker (\"{}NAME[hi:lo] = "
                              "0xVALUE: why\")",
                              reg.child("name").text().as_string(),
                              marker));
            }
            Field key;
            key.name        = sanitizeName(std::string{rest.substr(0, open)});
            key.description = "Write key: every write must carry it (Kvasir: write key marker)";
            key.stopBit     = fromSVDString<std::uint64_t>(rest.substr(open + 1, colon - open - 1));
            key.startBit = fromSVDString<std::uint64_t>(rest.substr(colon + 1, close - colon - 1));
            key.access   = Access::writeOnly;
            key.dataType = dataType;
            key.key      = true;
            key.type     = FieldType::typeEnum;
            key.values.push_back(Value{
              .name        = key.name,
              .description = "",
              .value = fromSVDString<std::uint64_t>(rest.substr(valueAt, valueTo - valueAt))});
            if(key.startBit > key.stopBit || key.stopBit >= dataTypeSize(dataType)) {
                throw std::runtime_error(
                  std::format("register {}: write key bits {}:{} out of range",
                              reg.child("name").text().as_string(),
                              key.stopBit,
                              key.startBit));
            }
            return key;
        }
        return std::nullopt;
    }

    // `node` and the elements it derives from, `node` first and the one that derives from nothing
    // last (C from B from A: C, B, A). derivedFrom names a sibling `kind` element by its SVD <name>.
    // `complete` is false when a base is missing; `missing` then names it, `from` the element that
    // derives from it.
    struct DerivedChain {
        std::vector<pugi::xml_node> nodes;
        bool                        complete = true;
        std::string                 missing;
        std::string                 from;
    };

    DerivedChain derivedChain(pugi::xml_node const& node,
                              char const*           kind) {
        DerivedChain chain;
        chain.nodes.push_back(node);
        while(true) {
            auto const base = chain.nodes.back().attribute("derivedFrom");
            if(base.empty()) { return chain; }
            auto const     baseName = sanitizeName(base.value());
            pugi::xml_node found;
            for(auto const& sibling : node.parent().children(kind)) {
                if(sanitizeName(sibling.child("name").text().as_string()) == baseName) {
                    found = sibling;
                    break;
                }
            }
            if(found.empty()) {
                chain.complete = false;
                chain.missing  = base.value();
                chain.from     = chain.nodes.back().child("name").text().as_string();
                return chain;
            }
            if(std::ranges::find(chain.nodes, found) != chain.nodes.end()) {
                throw std::runtime_error(std::format("{} {}: derivedFrom loops back to {}",
                                                     kind,
                                                     node.child("name").text().as_string(),
                                                     base.value()));
            }
            chain.nodes.push_back(found);
        }
    }

    // The first element of the chain that has `element` itself (a derived element's own <dim>,
    // <description>, ... override its base's); the base without it when none has.
    pugi::xml_node firstWith(std::vector<pugi::xml_node> const& chain,
                             char const*                        element) {
        for(auto const& node : chain) {
            if(!node.child(element).empty()) { return node; }
        }
        return chain.back();
    }

    // The register properties along a chain: the base's over `inherited`, each derived element's
    // own over its base's.
    RegisterProperties chainProperties(std::vector<pugi::xml_node> const& chain,
                                       RegisterProperties const&          inherited) {
        RegisterProperties properties = inherited;
        for(auto const& node : std::views::reverse(chain)) {
            properties = readProperties(node, properties);
        }
        return properties;
    }

    bool hasOwnProperties(pugi::xml_node const& node) {
        return !node.child("size").empty() || !node.child("access").empty()
            || !node.child("resetValue").empty() || !node.child("resetMask").empty();
    }

    // One register; `chain` is the register and what it derives from (derivedChain): its name,
    // displayName and addressOffset are its own, every other element the first of the chain that
    // has it, the fields those of the first that has <fields>. `place`: the SVD name of the cluster
    // or peripheral the register sits in.
    Register registerFromSVD(std::vector<pugi::xml_node> const& chain,
                             RegisterProperties const&          inherited,
                             std::string const&                 place) {
        auto const& reg = chain.front();
        Register    registerResult;
        auto const  svdName    = getCheckedSVD<std::string>(reg, "name", "Register");
        registerResult.svdName = sanitizeName(svdName);
        registerResult.name    = registerResult.svdName;
        auto displayName       = getOptionalSVD<std::string>(reg, "displayName");
        if(displayName) { registerResult.name = sanitizeName(*displayName); }
        registerResult.addressOffset
          = getCheckedSVD<std::uint64_t>(reg, "addressOffset", "Register");

        registerResult.description = sanitizeDescription(
          getDefaultSVD(firstWith(chain, "description"), "description", std::string{}));
        auto const properties     = chainProperties(chain, inherited);
        registerResult.resetValue = properties.resetValue;
        registerResult.dataType   = properties.size;
        registerResult.resetMask
          = properties.resetMask & maskFromRange(dataTypeSize(registerResult.dataType) - 1, 0);
        registerResult.svdAccess = properties.access;
        // A derived register that changes none of them has its base's reset values: the base has
        // said what is wrong with them.
        bool const reportResets
          = chain.size() == 1
         || std::ranges::any_of(chain | std::views::take(chain.size() - 1), hasOwnProperties);
        FieldDefaults const defaults{
          .access              = properties.access,
          .dataType            = registerResult.dataType,
          .resetValue          = registerResult.resetValue,
          .resetMask           = registerResult.resetMask,
          .modifiedWriteValues = getDefaultSVD(firstWith(chain, "modifiedWriteValues"),
                                               "modifiedWriteValues",
                                               ModifiedWriteValues::empty),
          .readAction
          = getDefaultSVD(firstWith(chain, "readAction"), "readAction", ReadAction::empty),
          .reportResets = reportResets};

        auto const dim              = readDim(firstWith(chain, "dim"), "Register");
        registerResult.dim          = dim.count;
        registerResult.dimIncrement = dim.increment;
        if(registerResult.dim != 0) {
            registerResult.type = RepeatType::cluster;
            registerResult.name = removePercentS(registerResult.name);
        }

        auto const withFields = firstWith(chain, "fields");
        auto       fields     = withFields.child("fields");
        // No fields is valid SVD: the register is one value (RP USB_DPRAM buffers), reached through
        // the generated FULLREGISTER.
        if(fields.empty()) { return registerResult; }

        std::string const where = place + "::" + svdName;
        for(auto const& fieldNode : fields.children("field")) {
            auto fieldFromSvd = fieldFromSVD(fieldNode, defaults, where);

            // An element past the register's width is a broken SVD: its masks would be computed
            // with shifts past the 64 bits they are held in.
            std::uint64_t const elements = fieldFromSvd.dim == 0 ? 1 : fieldFromSvd.dim;
            auto const lastStop = fieldFromSvd.stopBit + (elements - 1) * fieldFromSvd.dimIncrement;
            if(lastStop >= dataTypeSize(registerResult.dataType)) {
                throw std::runtime_error(
                  std::format("field {} of register {} ends at bit {}, past the register's {} bits",
                              fieldFromSvd.name,
                              registerResult.name,
                              lastStop,
                              dataTypeSize(registerResult.dataType)));
            }
            registerResult.fields.push_back(std::move(fieldFromSvd));
        }
        if(auto key = writeKeyFromComment(withFields, registerResult.dataType)) {
            registerResult.fields.push_back(std::move(*key));
        }
        return registerResult;
    }

    // The registers of one <registers> or <cluster>, the derived ones (derivedFrom, also chained:
    // C from B from A) after the others. derivedFrom names the base by its SVD <name>, not its
    // <displayName>; the derived register keeps its own C name.
    std::vector<Register> makeRegister(pugi::xml_object_range<pugi::xml_named_node_iterator> regs,
                                       RegisterProperties const& properties,
                                       std::string const&        place) {
        std::vector<Register>       parsedRegs;
        std::vector<pugi::xml_node> derivedNodes;
        for(auto const& registerNode : regs) {
            if(!registerNode.attribute("derivedFrom").empty()) {
                derivedNodes.push_back(registerNode);
                continue;
            }
            parsedRegs.push_back(registerFromSVD({registerNode}, properties, place));
        }

        for(auto const& derivedNode : derivedNodes) {
            auto const chain = derivedChain(derivedNode, "register");
            if(!chain.complete) {
                throw std::runtime_error(
                  std::format("register {}::{} derives from {}, which {} does not have",
                              place,
                              sanitizeName(chain.from),
                              chain.missing,
                              place));
            }
            parsedRegs.push_back(registerFromSVD(chain.nodes, properties, place));
        }

        return parsedRegs;
    }

    RegisterGroup registerGroupFromSVD(pugi::xml_node            regg,
                                       RegisterProperties const& inherited) {
        RegisterGroup     registerGroup;
        std::string const svdName = getCheckedSVD<std::string>(regg, "name", "RegisterGroup");
        registerGroup.name        = sanitizeName(svdName);
        registerGroup.description
          = sanitizeDescription(getDefaultSVD(regg, "description", std::string{""}));
        registerGroup.dim = getCheckedSVD<std::uint64_t>(regg, "dim", "RegisterGroup");
        registerGroup.dimIncrement
          = getCheckedSVD<std::uint64_t>(regg, "dimIncrement", "RegisterGroup");
        registerGroup.addressOffset
          = getCheckedSVD<std::uint64_t>(regg, "addressOffset", "RegisterGroup");

        registerGroup.registers
          = makeRegister(regg.children("register"), readProperties(regg, inherited), svdName);

        return registerGroup;
    }

    // A cluster without <dim> is one of a peripheral's modes (SERCOM's USART / SPI / I2CM, USB's DEVICE /
    // HOST) and becomes a peripheral of its own, <PERIPHERAL>_<CLUSTER>. Its registers sit at the
    // cluster's addressOffset; a cluster with <dim> nested in it (USB DEVICE's DEVICE_ENDPOINT[8]) is
    // that peripheral's register group. The parent's own register groups are not the mode's.
    Peripheral clusterFromSVD(pugi::xml_node            cluster,
                              Peripheral const&         peripheralRef,
                              RegisterProperties const& inherited) {
        Peripheral peripheral = peripheralRef;
        peripheral.name
          = getCheckedSVD<std::string>(cluster, "name", "Cluster " + peripheralRef.name);
        peripheral.description
          += " " + sanitizeDescription(getDefaultSVD(cluster, "description", std::string{}));

        auto const clusterOffset = getDefaultSVD(cluster, "addressOffset", std::uint64_t{0});
        auto const properties    = readProperties(cluster, inherited);

        peripheral.registers
          = makeRegister(cluster.children("register"), properties, peripheral.name);
        for(auto& reg : peripheral.registers) { reg.addressOffset += clusterOffset; }

        peripheral.registerGroups.clear();
        for(auto const& nested : cluster.children("cluster")) {
            if(nested.child("dim").empty()) {
                std::print(stderr,
                           "cluster {} in {} of {}: a cluster without dim inside a cluster is not "
                           "supported, skipped\n",
                           getDefaultSVD(nested, "name", std::string{"?"}),
                           peripheral.name,
                           peripheralRef.name);
                continue;
            }
            auto group = registerGroupFromSVD(nested, properties);
            group.addressOffset += clusterOffset;
            peripheral.registerGroups.push_back(std::move(group));
        }

        return peripheral;
    }

    // A derived peripheral that has registers of its own: it sets <size>, <access>, <resetValue> or
    // <resetMask>, or has its own <registers> (which replace its base's).
    bool hasOwnRegisters(pugi::xml_node const& peripheral) {
        return hasOwnProperties(peripheral) || !peripheral.child("registers").empty();
    }

    // A derived peripheral without registers of its own (hasOwnRegisters) is one more instance of
    // its base (chip.cpp groups it, a chain C from B from A into A's group); one with them is read
    // as a peripheral of its own - its own <registers> or the base's, under its properties - with
    // its own <description> if it has one.
    std::variant<Peripheral,
                 ModeClusters,
                 DerivedRef>
    peripheralFromSVD(pugi::xml_node const&     peripheral,
                      RegisterProperties const& device,
                      DataType                  addressType) {
        Peripheral peripheralResult;
        peripheralResult.name = getCheckedSVD<std::string>(peripheral, "name", "Peripheral");
        peripheralResult.baseAddresses.push_back(
          {0,
           getCheckedSVD<std::uint64_t>(peripheral,
                                        "baseAddress",
                                        "Peripheral " + peripheralResult.name)});

        auto const chain = derivedChain(peripheral, "peripheral");
        if(chain.nodes.size() > 1 || !chain.complete) {
            if(!hasOwnRegisters(peripheral) || !chain.complete) {
                // The group of the first base that is a peripheral of its own: one that derives from
                // nothing or has registers of its own. A missing base is the group's to report.
                std::string base = chain.missing;
                for(std::size_t i = 1; i < chain.nodes.size(); ++i) {
                    auto const& node = chain.nodes[i];
                    if(node.attribute("derivedFrom").empty() || hasOwnRegisters(node)) {
                        base = node.child("name").text().as_string();
                        break;
                    }
                }
                return DerivedRef{.name      = peripheralResult.name,
                                  .base      = std::move(base),
                                  .address   = peripheralResult.baseAddresses.back().address,
                                  .processed = false};
            }
        }

        peripheralResult.description = sanitizeDescription(
          getDefaultSVD(firstWith(chain.nodes, "description"), "description", std::string{}));
        peripheralResult.addressType = addressType;
        peripheralResult.type        = RepeatType::normal;

        auto registers = firstWith(chain.nodes, "registers").child("registers");
        if(registers.empty()) {
            // A peripheral that only names interrupts (RP2350 SPARE_IRQ) has no registers on purpose.
            if(peripheral.child("interrupt").empty()) {
                std::print(stderr, "no registers in {}\n", peripheralResult.name);
            }
            return peripheralResult;
        }

        auto const properties = chainProperties(chain.nodes, device);
        peripheralResult.registers
          = makeRegister(registers.children("register"), properties, peripheralResult.name);

        auto const cluster = registers.children("cluster");
        for(auto const& clusterNode : cluster) {
            if(clusterNode.child("dim").empty()) { continue; }
            peripheralResult.registerGroups.push_back(
              registerGroupFromSVD(clusterNode, properties));
        }

        ModeClusters clusterResult{.parent = peripheralResult.name, .modes = {}};
        for(auto const& clusterNode : cluster) {
            if(!clusterNode.child("dim").empty()) { continue; }
            clusterResult.modes.push_back(
              clusterFromSVD(clusterNode, peripheralResult, properties));
        }

        if(!clusterResult.modes.empty()) { return clusterResult; }

        return peripheralResult;
    }
}   // namespace

RawDevice readDevice(pugi::xml_node const& device) {
    RawDevice raw;
    auto&     chip   = raw.chip;
    chip.name        = getCheckedSVD<std::string>(device, "name", "Chip");
    chip.description = sanitizeDescription(getDefaultSVD(device, "description", std::string{}));
    auto const width = getDefaultSVD(device, "width", DataType::u32);
    auto const properties = readProperties(device, RegisterProperties{});

    auto peripherals = device.child("peripherals");
    if(peripherals.empty()) { throw std::runtime_error("no peripherals"); }

    for(auto const& peripheral : peripherals.children("peripheral")) {
        for(auto const& irq : peripheral.children("interrupt")) {
            chip.interrupts.push_back({getCheckedSVD<std::string>(irq, "name", "interrupt"),
                                       getCheckedSVD<std::uint64_t>(irq, "value", "interrupt")});
        }
        auto peripheralVariant = peripheralFromSVD(peripheral, properties, width);
        if(std::holds_alternative<DerivedRef>(peripheralVariant)) {
            raw.derived.push_back(std::move(std::get<DerivedRef>(peripheralVariant)));
        } else if(std::holds_alternative<ModeClusters>(peripheralVariant)) {
            raw.modeClusters.push_back(std::move(std::get<ModeClusters>(peripheralVariant)));
        } else {
            chip.peripherals.push_back(std::move(std::get<Peripheral>(peripheralVariant)));
        }
    }
    return raw;
}

}   // namespace Svd
