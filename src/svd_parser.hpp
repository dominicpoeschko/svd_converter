#pragma once

#include "fmt_wrapper.hpp"
#include "inja_wrapper.hpp"
#include "svd_types.hpp"

#include <algorithm>
#include <map>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <variant>
#include <vector>

namespace {
constexpr std::uint64_t maskFromRange(std::uint64_t high,
                                      std::uint64_t low) noexcept {
    return (0xFFFFFFFFFFFFFFFFULL >> (63 - (high - low))) << low;
}

constexpr std::uint64_t makeResetValue(std::uint64_t parentResetValue,
                                       std::uint64_t startBit,
                                       std::uint64_t stopBit) noexcept {
    std::uint64_t const mask = maskFromRange(stopBit, startBit);
    return (parentResetValue & mask) >> startBit;
}

constexpr std::uint64_t clearBits(std::uint64_t value,
                                  std::uint64_t startBit,
                                  std::uint64_t stopBit) noexcept {
    std::uint64_t const mask = maskFromRange(stopBit, startBit);
    return value & (~mask);
}

using derived_t = std::tuple<std::string, std::string, std::uint64_t, bool>;
using cluster_t = std::pair<std::vector<Peripheral>, std::string>;

[[nodiscard]] std::string remove_PercentS(std::string name) noexcept {
    if(auto const pos = name.find("[%s]"); pos != std::string::npos) {
        name.erase(pos, 4);
    } else if(auto const pos2 = name.find("_%s"); pos2 != std::string::npos) {
        name.erase(pos2, 3);
    } else if(auto const pos3 = name.find("%s"); pos3 != std::string::npos) {
        name.erase(pos3, 2);
    }
    return name;
}

inline std::string sanitizeName(std::string name,
                                bool        full = false) {
    static constexpr std::array cppKeywords{
      "alignas",   "alignof",  "and",      "asm",      "auto",    "bitand",  "bitor",     "bool",
      "break",     "case",     "catch",    "char",     "class",   "compl",   "concept",   "const",
      "constexpr", "continue", "decltype", "default",  "delete",  "do",      "double",    "else",
      "enum",      "explicit", "export",   "extern",   "false",   "float",   "for",       "friend",
      "goto",      "if",       "inline",   "int",      "long",    "mutable", "namespace", "new",
      "noexcept",  "not",      "nullptr",  "operator", "or",      "private", "protected", "public",
      "register",  "requires", "return",   "short",    "signed",  "sizeof",  "static",    "struct",
      "switch",    "template", "this",     "throw",    "true",    "try",     "typedef",   "typeid",
      "typename",  "union",    "unsigned", "using",    "virtual", "void",    "volatile",  "while",
      "xor"};

    static constexpr std::array kvasirKeywords{"value"};
    for(auto& character : name) {
        if(full) {
            if(std::ispunct(character) != 0) {
                character = '_';
            }
        } else {
            if(std::ispunct(character) != 0 && character != '[' && character != ']'
               && character != '%')
            {
                character = '_';
            }
        }
    }

    auto sanitizeKeyword = [&](auto const& keywords) {
        for(auto const& keyword : keywords) {
            std::string lowercaseValue = name;
            std::ranges::transform(lowercaseValue, lowercaseValue.begin(), [](auto character) {
                return std::tolower(character);
            });
            if(keyword == lowercaseValue) {
                name.insert(0, "_");
                break;
            }
        }
    };

    sanitizeKeyword(cppKeywords);
    sanitizeKeyword(kvasirKeywords);

    if(!name.empty()) {
        if(std::isdigit(name.front()) != 0) {
            name.insert(0, "_");
        }
    }

    for(auto& character : name) {
        if(std::isspace(character) != 0) {
            character = '_';
        }
    }

    name = remove_PercentS(name);

    auto position = name.find("__");
    while(position != std::string::npos) {
        name.erase(position, 1);
        position = name.find("__");
    }

    return name;
}

[[nodiscard]] std::string sanitizeDescription(std::string description) noexcept {
    auto replace = [&](std::string_view needle) {
        while(true) {
            auto position = std::ranges::search(description, needle).begin();
            if(position == description.end()) {
                break;
            }
            auto erasePosition
              = description.erase(position,
                                  std::next(position, static_cast<int>(needle.size()) - 1));
            *erasePosition = ' ';
        }
    };
    replace("\n");
    replace("\r");
    replace("\t");
    replace("\\n");
    replace("\\r");
    replace("\\t");
    replace("  ");
    if(description.starts_with(" ")) {
        description.erase(0, 1);
    }
    if(description.ends_with(" ")) {
        description.erase(std::next(description.end(), -1), description.end());
    }

    return description;
}

template<typename T>
T fromSVDString(std::string_view svdString) {
    if constexpr(std::is_same_v<T, std::uint64_t>) {
        std::size_t       position = 0;
        std::string const str{svdString};
        auto              value = std::stoull(str, &position, 0);
        if(position == 0) {
            throw std::runtime_error(fmt::format("no valid number {}", svdString));
        }
        return value;
    } else if constexpr(std::is_same_v<T, std::string>) {
        return std::string{svdString};
    } else if constexpr(std::is_same_v<T, BitRange>) {
        if(svdString.empty()) {
            throw std::runtime_error(fmt::format("wrong bitRange {}", svdString));
        }
        if(svdString.front() != '[' || svdString.back() != ']') {
            throw std::runtime_error(fmt::format("wrong bitRange {}", svdString));
        }
        auto const colonPos = svdString.find_first_of(':');
        if(colonPos == std::string::npos || colonPos != svdString.find_last_of(':')) {
            throw std::runtime_error(fmt::format("wrong bitRange {}", svdString));
        }
        auto msbString = svdString.substr(1, colonPos);
        auto lsbString = svdString.substr(colonPos + 1, svdString.size() - 1);

        auto msb = fromSVDString<std::uint64_t>(msbString);
        auto lsb = fromSVDString<std::uint64_t>(lsbString);
        if(lsb > msb) {
            throw std::runtime_error(fmt::format("wrong bitRange {}", svdString));
        }
        return BitRange{.start = lsb, .stop = msb};
    } else if constexpr(std::is_same_v<T, DataType>) {
        auto size = fromSVDString<std::uint64_t>(svdString);
        switch(size) {
        case 8:  return DataType::u8;
        case 16: return DataType::u16;
        case 32: return DataType::u32;
        case 64: return DataType::u64;
        default: throw std::runtime_error(fmt::format("wrong size {} {}", size, svdString));
        }
    } else if constexpr(std::is_same_v<T, Access>) {
        if(svdString == "read-only") {
            return Access::readOnly;
        }
        if(svdString == "write-only") {
            return Access::writeOnly;
        }
        if(svdString == "read-write") {
            return Access::readWrite;
        }
        if(svdString == "writeOnce") {
            return Access::writeOnce;
        }
        if(svdString == "read-WriteOnce") {
            return Access::readWriteOnce;
        }
        throw std::runtime_error(fmt::format("bad access {}", svdString));
    } else if constexpr(std::is_same_v<T, ModifiedWriteValues>) {
        if(svdString.empty()) {
            return ModifiedWriteValues::empty;
        }
        if(svdString == "oneToClear") {
            return ModifiedWriteValues::oneToClear;
        }
        if(svdString == "oneToSet") {
            return ModifiedWriteValues::oneToSet;
        }
        if(svdString == "oneToToggle") {
            return ModifiedWriteValues::oneToToggle;
        }
        if(svdString == "zeroToClear") {
            return ModifiedWriteValues::zeroToClear;
        }
        if(svdString == "zeroToSet") {
            return ModifiedWriteValues::zeroToSet;
        }
        if(svdString == "zeroToToggle") {
            return ModifiedWriteValues::zeroToToggle;
        }
        if(svdString == "clear") {
            return ModifiedWriteValues::clear;
        }
        if(svdString == "set") {
            return ModifiedWriteValues::set;
        }
        if(svdString == "modify") {
            return ModifiedWriteValues::modify;
        }
        throw std::runtime_error(fmt::format("bad modifiedWriteValues {}", svdString));
    } else if constexpr(std::is_same_v<T, ReadAction>) {
        if(svdString.empty()) {
            return ReadAction::empty;
        }
        if(svdString == "clear") {
            return ReadAction::clear;
        }
        if(svdString == "set") {
            return ReadAction::set;
        }
        if(svdString == "modify") {
            return ReadAction::modify;
        }
        if(svdString == "modifyExternal") {
            return ReadAction::modifyExternal;
        }
        throw std::runtime_error(fmt::format("bad ReadAction {}", svdString));
    } else {
        static_assert(false, "Unsupported type for fromSVDString");
    }
}

template<typename T>
auto getDefaultSVD(pugi::xml_node const& node,
                   std::string_view      name,
                   T const&              defaultValue) -> T {
    auto const xmlValue = node.child(name);
    if(xmlValue.empty()) {
        return defaultValue;
    }
    return fromSVDString<T>(xmlValue.text().as_string());
}

template<typename T>
auto getOptionalSVD(pugi::xml_node const& node,
                    std::string_view      name) -> std::optional<T> {
    auto const xmlValue = node.child(name);
    if(xmlValue.empty()) {
        return std::nullopt;
    }
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

constexpr std::uint64_t DataTypeSize(DataType dataType) noexcept {
    switch(dataType) {
    case DataType::u8:  return 8;
    case DataType::u16: return 16;
    case DataType::u32: return 32;
    case DataType::u64: return 64;
    }
    return 0;
}

inline Value ValueFromSVD(pugi::xml_node const& value) {
    Value enumValue;
    auto  maybeName       = getOptionalSVD<std::string>(value, "name");
    enumValue.description = sanitizeDescription(getDefaultSVD(value, "description", std::string{}));
    enumValue.value       = getCheckedSVD<std::uint64_t>(value, "value");
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

inline Field FieldFromSVD(pugi::xml_node const& field,
                          Access                access,
                          DataType              dataType,
                          std::uint64_t         resetValue,
                          ModifiedWriteValues   modifiedWriteValues,
                          ReadAction            readAction) {
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

    fieldResult.access     = getDefaultSVD(field, "access", access);
    fieldResult.dataType   = dataType;
    fieldResult.resetValue = makeResetValue(resetValue, fieldResult.startBit, fieldResult.stopBit);
    fieldResult.type       = FieldType::normal;
    fieldResult.modifiedWriteValues
      = getDefaultSVD(field, "modifiedWriteValues", modifiedWriteValues);
    fieldResult.readAction = getDefaultSVD(field, "readAction", readAction);

    if(!field.child("dim").empty()) {
        fieldResult.dim          = getCheckedSVD<std::uint64_t>(field, "dim", "Field");
        fieldResult.dimIncrement = getCheckedSVD<std::uint64_t>(field, "dimIncrement", "Field");
        if(fieldResult.dim == 0) {
            throw std::runtime_error("register dim should not be 0");
        }
    } else {
        fieldResult.dim          = 0;
        fieldResult.dimIncrement = 0;
        fieldResult.repType      = RepeatType::normal;
    }

    if(fieldResult.dim != 0) {
        fieldResult.repType = RepeatType::cluster;
        fieldResult.name    = remove_PercentS(fieldResult.name);
    }

    auto values = field.child("enumeratedValues");
    if(values.empty()) {
        return fieldResult;
    }

    fieldResult.type = FieldType::enum_;
    for(auto const& enumValueNode : values.children("enumeratedValue")) {
        fieldResult.values.emplace_back(ValueFromSVD(enumValueNode));
    }

    bool found = false;
    for(auto const& enumValue : fieldResult.values) {
        if(enumValue.value == fieldResult.resetValue) {
            found = true;
            break;
        }
    }

    if(!found) {
        std::string const peripheral_name
          = field.parent().parent().parent().child("name").text().as_string();
        std::string const device_name
          = field.parent().parent().parent().parent().child("name").text().as_string();
        auto parent = peripheral_name.empty() ? device_name : peripheral_name;
        fmt::print(stderr,
                   "no valid ResetValue in {}::{}::{}\n",
                   parent,
                   field.parent().parent().child("name").text().as_string(),
                   fieldResult.name);
    }

    return fieldResult;
}

inline std::variant<Register,
                    derived_t>
RegisterFromSVD(pugi::xml_node const& reg,
                Access                access,
                DataType              dataType) {
    Register registerResult;
    registerResult.name = sanitizeName(getCheckedSVD<std::string>(reg, "name", "Register"));
    auto displayName    = getOptionalSVD<std::string>(reg, "displayName");
    if(displayName) {
        registerResult.name = sanitizeName(*displayName);
    }
    registerResult.addressOffset = getCheckedSVD<std::uint64_t>(reg, "addressOffset", "Register");

    auto derived = reg.attribute("derivedFrom");
    if(!derived.empty()) {
        return derived_t{registerResult.name, derived.value(), registerResult.addressOffset, false};
    }

    registerResult.description
      = sanitizeDescription(getDefaultSVD(reg, "description", std::string{}));
    registerResult.resetValue = getDefaultSVD<std::uint64_t>(reg, "resetValue", 0);
    registerResult.dataType   = getDefaultSVD<DataType>(reg, "size", dataType);
    registerResult.zeroMask   = maskFromRange(DataTypeSize(registerResult.dataType) - 1, 0);
    registerResult.oneMask    = 0;
    auto accessType           = getDefaultSVD(reg, "access", access);
    auto modifiedWriteValues
      = getDefaultSVD(reg, "modifiedWriteValues", ModifiedWriteValues::empty);
    auto readAction = getDefaultSVD(reg, "readAction", ReadAction::empty);

    if(!reg.child("dim").empty()) {
        registerResult.dim          = getCheckedSVD<std::uint64_t>(reg, "dim", "Register");
        registerResult.dimIncrement = getCheckedSVD<std::uint64_t>(reg, "dimIncrement", "Register");
        if(registerResult.dim == 0) {
            throw std::runtime_error("register dim should not be 0");
        }
    } else {
        registerResult.dim          = 0;
        registerResult.dimIncrement = 0;
        registerResult.type         = RepeatType::normal;
    }

    if(registerResult.dim != 0) {
        registerResult.type = RepeatType::cluster;
        registerResult.name = remove_PercentS(registerResult.name);
    }

    auto fields = reg.child("fields");
    if(fields.empty()) {
        std::string const peripheral_name = reg.parent().parent().child("name").text().as_string();
        std::string const device_name
          = reg.parent().parent().parent().child("name").text().as_string();
        auto parent = peripheral_name.empty() ? device_name : peripheral_name;
        fmt::print(stderr, "no fields in {}::{}\n", parent, registerResult.name);
        return registerResult;
    }

    for(auto const& field_node : fields.children("field")) {
        auto fieldFromSvd = FieldFromSVD(field_node,
                                         accessType,
                                         registerResult.dataType,
                                         registerResult.resetValue,
                                         modifiedWriteValues,
                                         readAction);

        if(fieldFromSvd.modifiedWriteValues != ModifiedWriteValues::oneToClear
           && fieldFromSvd.modifiedWriteValues != ModifiedWriteValues::oneToSet
           && fieldFromSvd.modifiedWriteValues != ModifiedWriteValues::oneToToggle)
        {
            registerResult.zeroMask
              = clearBits(registerResult.zeroMask, fieldFromSvd.startBit, fieldFromSvd.stopBit);
        }
        if(fieldFromSvd.modifiedWriteValues == ModifiedWriteValues::zeroToClear
           || fieldFromSvd.modifiedWriteValues == ModifiedWriteValues::zeroToSet
           || fieldFromSvd.modifiedWriteValues == ModifiedWriteValues::zeroToToggle)
        {
            registerResult.oneMask
              = clearBits(registerResult.oneMask, fieldFromSvd.startBit, fieldFromSvd.stopBit);
        }
        registerResult.fields.push_back(std::move(fieldFromSvd));
    }

    return registerResult;
}

template<typename Regs>
std::vector<Register> makeRegister(Regs const& regs,
                                   Access      access,
                                   DataType    type) {
    std::vector<Register>  parsedRegs;
    std::vector<derived_t> parsedDerived;
    for(auto const& registerNode : regs) {
        auto parsed_reg = RegisterFromSVD(registerNode, access, type);

        if(std::holds_alternative<Register>(parsed_reg)) {
            parsedRegs.push_back(std::move(std::get<Register>(parsed_reg)));
        } else {
            parsedDerived.push_back(std::move(std::get<derived_t>(parsed_reg)));
        }
    }

    std::vector<Register> parsedRegsDerived;
    for(auto const& derived_reg : parsedDerived) {
        auto const& [name, baseName, address, processed] = derived_reg;
        auto basename                                    = remove_PercentS(baseName);
        bool found                                       = false;
        for(auto const& registerItem : parsedRegs) {
            if(registerItem.name == basename) {
                found = true;
                parsedRegsDerived.push_back(registerItem);
                auto& newreg         = parsedRegsDerived.back();
                newreg.name          = remove_PercentS(name);
                newreg.addressOffset = address;
                break;
            }
        }
        if(!found) {
            throw std::runtime_error("derived register not found");
        }
    }
    for(auto& derivedReg : parsedRegsDerived) {
        parsedRegs.push_back(std::move(derivedReg));
    }

    return parsedRegs;
}

inline RegisterGroup RegisterGroupFromSVD(pugi::xml_node regg,
                                          Access         access,
                                          DataType       type) {
    RegisterGroup registerGroup;
    registerGroup.name = getCheckedSVD<std::string>(regg, "name", "RegisterGroup");
    registerGroup.description
      = sanitizeDescription(getDefaultSVD(regg, "description", std::string{""}));
    registerGroup.dim = getCheckedSVD<std::uint64_t>(regg, "dim", "RegisterGroup");
    registerGroup.dimIncrement
      = getCheckedSVD<std::uint64_t>(regg, "dimIncrement", "RegisterGroup");
    registerGroup.addressOffset
      = getCheckedSVD<std::uint64_t>(regg, "addressOffset", "RegisterGroup");

    auto pos = registerGroup.name.find("[%s]");
    if(pos != std::string::npos) {
        registerGroup.name.erase(pos, 4);
    }

    registerGroup.registers = makeRegister(regg.children("register"), access, type);

    return registerGroup;
}

inline Peripheral ClusterFromSVD(pugi::xml_node    cluster,
                                 Peripheral const& peripheral_ref,
                                 Access            access,
                                 DataType          type) {
    Peripheral peripheral = peripheral_ref;
    peripheral.name = getCheckedSVD<std::string>(cluster, "name", "Cluster " + peripheral_ref.name);
    peripheral.description
      += " " + sanitizeDescription(getDefaultSVD(cluster, "description", std::string{}));

    peripheral.registers = makeRegister(cluster.children("register"), access, type);

    return peripheral;
}

inline std::variant<Peripheral,
                    cluster_t,
                    derived_t>
PeripheralFromSVD(pugi::xml_node const& peripheral,
                  Access                access,
                  DataType              addressType,
                  DataType              type) {
    Peripheral peripheral_result;
    peripheral_result.name = getCheckedSVD<std::string>(peripheral, "name", "Peripheral");
    peripheral_result.baseAddresses.push_back(
      {0,
       getCheckedSVD<std::uint64_t>(peripheral,
                                    "baseAddress",
                                    "Peripheral " + peripheral_result.name)});

    auto derived = peripheral.attribute("derivedFrom");
    if(!derived.empty()) {
        return derived_t{peripheral_result.name,
                         derived.value(),
                         peripheral_result.baseAddresses.back().address,
                         false};
    }

    peripheral_result.description
      = sanitizeDescription(getDefaultSVD(peripheral, "description", std::string{}));
    peripheral_result.addressType = addressType;
    peripheral_result.type        = RepeatType::normal;

    auto registers = peripheral.child("registers");
    if(registers.empty()) {
        fmt::print(stderr, "no registers in {}\n", peripheral_result.name);
        return peripheral_result;
    }

    peripheral_result.registers = makeRegister(registers.children("register"), access, type);

    auto const cluster = registers.children("cluster");
    for(auto const& cluster_node : cluster) {
        if(cluster_node.child("dim").empty()) {
            continue;
        }
        peripheral_result.registerGroups.push_back(
          RegisterGroupFromSVD(cluster_node, access, type));
    }

    cluster_t cluster_result;
    cluster_result.second = peripheral_result.name;
    for(auto const& cluster_node : cluster) {
        if(!cluster_node.child("dim").empty()) {
            continue;
        }
        cluster_result.first.push_back(
          ClusterFromSVD(cluster_node, peripheral_result, access, type));
    }

    if(!cluster_result.first.empty()) {
        return cluster_result;
    }

    return peripheral_result;
}
}   // namespace

inline Chip ChipFromSVD(pugi::xml_node const& device) {
    Chip chip;
    auto access      = getDefaultSVD(device, "access", Access::readWrite);
    chip.name        = getCheckedSVD<std::string>(device, "name", "Chip");
    chip.description = sanitizeDescription(getDefaultSVD(device, "description", std::string{}));
    auto width       = getDefaultSVD(device, "width", DataType::u32);
    auto size        = getDefaultSVD(device, "size", DataType::u32);

    auto peripherals = device.child("peripherals");
    if(peripherals.empty()) {
        throw std::runtime_error("no peripherals");
    }

    std::vector<derived_t> derived;
    std::vector<cluster_t> cluster;
    for(auto const& peripheral : peripherals.children("peripheral")) {
        auto peripheral_variant = PeripheralFromSVD(peripheral, access, width, size);
        if(std::holds_alternative<derived_t>(peripheral_variant)) {
            derived.push_back(std::move(std::get<derived_t>(peripheral_variant)));
        } else if(std::holds_alternative<cluster_t>(peripheral_variant)) {
            cluster.push_back(std::move(std::get<cluster_t>(peripheral_variant)));
        } else {
            chip.peripherals.push_back(std::move(std::get<Peripheral>(peripheral_variant)));
        }
    }

    for(auto& peripheral_ref : chip.peripherals) {
        std::string newName = peripheral_ref.name;
        RepeatType  newType = peripheral_ref.type;
        for(auto& derived_ref : derived) {
            auto& [name, baseName, address, processed] = derived_ref;
            if(baseName != peripheral_ref.name) {
                continue;
            }
            if(processed) {
                throw std::runtime_error("found derived peripheral multiple times " + name);
            }

            auto const& derivedName = name;

            auto miss = std::ranges::mismatch(peripheral_ref.name, derivedName);
            if(miss.in1 == end(peripheral_ref.name)) {
                if(derivedName.size() != peripheral_ref.name.size() + 1) {
                    fmt::print(stderr,
                               "something wrong ? {} {}\n",
                               peripheral_ref.name,
                               derivedName);
                    continue;
                }
            }

            processed = true;

            newType = RepeatType::cluster;
            newName.resize(
              static_cast<std::size_t>(std::distance(begin(peripheral_ref.name), miss.in1)));

            std::string const num = derivedName.substr(newName.size());

            peripheral_ref.baseAddresses.emplace_back(std::stoull(num), address);
        }
        peripheral_ref.name = newName;
        peripheral_ref.type = newType;
    }

    for(auto& cluster_ref : cluster) {
        auto& [clusterPeripherals, clusterName] = cluster_ref;
        std::string              newName        = clusterName;
        RepeatType               newType        = RepeatType::normal;
        std::vector<AddressType> newAddr;
        for(auto& derived_ref : derived) {
            auto& [name, baseName, address, processed] = derived_ref;
            if(baseName != clusterName) {
                continue;
            }
            if(processed) {
                throw std::runtime_error("found derived peripheral multiple times " + name);
            }

            auto const& derivedName = name;

            auto miss = std::ranges::mismatch(clusterName, derivedName);
            if(miss.in1 == end(clusterName)) {
                if(derivedName.size() != clusterName.size() + 1) {
                    fmt::print(stderr, "something wrong ? {} {}\n", clusterName, derivedName);
                    continue;
                }
            }

            processed = true;

            newType = RepeatType::cluster;
            newName.resize(static_cast<std::size_t>(std::distance(begin(clusterName), miss.in1)));

            std::string const num = derivedName.substr(newName.size());

            newAddr.emplace_back(std::stoull(num), address);
        }

        for(auto& peripheral_ref : clusterPeripherals) {
            peripheral_ref.name = fmt::format("{}_{}", newName, peripheral_ref.name);
            peripheral_ref.type = newType;
            std::ranges::copy(newAddr, std::back_inserter(peripheral_ref.baseAddresses));
            chip.peripherals.push_back(std::move(peripheral_ref));
        }
    }

    for(auto const& derived_ref : derived) {
        auto const& [name, baseName, address, processed] = derived_ref;
        if(!processed) {
            fmt::print(stderr, "have not found derived peripheral {}\n", name);
        }
    }

    for(auto& peripheral_ref : chip.peripherals) {
        auto pname = peripheral_ref.name + "_";

        auto removePeripheral = [&](std::string& str_ref) {
            if(str_ref.starts_with(pname)) {
                str_ref.erase(0, pname.size());
            }
        };

        auto removePeripheralReg = [&](auto& reg) {
            removePeripheral(reg.name);
            for(auto field_ref : reg.fields) {
                removePeripheral(field_ref.name);
                for(auto value_ref : field_ref.values) {
                    removePeripheral(value_ref.name);
                }
            }
        };
        for(auto& register_ref : peripheral_ref.registers) {
            removePeripheralReg(register_ref);
        }
        for(auto& register_group : peripheral_ref.registerGroups) {
            for(auto& register_ref : register_group.registers) {
                removePeripheralReg(register_ref);
            }
        }
    }

    return chip;
}
