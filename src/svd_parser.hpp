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
            if(std::ispunct(character)) {
                character = '_';
            }
        } else {
            if(std::ispunct(character) && character != '[' && character != ']' && character != '%')
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
                name = "_" + name;
                break;
            }
        }
    };

    sanitizeKeyword(cppKeywords);
    sanitizeKeyword(kvasirKeywords);

    if(!name.empty()) {
        if(std::isdigit(name.front())) {
            name = "_" + name;
        }
    }

    for(auto& character : name) {
        if(std::isspace(character)) {
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
        std::size_t position;
        std::string str{svdString};
        auto        value = std::stoull(str, &position, 0);
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
        if(svdString == "") {
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
        if(svdString == "") {
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
        std::string p1 = field.parent().parent().parent().child("name").text().as_string();
        std::string p2 = field.parent().parent().parent().parent().child("name").text().as_string();
        auto        parent = p1.empty() ? p2 : p1;
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
        std::string p1     = reg.parent().parent().child("name").text().as_string();
        std::string p2     = reg.parent().parent().parent().child("name").text().as_string();
        auto        parent = p1.empty() ? p2 : p1;
        fmt::print(stderr, "no fields in {}::{}\n", parent, registerResult.name);
        return registerResult;
    }

    for(auto const& f : fields.children("field")) {
        auto fieldFromSvd = FieldFromSVD(f,
                                         accessType,
                                         registerResult.dataType,
                                         registerResult.resetValue,
                                         modifiedWriteValues,
                                         readAction);

        if(!(fieldFromSvd.modifiedWriteValues == ModifiedWriteValues::oneToClear
             || fieldFromSvd.modifiedWriteValues == ModifiedWriteValues::oneToSet
             || fieldFromSvd.modifiedWriteValues == ModifiedWriteValues::oneToToggle))
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
    for(auto const& r : regs) {
        auto parsed_reg = RegisterFromSVD(r, access, type);

        if(std::holds_alternative<Register>(parsed_reg)) {
            parsedRegs.push_back(std::move(std::get<Register>(parsed_reg)));
        } else {
            parsedDerived.push_back(std::move(std::get<derived_t>(parsed_reg)));
        }
    }

    std::vector<Register> parsedRegsDerived;
    for(auto const& d : parsedDerived) {
        auto const& [name, baseName, address, processed] = d;
        auto basename                                    = remove_PercentS(baseName);
        bool found                                       = false;
        for(auto const& r : parsedRegs) {
            if(r.name == basename) {
                found = true;
                parsedRegsDerived.push_back(r);
                auto& newreg         = parsedRegsDerived.back();
                newreg.name          = remove_PercentS(name);
                newreg.addressOffset = address;
                break;
            }
        }
        if(found == false) {
            throw std::runtime_error("derived register not found");
        }
    }
    for(auto& d : parsedRegsDerived) {
        parsedRegs.push_back(std::move(d));
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
                                 Peripheral const& p,
                                 Access            access,
                                 DataType          type) {
    Peripheral peripheral = p;
    peripheral.name       = getCheckedSVD<std::string>(cluster, "name", "Cluster " + p.name);
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
    Peripheral p;
    p.name = getCheckedSVD<std::string>(peripheral, "name", "Peripheral");
    p.baseAddresses.push_back(
      {0, getCheckedSVD<std::uint64_t>(peripheral, "baseAddress", "Peripheral " + p.name)});

    auto derived = peripheral.attribute("derivedFrom");
    if(!derived.empty()) {
        return derived_t{p.name, derived.value(), p.baseAddresses.back().address, false};
    }

    p.description = sanitizeDescription(getDefaultSVD(peripheral, "description", std::string{}));
    p.addressType = addressType;
    p.type        = RepeatType::normal;

    auto registers = peripheral.child("registers");
    if(registers.empty()) {
        fmt::print(stderr, "no registers in {}\n", p.name);
        return p;
    }

    p.registers = makeRegister(registers.children("register"), access, type);

    auto const cluster = registers.children("cluster");
    for(auto const& c : cluster) {
        if(c.child("dim").empty()) {
            continue;
        }
        p.registerGroups.push_back(RegisterGroupFromSVD(c, access, type));
    }

    cluster_t cl;
    cl.second = p.name;
    for(auto const& c : cluster) {
        if(!c.child("dim").empty()) {
            continue;
        }
        cl.first.push_back(ClusterFromSVD(c, p, access, type));
    }

    if(!cl.first.empty()) {
        return cl;
    }

    return p;
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
        auto pp = PeripheralFromSVD(peripheral, access, width, size);
        if(std::holds_alternative<derived_t>(pp)) {
            derived.push_back(std::move(std::get<derived_t>(pp)));
        } else if(std::holds_alternative<cluster_t>(pp)) {
            cluster.push_back(std::move(std::get<cluster_t>(pp)));
        } else {
            chip.peripherals.push_back(std::move(std::get<Peripheral>(pp)));
        }
    }

    for(auto& p : chip.peripherals) {
        std::string newName = p.name;
        RepeatType  newType = p.type;
        for(auto& d : derived) {
            auto& [name, baseName, address, processed] = d;
            if(baseName != p.name) {
                continue;
            }
            if(processed) {
                throw std::runtime_error("found derived peripheral multiple times " + name);
            }

            auto const& derivedName = name;

            auto miss
              = std::mismatch(begin(p.name), end(p.name), begin(derivedName), end(derivedName));
            if(miss.first == end(p.name)) {
                if(derivedName.size() != p.name.size() + 1) {
                    fmt::print(stderr, "something wrong ? {} {}\n", p.name, derivedName);
                    continue;
                }
            }

            processed = true;

            newType = RepeatType::cluster;
            newName.resize(static_cast<std::size_t>(std::distance(begin(p.name), miss.first)));

            std::string num = derivedName.substr(newName.size());

            p.baseAddresses.emplace_back(std::stoull(num), address);
        }
        p.name = newName;
        p.type = newType;
    }

    for(auto& c : cluster) {
        auto& [clusterPeripherals, clusterName] = c;
        std::string              newName        = clusterName;
        RepeatType               newType        = RepeatType::normal;
        std::vector<AddressType> newAddr;
        for(auto& d : derived) {
            auto& [name, baseName, address, processed] = d;
            if(baseName != clusterName) {
                continue;
            }
            if(processed) {
                throw std::runtime_error("found derived peripheral multiple times " + name);
            }

            auto const& derivedName = name;

            auto miss = std::mismatch(begin(clusterName),
                                      end(clusterName),
                                      begin(derivedName),
                                      end(derivedName));
            if(miss.first == end(clusterName)) {
                if(derivedName.size() != clusterName.size() + 1) {
                    fmt::print(stderr, "something wrong ? {} {}\n", clusterName, derivedName);
                    continue;
                }
            }

            processed = true;

            newType = RepeatType::cluster;
            newName.resize(static_cast<std::size_t>(std::distance(begin(clusterName), miss.first)));

            std::string num = derivedName.substr(newName.size());

            newAddr.emplace_back(std::stoull(num), address);
        }

        for(auto& p : clusterPeripherals) {
            p.name = fmt::format("{}_{}", newName, p.name);
            p.type = newType;
            std::copy(begin(newAddr), end(newAddr), std::back_inserter(p.baseAddresses));
            chip.peripherals.push_back(std::move(p));
        }
    }

    for(auto const& d : derived) {
        auto const& [name, baseName, address, processed] = d;
        if(!processed) {
            fmt::print(stderr, "have not found derived peripheral {}\n", name);
        }
    }

    for(auto& p : chip.peripherals) {
        auto pname = p.name + "_";

        auto removePeripheral = [&](std::string& s) {
            if(s.find(pname) == 0) {
                s.erase(0, pname.size());
            }
        };

        auto removePeripheralReg = [&](auto& reg) {
            removePeripheral(reg.name);
            for(auto f : reg.fields) {
                removePeripheral(f.name);
                for(auto v : f.values) {
                    removePeripheral(v.name);
                }
            }
        };
        for(auto& r : p.registers) {
            removePeripheralReg(r);
        }
        for(auto& rg : p.registerGroups) {
            for(auto& r : rg.registers) {
                removePeripheralReg(r);
            }
        }
    }

    return chip;
}
