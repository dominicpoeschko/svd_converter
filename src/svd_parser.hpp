#pragma once

#include "svd_types.hpp"

#include <fmt/format.h>
#include <fmt/ranges.h>
#include <inja/inja.hpp>
#include <map>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <variant>
#include <vector>

namespace {
constexpr std::uint64_t maskFromRange(std::uint64_t high, std::uint64_t low) {
    return (0xFFFFFFFFFFFFFFFFULL >> (63 - (high - low))) << low;
}

constexpr std::uint64_t
makeResetValue(std::uint64_t parrentResetValue, std::uint64_t startBit, std::uint64_t stopBit) {
    std::uint64_t mask = maskFromRange(stopBit, startBit);
    return (parrentResetValue & mask) >> startBit;
}

constexpr std::uint64_t
clearBits(std::uint64_t value, std::uint64_t startBit, std::uint64_t stopBit) {
    std::uint64_t mask = maskFromRange(stopBit, startBit);
    return value & (~mask);
}

using derived_t = std::tuple<std::string, std::string, std::uint64_t, bool>;
using cluster_t = std::pair<std::vector<Peripheral>, std::string>;

inline std::string remove_PercentS(std::string name) {
    auto pos = name.find("[%s]");
    if(pos != std::string::npos) {
        name.erase(pos, 4);
    } else {
        pos = name.find("_%s");
        if(pos != std::string::npos) {
            name.erase(pos, 3);
        } else {
            pos = name.find("%s");
            if(pos != std::string::npos) {
                name.erase(pos, 2);
            }
        }
    }

    return name;
}

inline std::string sanitizeName(std::string v, bool full = false) {
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
    for(auto& c : v) {
        if(full) {
            if(std::ispunct(c)) {
                c = '_';
            }
        } else {
            if(std::ispunct(c) && c != '[' && c != ']' && c != '%') {
                c = '_';
            }
        }
    }

    auto sanitizeKeyword = [&](auto const& keywords) {
        for(auto const& w : keywords) {
            std::string vv = v;
            std::transform(begin(vv), end(vv), begin(vv), [](auto c) { return std::tolower(c); });
            if(w == vv) {
                v = "_" + v;
                break;
            }
        }
    };

    sanitizeKeyword(cppKeywords);
    sanitizeKeyword(kvasirKeywords);

    if(!v.empty()) {
        if(std::isdigit(v.front())) {
            v = "_" + v;
        }
    }

    for(auto& c : v) {
        if(std::isspace(c)) {
            c = '_';
        }
    }

    v = remove_PercentS(v);

    auto pos = v.find("__");
    while(pos != std::string::npos) {
        v.erase(pos, 1);
        pos = v.find("__");
    }

    return v;
}

inline std::string sanitizeDescription(std::string v) {
    auto replace = [&](std::string_view needle) {
        while(true) {
            auto pos = std::search(v.begin(), v.end(), needle.begin(), needle.end());
            if(pos == v.end()) {
                break;
            }
            auto pp = v.erase(pos, std::next(pos, static_cast<int>(needle.size()) - 1));
            *pp     = ' ';
        }
    };
    replace("\n");
    replace("\r");
    replace("\t");
    replace("\\n");
    replace("\\r");
    replace("\\t");
    replace("  ");
    if(v.starts_with(" ")) {
        v.erase(0, 1);
    }
    if(v.ends_with(" ")) {
        v.erase(std::next(v.end(), -1), v.end());
    }

    return v;
}

template<typename T>
T fromSVDString(std::string const& s);

template<>
std::uint64_t fromSVDString<std::uint64_t>(std::string const& s) {
    std::size_t pos;
    auto        v = std::stoull(s, &pos, 0);
    if(pos == 0) {
        throw std::runtime_error(fmt::format("no valid number {}", s));
    }
    return v;
}

template<>
std::string fromSVDString(std::string const& s) {
    return s;
}

template<typename T>
T getDefaultSVD(pugi::xml_node const& node, std::string const& name, T const& u) {
    auto v = node.child(name.c_str());
    if(v.empty()) {
        return u;
    }
    return fromSVDString<T>(v.text().as_string());
}

template<typename T>
std::optional<T> getOptionalSVD(pugi::xml_node const& node, std::string const& name) {
    auto v = node.child(name.c_str());
    if(v.empty()) {
        return {};
    }
    return fromSVDString<T>(v.text().as_string());
}

template<typename T>
T getCheckedSVD(pugi::xml_node const& node, std::string const& name, std::string const& msg = "") {
    auto v = node.child(name.c_str());
    if(v.empty()) {
        throw std::runtime_error(name + " not found in " + msg);
    }
    return fromSVDString<T>(v.text().as_string());
}

template<>
BitRange fromSVDString<BitRange>(std::string const& s) {
    if(s.empty()) {
        throw std::runtime_error(fmt::format("wrong bitRange {}", s));
    }
    if(s.front() != '[' || s.back() != ']') {
        throw std::runtime_error(fmt::format("wrong bitRange {}", s));
    }
    auto const colonPos = s.find_first_of(':');
    if(colonPos == std::string::npos || colonPos != s.find_last_of(':')) {
        throw std::runtime_error(fmt::format("wrong bitRange {}", s));
    }
    auto msbString = s.substr(1, colonPos);
    auto lsbString = s.substr(colonPos + 1, s.size() - 1);

    auto msb = fromSVDString<std::uint64_t>(msbString);
    auto lsb = fromSVDString<std::uint64_t>(lsbString);
    if(lsb > msb) {
        throw std::runtime_error(fmt::format("wrong bitRange {}", s));
    }

    return BitRange{lsb, msb};
}

inline std::uint64_t DataTypeSize(DataType d) {
    if(d == DataType::u8) {
        return 8;
    }
    if(d == DataType::u16) {
        return 16;
    }
    if(d == DataType::u32) {
        return 32;
    }
    if(d == DataType::u64) {
        return 64;
    }
    return 0;
}

template<>
DataType fromSVDString<DataType>(std::string const& s) {
    auto size = fromSVDString<std::uint64_t>(s);
    if(size == 8) {
        return DataType::u8;
    }
    if(size == 16) {
        return DataType::u16;
    }
    if(size == 32) {
        return DataType::u32;
    }
    if(size == 64) {
        return DataType::u64;
    }

    throw std::runtime_error(fmt::format("wrong size {} {}", size, s));
}

template<>
Access fromSVDString<Access>(std::string const& s) {
    if(s == "read-only") {
        return Access::readOnly;
    }
    if(s == "write-only") {
        return Access::writeOnly;
    }
    if(s == "read-write") {
        return Access::readWrite;
    }
    if(s == "writeOnce") {
        return Access::writeOnce;
    }
    if(s == "read-WriteOnce") {
        return Access::readWriteOnce;
    }

    throw std::runtime_error(fmt::format("bad access {}", s));
}

template<>
ModifiedWriteValues fromSVDString<ModifiedWriteValues>(std::string const& s) {
    if(s == "") {
        return ModifiedWriteValues::empty;
    }
    if(s == "oneToClear") {
        return ModifiedWriteValues::oneToClear;
    }
    if(s == "oneToSet") {
        return ModifiedWriteValues::oneToSet;
    }
    if(s == "oneToToggle") {
        return ModifiedWriteValues::oneToToggle;
    }
    if(s == "zeroToClear") {
        return ModifiedWriteValues::zeroToClear;
    }
    if(s == "zeroToSet") {
        return ModifiedWriteValues::zeroToSet;
    }
    if(s == "zeroToToggle") {
        return ModifiedWriteValues::zeroToToggle;
    }
    if(s == "clear") {
        return ModifiedWriteValues::clear;
    }
    if(s == "set") {
        return ModifiedWriteValues::set;
    }
    if(s == "modify") {
        return ModifiedWriteValues::modify;
    }

    throw std::runtime_error(fmt::format("bad modifiedWriteValues {}", s));
}

template<>
ReadAction fromSVDString<ReadAction>(std::string const& s) {
    if(s == "") {
        return ReadAction::empty;
    }
    if(s == "clear") {
        return ReadAction::clear;
    }
    if(s == "set") {
        return ReadAction::set;
    }
    if(s == "modify") {
        return ReadAction::modify;
    }
    if(s == "modifyExternal") {
        return ReadAction::modifyExternal;
    }

    throw std::runtime_error(fmt::format("bad ReadAction {}", s));
}

inline Value ValueFromSVD(pugi::xml_node const& value) {
    Value v;
    auto  mabyeName = getOptionalSVD<std::string>(value, "name");
    v.description   = sanitizeDescription(getDefaultSVD(value, "description", std::string{}));
    v.value         = getCheckedSVD<std::uint64_t>(value, "value");
    if(mabyeName) {
        v.name = sanitizeName(*mabyeName);
    } else {
        if(!v.description.empty()) {
            v.name = sanitizeName(v.description, true);
        } else {
            v.name = "_" + std::to_string(v.value);
        }
    }
    return v;
}

inline Field FieldFromSVD(
  pugi::xml_node const& field,
  Access                access,
  DataType              dataType,
  std::uint64_t         resetValue,
  ModifiedWriteValues   mval,
  ReadAction            ract) {
    Field f;
    f.name        = sanitizeName(getCheckedSVD<std::string>(field, "name", "Field"));
    f.description = sanitizeDescription(getDefaultSVD(field, "description", std::string{}));

    auto bitRange = getOptionalSVD<BitRange>(field, "bitRange");
    if(bitRange) {
        f.startBit = bitRange->start;
        f.stopBit  = bitRange->stop;
    } else {
        f.startBit = getCheckedSVD<std::uint64_t>(field, "bitOffset", "Field");
        f.stopBit  = getCheckedSVD<std::uint64_t>(field, "bitWidth", "Field") + f.startBit - 1;
    }

    f.access              = getDefaultSVD(field, "access", access);
    f.dataType            = dataType;
    f.resetValue          = makeResetValue(resetValue, f.startBit, f.stopBit);
    f.type                = FieldType::normal;
    f.modifiedWriteValues = getDefaultSVD(field, "modifiedWriteValues", mval);
    f.readAction          = getDefaultSVD(field, "readAction", ract);

    if(!field.child("dim").empty()) {
        f.dim          = getCheckedSVD<std::uint64_t>(field, "dim", "Field");
        f.dimIncrement = getCheckedSVD<std::uint64_t>(field, "dimIncrement", "Field");
        if(f.dim == 0) {
            throw std::runtime_error("register dim should not be 0");
        }
    } else {
        f.dim          = 0;
        f.dimIncrement = 0;
        f.repType      = RepeatType::normal;
    }

    if(f.dim != 0) {
        f.repType = RepeatType::cluster;
        f.name    = remove_PercentS(f.name);
    }

    auto values = field.child("enumeratedValues");
    if(values.empty()) {
        return f;
    }

    f.type = FieldType::enum_;
    for(auto const& v : values.children("enumeratedValue")) {
        f.values.push_back(ValueFromSVD(v));
    }

    bool found = false;
    for(auto const& v : f.values) {
        if(v.value == f.resetValue) {
            found = true;
            break;
        }
    }

    if(!found) {
        std::string p1 = field.parent().parent().parent().child("name").text().as_string();
        std::string p2 = field.parent().parent().parent().parent().child("name").text().as_string();
        auto        parent = p1.empty() ? p2 : p1;
        fmt::print(
          stderr,
          "no valid ResetValue in {}::{}::{}\n",
          parent,
          field.parent().parent().child("name").text().as_string(),
          f.name);
    }

    return f;
}

inline std::variant<Register, derived_t>
RegisterFromSVD(pugi::xml_node const& reg, Access access, DataType type) {
    Register r;
    r.name          = sanitizeName(getCheckedSVD<std::string>(reg, "name", "Register"));
    auto diplayName = getOptionalSVD<std::string>(reg, "displayName");
    if(diplayName) {
        r.name = sanitizeName(*diplayName);
    }
    r.addressOffset = getCheckedSVD<std::uint64_t>(reg, "addressOffset", "Register");

    auto derived = reg.attribute("derivedFrom");
    if(!derived.empty()) {
        return derived_t{r.name, derived.value(), r.addressOffset, false};
    }

    r.description = sanitizeDescription(getDefaultSVD(reg, "description", std::string{}));
    r.resetValue  = getDefaultSVD<std::uint64_t>(reg, "resetValue", 0);
    r.dataType    = getDefaultSVD<DataType>(reg, "size", type);
    r.zeroMask    = maskFromRange(DataTypeSize(r.dataType) - 1, 0);
    r.oneMask     = 0;
    auto acc      = getDefaultSVD(reg, "access", access);
    auto modifiedWriteValues
      = getDefaultSVD(reg, "modifiedWriteValues", ModifiedWriteValues::empty);
    auto readAction = getDefaultSVD(reg, "readAction", ReadAction::empty);

    if(!reg.child("dim").empty()) {
        r.dim          = getCheckedSVD<std::uint64_t>(reg, "dim", "Register");
        r.dimIncrement = getCheckedSVD<std::uint64_t>(reg, "dimIncrement", "Register");
        if(r.dim == 0) {
            throw std::runtime_error("register dim should not be 0");
        }
    } else {
        r.dim          = 0;
        r.dimIncrement = 0;
        r.type         = RepeatType::normal;
    }

    if(r.dim != 0) {
        r.type = RepeatType::cluster;
        r.name = remove_PercentS(r.name);
    }

    auto fields = reg.child("fields");
    if(fields.empty()) {
        std::string p1     = reg.parent().parent().child("name").text().as_string();
        std::string p2     = reg.parent().parent().parent().child("name").text().as_string();
        auto        parent = p1.empty() ? p2 : p1;
        fmt::print(stderr, "no fields in {}::{}\n", parent, r.name);
        return r;
    }

    for(auto const& f : fields.children("field")) {
        auto ff = FieldFromSVD(f, acc, r.dataType, r.resetValue, modifiedWriteValues, readAction);

        r.fields.push_back(ff);
        if(!(ff.modifiedWriteValues == ModifiedWriteValues::oneToClear
             || ff.modifiedWriteValues == ModifiedWriteValues::oneToSet
             || ff.modifiedWriteValues == ModifiedWriteValues::oneToToggle))
        {
            r.zeroMask = clearBits(r.zeroMask, ff.startBit, ff.stopBit);
        }
        if(
          ff.modifiedWriteValues == ModifiedWriteValues::zeroToClear
          || ff.modifiedWriteValues == ModifiedWriteValues::zeroToSet
          || ff.modifiedWriteValues == ModifiedWriteValues::zeroToToggle)
        {
            r.oneMask = clearBits(r.oneMask, ff.startBit, ff.stopBit);
        }
    }

    return r;
}

template<typename Regs>
std::vector<Register> makeRegister(Regs const& regs, Access access, DataType type) {
    std::vector<Register>  parsedRegs;
    std::vector<derived_t> parsedDerived;
    for(auto const& r : regs) {
        auto parsed_reg = RegisterFromSVD(r, access, type);

        if(std::holds_alternative<Register>(parsed_reg)) {
            parsedRegs.push_back(std::get<Register>(parsed_reg));
        } else {
            parsedDerived.push_back(std::get<derived_t>(parsed_reg));
        }
    }

    std::vector<Register> parsedRegsDerived;
    for(auto const& d : parsedDerived) {
        auto basename = remove_PercentS(std::get<1>(d));
        bool found    = false;
        for(auto const& r : parsedRegs) {
            if(r.name == basename) {
                found = true;
                parsedRegsDerived.push_back(r);
                auto& newreg         = parsedRegsDerived.back();
                newreg.name          = remove_PercentS(std::get<0>(d));
                newreg.addressOffset = std::get<2>(d);
                break;
            }
        }
        if(found == false) {
            throw std::runtime_error("derived register not found");
        }
    }
    for(auto const& d : parsedRegsDerived) {
        parsedRegs.push_back(d);
    }

    return parsedRegs;
}

inline RegisterGroup RegisterGroupFromSVD(pugi::xml_node regg, Access access, DataType type) {
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

inline Peripheral
ClusterFromSVD(pugi::xml_node cluster, Peripheral const& p, Access access, DataType type) {
    Peripheral peripheral = p;
    peripheral.name       = getCheckedSVD<std::string>(cluster, "name", "Cluster " + p.name);
    peripheral.description
      += " " + sanitizeDescription(getDefaultSVD(cluster, "description", std::string{}));

    peripheral.registers = makeRegister(cluster.children("register"), access, type);

    return peripheral;
}

inline std::variant<Peripheral, cluster_t, derived_t> PeripheralFromSVD(
  pugi::xml_node const& peripheral,
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
            derived.push_back(std::get<derived_t>(pp));
        } else if(std::holds_alternative<cluster_t>(pp)) {
            cluster.push_back(std::get<cluster_t>(pp));
        } else {
            chip.peripherals.push_back(std::get<Peripheral>(pp));
        }
    }

    for(auto& p : chip.peripherals) {
        std::string newName = p.name;
        RepeatType  newType = p.type;
        for(auto& d : derived) {
            if(std::get<1>(d) != p.name) {
                continue;
            }
            if(std::get<3>(d)) {
                throw std::runtime_error(
                  "found derived peripheral multible times " + std::get<0>(d));
            }

            auto const& derivedName = std::get<0>(d);

            auto miss
              = std::mismatch(begin(p.name), end(p.name), begin(derivedName), end(derivedName));
            if(miss.first == end(p.name)) {
                if(derivedName.size() != p.name.size() + 1) {
                    fmt::print(stderr, "something wrong ? {} {}\n", p.name, derivedName);
                    continue;
                }
            }

            std::get<3>(d) = true;

            newType = RepeatType::cluster;
            newName.resize(static_cast<std::size_t>(std::distance(begin(p.name), miss.first)));

            std::string num = derivedName.substr(newName.size());

            p.baseAddresses.push_back(AddressType{std::stoull(num), std::get<2>(d)});
        }
        p.name = newName;
        p.type = newType;
    }

    for(auto& c : cluster) {
        std::string              newName = c.second;
        RepeatType               newType = RepeatType::normal;
        std::vector<AddressType> newAddr;
        for(auto& d : derived) {
            if(std::get<1>(d) != c.second) {
                continue;
            }
            if(std::get<3>(d)) {
                throw std::runtime_error(
                  "found derived peripheral multible times " + std::get<0>(d));
            }

            auto const& derivedName = std::get<0>(d);

            auto miss
              = std::mismatch(begin(c.second), end(c.second), begin(derivedName), end(derivedName));
            if(miss.first == end(c.second)) {
                if(derivedName.size() != c.second.size() + 1) {
                    fmt::print(stderr, "something wrong ? {} {}\n", c.second, derivedName);
                    continue;
                }
            }

            std::get<3>(d) = true;

            newType = RepeatType::cluster;
            newName.resize(static_cast<std::size_t>(std::distance(begin(c.second), miss.first)));

            std::string num = derivedName.substr(newName.size());

            newAddr.push_back(AddressType{std::stoull(num), std::get<2>(d)});
        }

        for(auto& p : c.first) {
            p.name = fmt::format("{}_{}", newName, p.name);
            p.type = newType;
            std::copy(begin(newAddr), end(newAddr), std::back_inserter(p.baseAddresses));
            chip.peripherals.push_back(p);
        }
    }

    for(auto const& d : derived) {
        if(!std::get<3>(d)) {
            fmt::print(stderr, "have not found derived peripheral {}\n", std::get<0>(d));
        }
    }

    for(auto& p : chip.peripherals) {
        auto pname = p.name + "_";

        auto removePeriphery = [&](std::string& s) {
            if(s.find(pname) == 0) {
                s.erase(0, pname.size());
            }
        };

        auto removePeripheryReg = [&](auto& reg) {
            removePeriphery(reg.name);
            for(auto f : reg.fields) {
                removePeriphery(f.name);
                for(auto v : f.values) {
                    removePeriphery(v.name);
                }
            }
        };
        for(auto& r : p.registers) {
            removePeripheryReg(r);
        }
        for(auto& rg : p.registerGroups) {
            for(auto& r : rg.registers) {
                removePeripheryReg(r);
            }
        }
    }

    return chip;
}
