#pragma once

#include "inja_wrapper.hpp"
#include "svd_types.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <map>
#include <optional>
#include <print>
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
            if(std::ispunct(character) != 0) { character = '_'; }
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
        if(std::isdigit(name.front()) != 0) { name.insert(0, "_"); }
    }

    for(auto& character : name) {
        if(std::isspace(character) != 0) { character = '_'; }
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
            if(position == description.end()) { break; }
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
    if(description.starts_with(" ")) { description.erase(0, 1); }
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
        if(svdString == "read-WriteOnce") { return Access::readWriteOnce; }
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
    } else if constexpr(std::is_same_v<T, ReadAction>) {
        if(svdString.empty()) { return ReadAction::empty; }
        if(svdString == "clear") { return ReadAction::clear; }
        if(svdString == "set") { return ReadAction::set; }
        if(svdString == "modify") { return ReadAction::modify; }
        if(svdString == "modifyExternal") { return ReadAction::modifyExternal; }
        throw std::runtime_error(std::format("bad ReadAction {}", svdString));
    } else {
        static_assert(false, "Unsupported type for fromSVDString");
    }
}

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
    // A writeConstraint range of one value: every write of the register must carry it (a key).
    if(auto const range = field.child("writeConstraint").child("range"); !range.empty()) {
        fieldResult.key = getCheckedSVD<std::uint64_t>(range, "minimum", "writeConstraint")
                       == getCheckedSVD<std::uint64_t>(range, "maximum", "writeConstraint");
    }
    // `<!-- Kvasir: write-only accepted: ... -->`: seen only when the document was loaded with
    // pugi::parse_comments (main.cpp does)
    for(auto const& node : field.children()) {
        if(node.type() == pugi::node_comment
           && std::string_view{node.value()}.find("Kvasir: write-only accepted")
                != std::string_view::npos)
        {
            fieldResult.writeOnlyAccepted = true;
        }
    }

    if(!field.child("dim").empty()) {
        fieldResult.dim          = getCheckedSVD<std::uint64_t>(field, "dim", "Field");
        fieldResult.dimIncrement = getCheckedSVD<std::uint64_t>(field, "dimIncrement", "Field");
        if(fieldResult.dim == 0) { throw std::runtime_error("register dim should not be 0"); }
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
    if(values.empty()) { return fieldResult; }

    fieldResult.type = FieldType::enum_;
    for(auto const& enumValueNode : values.children("enumeratedValue")) {
        fieldResult.values.emplace_back(ValueFromSVD(enumValueNode));
    }

    // A write-only field has no value to read back after reset (the RP2350 POWMAN PASSWORD key
    // reads 0, its one enumerated value is 0x5AFE): nothing to check.
    bool found = fieldResult.access == Access::writeOnly;
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
        std::print(stderr,
                   "no valid ResetValue in {}::{}::{}\n",
                   parent,
                   field.parent().parent().child("name").text().as_string(),
                   fieldResult.name);
    }

    return fieldResult;
}

enum class Identity : std::uint8_t { none, zero, one };

// What can be written to a field without changing it. The same table is
// Kvasir::Register::Detail::identityOf in the SDK (Register/Utility.hpp); keep the two in step.
// SVD `clear`/`set` (any write clears/sets the field) stays `none`: the RP2040's SVD marks its
// self-clearing triggers that way, but also I2C IC_DATA_CMD's CMD/STOP/RESTART, where a 0 is a
// value. A self-clearing trigger whose 0 does nothing is written `oneToSet` in the SVD.
constexpr Identity identityOf(Access              a,
                              ModifiedWriteValues m) {
    using M = ModifiedWriteValues;
    if(a == Access::readOnly) { return Identity::zero; }
    switch(m) {
    case M::oneToClear:
    case M::oneToSet:
    case M::oneToToggle:  return Identity::zero;
    case M::zeroToClear:
    case M::zeroToSet:
    case M::zeroToToggle: return Identity::one;
    case M::empty:
    case M::clear:
    case M::set:
    case M::modify:       return Identity::none;
    }
    return Identity::none;
}

constexpr std::string_view identityName(Identity i) {
    switch(i) {
    case Identity::zero: return "zero";
    case Identity::one:  return "one";
    case Identity::none: return "none";
    }
    return "none";
}

// The register's write-ignored masks from its fields. zeroMask: the bits a write of zero leaves
// alone - reserved bits, fields with identity zero (one-to-* flags, read-only fields). oneMask: the
// bits a write of one leaves alone - zero-to-* fields. Both disjoint. writeBackActsMask: the part of
// zeroMask a read-modify-write must still write as 0, the writable one-to-* fields (a 1 read back and
// written again clears a flag or fires a trigger); reserved and read-only bits are kept as read - Arm
// RES0 is should-be-zero-or-preserved (DDI0553B.y glossary "RES0"), SAM D21 reserved bits always read
// 0 (DS40001882L 46.4 "Registers and Bits"), STM32 reserved bits "must be kept at reset value" (RM0430
// 1.2). A partial write that covers
// every bit outside them needs no read. Also the register's RmwHazard: the bits of its key fields
// (mustSupplyMask) and whether any field's read has a side effect (a readAction).
inline void computeMasks(Register& r) {
    r.zeroMask          = maskFromRange(DataTypeSize(r.dataType) - 1, 0);
    r.oneMask           = 0;
    r.writeBackActsMask = 0;
    r.mustSupplyMask    = 0;
    r.readHasSideEffect = false;
    bool allReadOnly    = !r.fields.empty();
    for(auto& f : r.fields) {
        auto const id       = identityOf(f.access, f.modifiedWriteValues);
        f.identity          = identityName(id);
        allReadOnly         = allReadOnly && f.access == Access::readOnly;
        r.readHasSideEffect = r.readHasSideEffect || f.readAction != ReadAction::empty;
        // startBit/stopBit describe element 0 of a dim field; the masks cover every element.
        std::uint64_t const elements = f.dim == 0 ? 1 : f.dim;
        for(std::uint64_t i = 0; i < elements; ++i) {
            auto const start = f.startBit + i * f.dimIncrement;
            auto const stop  = f.stopBit + i * f.dimIncrement;
            if(f.key) { r.mustSupplyMask |= maskFromRange(stop, start); }
            switch(id) {
            case Identity::none: r.zeroMask = clearBits(r.zeroMask, start, stop); break;
            case Identity::one:
                r.zeroMask = clearBits(r.zeroMask, start, stop);
                r.oneMask |= maskFromRange(stop, start);
                break;
            case Identity::zero:   // stays in zeroMask
                if(f.access != Access::readOnly) {
                    r.writeBackActsMask |= maskFromRange(stop, start);
                }
                break;
            }
        }
    }
    r.access = allReadOnly ? Access::readOnly : Access::readWrite;
}

// The bits of a field, every element of a dim field.
inline std::uint64_t fieldMask(Field const& f) {
    std::uint64_t const elements = f.dim == 0 ? 1 : f.dim;
    std::uint64_t       mask     = 0;
    for(std::uint64_t i = 0; i < elements; ++i) {
        mask |= maskFromRange(f.stopBit + i * f.dimIncrement, f.startBit + i * f.dimIncrement);
    }
    return mask;
}

constexpr bool isWriteOnly(Access a) { return a == Access::writeOnly || a == Access::writeOnce; }

// After computeMasks. A write-only field W without a write identity (no modifiedWriteValues that
// makes a written 0 or 1 harmless), not a key, not accepted: a partial write of a sibling field F
// that needs a read (the needsRead rule of Kvasir's Register/Exec.hpp) and does not cover W reads
// the register and writes W back as read - a strobe fires again, a FIFO push pushes. Returns the
// siblings that do so per such W; W's bits go into writeOnlyNoIdentityMask. With `derived`, a
// register none of whose fields can be read is a whole-register-only register (writeOnlyRegister)
// and has no findings: a partial write of it will not read.
inline std::vector<std::pair<std::size_t,
                             std::vector<std::size_t>>>
computeWriteOnlyHazards(Register&          r,
                        WriteOnlyRegisters registers) {
    r.writeOnlyNoIdentityMask = 0;
    r.writeOnlyRegister
      = registers == WriteOnlyRegisters::derived && !r.fields.empty()
     && std::ranges::all_of(r.fields, [](Field const& f) { return isWriteOnly(f.access); });
    std::vector<std::pair<std::size_t, std::vector<std::size_t>>> hazards;
    if(r.readHasSideEffect || r.writeOnlyRegister) {
        return hazards;
    }   // never read-modify-written
    auto const all = maskFromRange(DataTypeSize(r.dataType) - 1, 0);
    for(std::size_t wi = 0; wi < r.fields.size(); ++wi) {
        auto const& w = r.fields[wi];
        if(!isWriteOnly(w.access) || w.key || w.writeOnlyAccepted || w.identity != "none") {
            continue;
        }
        auto const               wm = fieldMask(w);
        std::vector<std::size_t> by;
        for(std::size_t fi = 0; fi < r.fields.size(); ++fi) {
            auto const& f = r.fields[fi];
            if(fi == wi || f.access == Access::readOnly) { continue; }
            auto const cm        = fieldMask(f);
            bool const needsRead = ((cm | r.zeroMask) | (r.oneMask & ~cm)) != all;
            if(needsRead && (wm & ~cm) != 0) { by.push_back(fi); }
        }
        if(!by.empty()) {
            r.writeOnlyNoIdentityMask |= wm;
            hazards.emplace_back(wi, std::move(by));
        }
    }
    return hazards;
}

inline std::variant<Register,
                    derived_t>
RegisterFromSVD(pugi::xml_node const& reg,
                Access                access,
                DataType              dataType) {
    Register registerResult;
    registerResult.name = sanitizeName(getCheckedSVD<std::string>(reg, "name", "Register"));
    auto displayName    = getOptionalSVD<std::string>(reg, "displayName");
    if(displayName) { registerResult.name = sanitizeName(*displayName); }
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
        if(registerResult.dim == 0) { throw std::runtime_error("register dim should not be 0"); }
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
    // No fields is valid SVD: the register is one value (RP USB_DPRAM buffers), reached through
    // the generated FULLREGISTER.
    if(fields.empty()) { return registerResult; }

    for(auto const& field_node : fields.children("field")) {
        auto fieldFromSvd = FieldFromSVD(field_node,
                                         accessType,
                                         registerResult.dataType,
                                         registerResult.resetValue,
                                         modifiedWriteValues,
                                         readAction);

        // An element past the register's width is a broken SVD: its masks would be computed
        // with shifts past the 64 bits they are held in.
        std::uint64_t const elements = fieldFromSvd.dim == 0 ? 1 : fieldFromSvd.dim;
        auto const lastStop = fieldFromSvd.stopBit + (elements - 1) * fieldFromSvd.dimIncrement;
        if(lastStop >= DataTypeSize(registerResult.dataType)) {
            throw std::runtime_error(
              std::format("field {} of register {} ends at bit {}, past the register's {} bits",
                          fieldFromSvd.name,
                          registerResult.name,
                          lastStop,
                          DataTypeSize(registerResult.dataType)));
        }
        registerResult.fields.push_back(std::move(fieldFromSvd));
    }

    computeMasks(registerResult);
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
        if(!found) { throw std::runtime_error("derived register not found"); }
    }
    for(auto& derivedReg : parsedRegsDerived) { parsedRegs.push_back(std::move(derivedReg)); }

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
    if(pos != std::string::npos) { registerGroup.name.erase(pos, 4); }

    registerGroup.registers = makeRegister(regg.children("register"), access, type);

    return registerGroup;
}

// A cluster without <dim> is one of a peripheral's modes (SERCOM's USART / SPI / I2CM, USB's DEVICE /
// HOST) and becomes a peripheral of its own, <PERIPHERAL>_<CLUSTER>. Its registers sit at the
// cluster's addressOffset; a cluster with <dim> nested in it (USB DEVICE's DEVICE_ENDPOINT[8]) is
// that peripheral's register group. The parent's own register groups are not the mode's.
inline Peripheral ClusterFromSVD(pugi::xml_node    cluster,
                                 Peripheral const& peripheral_ref,
                                 Access            access,
                                 DataType          type) {
    Peripheral peripheral = peripheral_ref;
    peripheral.name = getCheckedSVD<std::string>(cluster, "name", "Cluster " + peripheral_ref.name);
    peripheral.description
      += " " + sanitizeDescription(getDefaultSVD(cluster, "description", std::string{}));

    auto const clusterOffset = getDefaultSVD(cluster, "addressOffset", std::uint64_t{0});

    peripheral.registers = makeRegister(cluster.children("register"), access, type);
    for(auto& reg : peripheral.registers) { reg.addressOffset += clusterOffset; }

    peripheral.registerGroups.clear();
    for(auto const& nested : cluster.children("cluster")) {
        if(nested.child("dim").empty()) {
            std::print(stderr,
                       "cluster {} in {} of {}: a cluster without dim inside a cluster is not "
                       "supported, skipped\n",
                       getDefaultSVD(nested, "name", std::string{"?"}),
                       peripheral.name,
                       peripheral_ref.name);
            continue;
        }
        auto group = RegisterGroupFromSVD(nested, access, type);
        group.addressOffset += clusterOffset;
        peripheral.registerGroups.push_back(std::move(group));
    }

    return peripheral;
}

struct InstanceName {
    std::string   stem;
    std::uint64_t index;
};

// The instance in a peripheral's name: trailing digits (USART3 -> USART, 3), or with
// letterInstances one trailing capital (GPIOC -> GPIO, 2). nullopt for a name with neither.
inline std::optional<InstanceName> splitInstanceName(std::string_view name,
                                                     bool             letterInstances) {
    if(letterInstances) {
        if(name.size() < 2 || name.back() < 'A' || name.back() > 'Z') { return std::nullopt; }
        return InstanceName{std::string{name.substr(0, name.size() - 1)},
                            static_cast<std::uint64_t>(name.back() - 'A')};
    }
    auto const firstDigit = name.find_last_not_of("0123456789") + 1;
    if(firstDigit == 0 || firstDigit == name.size()) { return std::nullopt; }
    return InstanceName{std::string{name.substr(0, firstDigit)},
                        std::stoull(std::string{name.substr(firstDigit)})};
}

struct InstanceGroup {
    std::string              stem;
    std::vector<AddressType> instances;
};

// The base peripheral and the ones derived from it, as instances grouped by name stem; the base's
// group first, its own instance first. Each instance number comes from the peripheral's own name
// (TC3 <- TC4 is TC 3 and 4; GPIOF <- GPIOC is GPIO 5 and 2); a base without one is instance 0
// (AC <- AC1). A derived peripheral with a stem of its own makes a group of its own (ST: UART4 <-
// USART3). Empty when nothing derives from base.
inline std::vector<InstanceGroup> groupInstances(std::string const&      baseName,
                                                 std::uint64_t           baseAddress,
                                                 std::vector<derived_t>& derived) {
    std::vector<derived_t*> members;
    for(auto& derived_ref : derived) {
        auto& [name, derivedBase, address, processed] = derived_ref;
        if(derivedBase != baseName) { continue; }
        if(processed) {
            throw std::runtime_error("found derived peripheral multiple times " + name);
        }
        members.push_back(&derived_ref);
    }
    if(members.empty()) { return {}; }

    // Port letters only when there are no digits and every name is the same stem plus a capital.
    bool const letterInstances = [&] {
        if(splitInstanceName(baseName, false)) { return false; }
        auto const base = splitInstanceName(baseName, true);
        return base && std::ranges::all_of(members, [&](derived_t const* member) {
                   auto const instance = splitInstanceName(std::get<0>(*member), true);
                   return instance && instance->stem == base->stem;
               });
    }();
    auto const base
      = splitInstanceName(baseName, letterInstances).value_or(InstanceName{baseName, 0});

    std::vector<InstanceGroup> groups{
      {base.stem, {AddressType{base.index, baseAddress}}}
    };
    for(auto* member : members) {
        auto& [name, derivedBase, address, processed] = *member;
        auto const instance                           = splitInstanceName(name, letterInstances);
        if(!instance) {
            std::print(stderr, "something wrong ? {} {}\n", baseName, name);
            continue;
        }
        processed  = true;
        auto group = std::ranges::find(groups, instance->stem, &InstanceGroup::stem);
        if(group == groups.end()) {
            groups.push_back({instance->stem, {}});
            group = std::prev(groups.end());
        }
        group->instances.push_back(AddressType{instance->index, address});
    }
    if(groups.size() == 1 && groups.front().instances.size() == 1) { return {}; }
    return groups;
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
        // A peripheral that only names interrupts (RP2350 SPARE_IRQ) has no registers on purpose.
        if(peripheral.child("interrupt").empty()) {
            std::print(stderr, "no registers in {}\n", peripheral_result.name);
        }
        return peripheral_result;
    }

    peripheral_result.registers = makeRegister(registers.children("register"), access, type);

    auto const cluster = registers.children("cluster");
    for(auto const& cluster_node : cluster) {
        if(cluster_node.child("dim").empty()) { continue; }
        peripheral_result.registerGroups.push_back(
          RegisterGroupFromSVD(cluster_node, access, type));
    }

    cluster_t cluster_result;
    cluster_result.second = peripheral_result.name;
    for(auto const& cluster_node : cluster) {
        if(!cluster_node.child("dim").empty()) { continue; }
        cluster_result.first.push_back(
          ClusterFromSVD(cluster_node, peripheral_result, access, type));
    }

    if(!cluster_result.first.empty()) { return cluster_result; }

    return peripheral_result;
}
}   // namespace

namespace {
// "bit 11", "bits 15:8"; a dim field: its first element and the count
inline std::string bitsText(Field const& f) {
    std::string t = f.startBit == f.stopBit ? std::format("bit {}", f.startBit)
                                            : std::format("bits {}:{}", f.stopBit, f.startBit);
    if(f.dim != 0) { t += std::format(" x{} every {}", f.dim, f.dimIncrement); }
    return t;
}

inline void collectWriteOnlyFindings(std::string const&             where,
                                     std::vector<Register>&         registers,
                                     ConvertOptions const&          options,
                                     std::vector<WriteOnlyFinding>* findings) {
    for(auto& r : registers) {
        for(auto const& [wi, by] : computeWriteOnlyHazards(r, options.writeOnlyRegisters)) {
            if(findings == nullptr) { continue; }
            WriteOnlyFinding finding{.where       = where,
                                     .reg         = r.name,
                                     .field       = r.fields[wi].name,
                                     .bits        = bitsText(r.fields[wi]),
                                     .rewrittenBy = {},
                                     .readableSibling
                                     = std::ranges::any_of(r.fields, [](Field const& f) {
                                           return !isWriteOnly(f.access);
                                       })};
            for(auto const fi : by) { finding.rewrittenBy.push_back(r.fields[fi].name); }
            findings->push_back(std::move(finding));
        }
    }
}

inline void collectWriteOnlyFindings(std::string const&             where,
                                     Peripheral&                    peripheral,
                                     ConvertOptions const&          options,
                                     std::vector<WriteOnlyFinding>* findings) {
    collectWriteOnlyFindings(where, peripheral.registers, options, findings);
    for(auto& group : peripheral.registerGroups) {
        collectWriteOnlyFindings(where + "." + group.name, group.registers, options, findings);
    }
}
}   // namespace

// The one-line report of a finding, as the converter prints it.
inline std::string describe(WriteOnlyFinding const& f) {
    std::string by;
    for(auto const& name : f.rewrittenBy) { by += (by.empty() ? "" : ", ") + name; }
    return std::format(
      "{}.{}.{} ({}) is write-only with no write identity: a write of {} reads the "
      "register and writes {} back as read",
      f.where,
      f.reg,
      f.field,
      f.bits,
      by,
      f.field);
}

// ChipFromSVD with the write-only guard's options; the findings (one per unclassified write-only
// field, each peripheral once: derived peripherals share their base's registers) go to `findings`.
inline Chip ChipFromSVD(pugi::xml_node const&          device,
                        ConvertOptions const&          options,
                        std::vector<WriteOnlyFinding>* findings) {
    Chip chip;
    auto access      = getDefaultSVD(device, "access", Access::readWrite);
    chip.name        = getCheckedSVD<std::string>(device, "name", "Chip");
    chip.description = sanitizeDescription(getDefaultSVD(device, "description", std::string{}));
    auto width       = getDefaultSVD(device, "width", DataType::u32);
    auto size        = getDefaultSVD(device, "size", DataType::u32);

    auto peripherals = device.child("peripherals");
    if(peripherals.empty()) { throw std::runtime_error("no peripherals"); }

    std::vector<derived_t> derived;
    std::vector<cluster_t> cluster;
    for(auto const& peripheral : peripherals.children("peripheral")) {
        for(auto const& irq : peripheral.children("interrupt")) {
            chip.interrupts.push_back({getCheckedSVD<std::string>(irq, "name", "interrupt"),
                                       getCheckedSVD<std::uint64_t>(irq, "value", "interrupt")});
        }
        auto peripheral_variant = PeripheralFromSVD(peripheral, access, width, size);
        if(std::holds_alternative<derived_t>(peripheral_variant)) {
            derived.push_back(std::move(std::get<derived_t>(peripheral_variant)));
        } else if(std::holds_alternative<cluster_t>(peripheral_variant)) {
            cluster.push_back(std::move(std::get<cluster_t>(peripheral_variant)));
        } else {
            chip.peripherals.push_back(std::move(std::get<Peripheral>(peripheral_variant)));
        }
    }

    // Before any peripheral is copied into its derived instances: the copies carry the result, the
    // findings name each SVD peripheral once.
    for(auto& peripheral_ref : chip.peripherals) {
        collectWriteOnlyFindings(peripheral_ref.name, peripheral_ref, options, findings);
    }
    for(auto& [clusterPeripherals, clusterName] : cluster) {
        for(auto& peripheral_ref : clusterPeripherals) {
            collectWriteOnlyFindings(clusterName + "." + peripheral_ref.name,
                                     peripheral_ref,
                                     options,
                                     findings);
        }
    }

    // A merged peripheral and the instance groups it came from, for the name clash check below.
    std::vector<std::pair<std::size_t, std::vector<AddressType>>> merged;
    std::vector<Peripheral>                                       otherStems;

    for(std::size_t i = 0; i < chip.peripherals.size(); ++i) {
        auto& peripheral_ref = chip.peripherals[i];
        auto  groups         = groupInstances(peripheral_ref.name,
                                              peripheral_ref.baseAddresses.front().address,
                                              derived);
        if(groups.empty()) { continue; }

        peripheral_ref.name          = groups.front().stem;
        peripheral_ref.type          = RepeatType::cluster;
        peripheral_ref.baseAddresses = groups.front().instances;

        // ST: UART4 derivedFrom USART3 - same registers, a family of its own.
        for(auto& group : groups | std::views::drop(1)) {
            Peripheral copy    = peripheral_ref;
            copy.name          = group.stem;
            copy.baseAddresses = std::move(group.instances);
            otherStems.push_back(std::move(copy));
        }
        merged.emplace_back(i, peripheral_ref.baseAddresses);
    }
    for(auto& peripheral_ref : otherStems) {
        merged.emplace_back(chip.peripherals.size(), peripheral_ref.baseAddresses);
        chip.peripherals.push_back(std::move(peripheral_ref));
    }

    // ST: TIM1 <- TIM8 and TIM3 <- TIM4 both have the stem TIM, with different registers; such
    // groups are named after their members instead: TIM1_8, TIM3_4.
    auto const nameCount = [&](std::string const& name) {
        return std::ranges::count(chip.peripherals, name, &Peripheral::name);
    };
    std::vector<std::string> newNames;
    for(auto const& [index, instances] : merged) {
        auto const& name = chip.peripherals[index].name;
        if(nameCount(name) < 2) {
            newNames.push_back(name);
            continue;
        }
        std::vector<std::uint64_t> numbers;
        std::ranges::transform(instances, std::back_inserter(numbers), &AddressType::index);
        std::ranges::sort(numbers);
        std::string newName = name;
        for(auto const number : numbers) {
            newName += std::format("{}{}", newName == name ? "" : "_", number);
        }
        newNames.push_back(std::move(newName));
    }
    for(std::size_t i = 0; i < merged.size(); ++i) {
        chip.peripherals[merged[i].first].name = newNames[i];
    }

    for(auto& cluster_ref : cluster) {
        auto& [clusterPeripherals, clusterName] = cluster_ref;
        std::string              newName        = clusterName;
        RepeatType               newType        = RepeatType::normal;
        std::vector<AddressType> newAddr;

        auto groups = groupInstances(
          clusterName,
          clusterPeripherals.empty() ? 0 : clusterPeripherals.front().baseAddresses.front().address,
          derived);
        if(!groups.empty()) {
            newName = groups.front().stem;
            newType = RepeatType::cluster;
            // The cluster peripherals carry the base's address as instance 0 already.
            auto const& instances = groups.front().instances;
            newAddr.assign(std::next(instances.begin()), instances.end());
            for(auto& peripheral_ref : clusterPeripherals) {
                peripheral_ref.baseAddresses.front().index = instances.front().index;
            }
            for(auto const& group : groups | std::views::drop(1)) {
                std::print(stderr,
                           "derived peripherals of cluster {} named {}* are not supported\n",
                           clusterName,
                           group.stem);
            }
        }

        for(auto& peripheral_ref : clusterPeripherals) {
            peripheral_ref.name = std::format("{}_{}", newName, peripheral_ref.name);
            peripheral_ref.type = newType;
            std::ranges::copy(newAddr, std::back_inserter(peripheral_ref.baseAddresses));
            chip.peripherals.push_back(std::move(peripheral_ref));
        }
    }

    for(auto const& derived_ref : derived) {
        auto const& [name, baseName, address, processed] = derived_ref;
        if(!processed) { std::print(stderr, "have not found derived peripheral {}\n", name); }
    }

    for(auto& peripheral_ref : chip.peripherals) {
        auto pname = peripheral_ref.name + "_";

        auto removePeripheral = [&](std::string& str_ref) {
            if(str_ref.starts_with(pname) && str_ref.size() > pname.size()) {
                str_ref.erase(0, pname.size());
                // RTC_1 -> 1 is no identifier: keep the underscore, as sanitizeName does for
                // names that start with a digit.
                if(std::isdigit(static_cast<unsigned char>(str_ref.front())) != 0) {
                    str_ref.insert(str_ref.begin(), '_');
                }
            }
        };

        auto removePeripheralReg = [&](auto& reg) {
            removePeripheral(reg.name);
            for(auto& field_ref : reg.fields) {
                removePeripheral(field_ref.name);
                for(auto& value_ref : field_ref.values) { removePeripheral(value_ref.name); }
            }
        };
        for(auto& register_ref : peripheral_ref.registers) { removePeripheralReg(register_ref); }
        for(auto& register_group : peripheral_ref.registerGroups) {
            for(auto& register_ref : register_group.registers) {
                removePeripheralReg(register_ref);
            }
        }
    }

    // one name per vector: SVDs repeat an interrupt in every peripheral that shares it (RP2350 TIMER0/1 ...)
    std::ranges::stable_sort(chip.interrupts, {}, &Interrupt::value);
    auto const dup = std::ranges::unique(chip.interrupts, {}, &Interrupt::value);
    chip.interrupts.erase(dup.begin(), dup.end());
    return chip;
}

inline Chip ChipFromSVD(pugi::xml_node const& device) { return ChipFromSVD(device, {}, nullptr); }
