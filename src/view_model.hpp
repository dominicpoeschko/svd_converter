#pragma once

// The kvasir_bit generator's view model: every rendering decision, computed in C++ from the parsed
// model, so the template is a transcription (one loop per list, no callbacks that read the model
// back). render() adds these keys to a copy of the peripheral's JSON; the json generator never sees
// them. Each function is a pure function of a Register (or Field) and the options, unit-tested in
// tests/view_model_tests.cpp.

#include "inja_wrapper.hpp"
#include "svd/json.hpp"
#include "svd/model.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <format>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace Generator { namespace Kvasir {

    // The converter options the kvasir_bit template reads (svd_converter --write-only-mask,
    // --named-registers); the json generator writes the model alone.
    struct RenderOptions {
        bool writeOnlyMask
          = false;   // Semantics::writeOnlyNoIdentity: unclassified write-only bits
        bool namedRegisters
          = false;   // every register's Address names it (Register::Mode's 2nd argument)
    };

    namespace ViewModel {

        inline std::string upper(std::string s) {
            std::ranges::transform(s, s.begin(), [](char c) {
                return static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
            });
            return s;
        }

        inline std::string lower(std::string s) {
            std::ranges::transform(s, s.begin(), [](char c) {
                return static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
            });
            return s;
        }

        // "0x" and the value in upper-case hex, as wide as the type ("min": two digits at least)
        inline std::string hex(std::uint64_t    value,
                               std::string_view type) {
            unsigned width{};
            if(type == "u8" || type == "min") {
                width = 2;
            } else if(type == "u16") {
                width = 4;
            } else if(type == "u32") {
                width = 8;
            } else if(type == "u64") {
                width = 16;
            } else {
                throw std::runtime_error("hex failed");
            }
            return std::vformat("0x{:0{}X}", std::make_format_args(value, width));
        }

        inline std::string typeKey(Svd::DataType type) {
            return inja::json(type).get<std::string>();
        }

        inline std::string hex(std::uint64_t value,
                               Svd::DataType type) {
            return hex(value, typeKey(type));
        }

        inline bool isCluster(Svd::Field const& field) {
            return field.repType == Svd::RepeatType::cluster;
        }

        // The Address's mode argument (Kvasir::Register::Mode<Semantics, Register>), one line printed after the
        // data type, or none for a plain register. Semantics names only the facts that differ from the default, as
        // designated initialisers in declaration order:
        //  - a register nothing of which reads: writeOnlyRegister, and its key (mustSupply) - it never reads, so a
        //    partial write that does not name the key is a compile error (Register/Exec.hpp); no write-back mask;
        //  - else mustSupply (a key), readSideEffect, writeOnlyNoIdentity (--write-only-mask), and writeBackActs
        //    when it is not 0 (Semantics' default) and a read has no side effect (a whole write only then).
        // The register names itself when it has a hazard (key, read side effect, unclassified write-only bits,
        // write-only register) or with --named-registers.
        inline std::vector<std::string> modeLines(Svd::Register const& r,
                                                  RenderOptions const& o) {
            std::string const        name = upper(r.name);
            std::vector<std::string> facts;
            bool                     hazard = false;
            if(r.mustSupplyMask != 0) {
                facts.push_back(".mustSupply = " + hex(r.mustSupplyMask, r.dataType));
                hazard = true;
            }
            if(r.writeOnlyRegister) {
                facts.emplace_back(".writeOnlyRegister = true");
                hazard = true;
            } else {
                if(r.readHasSideEffect) {
                    facts.emplace_back(".readSideEffect = true");
                    hazard = true;
                }
                if(o.writeOnlyMask && r.writeOnlyNoIdentityMask != 0) {
                    facts.push_back(".writeOnlyNoIdentity = "
                                    + hex(r.writeOnlyNoIdentityMask, r.dataType));
                    hazard = true;
                }
                if(!r.readHasSideEffect && r.writeBackActsMask != 0) {
                    facts.push_back(".writeBackActs = " + hex(r.writeBackActsMask, r.dataType));
                }
            }
            bool const named = hazard || o.namedRegisters;
            if(facts.empty() && !named) { return {}; }
            std::string semantics = "{";
            for(std::size_t i = 0; i < facts.size(); ++i) {
                semantics += (i == 0 ? "" : ", ") + facts[i];
            }
            semantics += '}';
            return {", Kvasir::Register::Mode<" + semantics + (named ? ", " + name : std::string{})
                    + ">"};
        }

        // The register's reset value as its own type: some SVDs give one wider than the register (E51
        // SDHC HC1R 0xE00 on an 8-bit register); the fields' reset values come from its bits anyway.
        inline std::uint64_t resetValue(Svd::Register const& r) {
            switch(r.dataType) {
            case Svd::DataType::u8:  return r.resetValue & 0xFFU;
            case Svd::DataType::u16: return r.resetValue & 0xFFFFU;
            case Svd::DataType::u32: return r.resetValue & 0xFFFF'FFFFU;
            case Svd::DataType::u64: return r.resetValue;
            }
            throw std::runtime_error("dataType failed");
        }

        // The name of the enum value the field resets to (lower case), "???" when none has its value.
        inline std::string resetName(Svd::Field const& field) {
            for(auto const& value : field.values) {
                if(value.value == field.resetValue) { return lower(value.name); }
            }
            return "???";
        }

        // overrideDefaults exists when there is something to write, every writable field has a reset
        // value (some bit inside the register's <resetMask>) and every enum field among them resets to
        // one of its named values (the SDK's DefaultValues writes the rest).
        inline bool hasDefaults(Svd::Register const& r) {
            bool anyField = false;
            for(auto const& field : r.fields) {
                if(field.access == Svd::Access::readOnly) { continue; }
                anyField = true;
                if(!hasResetValue(field, r.resetMask)) { return false; }
                if(!field.values.empty() && resetName(field) == "???") { return false; }
            }
            return anyField;
        }

        // The field's full write semantics. The plain kinds keep their short names (most fields; the
        // generated headers stay readable), everything else is the fully qualified Access<>, so a
        // peripheral with something called Access in scope cannot capture it.
        inline std::string accessText(Svd::Access              access,
                                      Svd::ModifiedWriteValues mwv,
                                      Svd::ReadAction          ra) {
            auto const a = inja::json(access).get<std::string>();
            auto const m = inja::json(mwv).get<std::string>();
            auto const r = inja::json(ra).get<std::string>();
            if(m == "empty" && r.empty()
               && (a == "readWrite" || a == "readOnly" || a == "writeOnly"))
            {
                auto s = a;
                s[0]   = static_cast<char>(std::toupper(static_cast<unsigned char>(s[0])));
                return s + "Access";
            }
            return "Kvasir::Register::Access<Kvasir::Register::AccessType::" + a
                 + ", Kvasir::Register::ReadActionType::" + (r.empty() ? std::string{"normal"} : r)
                 + ", Kvasir::Register::ModifiedWriteValueType::"
                 + (m == "empty" ? std::string{"normal"} : m) + ">";
        }

        inline std::string accessText(Svd::Field const& field) {
            return accessText(field.access, field.modifiedWriteValues, field.readAction);
        }

        // One field location of the register: a plain field once, a field cluster (dim) once per
        // element. The register's lists (fields, field_names, fmt_string) are all in this order.
        struct Element {
            std::string name;   // "irqover", "pri[3]"
            std::string expr;   // "irqover", "PRI<3>::pri"
            bool        isEnum = false;
        };

        inline std::vector<Element> elements(Svd::Register const& r) {
            std::vector<Element> out;
            for(auto const& field : r.fields) {
                auto const l      = lower(field.name);
                bool const isEnum = field.type == Svd::FieldType::typeEnum;
                if(!isCluster(field)) {
                    out.push_back({.name = l, .expr = l, .isEnum = isEnum});
                    continue;
                }
                for(std::uint64_t i = 0; i < field.dim; ++i) {
                    out.push_back({.name   = std::format("{}[{}]", l, i),
                                   .expr   = std::format("{}<{}>::{}", upper(field.name), i, l),
                                   .isEnum = isEnum});
                }
            }
            return out;
        }

        // Where the register sits, for its log text: the peripheral, the register group (if any).
        struct Place {
            std::string peripheral;
            std::string group;   // empty outside a register group
        };

        // fmt_string's body as it goes between the quotes of a C++ literal:
        // "periph::[group[{}]::]reg[[{}]](\n\"field\": fmt\n...)" with {} for an enum's value name and
        // 0x{:x} for a number.
        inline std::string fmtString(Place const&         place,
                                     Svd::Register const& r) {
            std::string s = lower(place.peripheral) + "::";
            if(!place.group.empty()) { s += lower(place.group) + "[{}]::"; }
            s += lower(r.name);
            if(r.type == Svd::RepeatType::cluster) { s += "[{}]"; }
            s += "(\\n";
            bool first = true;
            for(auto const& e : elements(r)) {
                if(!first) { s += "\\n"; }
                first = false;
                s += "\\\"" + e.name + "\\\": " + (e.isEnum ? "{}" : "0x{:x}");
            }
            return s + ")";
        }

        // field_dims' size: one per dimension in front of the fields (group, cluster)
        inline std::string dimsCount(Place const&         place,
                                     Svd::Register const& r) {
            std::string s = "0";
            if(!place.group.empty()) { s += "+ 1"; }
            if(r.type == Svd::RepeatType::cluster) { s += "+ 1"; }
            return s;
        }

        inline inja::json registerView(inja::json const&    model,
                                       Place const&         place,
                                       RenderOptions const& options) {
            auto const r       = model.get<Svd::Register>();
            inja::json view    = model;
            view["cppName"]    = upper(r.name);
            view["resetValue"] = resetValue(r);
            view["modeLines"]  = modeLines(r, options);
            view["fmtString"]  = fmtString(place, r);
            view["dimsCount"]  = dimsCount(place, r);
            view["cluster"]    = r.type == Svd::RepeatType::cluster;
            // FULLREGISTER's access (semantics.cpp fullRegisterAccess): without fields the register's own
            // <access>; with fields readOnly when every field is, writeOnly when no field can be read,
            // readWrite otherwise
            view["accessText"]
              = accessText(r.access, Svd::ModifiedWriteValues::empty, Svd::ReadAction::empty);

            auto& fields = view["fields"];
            for(std::size_t i = 0; i < r.fields.size(); ++i) {
                auto const& field      = r.fields[i];
                fields[i]["cluster"]   = isCluster(field);
                fields[i]["isEnum"]    = field.type == Svd::FieldType::typeEnum;
                fields[i]["resetName"] = resetName(field);
                // a field wholly outside <resetMask> has no reset value: its comment says so instead
                // of printing the 0 the reader assumed
                fields[i]["hasReset"]   = hasResetValue(field, r.resetMask);
                fields[i]["accessText"] = accessText(field);
                // ValC names the field through its enclosing struct: a value with the field's own
                // name (SOURCE::clk_sys and CLK_SYSValC::clk_sys) would otherwise change the meaning
                // of the unqualified name inside ValC, which gcc 16 refuses (-Wchanges-meaning).
                fields[i]["owner"] = isCluster(field) ? upper(field.name) : upper(r.name);
            }

            inja::json elems = inja::json::array();
            for(auto const& e : elements(r)) {
                elems.push_back({
                  {"name", e.name},
                  {"expr", e.expr}
                });
            }
            view["elements"]    = elems;
            view["hasDefaults"] = hasDefaults(r);
            return view;
        }

        // The peripheral's JSON with every register's view keys added.
        inline inja::json peripheralView(inja::json const&    peripheral,
                                         RenderOptions const& options) {
            inja::json  view = peripheral;
            auto const& name = peripheral.at("name").get_ref<std::string const&>();
            for(auto& reg : view["registers"]) {
                reg = registerView(reg, {.peripheral = name, .group = {}}, options);
            }
            for(auto& group : view["registerGroups"]) {
                auto const& groupName = group.at("name").get_ref<std::string const&>();
                for(auto& reg : group["registers"]) {
                    reg = registerView(reg, {.peripheral = name, .group = groupName}, options);
                }
            }
            return view;
        }
    }   // namespace ViewModel
}}   // namespace Generator::Kvasir
