#pragma once

#include "svd_types.hpp"
#include "inja_wrapper.hpp"
#include "fmt_wrapper.hpp"

#include <string>

namespace Generator { namespace Kvasir {

    static constexpr auto PeripheralTemplate = R"(
#pragma once
#include "kvasir/Register/Register.hpp"
#include "kvasir/Register/Types.hpp"
#include "kvasir/Register/Utility.hpp"

#include <cstdint>
#include <string_view>

//{{ description }}
namespace Kvasir::Peripheral::{{ upper(name) }} {
    using Kvasir::Register::Address;
    using Kvasir::Register::FieldLocation;
    using Kvasir::Register::FieldValue;
    using Kvasir::Register::maskFromRange;
    using Kvasir::Register::ReadWriteAccess;
    using Kvasir::Register::WriteOnlyAccess;
    using Kvasir::Register::ReadOnlyAccess;
    using Kvasir::Register::value;

    //{{ description }}
    template<unsigned Instance {% if type == "normal" %} = 0 {% endif %}>
    struct Registers{

        static constexpr {{ dataType(addressType) }} getBaseAddr(unsigned instance){
            {% for i in baseAddresses %}
            if( instance == {{ i.index }}) {
                return {{ hex(i.address, addressType) }};
            }
            {% endfor %}

            return 0;
        }

        static constexpr {{ dataType(addressType) }} baseAddr = getBaseAddr(Instance);

        static_assert(baseAddr != 0, "Wrong Instance?");

        {% for register in registers %}
            {% set baseAddrString = "baseAddr" %}
            {% set inRegisterGroup = false %}
            {% include "Register" %}
        {% endfor %}
        {% for registerGroup in registerGroups %}
            {% include "RegisterGroup" %}
        {% endfor %}

    };
}
)";

    static constexpr auto RegisterGroupTemplate = R"(
//{{ registerGroup.description }}
template<unsigned GroupIndex>
struct {{ upper(registerGroup.name) }} {
    static_assert(GroupIndex < {{ registerGroup.dim }});
    static constexpr {{ dataType(addressType) }} groupBaseAddr =
      baseAddr
        + GroupIndex  * {{ hex(registerGroup.dimIncrement, "min") }}
        + {{ hex(registerGroup.addressOffset, "min") }};
    {% for register in registerGroup.registers %}
        {% set baseAddrString = "groupBaseAddr" %}
        {% set inRegisterGroup = true %}
        {% include "Register" %}
    {% endfor %}
};
)";

    static constexpr auto RegisterTemplate = R"delim(
//{{ register.description }}
{% if register.type == "cluster" %}
template<unsigned Index>
{% endif %}
struct {{ upper(register.name) }} {
{% if register.type == "cluster" %}
    static_assert(Index < {{ register.dim }});
{% endif %}
    // ResetValue: {{ hex(register.resetValue, register.dataType) }}
    using Addr = Address< {{ baseAddrString }}
                            + {{ hex(register.addressOffset, "min") }}
                            {% if register.type != "normal" %}
                                + Index * {{ hex(register.dimIncrement, "min") }}
                            {% endif %},
                        {{ hex(register.zeroMask, register.dataType) }},
                        {{ hex(register.oneMask, register.dataType) }},
                        {{ dataType(register.dataType) }}>;

    {% for field in register.fields %}
        {% if field.type == "enum" %}
            {% include "Enum" %}
        {% else %}
            {% include "Field" %}
        {% endif %}
    {% endfor %}

    {% if length(register.fields) > 0 and allValidDefault(register) %}
    using default_values = decltype(MPL::list(
    {% for field in register.fields %}
        {% if field.type == "enum" %}
            {% if field.repType == "cluster" %}
                {% for i in range(field.dim) %}
                    write( {{ upper(field.name)  }}<{{ i }}>::{{ upper(field.name) }}ValC::{{ enumResetValue(field.resetValue, field.values) }})
                    {% if not loop/is_last %}
                        ,
                    {% endif %}
                {% endfor %}
            {% else %}
                write( {{ upper(field.name) }}ValC::{{ enumResetValue(field.resetValue, field.values) }})
            {% endif %}
        {% else %}
            {% if field.repType == "cluster" %}
                {% for i in range(field.dim) %}
                    write( {{ upper(field.name) }}<{{ i }}>::{{ lower(field.name) }}, value<{{ dataType(field.dataType) }}, {{ field.resetValue }}>() )
                    {% if not loop/is_last %}
                        ,
                    {% endif %}
                {% endfor %}
            {% else %}
                write( {{ lower(field.name) }}, value<{{ dataType(field.dataType) }}, {{ field.resetValue }}>() )
            {% endif %}
        {% else %}
        {% endif %}
        {% if not loop/is_last %}
            ,
        {% endif %}
    {% endfor %}
    ));
    template<typename... Ts>
    static constexpr auto overrideDefaults(Ts const&... ts){
        return Register::overrideDefaults<default_values>::value(ts...);
    }
    template<typename... Ts>
    static constexpr auto overrideDefaultsRuntime(Ts const&... ts){
        return Register::overrideDefaultsRuntime<default_values>::exec(ts...);
    }

    {% endif %}

    static constexpr std::string_view fmt_string{"{{ lower(name) }}::{% set noNL=1 %}
    {% if inRegisterGroup %}
{{ lower(registerGroup.name) }}[{}]::{% set noNL=1 %}
    {% endif %}
{{ lower(register.name) }}{% set noNL=1 %}
    {% if register.type == "cluster" %}
[{}]{% set noNL=1 %}
    {% endif %}
({% set noNL=1 %}
    {% for field in register.fields %}
        {% if field.type == "enum" %}
            {% set fmt="{}" %}
        {% else %}
            {% set fmt="0x{:x}" %}
        {% endif %}
        {% if field.repType == "cluster" %}
            {% for i in range(field.dim) %}
\"{{ lower(field.name)  }}[{{ i }}]\": {{ fmt }}{% set noNL=1 %}
                {% if not loop/is_last -%}
                    , {% set noNL=1 %}
                {% endif %}
            {% endfor %}
        {% else %}
\"{{ lower(field.name) }}\": {{ fmt }}{% set noNL=1 %}
        {% endif %}
        {% if not loop/is_last -%}
            , {% set noNL=1 %}
        {% endif %}
    {% endfor %})"};

    template<typename F>
    static constexpr auto apply_fields(F&& f){
        {% if length(register.fields) > 0  %}
            auto const regs = apply(read(
        {% endif %}
           {% for field in register.fields %}
                {% if field.repType == "cluster" %}
                    {% for i in range(field.dim) %}
                        {{ upper(field.name) }}<{{ i }}>::{{ lower(field.name) }}
                        {% if not loop/is_last -%}
                            ,
                        {% endif %}
                    {% endfor %}
                {% else %}
                    {{ lower(field.name) }}
                {% endif %}
                {% if not loop/is_last -%}
                    ,
                {% endif %}
            {% endfor %}
        {% if length(register.fields) > 0  %}
            ));
        {% endif %}

        {% set getIndex = 0 %}
        return std::invoke(std::forward<F>(f)
            {% if length(register.fields) > 0  %}
                ,
            {% endif %}
            {% for field in register.fields %}
                {% if field.type == "enum" %}
                    {% set cast_t=upper(field.name) + "Val" %}
                {% else %}
                    {% set cast_t=dataType(field.dataType) %}
                {% endif %}
                {% if field.repType == "cluster" %}
                    {% for i in range(field.dim) %}
                        {% if field.type == "enum" %}
                            static_cast<typename {{ upper(field.name) }}<{{ i }}>::{{ upper(field.name) }}Val>(get<{{ getIndex }}>(regs))
                        {% else %}
                            static_cast<{{ cast_t }}>(get<{{ getIndex }}>(regs))
                        {% endif %}
                        {% set getIndex = getIndex + 1 %}
                        {% if not loop/is_last -%}
                            ,
                        {% endif %}
                    {% endfor %}
                {% else %}
                    static_cast<{{ cast_t }}>(get<{{ getIndex }}>(regs))
                    {% set getIndex = getIndex + 1 %}
                {% endif %}
                {% if not loop/is_last -%}
                    ,
                {% endif %}
            {% endfor %}
        );
    }
    template<typename F>
    static constexpr auto apply_fields_with_dim(F&& f) {
        return apply_fields([&](auto&&... fields) {
            return std::invoke(std::forward<F>(f),
            {% if inRegisterGroup %}
                GroupIndex,
            {% endif %}
            {% if register.type == "cluster" %}
                Index,
            {% endif %}
            fields...);
        });
    }
};
)delim";

    static constexpr auto EnumTemplate = R"(
//{{ field.description }}
{% if field.repType == "cluster" %}
template<unsigned FieldIndex>
struct {{ upper(field.name) }} {
    static_assert(FieldIndex < {{ field.dim }});
{% endif %}

enum class {{ upper(field.name) }}Val : {{ dataType(field.dataType) }} {
    {% for value in field.values %}
    {{ lower(value.name) }} = {{ value.value }}, //{{ value.description }}
    {% endfor %}
};

// ResetValue: {{ enumResetValue(field.resetValue, field.values) }} {{ field.resetValue }}
static constexpr FieldLocation<Addr,
                               maskFromRange(
                                {{ field.stopBit }}
                                {% if field.repType != "normal" %}
                                    + FieldIndex * {{ field.dimIncrement }}
                                {% endif %},
                                {{ field.startBit }}
                                {% if field.repType != "normal" %}
                                    + FieldIndex * {{ field.dimIncrement }}
                                {% endif %}),
                               {{ makeAccess(field.access, field.modifiedWriteValues, field.readAction) }},
                               {{ upper(field.name) }}Val>
    {{ lower(field.name) }}{};

struct {{ upper(field.name) }}ValC{
    {% for value in field.values %}
    static constexpr FieldValue<typename decltype({{ lower(field.name) }})::type,
                                {{ upper(field.name) }}Val::{{ lower(value.name) }}>
                                {{ lower(value.name) }}{};
    {% endfor %}
};

{% if field.repType == "cluster" %}
};
{% endif %}
)";

    static constexpr auto FieldTemplate = R"(
//{{ field.description }}
{% if field.repType == "cluster" %}
template<unsigned FieldIndex>
struct {{ upper(field.name) }} {
    static_assert(FieldIndex < {{ field.dim }});
{% endif %}
// ResetValue: {{ field.resetValue }}
static constexpr FieldLocation<Addr,
              maskFromRange(
                {{ field.stopBit }}
                {% if field.repType != "normal" %}
                    + FieldIndex * {{ field.dimIncrement }}
                {% endif %},
                {{ field.startBit }}
                {% if field.repType != "normal" %}
                    + FieldIndex * {{ field.dimIncrement }}
                {% endif %}),
              {{ makeAccess(field.access, field.modifiedWriteValues, field.readAction) }},
              {{ dataType(field.dataType) }}>
    {{ lower(field.name) }}{};
{% if field.repType == "cluster" %}
};
{% endif %}
)";

    inline inja::Environment getEnvironment() {
        inja::Environment env;
        env.set_search_included_templates_in_files(false);

        env.set_trim_blocks(true);
        env.set_lstrip_blocks(true);

        env.add_callback("hex", 2, [](inja::Arguments& args) {
            auto number   = args.at(0)->get<std::uint64_t>();
            auto dataType = args.at(1)->get<std::string>();

            unsigned width{};
            if(dataType == "u8" || dataType == "b") {
                width = 2;
            } else if(dataType == "u16") {
                width = 4;
            } else if(dataType == "u32") {
                width = 8;
            } else if(dataType == "u64") {
                width = 16;
            } else if(dataType == "min") {
                width = 2;
            } else {
                throw std::runtime_error("hex failed");
            }

            return fmt::format("0x{:0{}X}", number, width);
        });

        env.add_callback("dataType", 1, [](inja::Arguments& args) {
            auto dataType = args.at(0)->get<std::string>();

            if(dataType == "b") {
                return std::string{"bool"};
            }

            if(dataType != "u8" && dataType != "u16" && dataType != "u32" && dataType != "u64") {
                throw std::runtime_error(dataType + " dataType failed");
            }

            dataType.erase(0, 1);

            return fmt::format("std::uint{}_t", dataType);
        });

        env.add_callback("enumResetValue", 2, [](inja::Arguments& args) {
            auto value  = args.at(0)->get<std::uint64_t>();
            auto values = args.at(1);

            std::string vv;
            for(auto const& v : *values) {
                if(v["value"].get<std::uint64_t>() == value) {
                    vv = v["name"].get<std::string>();
                    break;
                }
            }

            if(vv.empty()) {
                return std::string{"???"};
            }

            std::transform(begin(vv), end(vv), begin(vv), [](auto c) { return std::tolower(c); });

            return vv;
        });

        env.add_callback("allValidDefault", 1, [](inja::Arguments& args) {
            auto reg = args.at(0)->get<Register>();

            for(auto const& field : reg.fields) {
                bool valid = false;
                for(auto const& value : field.values) {
                    if(field.resetValue == value.value) {
                        valid = true;
                        break;
                    }
                }
                if(!valid && !field.values.empty()) {
                    return false;
                }
            }

            return true;
        });

        env.add_callback("makeAccess", 3, [](inja::Arguments& args) {
            //auto access = args.at(0)->get<Access>();
            //auto mval   = args.at(1)->get<ModifiedWriteValues>();
            //auto ract   = args.at(2)->get<ReadAction>();

            auto as = args.at(0)->get<std::string>();
            as[0]   = static_cast<char>(std::toupper(as[0]));
            return as + "Access";
        });

        env.include_template("RegisterGroup", env.parse(RegisterGroupTemplate));
        env.include_template("Register", env.parse(RegisterTemplate));
        env.include_template("Enum", env.parse(EnumTemplate));
        env.include_template("Field", env.parse(FieldTemplate));
        return env;
    }

}}   // namespace Generator::Kvasir
