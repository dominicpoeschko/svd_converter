#pragma once

#include "inja_wrapper.hpp"
#include "view_model.hpp"

#include <format>
#include <string>

namespace Generator { namespace Kvasir {

    static constexpr auto PeripheralTemplate = R"(
#pragma once
#include "kvasir/Register/Register.hpp"
#include "kvasir/Register/Types.hpp"
#include "kvasir/Register/Utility.hpp"
#include "kvasir/Register/Views.hpp"

#include <array>
#include <cstdint>
#include <string_view>
#include <utility>

// Each register struct holds facts only: Addr (address, write-ignored masks, write semantics), resetValue,
// FULLREGISTER, its fields with their enums (and per enumerated field the names of its values, <FIELD>ValueNames),
// `fields` (every field location, a dim field once per element), fmt_string, field_names and field_dims (the
// dimensions in front of the fields in fmt_string: a group's and a dim register's index). What follows from them -
// the default values, the masks, the read of every field, the value names' catalog ids - is derived by the SDK
// (kvasir/Register/Views.hpp, Fields.hpp, RegisterFmt.hpp) only where it is used; the members overrideDefaults (where
// every writable field has a defined reset value and every enum field resets to a named value: a type list with
// literal actions, a bundle for apply() with a runtime value), apply_fields and apply_fields_with_dim are one-line
// calls into it (the executing ones always inlined: a RAM function may use them).
// `glaze_reflect = false`: a register is no reflectable struct, so a logged register prints through RegisterFmt's
// formatter (brought in with the log) or does not compile - never through a reflection formatter (aglio's) with other
// text.
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

    {# A peripheral without registers names only interrupts (RP2350 SPARE_IRQ, base address 0): no
       Registers<>, whose instance would fail its own "Wrong Instance?" assert. #}
    {% if length(registers) > 0 or length(registerGroups) > 0 %}
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
    {% endif %}
}
)";

    // The interrupt vectors' names, for compile errors that name an interrupt (Kvasir_SDK StartUp diagnostics), and
    // the SDK's DeviceInterruptNames hook specialised from them: constant-expression only, no flash.
    static constexpr auto InterruptNamesTemplate = R"(
#pragma once
#include "kvasir/Common/Tags.hpp"
#include "kvasir/StartUp/InterruptNames.hpp"

#include <array>
#include <cstdint>
#include <string_view>

// the device interrupts of {{ name }} from its SVD: index (0 = the first after the core's exceptions) and name
namespace Kvasir::Peripheral {
struct InterruptName {
    std::uint32_t    index;
    std::string_view name;
};

inline constexpr std::array<InterruptName, {{ length(interrupts) }}> interruptNames{ {
{% for irq in interrupts %}
    { {{ irq.value }}, "{{ irq.name }}" },
{% endfor %}
} };
}

// the names for Startup's reports (Kvasir_SDK StartUp/InterruptNames.hpp's hook)
template<typename... Ts>
struct Kvasir::Startup::Diagnostics::DeviceInterruptNames<Kvasir::Tag::User, Ts...> {
    static consteval std::string_view of(int index) {
        for(auto const& irq : ::Kvasir::Peripheral::interruptNames) {
            if(static_cast<int>(irq.index) == index) { return irq.name; }
        }
        return {};
    }
};
)";

    static constexpr auto DeviceInterruptsTemplate = R"(
#pragma once
#include "InterruptNames.hpp"
#include "kvasir/Common/Interrupt.hpp"

#include <array>

// the device interrupts of {{ name }} from its SVD, as Nvic indexes (0 = the first after the core's exceptions); a
// name is the SVD's, lower-cased, every IRQ token dropped
namespace Kvasir::Peripheral {
struct DeviceInterrupts {
{% for irq in deviceInterrupts %}
    static constexpr ::Kvasir::Nvic::Index<{{ irq.value }}> {{ irq.cppName }}{};   // {{ irq.name }}
{% endfor %}
};

// one past the highest line
inline constexpr int interruptEnd = {{ interruptEnd }};
// the indexes below interruptEnd that no line names
inline constexpr std::array<int, {{ length(interruptGaps) }}> interruptGaps{ {{ join(interruptGaps, ", ") }} };
}
)";

    static constexpr auto RegisterGroupTemplate = R"(
//{{ registerGroup.description }}
template<unsigned GroupIndex>
struct {{ upper(registerGroup.name) }} {
    static_assert(GroupIndex < {{ registerGroup.dim }});
    static constexpr {{ dataType(addressType) }} groupBaseAddr =
      baseAddr
        + (GroupIndex  * {{ hex(registerGroup.dimIncrement, "min") }})
        + {{ hex(registerGroup.addressOffset, "min") }};
    {% for register in registerGroup.registers %}
        {% set baseAddrString = "groupBaseAddr" %}
        {% set inRegisterGroup = true %}
        {% include "Register" %}
    {% endfor %}
};
)";

    // The templates are a transcription of the view model (view_model.hpp): every decision - the
    // mode argument, which fields have defaults and what they write, the dim-expanded field lists,
    // the log text - is computed there and unit-tested; the template only prints it.
    static constexpr auto RegisterTemplate = R"delim(
//{{ register.description }}
{% if register.cluster %}
template<unsigned Index>
{% endif %}
struct {{ register.cppName }} {
{% if register.cluster %}
    static_assert(Index < {{ register.dim }});
{% endif %}
    using Addr = Address< {{ baseAddrString }}
                            + {{ hex(register.addressOffset, "min") }}
                            {% if register.cluster %}
                                + (Index * {{ hex(register.dimIncrement, "min") }})
                            {% endif %},
                        {{ hex(register.zeroMask, register.dataType) }},
                        {{ hex(register.oneMask, register.dataType) }},
                        {{ dataType(register.dataType) }}
                        {% for line in register.modeLines %}
                        {{ line }}
                        {% endfor %}>;

    static constexpr {{ dataType(register.dataType) }} resetValue = {{ hex(register.resetValue, register.dataType) }};

    static constexpr FieldLocation<Addr, maskFromRange({{maxBitDataType(register.dataType) }}, 0), {{ register.accessText }}, {{ dataType(register.dataType) }}> FULLREGISTER{};

    {% for field in register.fields %}
            {% include "Field" %}
    {% endfor %}

    using fields = Kvasir::Register::Fields<
    {% for e in register.elements %}
        decltype({{ e.expr }}){% if not loop.is_last %},{% endif %}

    {% endfor %}
    >;

    {% if register.hasDefaults %}
    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<{{ register.cppName }}>>(actions...);
    }

    {% endif %}
    static constexpr std::string_view fmt_string{"{{ register.fmtString }}"};
    static constexpr bool glaze_reflect = false;

    {% if length(register.elements) > 0 %}
    static constexpr auto field_names = std::to_array<std::string_view>({
    {% for e in register.elements %}
        "{{ e.name }}",
    {% endfor %}
    });

    static constexpr std::array<unsigned, {{ register.dimsCount }}> field_dims{ {% if inRegisterGroup %}GroupIndex,{% endif %}{% if register.cluster %}Index,{% endif %} };
    {% endif %}

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<{{ register.cppName }}>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<{{ register.cppName }}>(std::forward<Func>(func));
    }
};
)delim";

    // One field: a number, or an enumeration with its values (ValC). A field cluster (dim) is a
    // template over its index.
    static constexpr auto FieldTemplate = R"(
{% if field.isEnum %}
// the names of {{ lower(field.name) }}'s values (Kvasir::Register::ValueName), found through its enum
struct {{ upper(field.name) }}ValueNames {
    static consteval auto get() {
        return std::to_array<Kvasir::Register::ValueName>({
    {% for value in field.values %}
            { {{ value.value }}, "{{ lower(value.name) }}" },
    {% endfor %}
        });
    }
};
{% endif %}
//{{ field.description }}
{% if field.cluster %}
template<unsigned FieldIndex>
struct {{ upper(field.name) }} {
    static_assert(FieldIndex < {{ field.dim }});
{% endif %}
{% if field.isEnum %}

enum class {{ upper(field.name) }}Val : {{ dataType(field.dataType) }} {
    {% for value in field.values %}
    {{ lower(value.name) }} = {{ value.value }}, //{{ value.description }}
    {% endfor %}
};
friend constexpr {{ upper(field.name) }}ValueNames kvasirValueNames({{ upper(field.name) }}Val) { return {}; }

{% if field.hasReset %}
// ResetValue: {{ field.resetName }} {{ field.resetValue }}
{% else %}
// ResetValue: undefined (outside resetMask)
{% endif %}
static constexpr FieldLocation<Addr,
                               maskFromRange(
                                {{ field.stopBit }}
                                {% if field.cluster %}
                                    + (FieldIndex * {{ field.dimIncrement }})
                                {% endif %},
                                {{ field.startBit }}
                                {% if field.cluster %}
                                    + (FieldIndex * {{ field.dimIncrement }})
                                {% endif %}),
                               {{ field.accessText }},
                               {{ upper(field.name) }}Val>
    {{ lower(field.name) }}{};

struct {{ upper(field.name) }}ValC{
    {% for value in field.values %}
    static constexpr FieldValue<typename decltype({{ field.owner }}::{{ lower(field.name) }})::type,
                                {{ upper(field.name) }}Val::{{ lower(value.name) }}>
                                {{ lower(value.name) }}{};
    {% endfor %}
};

{% else %}
{% if field.hasReset %}
// ResetValue: {{ field.resetValue }}
{% else %}
// ResetValue: undefined (outside resetMask)
{% endif %}
static constexpr FieldLocation<Addr,
              maskFromRange(
                {{ field.stopBit }}
                {% if field.cluster %}
                    + (FieldIndex * {{ field.dimIncrement }})
                {% endif %},
                {{ field.startBit }}
                {% if field.cluster %}
                    + (FieldIndex * {{ field.dimIncrement }})
                {% endif %}),
              {{ field.accessText }},
              {{ dataType(field.dataType) }}>
    {{ lower(field.name) }}{};
{% endif %}
{% if field.cluster %}
};
{% endif %}
)";

    inline inja::Environment getEnvironment() {
        inja::Environment env;
        env.set_search_included_templates_in_files(false);

        env.set_trim_blocks(true);
        env.set_lstrip_blocks(true);

        env.add_callback("hex", 2, [](inja::Arguments& args) {
            return ViewModel::hex(args.at(0)->get<std::uint64_t>(), args.at(1)->get<std::string>());
        });

        env.add_callback("dataType", 1, [](inja::Arguments& args) {
            auto dataType = args.at(0)->get<std::string>();

            if(dataType != "u8" && dataType != "u16" && dataType != "u32" && dataType != "u64") {
                throw std::runtime_error(dataType + " dataType failed");
            }

            dataType.erase(0, 1);

            return std::vformat("std::uint{}_t", std::make_format_args(dataType));
        });

        env.add_callback("maxBitDataType", 1, [](inja::Arguments& args) {
            auto dataType = args.at(0)->get<std::string>();

            if(dataType == "u8") { return std::string{"7"}; }
            if(dataType == "u16") { return std::string{"15"}; }
            if(dataType == "u32") { return std::string{"31"}; }
            if(dataType == "u64") { return std::string{"63"}; }

            throw std::runtime_error(dataType + " maxBitDataType failed");
        });

        env.include_template("RegisterGroup", env.parse(RegisterGroupTemplate));
        env.include_template("Register", env.parse(RegisterTemplate));
        env.include_template("Field", env.parse(FieldTemplate));
        return env;
    }

    // A peripheral's header: the template over the view model (view_model.hpp) of its JSON.
    inline std::string render(inja::Environment&   env,
                              inja::json const&    peripheral,
                              RenderOptions const& options) {
        return env.render(PeripheralTemplate, ViewModel::peripheralView(peripheral, options));
    }

}}   // namespace Generator::Kvasir
