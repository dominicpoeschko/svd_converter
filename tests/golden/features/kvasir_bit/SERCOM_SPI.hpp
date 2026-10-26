
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
//Modes: clusters without dim become peripherals of their own 
namespace Kvasir::Peripheral::SERCOM_SPI {
    using Kvasir::Register::Address;
    using Kvasir::Register::FieldLocation;
    using Kvasir::Register::FieldValue;
    using Kvasir::Register::maskFromRange;
    using Kvasir::Register::ReadWriteAccess;
    using Kvasir::Register::WriteOnlyAccess;
    using Kvasir::Register::ReadOnlyAccess;
    using Kvasir::Register::value;

    //Modes: clusters without dim become peripherals of their own 
    template<unsigned Instance = 0 >
    struct Registers{

        static constexpr std::uint32_t getBaseAddr(unsigned instance){
            if( instance == 0) {
                return 0x40002000;
            }

            return 0;
        }

        static constexpr std::uint32_t baseAddr = getBaseAddr(Instance);

        static_assert(baseAddr != 0, "Wrong Instance?");


//
struct CTRLA {
    using Addr = Address< baseAddr
                            + 0x00
,
                        0xFFFFFFF9,
                        0x00000000,
                        std::uint32_t
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
,
                1
),
              ReadWriteAccess,
              std::uint32_t>
    enable{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                2
,
                2
),
              ReadWriteAccess,
              std::uint32_t>
    master{};

    using fields = Kvasir::Register::Fields<
        decltype(enable),
        decltype(master)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<CTRLA>>(actions...);
    }

    static constexpr std::string_view fmt_string{"sercom_spi::ctrla(\n\"enable\": 0x{:x}\n\"master\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "enable",
        "master",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<CTRLA>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<CTRLA>(std::forward<Func>(func));
    }
};

    };
}
