
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
//
namespace Kvasir::Peripheral::EVSYS {
    using Kvasir::Register::Address;
    using Kvasir::Register::FieldLocation;
    using Kvasir::Register::FieldValue;
    using Kvasir::Register::maskFromRange;
    using Kvasir::Register::ReadWriteAccess;
    using Kvasir::Register::WriteOnlyAccess;
    using Kvasir::Register::ReadOnlyAccess;
    using Kvasir::Register::value;

    //
    template<unsigned Instance = 0 >
    struct Registers{

        static constexpr std::uint32_t getBaseAddr(unsigned instance){
            if( instance == 0) {
                return 0x42000400;
            }

            return 0;
        }

        static constexpr std::uint32_t baseAddr = getBaseAddr(Instance);

        static_assert(baseAddr != 0, "Wrong Instance?");


//
struct SWEVT {
    using Addr = Address< baseAddr
                            + 0x0C
,
                        0xFFFFFFFC,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{.writeOnlyRegister = true}, SWEVT>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), WriteOnlyAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                0
,
                0
),
              WriteOnlyAccess,
              std::uint32_t>
    channel0{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
,
                1
),
              WriteOnlyAccess,
              std::uint32_t>
    channel1{};

    using fields = Kvasir::Register::Fields<
        decltype(channel0),
        decltype(channel1)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<SWEVT>>(actions...);
    }

    static constexpr std::string_view fmt_string{"evsys::swevt(\n\"channel0\": 0x{:x}\n\"channel1\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "channel0",
        "channel1",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<SWEVT>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<SWEVT>(std::forward<Func>(func));
    }
};

    };
}
