
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
//Register properties passed down (device, peripheral, cluster, register), derivedFrom by SVD name, resetMask
namespace Kvasir::Peripheral::INHERIT {
    using Kvasir::Register::Address;
    using Kvasir::Register::FieldLocation;
    using Kvasir::Register::FieldValue;
    using Kvasir::Register::maskFromRange;
    using Kvasir::Register::ReadWriteAccess;
    using Kvasir::Register::WriteOnlyAccess;
    using Kvasir::Register::ReadOnlyAccess;
    using Kvasir::Register::value;

    //Register properties passed down (device, peripheral, cluster, register), derivedFrom by SVD name, resetMask
    template<unsigned Instance = 0 >
    struct Registers{

        static constexpr std::uint32_t getBaseAddr(unsigned instance){
            if( instance == 0) {
                return 0x40003000;
            }

            return 0;
        }

        static constexpr std::uint32_t baseAddr = getBaseAddr(Instance);

        static_assert(baseAddr != 0, "Wrong Instance?");


//Inherits the peripheral's size (16) and access (read-only)
struct STATUS {
    using Addr = Address< baseAddr
                            + 0x00
,
                        0xFFFF,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, STATUS>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), ReadOnlyAccess, std::uint16_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                3
,
                0
),
              ReadOnlyAccess,
              std::uint16_t>
    level{};

    using fields = Kvasir::Register::Fields<
        decltype(level)
    >;

    static constexpr std::string_view fmt_string{"inherit::status(\n\"level\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "level",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<STATUS>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<STATUS>(std::forward<Func>(func));
    }
};

//A base whose C name (displayName) differs from its SVD name
struct CTRLA {
    using Addr = Address< baseAddr
                            + 0x02
,
                        0xFFFE,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, CTRLA>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), ReadWriteAccess, std::uint16_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                0
,
                0
),
              ReadWriteAccess,
              std::uint16_t>
    en{};

    using fields = Kvasir::Register::Fields<
        decltype(en)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<CTRLA>>(actions...);
    }

    static constexpr std::string_view fmt_string{"inherit::ctrla(\n\"en\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "en",
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

//resetMask 0: no defined reset value, so no overrideDefaults
struct DIV {
    using Addr = Address< baseAddr
                            + 0x06
,
                        0xFFFC,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, DIV>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), ReadWriteAccess, std::uint16_t> FULLREGISTER{};


// the names of mode's values (Kvasir::Register::ValueName), found through its enum
struct MODEValueNames {
    static consteval auto get() {
        return std::to_array<Kvasir::Register::ValueName>({
            { 0, "off" },
            { 1, "on" },
        });
    }
};
//

enum class MODEVal : std::uint16_t {
    off = 0, //
    on = 1, //
};
friend constexpr MODEValueNames kvasirValueNames(MODEVal) { return {}; }

// ResetValue: undefined (outside resetMask)
static constexpr FieldLocation<Addr,
                               maskFromRange(
                                1
,
                                0
),
                               ReadWriteAccess,
                               MODEVal>
    mode{};

struct MODEValC{
    static constexpr FieldValue<typename decltype(DIV::mode)::type,
                                MODEVal::off>
                                off{};
    static constexpr FieldValue<typename decltype(DIV::mode)::type,
                                MODEVal::on>
                                on{};
};


    using fields = Kvasir::Register::Fields<
        decltype(mode)
    >;

    static constexpr std::string_view fmt_string{"inherit::div(\n\"mode\": {})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "mode",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<DIV>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<DIV>(std::forward<Func>(func));
    }
};

//resetMask covers bit 1 only: the field's defined bit is checked
struct PART {
    using Addr = Address< baseAddr
                            + 0x08
,
                        0xFFFC,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, PART>
>;

    static constexpr std::uint16_t resetValue = 0x0002;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), ReadWriteAccess, std::uint16_t> FULLREGISTER{};


// the names of mode's values (Kvasir::Register::ValueName), found through its enum
struct MODEValueNames {
    static consteval auto get() {
        return std::to_array<Kvasir::Register::ValueName>({
            { 2, "a" },
            { 3, "b" },
        });
    }
};
//

enum class MODEVal : std::uint16_t {
    a = 2, //
    b = 3, //
};
friend constexpr MODEValueNames kvasirValueNames(MODEVal) { return {}; }

// ResetValue: a 2
static constexpr FieldLocation<Addr,
                               maskFromRange(
                                1
,
                                0
),
                               ReadWriteAccess,
                               MODEVal>
    mode{};

struct MODEValC{
    static constexpr FieldValue<typename decltype(PART::mode)::type,
                                MODEVal::a>
                                a{};
    static constexpr FieldValue<typename decltype(PART::mode)::type,
                                MODEVal::b>
                                b{};
};


    using fields = Kvasir::Register::Fields<
        decltype(mode)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<PART>>(actions...);
    }

    static constexpr std::string_view fmt_string{"inherit::part(\n\"mode\": {})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "mode",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<PART>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<PART>(std::forward<Func>(func));
    }
};

//No fields, inherits read-only: FULLREGISTER is read-only
struct ID {
    using Addr = Address< baseAddr
                            + 0x0A
,
                        0xFFFF,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, ID>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), ReadOnlyAccess, std::uint16_t> FULLREGISTER{};


    using fields = Kvasir::Register::Fields<
    >;

    static constexpr std::string_view fmt_string{"inherit::id(\n)"};
    static constexpr bool glaze_reflect = false;


    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<ID>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<ID>(std::forward<Func>(func));
    }
};

//No fields, write-only: FULLREGISTER is write-only
struct TXDATA {
    using Addr = Address< baseAddr
                            + 0x0C
,
                        0xFFFF,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, TXDATA>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), WriteOnlyAccess, std::uint16_t> FULLREGISTER{};


    using fields = Kvasir::Register::Fields<
    >;

    static constexpr std::string_view fmt_string{"inherit::txdata(\n)"};
    static constexpr bool glaze_reflect = false;


    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<TXDATA>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<TXDATA>(std::forward<Func>(func));
    }
};

//No fields, writeOnce: FULLREGISTER is write-only
struct ONCE {
    using Addr = Address< baseAddr
                            + 0x0E
,
                        0xFFFF,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, ONCE>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), WriteOnlyAccess, std::uint16_t> FULLREGISTER{};


    using fields = Kvasir::Register::Fields<
    >;

    static constexpr std::string_view fmt_string{"inherit::once(\n)"};
    static constexpr bool glaze_reflect = false;


    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<ONCE>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<ONCE>(std::forward<Func>(func));
    }
};

//A base whose C name (displayName) differs from its SVD name
struct CTRLB {
    using Addr = Address< baseAddr
                            + 0x04
,
                        0xFFFE,
                        0x0000,
                        std::uint16_t
                        , Kvasir::Register::Mode<{}, CTRLB>
>;

    static constexpr std::uint16_t resetValue = 0x0000;

    static constexpr FieldLocation<Addr, maskFromRange(15, 0), ReadWriteAccess, std::uint16_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                0
,
                0
),
              ReadWriteAccess,
              std::uint16_t>
    en{};

    using fields = Kvasir::Register::Fields<
        decltype(en)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<CTRLB>>(actions...);
    }

    static constexpr std::string_view fmt_string{"inherit::ctrlb(\n\"en\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "en",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<CTRLB>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<CTRLB>(std::forward<Func>(func));
    }
};

//A dim cluster with its own size and access, and a name sanitizeName changes
template<unsigned GroupIndex>
struct LANE_CFG {
    static_assert(GroupIndex < 2);
    static constexpr std::uint32_t groupBaseAddr =
      baseAddr
        + (GroupIndex  * 0x01)
        + 0x10;

//
struct SEL {
    using Addr = Address< groupBaseAddr
                            + 0x00
,
                        0xFC,
                        0x00,
                        std::uint8_t
                        , Kvasir::Register::Mode<{}, SEL>
>;

    static constexpr std::uint8_t resetValue = 0x00;

    static constexpr FieldLocation<Addr, maskFromRange(7, 0), ReadWriteAccess, std::uint8_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
,
                0
),
              ReadWriteAccess,
              std::uint8_t>
    src{};

    using fields = Kvasir::Register::Fields<
        decltype(src)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<SEL>>(actions...);
    }

    static constexpr std::string_view fmt_string{"inherit::lane_cfg[{}]::sel(\n\"src\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "src",
    });

    static constexpr std::array<unsigned, 0+ 1> field_dims{ GroupIndex,};

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<SEL>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<SEL>(std::forward<Func>(func));
    }
};
};

    };
}
