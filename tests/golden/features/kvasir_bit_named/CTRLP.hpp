
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
//Plain peripheral: one instance, every kind of register
namespace Kvasir::Peripheral::CTRLP {
    using Kvasir::Register::Address;
    using Kvasir::Register::FieldLocation;
    using Kvasir::Register::FieldValue;
    using Kvasir::Register::maskFromRange;
    using Kvasir::Register::ReadWriteAccess;
    using Kvasir::Register::WriteOnlyAccess;
    using Kvasir::Register::ReadOnlyAccess;
    using Kvasir::Register::value;

    //Plain peripheral: one instance, every kind of register
    template<unsigned Instance = 0 >
    struct Registers{

        static constexpr std::uint32_t getBaseAddr(unsigned instance){
            if( instance == 0) {
                return 0x40000000;
            }

            return 0;
        }

        static constexpr std::uint32_t baseAddr = getBaseAddr(Instance);

        static_assert(baseAddr != 0, "Wrong Instance?");


//Enum field, plain field, an accepted write-only strobe next to them
struct CTRL {
    using Addr = Address< baseAddr
                            + 0x00
,
                        0xFFFFFFE0,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{}, CTRL>
>;

    static constexpr std::uint32_t resetValue = 0x00000002;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//Enable
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                0
,
                0
),
              ReadWriteAccess,
              std::uint32_t>
    enable{};

// the names of mode's values (Kvasir::Register::ValueName), found through its enum
struct MODEValueNames {
    static consteval auto get() {
        return std::to_array<Kvasir::Register::ValueName>({
            { 0, "off" },
            { 1, "slow" },
            { 2, "fast" },
        });
    }
};
//Operating mode

enum class MODEVal : std::uint32_t {
    off = 0, //Off
    slow = 1, //Slow
    fast = 2, //Fast
};
friend constexpr MODEValueNames kvasirValueNames(MODEVal) { return {}; }

// ResetValue: slow 1
static constexpr FieldLocation<Addr,
                               maskFromRange(
                                3
,
                                1
),
                               ReadWriteAccess,
                               MODEVal>
    mode{};

struct MODEValC{
    static constexpr FieldValue<typename decltype(CTRL::mode)::type,
                                MODEVal::off>
                                off{};
    static constexpr FieldValue<typename decltype(CTRL::mode)::type,
                                MODEVal::slow>
                                slow{};
    static constexpr FieldValue<typename decltype(CTRL::mode)::type,
                                MODEVal::fast>
                                fast{};
};


//Software reset
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                4
,
                4
),
              WriteOnlyAccess,
              std::uint32_t>
    swrst{};

    using fields = Kvasir::Register::Fields<
        decltype(enable),
        decltype(mode),
        decltype(swrst)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<CTRL>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::ctrl(\n\"enable\": 0x{:x}\n\"mode\": {}\n\"swrst\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "enable",
        "mode",
        "swrst",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<CTRL>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<CTRL>(std::forward<Func>(func));
    }
};

//8-bit register of write-one-to-clear flags
struct INTFLAG {
    using Addr = Address< baseAddr
                            + 0x04
,
                        0xFF,
                        0x00,
                        std::uint8_t
                        , Kvasir::Register::Mode<{.writeBackActs = 0x03}, INTFLAG>
>;

    static constexpr std::uint8_t resetValue = 0x00;

    static constexpr FieldLocation<Addr, maskFromRange(7, 0), ReadWriteAccess, std::uint8_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                0
,
                0
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToClear>,
              std::uint8_t>
    ready{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
,
                1
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToClear>,
              std::uint8_t>
    error{};

    using fields = Kvasir::Register::Fields<
        decltype(ready),
        decltype(error)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<INTFLAG>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::intflag(\n\"ready\": 0x{:x}\n\"error\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "ready",
        "error",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<INTFLAG>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<INTFLAG>(std::forward<Func>(func));
    }
};

//16-bit register: a write-only trigger whose 0 does nothing, toggle and zero-to-clear fields
struct TRIGGER {
    using Addr = Address< baseAddr
                            + 0x08
,
                        0xF0FB,
                        0x0004,
                        std::uint16_t
                        , Kvasir::Register::Mode<{.writeBackActs = 0x0003}, TRIGGER>
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
              Kvasir::Register::Access<Kvasir::Register::AccessType::writeOnly, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToSet>,
              std::uint16_t>
    start{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
,
                1
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToToggle>,
              std::uint16_t>
    flip{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                2
,
                2
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::zeroToClear>,
              std::uint16_t>
    done{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                11
,
                8
),
              ReadWriteAccess,
              std::uint16_t>
    level{};

    using fields = Kvasir::Register::Fields<
        decltype(start),
        decltype(flip),
        decltype(done),
        decltype(level)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<TRIGGER>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::trigger(\n\"start\": 0x{:x}\n\"flip\": 0x{:x}\n\"done\": 0x{:x}\n\"level\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "start",
        "flip",
        "done",
        "level",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<TRIGGER>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<TRIGGER>(std::forward<Func>(func));
    }
};

//A key field: every write must carry it
struct KEYREG {
    using Addr = Address< baseAddr
                            + 0x0C
,
                        0x0000FF00,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{.mustSupply = 0xFFFF0000}, KEYREG>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                7
,
                0
),
              ReadWriteAccess,
              std::uint32_t>
    _value{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                31
,
                16
),
              WriteOnlyAccess,
              std::uint32_t>
    key{};

    using fields = Kvasir::Register::Fields<
        decltype(_value),
        decltype(key)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<KEYREG>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::keyreg(\n\"_value\": 0x{:x}\n\"key\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "_value",
        "key",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<KEYREG>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<KEYREG>(std::forward<Func>(func));
    }
};

//Reading pops: a read with a side effect
struct FIFO {
    using Addr = Address< baseAddr
                            + 0x10
,
                        0xFFFFFFFF,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{.readSideEffect = true}, FIFO>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadOnlyAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                31
,
                0
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readOnly, Kvasir::Register::ReadActionType::clear, Kvasir::Register::ModifiedWriteValueType::normal>,
              std::uint32_t>
    data{};

    using fields = Kvasir::Register::Fields<
        decltype(data)
    >;

    static constexpr std::string_view fmt_string{"ctrlp::fifo(\n\"data\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "data",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<FIFO>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<FIFO>(std::forward<Func>(func));
    }
};

//No readable field: a write-only register
struct STROBE {
    using Addr = Address< baseAddr
                            + 0x14
,
                        0xFFFFFFFC,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{.writeOnlyRegister = true}, STROBE>
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
    go{};

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
    stop{};

    using fields = Kvasir::Register::Fields<
        decltype(go),
        decltype(stop)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<STROBE>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::strobe(\n\"go\": 0x{:x}\n\"stop\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "go",
        "stop",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<STROBE>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<STROBE>(std::forward<Func>(func));
    }
};

//No fields: one value, FULLREGISTER only
struct BUF {
    using Addr = Address< baseAddr
                            + 0x18
,
                        0xFFFFFFFF,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{}, BUF>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


    using fields = Kvasir::Register::Fields<
    >;

    static constexpr std::string_view fmt_string{"ctrlp::buf(\n)"};
    static constexpr bool glaze_reflect = false;


    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<BUF>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<BUF>(std::forward<Func>(func));
    }
};

//A dim register
template<unsigned Index>
struct CH {
    static_assert(Index < 4);
    using Addr = Address< baseAddr
                            + 0x20
                                + (Index * 0x04)
,
                        0xFFFFFC00,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{}, CH>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                7
,
                0
),
              ReadWriteAccess,
              std::uint32_t>
    threshold{};

//
template<unsigned FieldIndex>
struct EN {
    static_assert(FieldIndex < 2);
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                8
                    + (FieldIndex * 1)
,
                8
                    + (FieldIndex * 1)
),
              ReadWriteAccess,
              std::uint32_t>
    en{};
};

    using fields = Kvasir::Register::Fields<
        decltype(threshold),
        decltype(EN<0>::en),
        decltype(EN<1>::en)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<CH>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::ch[{}](\n\"threshold\": 0x{:x}\n\"en[0]\": 0x{:x}\n\"en[1]\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "threshold",
        "en[0]",
        "en[1]",
    });

    static constexpr std::array<unsigned, 0+ 1> field_dims{ Index,};

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<CH>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<CH>(std::forward<Func>(func));
    }
};

//A dim field
struct PINS {
    using Addr = Address< baseAddr
                            + 0x30
,
                        0xFFFFFF00,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{}, PINS>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
template<unsigned FieldIndex>
struct IN {
    static_assert(FieldIndex < 4);
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
                    + (FieldIndex * 2)
,
                0
                    + (FieldIndex * 2)
),
              ReadWriteAccess,
              std::uint32_t>
    in{};
};

    using fields = Kvasir::Register::Fields<
        decltype(IN<0>::in),
        decltype(IN<1>::in),
        decltype(IN<2>::in),
        decltype(IN<3>::in)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<PINS>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::pins(\n\"in[0]\": 0x{:x}\n\"in[1]\": 0x{:x}\n\"in[2]\": 0x{:x}\n\"in[3]\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "in[0]",
        "in[1]",
        "in[2]",
        "in[3]",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<PINS>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<PINS>(std::forward<Func>(func));
    }
};

//One field per access and modifiedWriteValues: the identity cross-check (Kvasir_SDK generated_registers_tests.cpp lists the same rows)
struct IDENT {
    using Addr = Address< baseAddr
                            + 0x34
,
                        0xFFFFCC0E,
                        0x00001070,
                        std::uint32_t
                        , Kvasir::Register::Mode<{.writeBackActs = 0x0000080E}, IDENT>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                0
,
                0
),
              ReadWriteAccess,
              std::uint32_t>
    rw_plain{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                1
,
                1
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToClear>,
              std::uint32_t>
    rw_onetoclear{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                2
,
                2
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToSet>,
              std::uint32_t>
    rw_onetoset{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                3
,
                3
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToToggle>,
              std::uint32_t>
    rw_onetotoggle{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                4
,
                4
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::zeroToClear>,
              std::uint32_t>
    rw_zerotoclear{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                5
,
                5
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::zeroToSet>,
              std::uint32_t>
    rw_zerotoset{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                6
,
                6
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::zeroToToggle>,
              std::uint32_t>
    rw_zerototoggle{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                7
,
                7
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::clear>,
              std::uint32_t>
    rw_clear{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                8
,
                8
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::set>,
              std::uint32_t>
    rw_set{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                9
,
                9
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::modify>,
              std::uint32_t>
    rw_modify{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                10
,
                10
),
              ReadOnlyAccess,
              std::uint32_t>
    ro{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                11
,
                11
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::writeOnly, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::oneToSet>,
              std::uint32_t>
    wo_onetoset{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                12
,
                12
),
              Kvasir::Register::Access<Kvasir::Register::AccessType::writeOnly, Kvasir::Register::ReadActionType::normal, Kvasir::Register::ModifiedWriteValueType::zeroToClear>,
              std::uint32_t>
    wo_zerotoclear{};

//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                13
,
                13
),
              WriteOnlyAccess,
              std::uint32_t>
    wo_plain{};

    using fields = Kvasir::Register::Fields<
        decltype(rw_plain),
        decltype(rw_onetoclear),
        decltype(rw_onetoset),
        decltype(rw_onetotoggle),
        decltype(rw_zerotoclear),
        decltype(rw_zerotoset),
        decltype(rw_zerototoggle),
        decltype(rw_clear),
        decltype(rw_set),
        decltype(rw_modify),
        decltype(ro),
        decltype(wo_onetoset),
        decltype(wo_zerotoclear),
        decltype(wo_plain)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<IDENT>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::ident(\n\"rw_plain\": 0x{:x}\n\"rw_onetoclear\": 0x{:x}\n\"rw_onetoset\": 0x{:x}\n\"rw_onetotoggle\": 0x{:x}\n\"rw_zerotoclear\": 0x{:x}\n\"rw_zerotoset\": 0x{:x}\n\"rw_zerototoggle\": 0x{:x}\n\"rw_clear\": 0x{:x}\n\"rw_set\": 0x{:x}\n\"rw_modify\": 0x{:x}\n\"ro\": 0x{:x}\n\"wo_onetoset\": 0x{:x}\n\"wo_zerotoclear\": 0x{:x}\n\"wo_plain\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "rw_plain",
        "rw_onetoclear",
        "rw_onetoset",
        "rw_onetotoggle",
        "rw_zerotoclear",
        "rw_zerotoset",
        "rw_zerototoggle",
        "rw_clear",
        "rw_set",
        "rw_modify",
        "ro",
        "wo_onetoset",
        "wo_zerotoclear",
        "wo_plain",
    });

    static constexpr std::array<unsigned, 0> field_dims{ };

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<IDENT>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<IDENT>(std::forward<Func>(func));
    }
};

//A register group; its second register is named like the group
template<unsigned GroupIndex>
struct GRP {
    static_assert(GroupIndex < 2);
    static constexpr std::uint32_t groupBaseAddr =
      baseAddr
        + (GroupIndex  * 0x10)
        + 0x40;

//
struct CFG {
    using Addr = Address< groupBaseAddr
                            + 0x00
,
                        0xFFFFFFF0,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{}, CFG>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                3
,
                0
),
              ReadWriteAccess,
              std::uint32_t>
    sel{};

    using fields = Kvasir::Register::Fields<
        decltype(sel)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<CFG>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::grp[{}]::cfg(\n\"sel\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "sel",
    });

    static constexpr std::array<unsigned, 0+ 1> field_dims{ GroupIndex,};

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<CFG>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<CFG>(std::forward<Func>(func));
    }
};

//Named like its group
struct GRP_ {
    using Addr = Address< groupBaseAddr
                            + 0x04
,
                        0xFFFFFF00,
                        0x00000000,
                        std::uint32_t
                        , Kvasir::Register::Mode<{}, GRP_>
>;

    static constexpr std::uint32_t resetValue = 0x00000000;

    static constexpr FieldLocation<Addr, maskFromRange(31, 0), ReadWriteAccess, std::uint32_t> FULLREGISTER{};


//
// ResetValue: 0
static constexpr FieldLocation<Addr,
              maskFromRange(
                7
,
                0
),
              ReadWriteAccess,
              std::uint32_t>
    evt{};

    using fields = Kvasir::Register::Fields<
        decltype(evt)
    >;

    template<typename... Actions>
    [[gnu::always_inline]] static constexpr auto overrideDefaults(Actions const&... actions){
        return Kvasir::Register::overrideDefaults<Kvasir::Register::DefaultValues<GRP_>>(actions...);
    }

    static constexpr std::string_view fmt_string{"ctrlp::grp[{}]::grp_(\n\"evt\": 0x{:x})"};
    static constexpr bool glaze_reflect = false;

    static constexpr auto field_names = std::to_array<std::string_view>({
        "evt",
    });

    static constexpr std::array<unsigned, 0+ 1> field_dims{ GroupIndex,};

    template<typename Func>
    [[gnu::always_inline]] static constexpr auto apply_fields(Func&& func){
        return Kvasir::Register::applyFields<GRP_>(std::forward<Func>(func));
    }
    template<typename Func>
    static constexpr auto apply_fields_with_dim(Func&& func) {
        return Kvasir::Register::applyFieldsWithDim<GRP_>(std::forward<Func>(func));
    }
};
};

    };
}
