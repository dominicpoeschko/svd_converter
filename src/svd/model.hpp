#pragma once

// The converter's model of an SVD: what read.hpp makes of the XML, semantics.hpp completes and the
// generators print (the json generator through json.hpp, kvasir_bit through view_model.hpp). Plain
// types and the bit arithmetic over them; no XML, no json.

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace Svd {

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

struct BitRange {
    std::uint64_t start;
    std::uint64_t stop;
};

enum class DataType : std::uint8_t { u8, u16, u32, u64 };

constexpr std::uint64_t dataTypeSize(DataType dataType) noexcept {
    switch(dataType) {
    case DataType::u8:  return 8;
    case DataType::u16: return 16;
    case DataType::u32: return 32;
    case DataType::u64: return 64;
    }
    return 0;
}

enum class RepeatType : std::uint8_t {
    normal,
    cluster,
};

enum class FieldType : std::uint8_t { normal, typeEnum };

enum class Access : std::uint8_t { readOnly, writeOnly, readWrite, writeOnce, readWriteOnce };

enum class ModifiedWriteValues : std::uint8_t {
    empty,
    oneToClear,
    oneToSet,
    oneToToggle,
    zeroToClear,
    zeroToSet,
    zeroToToggle,
    clear,
    set,
    modify
};

enum class ReadAction : std::uint8_t { empty, clear, set, modify, modifyExternal };

// What can be written to a field without changing it (identityOf): nothing, a 0, or a 1.
enum class Identity : std::uint8_t { none, zero, one };

struct Value {
    std::string   name;
    std::string   description;
    std::uint64_t value = 0;
};

struct Field {
    std::string         name;
    std::string         description;
    FieldType           type                = FieldType::normal;
    RepeatType          repType             = RepeatType::normal;
    std::uint64_t       dim                 = 0;
    std::uint64_t       dimIncrement        = 0;
    std::uint64_t       resetValue          = 0;
    std::uint64_t       startBit            = 0;
    std::uint64_t       stopBit             = 0;
    DataType            dataType            = DataType::u32;
    Access              access              = Access::readWrite;
    ModifiedWriteValues modifiedWriteValues = ModifiedWriteValues::empty;
    ReadAction          readAction          = ReadAction::empty;
    // a key: the SVD's writeConstraint allows one value only (AIRCR.VECTKEY), so every write of
    // the register must carry it - Register::Semantics::mustSupply
    bool key = false;
    // what can be written without changing the field (identityOf; the json says "none", "zero" or "one")
    Identity identity = Identity::none;
    // `<!-- Kvasir: write-only accepted: <why> -->` inside the <field>: a write-only field without a
    // write identity that was looked at and is fine as it is - no write-only finding for it
    bool               writeOnlyAccepted = false;
    std::vector<Value> values;
};

// The bits of a field, every element of a dim field. Bits past 63 are left out (a broken SVD, which
// the reader refuses).
constexpr std::uint64_t fieldBits(Field const& f) noexcept {
    std::uint64_t const elements = f.dim == 0 ? 1 : f.dim;
    std::uint64_t       bits     = 0;
    for(std::uint64_t i = 0; i < elements; ++i) {
        auto const stop = f.stopBit + i * f.dimIncrement;
        if(stop < 64) { bits |= maskFromRange(stop, f.startBit + i * f.dimIncrement); }
    }
    return bits;
}

// Whether the SVD gives the field a reset value: some bit of it lies inside its register's
// <resetMask> (the RP2350's OTP_DATA, ROSC DIV/DORMANT have resetMask 0).
constexpr bool hasResetValue(Field const&  f,
                             std::uint64_t resetMask) noexcept {
    return (fieldBits(f) & resetMask) != 0;
}

// A write-only field without a write identity that partial writes of sibling fields would read and
// write back as read: the field's index and the siblings' (analyze()).
struct WriteOnlyHazard {
    std::size_t              field{};
    std::vector<std::size_t> rewrittenBy;
};

struct Register {
    std::string name;
    // the name the SVD gives the register (sanitized, `%s` removed) where `name` is its <displayName>;
    // derivedFrom names this one (not in the json)
    std::string   svdName;
    std::string   description;
    RepeatType    type          = RepeatType::normal;
    std::uint64_t dim           = 0;
    std::uint64_t dimIncrement  = 0;
    DataType      dataType      = DataType::u32;
    std::uint64_t addressOffset = 0;
    std::uint64_t resetValue    = 0;
    // the bits <resetValue> defines (SVD <resetMask>, inherited device -> peripheral -> cluster ->
    // register); a bit outside it has no defined reset value
    std::uint64_t resetMask = ~std::uint64_t{0};
    std::uint64_t zeroMask  = 0;
    std::uint64_t oneMask   = 0;
    // the zeroMask bits a read-modify-write must write as 0: one-to-* fields, whose value read back
    // would act again; reserved and read-only bits are written back as read (Register::Semantics::writeBackActs)
    std::uint64_t writeBackActsMask = 0;
    // FULLREGISTER's access (analyze): without fields the register's own <access> (writeOnce as
    // writeOnly, read-writeOnce as readWrite); with fields readOnly when every field is, writeOnly when
    // no field can be read, readWrite otherwise
    Access access = Access::readWrite;
    // the register's <access> as the SVD gives it (inherited like resetMask), the default of its
    // fields; not in the json
    Access svdAccess = Access::readWrite;
    // Register::Semantics' mustSupply and readSideEffect: the key fields' bits every write must supply,
    // and whether a read has a side effect (a field with a readAction) - from the SVD (analyze)
    std::uint64_t mustSupplyMask    = 0;
    bool          readHasSideEffect = false;
    // write-only fields without a write identity that a partial write of a sibling field would read
    // and write back as read (analyze)
    std::uint64_t writeOnlyNoIdentityMask = 0;
    // no field of the register can be read (--write-only-registers=derived): a partial write never
    // needs to read it
    bool               writeOnlyRegister = false;
    std::vector<Field> fields;
    // the fields behind writeOnlyNoIdentityMask, for the write-only findings; not in the json
    std::vector<WriteOnlyHazard> writeOnlyHazards;
};

struct RegisterGroup {
    std::string           name;
    std::string           description;
    std::uint64_t         dim           = 0;
    std::uint64_t         dimIncrement  = 0;
    std::uint64_t         addressOffset = 0;
    std::vector<Register> registers;
};

struct AddressType {
    std::uint64_t index;
    std::uint64_t address;
};

struct Peripheral {
    std::string                name;
    std::string                description;
    RepeatType                 type        = RepeatType::normal;
    DataType                   addressType = DataType::u32;
    std::vector<Register>      registers;
    std::vector<RegisterGroup> registerGroups;
    std::vector<AddressType>   baseAddresses;
};

// A peripheral with derivedFrom that is one more instance of its base: its own name and address,
// the name of the base whose group it joins, and whether an instance group took it in (or said why
// not). A derived register is read as a register (read.cpp).
struct DerivedRef {
    std::string   name;
    std::string   base;
    std::uint64_t address   = 0;
    bool          processed = false;
};

// A peripheral whose clusters without <dim> are its modes (SERCOM's USART / SPI / I2CM): the
// peripheral's name and one peripheral per mode.
struct ModeClusters {
    std::string             parent;
    std::vector<Peripheral> modes;
};

// The write-only guard (svd_converter --write-only-guard / --write-only-registers / --write-only-mask).
enum class WriteOnlyGuard : std::uint8_t { off, warn, error };

enum class WriteOnlyRegisters : std::uint8_t { off, derived };

// The defaults are Kvasir's write semantics (svd_convert()'s too): every write-only field classified
// (an unclassified one is an error), registers with no readable field whole-register-only, the
// template's write-only mask on. An SVD that is not classified yet passes the three as off.
struct ConvertOptions {
    WriteOnlyGuard     guard              = WriteOnlyGuard::error;
    bool               writeOnlyMask      = true;   // passed to the template as `writeOnlyMask`
    WriteOnlyRegisters writeOnlyRegisters = WriteOnlyRegisters::derived;
    bool               namedRegisters
      = false;   // --named-registers: every register's Address names it (Register::Mode's 2nd argument)
};

// One unclassified write-only field: where it is and which sibling writes would rewrite it.
struct WriteOnlyFinding {
    std::string              where;   // the SVD peripheral (and cluster / register group)
    std::string              reg;
    std::string              field;
    std::string              bits;
    std::vector<std::string> rewrittenBy;
    bool                     readableSibling = false;   // false: no field of the register reads
};

// An <interrupt> of a peripheral: the vector's name and its index (0 = the first device interrupt, after the core's
// exceptions). For the compile errors that name an interrupt (the generated InterruptNames.hpp).
struct Interrupt {
    std::string   name;
    std::uint64_t value{};
};

struct Chip {
    std::string             name;
    std::string             description;
    std::vector<Peripheral> peripherals;
    std::vector<Interrupt>  interrupts;   // sorted by value, one per value
};

}   // namespace Svd
