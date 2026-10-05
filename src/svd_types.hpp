#pragma once

#include "inja_wrapper.hpp"

#ifdef __clang__
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wexit-time-destructors"
#endif

struct BitRange {
    std::uint64_t start;
    std::uint64_t stop;
};

enum class DataType : std::uint8_t { u8, u16, u32, u64 };

NLOHMANN_JSON_SERIALIZE_ENUM(DataType,
                             {
                               { DataType::u8,
                                "u8" },
                               {DataType::u16,
                                "u16"},
                               {DataType::u32,
                                "u32"},
                               {DataType::u64,
                                "u64"}
})

enum class RepeatType : std::uint8_t {
    normal,
    cluster,
};

NLOHMANN_JSON_SERIALIZE_ENUM(RepeatType,
                             {
                               { RepeatType::normal,
                                "normal" },
                               {RepeatType::cluster,
                                "cluster"}
})

enum class FieldType : std::uint8_t { normal, enum_ };

NLOHMANN_JSON_SERIALIZE_ENUM(FieldType,
                             {
                               {FieldType::normal,
                                "normal"},
                               { FieldType::enum_,
                                "enum"  }
})

enum class Access : std::uint8_t { readOnly, writeOnly, readWrite, writeOnce, readWriteOnce };

NLOHMANN_JSON_SERIALIZE_ENUM(Access,
                             {
                               {     Access::readOnly,
                                "readOnly"     },
                               {    Access::writeOnly,
                                "writeOnly"    },
                               {    Access::readWrite,
                                "readWrite"    },
                               {    Access::writeOnce,
                                "writeOnce"    },
                               {Access::readWriteOnce,
                                "readWriteOnce"},
})

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

NLOHMANN_JSON_SERIALIZE_ENUM(ModifiedWriteValues,
                             {
                               {       ModifiedWriteValues::empty,
                                "empty"       },
                               {  ModifiedWriteValues::oneToClear,
                                "oneToClear"  },
                               {    ModifiedWriteValues::oneToSet,
                                "oneToSet"    },
                               { ModifiedWriteValues::oneToToggle,
                                "oneToToggle" },
                               { ModifiedWriteValues::zeroToClear,
                                "zeroToClear" },
                               {   ModifiedWriteValues::zeroToSet,
                                "zeroToSet"   },
                               {ModifiedWriteValues::zeroToToggle,
                                "zeroToToggle"},
                               {       ModifiedWriteValues::clear,
                                "clear"       },
                               {         ModifiedWriteValues::set,
                                "set"         },
                               {      ModifiedWriteValues::modify,
                                "modify"      },
})
enum class ReadAction : std::uint8_t { empty, clear, set, modify, modifyExternal };

NLOHMANN_JSON_SERIALIZE_ENUM(ReadAction,
                             {
                               {         ReadAction::empty,
                                ""              },
                               {         ReadAction::clear,
                                "clear"         },
                               {           ReadAction::set,
                                "set"           },
                               {        ReadAction::modify,
                                "modify"        },
                               {ReadAction::modifyExternal,
                                "modifyExternal"},
})

struct Value {
    std::string   name;
    std::string   description;
    std::uint64_t value = 0;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Value,
                                   name,
                                   description,
                                   value)
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
    // the register must carry it - Register::RmwHazard's MustSupply
    bool key = false;
    // what can be written without changing the field: "none", "zero" or "one" (identityOf)
    std::string identity = "none";
    // `<!-- Kvasir: write-only accepted: <why> -->` inside the <field>: a write-only field without a
    // write identity that was looked at and is fine as it is - no write-only finding for it
    bool               writeOnlyAccepted = false;
    std::vector<Value> values;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Field,
                                   name,
                                   description,
                                   type,
                                   repType,
                                   dim,
                                   dimIncrement,
                                   resetValue,
                                   startBit,
                                   stopBit,
                                   dataType,
                                   access,
                                   modifiedWriteValues,
                                   readAction,
                                   key,
                                   identity,
                                   writeOnlyAccepted,
                                   values)
};

struct Register {
    std::string   name;
    std::string   description;
    RepeatType    type          = RepeatType::normal;
    std::uint64_t dim           = 0;
    std::uint64_t dimIncrement  = 0;
    DataType      dataType      = DataType::u32;
    std::uint64_t addressOffset = 0;
    std::uint64_t resetValue    = 0;
    std::uint64_t zeroMask      = 0;
    std::uint64_t oneMask       = 0;
    // FULLREGISTER's access: readOnly when every field is, readWrite otherwise
    Access access = Access::readWrite;
    // Register::RmwHazard: the key fields' bits every write must supply, and whether a read has a
    // side effect (a field with a readAction) - from the SVD (computeMasks)
    std::uint64_t mustSupplyMask    = 0;
    bool          readHasSideEffect = false;
    // write-only fields without a write identity that a partial write of a sibling field would read
    // and write back as read (computeWriteOnlyHazards)
    std::uint64_t writeOnlyNoIdentityMask = 0;
    // no field of the register can be read (--write-only-registers=derived): a partial write never
    // needs to read it
    bool               writeOnlyRegister = false;
    std::vector<Field> fields;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Register,
                                   name,
                                   description,
                                   type,
                                   dim,
                                   dimIncrement,
                                   dataType,
                                   addressOffset,
                                   resetValue,
                                   zeroMask,
                                   oneMask,
                                   access,
                                   mustSupplyMask,
                                   readHasSideEffect,
                                   writeOnlyNoIdentityMask,
                                   writeOnlyRegister,
                                   fields)
};

struct RegisterGroup {
    std::string           name;
    std::string           description;
    std::uint64_t         dim           = 0;
    std::uint64_t         dimIncrement  = 0;
    std::uint64_t         addressOffset = 0;
    std::vector<Register> registers;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(RegisterGroup,
                                   name,
                                   description,
                                   dim,
                                   dimIncrement,
                                   addressOffset,
                                   registers)
};

struct AddressType {
    std::uint64_t index;
    std::uint64_t address;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(AddressType,
                                   index,
                                   address)
};

struct Peripheral {
    std::string                name;
    std::string                description;
    RepeatType                 type        = RepeatType::normal;
    DataType                   addressType = DataType::u32;
    std::vector<Register>      registers;
    std::vector<RegisterGroup> registerGroups;
    std::vector<AddressType>   baseAddresses;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Peripheral,
                                   name,
                                   description,
                                   type,
                                   baseAddresses,
                                   addressType,
                                   registers,
                                   registerGroups)
};

// The write-only guard (svd_converter --write-only-guard / --write-only-registers / --write-only-mask).
enum class WriteOnlyGuard : std::uint8_t { off, warn, error };

enum class WriteOnlyRegisters : std::uint8_t { off, derived };

struct ConvertOptions {
    WriteOnlyGuard     guard              = WriteOnlyGuard::warn;
    bool               writeOnlyMask      = false;   // passed to the template as `writeOnlyMask`
    WriteOnlyRegisters writeOnlyRegisters = WriteOnlyRegisters::off;
    bool               namedRegisters
      = false;   // --named-registers: every register's Address names it (Register::Named)
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
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Interrupt,
                                   name,
                                   value)
};

struct Chip {
    std::string             name;
    std::string             description;
    std::vector<Peripheral> peripherals;
    std::vector<Interrupt>  interrupts;   // sorted by value, one per value
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Chip,
                                   name,
                                   description,
                                   peripherals,
                                   interrupts)
};
#ifdef __clang__
    #pragma clang diagnostic pop
#endif
