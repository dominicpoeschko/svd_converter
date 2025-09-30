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
    std::vector<Value>  values;
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
                                   values)
};

struct Register {
    std::string        name;
    std::string        description;
    RepeatType         type          = RepeatType::normal;
    std::uint64_t      dim           = 0;
    std::uint64_t      dimIncrement  = 0;
    DataType           dataType      = DataType::u32;
    std::uint64_t      addressOffset = 0;
    std::uint64_t      resetValue    = 0;
    std::uint64_t      zeroMask      = 0;
    std::uint64_t      oneMask       = 0;
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

struct Chip {
    std::string             name;
    std::string             description;
    std::vector<Peripheral> peripherals;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Chip,
                                   name,
                                   description,
                                   peripherals)
};
#ifdef __clang__
    #pragma clang diagnostic pop
#endif
