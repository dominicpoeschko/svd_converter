#pragma once

// The model's json form: what the json generator writes and the kvasir_bit view model reads back.
// The key names are the member names; the enums are their names ("u32", "readWrite", "oneToClear",
// "" for no readAction, "none"/"zero"/"one").

#include "inja_wrapper.hpp"
#include "svd/model.hpp"

#ifdef __clang__
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wexit-time-destructors"
#endif

namespace Svd {

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

NLOHMANN_JSON_SERIALIZE_ENUM(RepeatType,
                             {
                               { RepeatType::normal,
                                "normal" },
                               {RepeatType::cluster,
                                "cluster"}
})

NLOHMANN_JSON_SERIALIZE_ENUM(FieldType,
                             {
                               {  FieldType::normal,
                                "normal"},
                               {FieldType::typeEnum,
                                "enum"  }
})

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

NLOHMANN_JSON_SERIALIZE_ENUM(Identity,
                             {
                               {Identity::none,
                                "none"},
                               {Identity::zero,
                                "zero"},
                               { Identity::one,
                                "one" },
})

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Value,
                                   name,
                                   description,
                                   value)

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Field,
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

// writeOnlyHazards, svdName and svdAccess stay out: the findings are reported, not written, and the
// other two are what the SVD said, read by the converter only
NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Register,
                                   name,
                                   description,
                                   type,
                                   dim,
                                   dimIncrement,
                                   dataType,
                                   addressOffset,
                                   resetValue,
                                   resetMask,
                                   zeroMask,
                                   oneMask,
                                   writeBackActsMask,
                                   access,
                                   mustSupplyMask,
                                   readHasSideEffect,
                                   writeOnlyNoIdentityMask,
                                   writeOnlyRegister,
                                   fields)

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(RegisterGroup,
                                   name,
                                   description,
                                   dim,
                                   dimIncrement,
                                   addressOffset,
                                   registers)

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(AddressType,
                                   index,
                                   address)

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Peripheral,
                                   name,
                                   description,
                                   type,
                                   baseAddresses,
                                   addressType,
                                   registers,
                                   registerGroups)

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Interrupt,
                                   name,
                                   value)

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(Chip,
                                   name,
                                   description,
                                   peripherals,
                                   interrupts)

}   // namespace Svd

#ifdef __clang__
    #pragma clang diagnostic pop
#endif
