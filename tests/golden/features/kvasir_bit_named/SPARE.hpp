
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
//Interrupt only: no registers, base address 0
namespace Kvasir::Peripheral::SPARE {
    using Kvasir::Register::Address;
    using Kvasir::Register::FieldLocation;
    using Kvasir::Register::FieldValue;
    using Kvasir::Register::maskFromRange;
    using Kvasir::Register::ReadWriteAccess;
    using Kvasir::Register::WriteOnlyAccess;
    using Kvasir::Register::ReadOnlyAccess;
    using Kvasir::Register::value;

}
