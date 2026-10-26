#pragma once

// What a register's writes do, from its fields as read: the write-ignored masks, each field's
// identity, the key and read-side-effect facts, the write-only classification (analyze), and the
// write-only findings that are reported from it.

#include "svd/model.hpp"

#include <cstdint>
#include <string>
#include <vector>

namespace Svd {

// What can be written to a field without changing it. The same table is
// Kvasir::Register::identityOf in the SDK (Register/Utility.hpp); keep the two in step.
// SVD `clear`/`set` (any write clears/sets the field) stays `none`: the RP2040's SVD marks its
// self-clearing triggers that way, but also I2C IC_DATA_CMD's CMD/STOP/RESTART, where a 0 is a
// value. A self-clearing trigger whose 0 does nothing is written `oneToSet` in the SVD.
constexpr Identity identityOf(Access              a,
                              ModifiedWriteValues m) {
    using M = ModifiedWriteValues;
    if(a == Access::readOnly) { return Identity::zero; }
    switch(m) {
    case M::oneToClear:
    case M::oneToSet:
    case M::oneToToggle:  return Identity::zero;
    case M::zeroToClear:
    case M::zeroToSet:
    case M::zeroToToggle: return Identity::one;
    case M::empty:
    case M::clear:
    case M::set:
    case M::modify:       return Identity::none;
    }
    return Identity::none;
}

constexpr bool isWriteOnly(Access a) { return a == Access::writeOnly || a == Access::writeOnce; }

// The bits of a field, every element of a dim field.
[[nodiscard]] std::uint64_t fieldMask(Field const& f);

// The one analysis of a register, after reading: its write-ignored masks, the fields' identities, the
// key and read-side-effect facts, and the write-only classification with its hazards.
void analyze(Register&             r,
             ConvertOptions const& options);

// Every register of the peripheral and of its register groups.
void analyze(Peripheral&           peripheral,
             ConvertOptions const& options);

// The write-only findings of one SVD peripheral (its registers, then its register groups as
// `<where>.<group>`), from the hazards analyze() left on its registers. Only reads.
void writeOnlyFindings(std::string const&             where,
                       Peripheral const&              peripheral,
                       std::vector<WriteOnlyFinding>& findings);

// The one-line report of a finding, as the converter prints it.
[[nodiscard]] std::string describe(WriteOnlyFinding const& f);

}   // namespace Svd
