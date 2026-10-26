#include "svd/semantics.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <string>
#include <utility>
#include <vector>

namespace Svd {
namespace {
    // The register's write-ignored masks from its fields. zeroMask: the bits a write of zero leaves
    // alone - reserved bits, fields with identity zero (one-to-* flags, read-only fields). oneMask: the
    // bits a write of one leaves alone - zero-to-* fields. Both disjoint. writeBackActsMask: the part of
    // zeroMask a read-modify-write must still write as 0, the writable one-to-* fields (a 1 read back and
    // written again clears a flag or fires a trigger); reserved and read-only bits are kept as read - Arm
    // RES0 is should-be-zero-or-preserved (DDI0553B.y glossary "RES0"), SAM D21 reserved bits always read
    // 0 (DS40001882L 46.4 "Registers and Bits"), STM32 reserved bits "must be kept at reset value" (RM0430
    // 1.2). A partial write that covers every bit outside them needs no read. Also the register's
    // Register::Semantics: the bits of its key fields (mustSupplyMask) and whether any field's read has a
    // side effect (a readAction).
    constexpr bool readable(Access a) noexcept {
        return a == Access::readOnly || a == Access::readWrite || a == Access::readWriteOnce;
    }

    // FULLREGISTER's access: a register without fields is the one value its <access> says (writeOnce as
    // write-only, read-writeOnce as read-write); with fields it is read-only when every field is, write-only
    // when no field can be read, read-write otherwise.
    Access fullRegisterAccess(Register const& r) noexcept {
        if(r.fields.empty()) {
            switch(r.svdAccess) {
            case Access::readOnly:      return Access::readOnly;
            case Access::writeOnly:
            case Access::writeOnce:     return Access::writeOnly;
            case Access::readWrite:
            case Access::readWriteOnce: return Access::readWrite;
            }
        }
        bool const allReadOnly = std::ranges::all_of(r.fields, [](Field const& f) {
            return f.access == Access::readOnly;
        });
        bool const noneReadable
          = std::ranges::none_of(r.fields, [](Field const& f) { return readable(f.access); });
        if(allReadOnly) { return Access::readOnly; }
        if(noneReadable) { return Access::writeOnly; }
        return Access::readWrite;
    }

    void computeMasks(Register& r) {
        r.zeroMask          = maskFromRange(dataTypeSize(r.dataType) - 1, 0);
        r.oneMask           = 0;
        r.writeBackActsMask = 0;
        r.mustSupplyMask    = 0;
        r.readHasSideEffect = false;
        for(auto& f : r.fields) {
            auto const id       = identityOf(f.access, f.modifiedWriteValues);
            f.identity          = id;
            r.readHasSideEffect = r.readHasSideEffect || f.readAction != ReadAction::empty;
            // startBit/stopBit describe element 0 of a dim field; the masks cover every element.
            std::uint64_t const elements = f.dim == 0 ? 1 : f.dim;
            for(std::uint64_t i = 0; i < elements; ++i) {
                auto const start = f.startBit + i * f.dimIncrement;
                auto const stop  = f.stopBit + i * f.dimIncrement;
                if(f.key) { r.mustSupplyMask |= maskFromRange(stop, start); }
                switch(id) {
                case Identity::none: r.zeroMask = clearBits(r.zeroMask, start, stop); break;
                case Identity::one:
                    r.zeroMask = clearBits(r.zeroMask, start, stop);
                    r.oneMask |= maskFromRange(stop, start);
                    break;
                case Identity::zero:   // stays in zeroMask
                    if(f.access != Access::readOnly) {
                        r.writeBackActsMask |= maskFromRange(stop, start);
                    }
                    break;
                }
            }
        }
        r.access = fullRegisterAccess(r);
    }

    // After computeMasks. A write-only field W without a write identity (no modifiedWriteValues that
    // makes a written 0 or 1 harmless), not a key, not accepted: a partial write of a sibling field F
    // that needs a read (the needsRead rule of Kvasir's Register/Exec.hpp) and does not cover W reads
    // the register and writes W back as read - a strobe fires again, a FIFO push pushes. Each such W
    // goes into writeOnlyHazards with the siblings that do so, its bits into writeOnlyNoIdentityMask.
    // With `derived`, a register none of whose fields can be read is a whole-register-only register
    // (writeOnlyRegister) and has no hazards: a partial write of it will not read.
    void classifyWriteOnly(Register&          r,
                           WriteOnlyRegisters registers) {
        r.writeOnlyNoIdentityMask = 0;
        r.writeOnlyHazards.clear();
        r.writeOnlyRegister
          = registers == WriteOnlyRegisters::derived && !r.fields.empty()
         && std::ranges::all_of(r.fields, [](Field const& f) { return isWriteOnly(f.access); });
        if(r.readHasSideEffect || r.writeOnlyRegister) { return; }   // never read-modify-written
        auto const all = maskFromRange(dataTypeSize(r.dataType) - 1, 0);
        for(std::size_t wi = 0; wi < r.fields.size(); ++wi) {
            auto const& w = r.fields[wi];
            if(!isWriteOnly(w.access) || w.key || w.writeOnlyAccepted
               || w.identity != Identity::none)
            {
                continue;
            }
            auto const               wm = fieldMask(w);
            std::vector<std::size_t> by;
            for(std::size_t fi = 0; fi < r.fields.size(); ++fi) {
                auto const& f = r.fields[fi];
                if(fi == wi || f.access == Access::readOnly) { continue; }
                auto const cm        = fieldMask(f);
                bool const needsRead = ((cm | r.zeroMask) | (r.oneMask & ~cm)) != all;
                if(needsRead && (wm & ~cm) != 0) { by.push_back(fi); }
            }
            if(!by.empty()) {
                r.writeOnlyNoIdentityMask |= wm;
                r.writeOnlyHazards.push_back({.field = wi, .rewrittenBy = std::move(by)});
            }
        }
    }

    // "bit 11", "bits 15:8"; a dim field: its first element and the count
    std::string bitsText(Field const& f) {
        std::string t = f.startBit == f.stopBit ? std::format("bit {}", f.startBit)
                                                : std::format("bits {}:{}", f.stopBit, f.startBit);
        if(f.dim != 0) { t += std::format(" x{} every {}", f.dim, f.dimIncrement); }
        return t;
    }

    void registerFindings(std::string const&             where,
                          std::vector<Register> const&   registers,
                          std::vector<WriteOnlyFinding>& findings) {
        for(auto const& r : registers) {
            for(auto const& hazard : r.writeOnlyHazards) {
                auto const&      w = r.fields[hazard.field];
                WriteOnlyFinding finding{.where       = where,
                                         .reg         = r.name,
                                         .field       = w.name,
                                         .bits        = bitsText(w),
                                         .rewrittenBy = {},
                                         .readableSibling
                                         = std::ranges::any_of(r.fields, [](Field const& f) {
                                               return !isWriteOnly(f.access);
                                           })};
                for(auto const fi : hazard.rewrittenBy) {
                    finding.rewrittenBy.push_back(r.fields[fi].name);
                }
                findings.push_back(std::move(finding));
            }
        }
    }
}   // namespace

std::uint64_t fieldMask(Field const& f) {
    std::uint64_t const elements = f.dim == 0 ? 1 : f.dim;
    std::uint64_t       mask     = 0;
    for(std::uint64_t i = 0; i < elements; ++i) {
        mask |= maskFromRange(f.stopBit + i * f.dimIncrement, f.startBit + i * f.dimIncrement);
    }
    return mask;
}

void analyze(Register&             r,
             ConvertOptions const& options) {
    computeMasks(r);
    classifyWriteOnly(r, options.writeOnlyRegisters);
}

void analyze(Peripheral&           peripheral,
             ConvertOptions const& options) {
    for(auto& r : peripheral.registers) { analyze(r, options); }
    for(auto& group : peripheral.registerGroups) {
        for(auto& r : group.registers) { analyze(r, options); }
    }
}

void writeOnlyFindings(std::string const&             where,
                       Peripheral const&              peripheral,
                       std::vector<WriteOnlyFinding>& findings) {
    registerFindings(where, peripheral.registers, findings);
    for(auto const& group : peripheral.registerGroups) {
        registerFindings(where + "." + group.name, group.registers, findings);
    }
}

std::string describe(WriteOnlyFinding const& f) {
    std::string by;
    for(auto const& name : f.rewrittenBy) { by += (by.empty() ? "" : ", ") + name; }
    return std::format(
      "{}.{}.{} ({}) is write-only with no write identity: a write of {} reads the "
      "register and writes {} back as read",
      f.where,
      f.reg,
      f.field,
      f.bits,
      by,
      f.field);
}

}   // namespace Svd
