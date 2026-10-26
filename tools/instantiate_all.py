#!/usr/bin/env python3
"""One C++ translation unit that instantiates every register of a converted SVD.

A static_assert inside a generated register is only evaluated when something instantiates that
register: the registers are nested in `Registers<Instance>` templates. A firmware build therefore
proves only that the registers it touches compile. This script reads the converter's `json`
output (one file per peripheral) and writes a TU that names every instance, register, dim
register element, register group element and dim field element explicitly - the nested ones are
member templates, which an explicit instantiation of the outer class does not instantiate.

    svd_converter <svd> <json dir> json
    svd_converter <svd> <hpp dir>/<prefix> kvasir_bit
    instantiate_all.py <json dir> --include-prefix <prefix> [-o out.cpp] [--also-include H]...
    instantiate_all.py <json dir> --include-prefix <prefix> --traits-dump [-o dump.cpp]

`--traits-dump` makes the TU a host program instead: its main prints one line per register
(every instance and dim/group element) with what the SDK computes from the generated Address -
`RegisterTraits<R::Addr>`'s address, write-ignored masks and write facts. Run it on the output of
two converters and compare the two dumps: a generator change that must not change any register's
write semantics (a new spelling of the mode argument) shows zero differences.

Compile the result against Kvasir_SDK/src with -I<hpp dir>, for example
`-std=c++26 -fsyntax-only -DKVASIR_REGISTER_MOCK -include Kvasir_SDK/tests/kvasir_test.hpp`.

The names follow the kvasir_bit template (src/generator_templates/kvasir_bit.hpp): the namespace
`Kvasir::Peripheral::<UPPER(peripheral)>`, `Registers<i>` for each base address, `<UPPER(reg)>`
(`<Index>` when the register is a dim cluster), `<UPPER(group)><GroupIndex>`, and a dim field
`<UPPER(field)><FieldIndex>` inside its register. A drift between the two is a compile error of
this TU, never a silent pass. A peripheral without registers and register groups has no
`Registers<>` (an interrupt-only block) and is listed as a comment.
"""
import argparse
import json
import sys
from pathlib import Path


def upper(name):
    # inja's upper(): ASCII only, like std::toupper in the "C" locale
    return "".join(c.upper() if "a" <= c <= "z" else c for c in name)


def elements(register):
    """The indices of a dim register, or [None] for a plain one."""
    if register["type"] == "cluster":
        return list(range(register["dim"]))
    return [None]


def named(name, index):
    return upper(name) if index is None else f"{upper(name)}<{index}>"


def register_lines(prefix, register):
    """The instantiations of one register: its dim elements and, inside each, its dim fields.
    A plain register is a member class of an instantiated class and needs no line of its own
    unless something inside it is a template."""
    lines = []
    for index in elements(register):
        reg = f"{prefix}::{named(register['name'], index)}"
        if index is not None:
            lines.append(f"template struct {reg};")
        for field in register["fields"]:
            if field.get("repType") == "cluster":
                for k in range(field["dim"]):
                    lines.append(
                        f"template struct {reg}::{upper(field['name'])}<{k}>;")
    return lines


def concrete_registers(peripheral):
    """Every register struct of the peripheral that has an Addr: per instance, dim element and
    register group element."""
    if not peripheral["registers"] and not peripheral["registerGroups"]:
        return []
    out = []
    ns = f"Kvasir::Peripheral::{upper(peripheral['name'])}"
    for base in sorted(peripheral["baseAddresses"], key=lambda b: b["index"]):
        instance = f"{ns}::Registers<{base['index']}>"
        for register in peripheral["registers"]:
            out += [f"{instance}::{named(register['name'], i)}" for i in elements(register)]
        for group in peripheral["registerGroups"]:
            for g in range(group["dim"]):
                element = f"{instance}::{upper(group['name'])}<{g}>"
                for register in group["registers"]:
                    out += [
                        f"{element}::{named(register['name'], i)}" for i in elements(register)]
    return out


TRAITS_DUMP = """
#include "kvasir/Register/Utility.hpp"

#include <cstdio>

namespace {
template<typename R>
void dump(char const* name) {
    using T = Kvasir::Register::RegisterTraits<typename R::Addr>;
    std::printf("%s 0x%08X wiiz=0x%08X wiio=0x%08X mustSupply=0x%08X readSideEffect=%d "
                "writeOnlyNoIdentity=0x%08X writeOnlyRegister=%d writeBackActs=0x%08X\\n",
                name, T::address, T::writeIgnoredIfZeroMask, T::writeIgnoredIfOneMask, T::mustSupplyMask,
                T::readHasSideEffect ? 1 : 0, T::writeOnlyNoIdentityMask, T::writeOnlyRegister ? 1 : 0,
                T::writeBackActsMask);
}
}   // namespace

int main() {
"""


def peripheral_lines(peripheral):
    if not peripheral["registers"] and not peripheral["registerGroups"]:
        return [f"// {peripheral['name']}: no registers, no Registers<>"]
    lines = []
    ns = f"Kvasir::Peripheral::{upper(peripheral['name'])}"
    for base in sorted(peripheral["baseAddresses"], key=lambda b: b["index"]):
        instance = f"{ns}::Registers<{base['index']}>"
        lines.append(f"template struct {instance};")
        for register in peripheral["registers"]:
            lines += register_lines(instance, register)
        for group in peripheral["registerGroups"]:
            for g in range(group["dim"]):
                element = f"{instance}::{upper(group['name'])}<{g}>"
                lines.append(f"template struct {element};")
                for register in group["registers"]:
                    lines += register_lines(element, register)
    return lines


def traits_dump_unit(json_dir, include_prefix):
    peripherals = sorted((json.loads(p.read_text(encoding="utf-8"))
                          for p in Path(json_dir).glob("*.json")), key=lambda p: p["name"])
    prefix = f"{include_prefix}/" if include_prefix else ""
    head = ["// generated by svd_converter/tools/instantiate_all.py --traits-dump"]
    head += [f'#include "{prefix}{p["name"]}.hpp"' for p in peripherals]
    body = []
    for p in peripherals:
        body += [f'    dump<{r}>("{r}");' for r in concrete_registers(p)]
    text = "\n".join(head + [TRAITS_DUMP.rstrip("\n")] + body + ["}", ""])
    return text, len(body)


def translation_unit(json_dir, include_prefix, also_include=()):
    peripherals = []
    for path in sorted(Path(json_dir).glob("*.json")):
        peripherals.append(json.loads(path.read_text(encoding="utf-8")))
    peripherals.sort(key=lambda p: p["name"])
    head = ["// generated by svd_converter/tools/instantiate_all.py - every register instantiated"]
    body = []
    count = 0
    for p in peripherals:
        prefix = f"{include_prefix}/" if include_prefix else ""
        head.append(f'#include "{prefix}{p["name"]}.hpp"')
        lines = peripheral_lines(p)
        count += sum(1 for line in lines if line.startswith("template struct"))
        body += lines
    head += [f'#include "{h}"' for h in also_include]
    text = "\n".join(head + [""] + body + [""])
    return text, count


def main(argv=None):
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("json_dir")
    parser.add_argument("--include-prefix", default="",
                        help="folder in front of each header name")
    parser.add_argument("--also-include", action="append",
                        default=[], help="another header to include")
    parser.add_argument("-o", "--output", help="the TU (default: stdout)")
    parser.add_argument("--traits-dump", action="store_true",
                        help="a host program printing every register's RegisterTraits")
    args = parser.parse_args(argv)
    if not any(Path(args.json_dir).glob("*.json")):
        print(
            f"instantiate_all.py: no *.json in {args.json_dir}", file=sys.stderr)
        return 2
    if args.traits_dump:
        text, count = traits_dump_unit(args.json_dir, args.include_prefix)
    else:
        text, count = translation_unit(
            args.json_dir, args.include_prefix, args.also_include)
    if args.output:
        out = Path(args.output)
        # written only when changed: a build that runs this every time recompiles nothing
        if not out.exists() or out.read_text(encoding="utf-8") != text:
            out.write_text(text, encoding="utf-8")
        print(f"instantiate_all.py: {count} instantiations -> {out}")
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
