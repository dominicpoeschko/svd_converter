#!/usr/bin/env python3
"""Survey of write-only fields a partial write would read back and rewrite (plan 10 of
plans/embedded_cpp_ideas): reads the svd_converter's `json` generator output - one folder per SVD,
converted with the default --write-only-registers=off - and counts what the converter's write-only
guard reports, from the converter's own answer (each register's writeOnlyNoIdentityMask).

    svd_converter <svd> <dir> json            # per SVD
    write_only_survey.py <dir>...             # one table row per folder
    write_only_survey.py -v <dir>...          # and every field

Columns: hit fields, registers, registers with no readable field (their fields), registers with a
readable sibling (their fields) - the second kind is the dangerous one, the first never needs a read
once --write-only-registers=derived is on. A peripheral that is a copy of another (ST: UART4 from
USART3, same registers) is counted once.
"""
import argparse
import json
import sys
from pathlib import Path


def field_mask(f):
    elements = f["dim"] or 1
    mask = 0
    for i in range(elements):
        start = f["startBit"] + i * f["dimIncrement"]
        stop = f["stopBit"] + i * f["dimIncrement"]
        mask |= ((1 << (stop - start + 1)) - 1) << start
    return mask


def is_write_only(f):
    return f["access"] in ("writeOnly", "writeOnce")


def registers_of(peripheral):
    for r in peripheral["registers"]:
        yield peripheral["name"], r
    for g in peripheral["registerGroups"]:
        for r in g["registers"]:
            yield f'{peripheral["name"]}.{g["name"]}', r


def survey(folder):
    """(rows of findings, counts) for one json folder"""
    seen = set()
    findings = []   # (where, register, field, mixed)
    for path in sorted(Path(folder).glob("*.json")):
        peripheral = json.loads(path.read_text())
        key = json.dumps(
            [peripheral["registers"], peripheral["registerGroups"]], sort_keys=True)
        if key in seen:
            continue
        seen.add(key)
        for where, r in registers_of(peripheral):
            mask = r.get("writeOnlyNoIdentityMask", 0)
            if mask == 0:
                continue
            mixed = any(not is_write_only(f) for f in r["fields"])
            for f in r["fields"]:
                if is_write_only(f) and field_mask(f) & mask:
                    findings.append((where, r["name"], f["name"], mixed))
    registers = {(w, r) for w, r, _, _ in findings}
    mixed_regs = {(w, r) for w, r, _, m in findings if m}
    counts = {
        "fields": len(findings),
        "registers": len(registers),
        "unreadable_registers": len(registers - mixed_regs),
        "unreadable_fields": sum(1 for *_, m in findings if not m),
        "mixed_registers": len(mixed_regs),
        "mixed_fields": sum(1 for *_, m in findings if m),
    }
    return findings, counts


def main(argv=None):
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("-v", "--verbose",
                        action="store_true", help="list every field")
    parser.add_argument("folders", nargs="+", type=Path)
    args = parser.parse_args(argv)
    print("| SVD | hit fields | registers | no readable field: registers (fields) | readable sibling: registers (fields) |")
    print("|---|---|---|---|---|")
    total = {}
    for folder in args.folders:
        findings, c = survey(folder)
        for k, v in c.items():
            total[k] = total.get(k, 0) + v
        print(f'| {folder.name} | {c["fields"]} | {c["registers"]} | {c["unreadable_registers"]} '
              f'({c["unreadable_fields"]}) | {c["mixed_registers"]} ({c["mixed_fields"]}) |')
        if args.verbose:
            for where, reg, field, mixed in findings:
                print(
                    f'    {"mixed     " if mixed else "unreadable"} {where}.{reg}.{field}')
    if len(args.folders) > 1:
        print(f'| **total** | **{total["fields"]}** | **{total["registers"]}** | **{total["unreadable_registers"]} '
              f'({total["unreadable_fields"]})** | **{total["mixed_registers"]} ({total["mixed_fields"]})** |')
    return 0


if __name__ == "__main__":
    sys.exit(main())
