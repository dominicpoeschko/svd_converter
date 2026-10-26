#!/usr/bin/env python3
"""tools/instantiate_all.py on a hand-written json: a derived peripheral with two instances, a dim
register with a dim field, a register group, and an interrupt-only peripheral (no Registers<>)."""
import json
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "tools"))
import instantiate_all  # noqa: E402


def field(name, rep="normal", dim=0):
    return {"name": name, "repType": rep, "dim": dim, "type": "normal"}


def register(name, fields, rep="normal", dim=0):
    return {"name": name, "type": rep, "dim": dim, "fields": fields}


TIMER = {
    "name": "Timer",
    "baseAddresses": [{"index": 1, "address": 0x2000}, {"index": 0, "address": 0x1000}],
    "registers": [
        register("ctrl", [field("en")]),
        register("ch", [field("flag", "cluster", 2)], "cluster", 3),
    ],
    "registerGroups": [{"name": "grp", "dim": 2, "registers": [register("cfg", [field("x")])]}],
}
SPARE = {"name": "SPARE_IRQ", "baseAddresses": [
    {"index": 0, "address": 0}], "registers": [], "registerGroups": []}


class InstantiateAll(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        root = Path(self.dir.name)
        (root / "Timer.json").write_text(json.dumps(TIMER))
        (root / "SPARE_IRQ.json").write_text(json.dumps(SPARE))
        self.root = root

    def tearDown(self):
        self.dir.cleanup()

    def test_lines(self):
        text, count = instantiate_all.translation_unit(
            self.root, "peripherals", ["InterruptNames.hpp"])
        lines = text.splitlines()
        self.assertIn('#include "peripherals/Timer.hpp"', lines)
        self.assertIn('#include "peripherals/SPARE_IRQ.hpp"', lines)
        self.assertIn('#include "InterruptNames.hpp"', lines)
        self.assertIn("// SPARE_IRQ: no registers, no Registers<>", lines)
        ns = "Kvasir::Peripheral::TIMER::Registers"
        for i in (0, 1):
            self.assertIn(f"template struct {ns}<{i}>;", lines)
            for k in range(3):
                self.assertIn(f"template struct {ns}<{i}>::CH<{k}>;", lines)
                for j in range(2):
                    self.assertIn(
                        f"template struct {ns}<{i}>::CH<{k}>::FLAG<{j}>;", lines)
            for g in range(2):
                self.assertIn(f"template struct {ns}<{i}>::GRP<{g}>;", lines)
        # instances in index order
        self.assertLess(lines.index(f"template struct {ns}<0>;"), lines.index(
            f"template struct {ns}<1>;"))
        # a plain register is a member class: no line of its own
        self.assertFalse(any("::CTRL" in line for line in lines))
        # per instance: itself + 3 CH + 6 FLAG + 2 GRP
        self.assertEqual(count, 2 * (1 + 3 + 6 + 2))

    def test_cli_and_empty_dir(self):
        out = Path(self.dir.name) / "out.cpp"
        self.assertEqual(instantiate_all.main(
            [str(self.root), "--include-prefix", "p", "-o", str(out)]), 0)
        self.assertIn('#include "p/Timer.hpp"', out.read_text())
        with tempfile.TemporaryDirectory() as empty:
            self.assertEqual(instantiate_all.main([empty]), 2)


if __name__ == "__main__":
    unittest.main()
