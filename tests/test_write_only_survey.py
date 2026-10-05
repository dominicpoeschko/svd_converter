#!/usr/bin/env python3
"""tools/write_only_survey.py on a two-peripheral fixture: a mixed register (SWRST next to ENABLE), a
register with no readable field (two write-only channels), a copy of the first peripheral (counted
once), and a register without a hazard."""
import io
import json
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "tools"))
import write_only_survey  # noqa: E402


def field(name, start, stop, access="readWrite", dim=0, inc=0):
    return {"name": name, "startBit": start, "stopBit": stop, "access": access, "dim": dim, "dimIncrement": inc}


def register(name, fields, mask):
    return {"name": name, "fields": fields, "writeOnlyNoIdentityMask": mask}


TC = {
    "name": "TC",
    "registers": [
        register("CTRLA", [field("SWRST", 0, 0, "writeOnly"),
                 field("ENABLE", 1, 1)], 0x1),
        register("STATUS", [field("BUSY", 0, 0, "readOnly")], 0),
    ],
    "registerGroups": [],
}
EVSYS = {
    "name": "EVSYS",
    "registers": [],
    "registerGroups": [{
        "name": "CH",
        "registers": [register("SWEVT", [field("CHANNEL", 0, 0, "writeOnly", dim=2, inc=1)], 0x3)],
    }],
}


class Survey(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        root = Path(self.dir.name)
        (root / "TC.json").write_text(json.dumps(TC))
        # a copy: same registers
        (root / "TCC.json").write_text(json.dumps(dict(TC, name="TCC")))
        (root / "EVSYS.json").write_text(json.dumps(EVSYS))
        self.root = root

    def tearDown(self):
        self.dir.cleanup()

    def test_counts(self):
        findings, c = write_only_survey.survey(self.root)
        self.assertEqual(c, {"fields": 2, "registers": 2, "unreadable_registers": 1, "unreadable_fields": 1,
                             "mixed_registers": 1, "mixed_fields": 1})
        self.assertIn(("EVSYS.CH", "SWEVT", "CHANNEL", False), findings)
        self.assertIn(("TC", "CTRLA", "SWRST", True), findings)

    def test_table_and_verbose(self):
        out = io.StringIO()
        with redirect_stdout(out):
            self.assertEqual(write_only_survey.main(["-v", str(self.root)]), 0)
        text = out.getvalue()
        self.assertIn("| 2 | 2 | 1 (1) | 1 (1) |", text)
        self.assertIn("mixed      TC.CTRLA.SWRST", text)
        self.assertIn("unreadable EVSYS.CH.SWEVT.CHANNEL", text)

    def test_dim_mask(self):
        self.assertEqual(write_only_survey.field_mask(
            field("S", 0, 0, dim=4, inc=2)), 0x55)


if __name__ == "__main__":
    unittest.main()
