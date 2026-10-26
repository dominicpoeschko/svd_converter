#!/usr/bin/env python3
"""tools/regen_diff.py with fake converters (shell scripts that write fixed files) and a fake input
root: the comparison and its exit codes, the captured stdout/stderr/exit, the variant generators
(run into the out dir, never in place; a failing one is a tool error), --old-root, and the
--json-check invariants on crafted registers."""
import contextlib
import io
import os
import stat
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "tools"))
import regen_diff  # noqa: E402

# options first, then <svd> <out> <generator>; the SVD's text becomes the output, so two inputs
# give two outputs
FAKE = """#!/bin/sh
while [ "${1#--}" != "$1" ]; do shift; done
cat "$1" > "$2/P.$3"
%s
echo "to stdout"
echo "to stderr" >&2
exit %d
"""

GENERATOR = """import sys
from pathlib import Path
out = Path(sys.argv[sys.argv.index("--out") + 1])
out.write_text((Path("..") / "chip.svd").read_text() + "variant\\n")
"""


def tree(root):
    return sorted(str(p.relative_to(root)) for p in Path(root).rglob("*"))


class RegenDiff(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        self.root = self.make_root("root", "<device>a</device>\n")

    def tearDown(self):
        self.tmp.cleanup()

    def make_root(self, name, chip_text, generator=GENERATOR):
        root = self.dir / name
        (root / "chip_x" / "svd").mkdir(parents=True)
        (root / "chip_x" / "scripts").mkdir()
        (root / "core_y").mkdir()
        (root / "chip_x" / "chip.svd").write_text(chip_text)
        (root / "chip_x" / "svd" / "fragment.svd").write_text("<peripherals/>\n")
        (root / "chip_x" / "scripts" / "make_y_svd.py").write_text(generator)
        (root / "core_y" / "core.svd").write_text("<device>core</device>\n")
        return root

    def converter(self, name, extra="", code=0):
        path = self.dir / name
        path.write_text(FAKE % (extra, code))
        path.chmod(path.stat().st_mode | stat.S_IXUSR)
        return str(path)

    def run_tool(self, *args):
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            code = regen_diff.main([str(a) for a in args])
        return code, out.getvalue(), err.getvalue()

    def test_identical(self):
        conv = self.converter("conv")
        before = tree(self.root)
        code, out, _ = self.run_tool(
            "--old", conv, "--new", conv, "--root", self.root, "-o", self.dir / "o")
        self.assertEqual(code, 0, out)
        self.assertIn("3/3 identical", out)
        # the fragment is never converted, the generated variant is
        self.assertNotIn("fragment", out)
        self.assertIn("chip_x_gen_make_y_svd: identical", out)
        self.assertIn("chip_x_chip_svd: identical", out)
        self.assertIn("core_y_core_svd: identical", out)
        self.assertTrue((self.dir / "o" / "inputs" / "new" /
                        "chip_x" / "make_y_svd.svd").is_file())
        # nothing new in the root
        self.assertEqual(tree(self.root), before)

    def test_changed_added_removed(self):
        old = self.converter("old", 'echo gone > "$2/GONE.$3"')
        new = self.converter(
            "new", 'echo x >> "$2/P.$3"; echo new > "$2/NEW.$3"')
        code, out, _ = self.run_tool(
            "--old", old, "--new", new, "--root", self.root, "-o", self.dir / "o", "-v")
        self.assertEqual(code, 1, out)
        self.assertIn("changed kvasir_bit/P.kvasir_bit", out)
        self.assertIn("added json/NEW.json", out)
        self.assertIn("removed json/GONE.json", out)
        self.assertIn("+x", out)   # the unified diff
        self.assertIn("0/3 identical", out)

    def test_streams_and_exit_captured(self):
        conv = self.converter("conv", code=3)
        code, out, _ = self.run_tool(
            "--old", conv, "--new", conv, "--root", self.root, "-o", self.dir / "o")
        self.assertEqual(code, 0, out)
        side = self.dir / "o" / "new" / "chip_x_chip_svd"
        self.assertEqual(
            (side / "kvasir_bit.stdout.txt").read_text(), "to stdout\n")
        self.assertEqual(
            (side / "kvasir_bit.stderr.txt").read_text(), "to stderr\n")
        self.assertEqual((side / "json.exit.txt").read_text(), "3\n")
        # an exit code that differs is a difference
        other = self.converter("other", code=0)
        code, out, _ = self.run_tool(
            "--old", conv, "--new", other, "--root", self.root, "-o", self.dir / "o2")
        self.assertEqual(code, 1)
        self.assertIn("changed json.exit.txt", out)

    def test_failing_generator_is_a_tool_error(self):
        root = self.make_root("bad", "<device/>\n", "raise SystemExit(1)\n")
        conv = self.converter("conv")
        code, _, err = self.run_tool(
            "--old", conv, "--new", conv, "--root", root, "-o", self.dir / "o")
        self.assertEqual(code, 2)
        self.assertIn("make_y_svd.py", err)

    def test_old_root_shows_the_variant(self):
        old_root = self.make_root("snapshot", "<device>old</device>\n")
        conv = self.converter("conv")
        code, out, _ = self.run_tool("--converter", conv, "--old-root", old_root, "--root", self.root,
                                     "-o", self.dir / "o")
        self.assertEqual(code, 1, out)
        self.assertIn("chip_x_chip_svd: 2 changed", out)
        self.assertIn("chip_x_gen_make_y_svd: 2 changed", out)
        self.assertIn("core_y_core_svd: identical", out)

    def test_explicit_svds_and_no_variants(self):
        conv = self.converter("conv")
        code, out, _ = self.run_tool("--old", conv, "--new", conv, "--root", self.root, "--no-variants",
                                     "-o", self.dir / "o")
        self.assertEqual(code, 0)
        self.assertIn("2/2 identical", out)
        code, out, _ = self.run_tool("--old", conv, "--new", conv, self.root / "core_y" / "core.svd",
                                     "-o", self.dir / "o2")
        self.assertEqual(code, 0)
        self.assertIn("core_y_core_svd: identical", out)

    def test_out_dir_must_be_empty(self):
        (self.dir / "o").mkdir()
        (self.dir / "o" / "x").write_text("")
        conv = self.converter("conv")
        code, _, err = self.run_tool(
            "--old", conv, "--new", conv, "--root", self.root, "-o", self.dir / "o")
        self.assertEqual(code, 2)
        self.assertIn("not empty", err)


def field(name, start, stop, access="readWrite", identity="none", key=False):
    return {"name": name, "startBit": start, "stopBit": stop, "dim": 0, "dimIncrement": 0, "access": access,
            "identity": identity, "key": key}


def register(fields, zero=0, one=0, acts=0, must=0, wo=0, wo_register=False):
    return {"zeroMask": zero, "oneMask": one, "writeBackActsMask": acts, "mustSupplyMask": must,
            "writeOnlyNoIdentityMask": wo, "writeOnlyRegister": wo_register, "fields": fields}


class JsonCheck(unittest.TestCase):
    def texts(self, r):
        return [t for _, t in regen_diff.register_findings("P.R", r)]

    def test_clean(self):
        # bits 31:3 reserved, a W1C flag, a plain field, a zero-to-clear flag
        r = register([field("FLAG", 0, 0, identity="zero"), field("EN", 1, 1), field("Z", 2, 2, identity="one")],
                     zero=0xFFFFFFF9, one=0x4, acts=0x1)
        self.assertEqual(self.texts(r), [])

    def test_each_violation(self):
        cases = [
            (register([field("A", 0, 0, identity="zero")],
             zero=0x1, one=0x1), "zeroMask & oneMask"),
            (register([field("A", 0, 0)], zero=0x1), "identity none but"),
            (register([field("A", 0, 0, identity="zero")],
             zero=0x0), "identity zero but"),
            (register([field("A", 0, 0, identity="one")],
             one=0x0), "identity one but"),
            (register([field("A", 0, 0)], zero=0x0,
             acts=0x1), "writeBackActsMask"),
            (register([field("A", 0, 3)], must=0x3), "mustSupplyMask"),
            (register([field("A", 0, 0)], wo_register=True),
             "writeOnlyRegister with a readable"),
            # the write-only guard's mask: inside zeroMask, and a bit of no write-only field
            (register([field("W", 0, 0, "writeOnly", identity="zero")],
             zero=0x1, wo=0x1), "inside zeroMask"),
            (register([field("W", 0, 0, "writeOnly"), field(
                "E", 1, 1)], wo=0x2), "belong to no write-only"),
            (register([field("ALL", 0, 1, identity="zero"), field("B0", 0, 0)], zero=0x2, acts=0x3),
             "overlaps writable field(s) B0"),
        ]
        for r, text in cases:
            found = self.texts(r)
            self.assertTrue(any(text in f for f in found), (text, found))

    def test_read_view_over_a_key(self):
        # AIRCR: a read-only VECTKEYSTAT over the write-only key VECTKEY - the write view decides
        r = register([field("VECTKEYSTAT", 16, 31, "readOnly", identity="zero"),
                      field("VECTKEY", 16, 31, "writeOnly", key=True)], zero=0x0000FFF8, must=0xFFFF0000)
        self.assertEqual(self.texts(r), [])

    def test_known_entries(self):
        with tempfile.TemporaryDirectory() as d:
            known = Path(d) / "known.txt"
            known.write_text(
                "# comment\nsvd_a reason one\nsvd_b:P.R reason two\n")
            self.assertEqual(regen_diff.read_known(known), {
                             "svd_a": "reason one", "svd_b:P.R": "reason two"})


if __name__ == "__main__":
    if os.name != "posix":
        sys.exit(0)
    unittest.main()
