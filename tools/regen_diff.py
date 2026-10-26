#!/usr/bin/env python3
"""Convert a set of SVDs with two svd_converters (or one converter over two input roots) and compare
the raw output byte for byte: the check for every generator or SVD change.

    regen_diff.py --old <bin|ref|worktree> --new <bin|ref|worktree> --root <dir> [options]
    regen_diff.py --old <...> --new <...> <svd>... [options]
    regen_diff.py --converter <bin|ref|worktree> --old-root <snapshot> --root <dir> [options]

A converter is an executable file, the word `worktree` (this checkout, as it is on disk) or a git
ref of this repository (exported with `git archive`, read-only for the repository). Those two are
configured and built in the out dir; `--deps <dir>` makes that build offline, with the fetched
dependencies of a configured tree (`<dir>/<dep>-src`, for example
`<tree>/kvasir_svd_converter/svd_converter_host_build/_deps`).

The SVD set of a root: `chip_*/chip.svd` and `core_*/*.svd`, plus what every
`chip_*/scripts/make_*_svd.py` generates (`--out <file>`; run with the script's folder as cwd into
`<out>/inputs/`, never in place). `chip_*/svd/` is not read: it may hold fragments that are no
whole SVD. `--no-variants` leaves the generators out. With `--old-root` each root generates its
own variants, so the report shows every header an SVD edit reaches.

Every SVD is converted per side into `<out>/<side>/<svd-id>/`: `kvasir_bit/`, `json/` (and
`kvasir_bit_named/` with `--named`), each with the converter's stdout, stderr and exit code, into
a fresh folder. The options default to the write-only guard profile
`--write-only-guard=error --write-only-registers=derived --write-only-mask=on` and are always
passed explicitly.

    --compile       instantiate every register of the new side (tools/instantiate_all.py) and
                    compile it with each of --cxx against --sdk; tools/regen_diff_known.txt lists
                    the SVDs known not to compile, with the reason
    --json-check    mask invariants per register of the new side's json, without recomputing them;
                    a known finding is listed as `<svd-id>:<PERIPH.REG[.FIELD]> <reason>` (it
                    covers what lies below it)

Exit 0: identical (and compiled / checked clean), 1: differences or findings, 2: a tool error.
"""
import argparse
import concurrent.futures
import difflib
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
sys.path.insert(0, str(HERE))
import instantiate_all  # noqa: E402

PROFILE = "--write-only-guard=error --write-only-registers=derived --write-only-mask=on"
DEPS = ("CMAKE_HELPERS", "NLOHMANN_JSON", "PUGIXML", "INJA")


class ToolError(Exception):
    pass


def run(cmd, **kw):
    kw.setdefault("text", True)
    return subprocess.run(cmd, capture_output=True, **kw)


# --- converters --------------------------------------------------------------------------------


def build_converter(spec, where, deps):
    """The path of a converter binary: `spec` itself, or `worktree` / a git ref built in `where`."""
    if spec != "worktree" and Path(spec).is_file():
        if not os.access(spec, os.X_OK):
            raise ToolError(f"{spec} is not executable")
        return Path(spec).resolve()
    src = where / "src"
    if src.exists():
        shutil.rmtree(src)
    src.mkdir(parents=True)
    if spec == "worktree":
        ignore = shutil.ignore_patterns(".git", "build", "build_*", "build-*")
        shutil.copytree(REPO, src, dirs_exist_ok=True, ignore=ignore)
    else:
        archive = run(["git", "-C", str(REPO), "archive",
                      "--format=tar", spec], text=False)
        if archive.returncode != 0:
            raise ToolError(
                f"git archive {spec}: {archive.stderr.decode(errors='replace').strip()}")
        untar = subprocess.run(
            ["tar", "-x", "-C", str(src)], input=archive.stdout, capture_output=True)
        if untar.returncode != 0:
            raise ToolError(
                f"tar: {untar.stderr.decode(errors='replace').strip()}")
    build = where / "build"
    configure = ["cmake", "-S", str(src), "-B", str(build),
                 "-G", "Ninja", "-DCMAKE_BUILD_TYPE=Release"]
    if deps:
        configure.append("-DFETCHCONTENT_FULLY_DISCONNECTED=ON")
        for dep in DEPS:
            configure.append(
                f"-DFETCHCONTENT_SOURCE_DIR_{dep}={Path(deps).resolve() / (dep.lower() + '-src')}")
    for cmd in (configure, ["cmake", "--build", str(build), "--target", "svd_converter"]):
        r = run(cmd)
        if r.returncode != 0:
            raise ToolError(
                f"building converter {spec} failed: {' '.join(cmd)}\n{r.stdout[-3000:]}{r.stderr[-3000:]}")
    return build / "svd_converter"


# --- the SVD set -------------------------------------------------------------------------------


def svd_id(path, root):
    rel = Path(path).resolve().relative_to(Path(root).resolve()
                                           ) if root else Path(Path(path).parent.name) / Path(path).name
    return str(rel).replace("/", "_").replace(".", "_")


def collect(root, inputs, variants):
    """{svd-id: path} of one root: the checked-in SVDs and, with variants, the generated ones."""
    root = Path(root).resolve()
    svds = {}
    for pattern in ("chip_*/chip.svd", "core_*/*.svd"):
        for p in sorted(root.glob(pattern)):
            svds[svd_id(p, root)] = p
    if variants:
        for script in sorted(root.glob("chip_*/scripts/make_*_svd.py")):
            chip = script.parent.parent.name
            out = inputs / chip / f"{script.stem}.svd"
            out.parent.mkdir(parents=True, exist_ok=True)
            if out.exists():
                out.unlink()
            r = run([sys.executable, "-I", "-B", str(script),
                    "--out", str(out)], cwd=script.parent)
            if r.returncode != 0 or not out.is_file() or out.stat().st_size == 0:
                raise ToolError(f"generator {script} failed (exit {r.returncode}, wrote "
                                f"{'nothing' if not out.exists() else out.stat().st_size}):\n{r.stderr.strip()}")
            svds[f"{chip}_gen_{script.stem}"] = out
    if not svds:
        raise ToolError(f"no SVD under {root}")
    return svds


# --- converting --------------------------------------------------------------------------------


def modes(named):
    out = [("kvasir_bit", "kvasir_bit", []), ("json", "json", [])]
    if named:
        out.append(("kvasir_bit_named", "kvasir_bit",
                   ["--named-registers=on"]))
    return out


def convert(converter, svd, dest, options, named):
    for folder, generator, extra in modes(named):
        out = dest / folder
        out.mkdir(parents=True)
        r = run([str(converter), *options, *extra,
                str(svd), str(out), generator])
        (dest / f"{folder}.stdout.txt").write_text(r.stdout)
        (dest / f"{folder}.stderr.txt").write_text(r.stderr)
        (dest / f"{folder}.exit.txt").write_text(f"{r.returncode}\n")


def convert_side(converter, svds, side_dir, options, named, jobs):
    if side_dir.exists():
        shutil.rmtree(side_dir)
    side_dir.mkdir(parents=True)
    with concurrent.futures.ThreadPoolExecutor(jobs) as pool:
        for f in [pool.submit(convert, converter, p, side_dir / i, options, named) for i, p in svds.items()]:
            f.result()


# --- comparing ---------------------------------------------------------------------------------


def files(d):
    return {str(p.relative_to(d)) for p in d.rglob("*") if p.is_file()} if d.is_dir() else set()


def compare(old, new, ids, verbose, lines):
    """Prints one line per SVD; returns the number of SVDs that differ."""
    differing = 0
    for i in ids:
        a, b = old / i, new / i
        if not a.is_dir():
            print(f"{i}: added (only on the new side)")
            differing += 1
            continue
        if not b.is_dir():
            print(f"{i}: removed (only on the old side)")
            differing += 1
            continue
        fa, fb = files(a), files(b)
        changed = sorted(f for f in fa & fb if (
            a / f).read_bytes() != (b / f).read_bytes())
        added, removed = sorted(fb - fa), sorted(fa - fb)
        if not (changed or added or removed):
            print(f"{i}: identical")
            continue
        differing += 1
        print(
            f"{i}: {len(changed)} changed, {len(added)} added, {len(removed)} removed")
        for kind, names in (("changed", changed), ("added", added), ("removed", removed)):
            for n in names:
                print(f"    {kind} {n}")
        if verbose:
            for n in changed:
                diff = difflib.unified_diff((a / n).read_text(errors="replace").splitlines(),
                                            (b / n).read_text(errors="replace").splitlines(),
                                            f"old/{i}/{n}", f"new/{i}/{n}", lineterm="", n=2)
                for k, line in enumerate(diff):
                    if k >= lines:
                        print("    ...")
                        break
                    print(f"    {line}")
    return differing


# --- --compile ---------------------------------------------------------------------------------


def read_known(path):
    known = {}
    if path and Path(path).is_file():
        for line in Path(path).read_text().splitlines():
            line = line.strip()
            if line and not line.startswith("#"):
                name, _, reason = line.partition(" ")
                known[name] = reason.strip()
    return known


def sdk_dirs(sdk):
    sdk = Path(sdk).resolve()
    # Kvasir_SDK/src or Kvasir_SDK
    root = sdk.parent if (sdk / "kvasir").is_dir() else sdk
    if not (root / "src" / "kvasir").is_dir() or not (root / "tests" / "kvasir_test.hpp").is_file():
        raise ToolError(f"--sdk {sdk}: no src/kvasir or tests/kvasir_test.hpp")
    return root / "src", root / "tests" / "kvasir_test.hpp"


def compile_one(cxx, tu, include_dir, sdk_src, test_hpp):
    cmd = [cxx, "-std=c++26", "-fsyntax-only", "-Wall", "-Wextra", "-Wpedantic", "-Werror", f"-I{sdk_src}",
           f"-I{include_dir}", "-DKVASIR_REGISTER_MOCK", "-include", str(test_hpp), str(tu)]
    r = run(cmd)
    return r.returncode, r.stderr


def compile_side(side_dir, ids, compilers, sdk, known, jobs):
    """Prints one line per SVD and compiler; returns the number of unexpected results."""
    sdk_src, test_hpp = sdk_dirs(sdk)
    jobs_list = []
    for i in ids:
        d = side_dir / i
        if not (d / "json").is_dir() or not any((d / "json").glob("*.json")):
            continue
        tu = d / "instantiate_all.cpp"
        extra = ["InterruptNames.hpp"] if (
            d / "kvasir_bit" / "InterruptNames.hpp").is_file() else []
        text, count = instantiate_all.translation_unit(d / "json", "", extra)
        tu.write_text(text)
        for cxx in compilers:
            jobs_list.append((i, cxx, count, tu, d / "kvasir_bit"))
    bad = 0
    with concurrent.futures.ThreadPoolExecutor(jobs) as pool:
        futures = [(j, pool.submit(compile_one, j[1], j[3],
                    j[4], sdk_src, test_hpp)) for j in jobs_list]
        for (i, cxx, count, tu, _), f in futures:
            rc, err = f.result()
            if rc == 0 and i in known:
                print(
                    f"{i} [{cxx}]: compiles now, remove it from the known list ({count} instantiations)")
                bad += 1
            elif rc == 0:
                print(f"{i} [{cxx}]: ok ({count} instantiations)")
            elif i in known:
                print(f"{i} [{cxx}]: known failure: {known[i]}")
            else:
                first = [line for line in err.splitlines()
                         if "error" in line][:5]
                print(f"{i} [{cxx}]: FAILED ({count} instantiations)")
                for line in first:
                    print(f"    {line}")
                bad += 1
    return bad


# --- --json-check ------------------------------------------------------------------------------


def field_mask(f):
    mask = 0
    for k in range(f.get("dim") or 1):
        start = f["startBit"] + k * f.get("dimIncrement", 0)
        stop = f["stopBit"] + k * f.get("dimIncrement", 0)
        mask |= ((1 << (stop - start + 1)) - 1) << start
    return mask


def writable(f):
    return f["access"] != "readOnly"


def register_findings(where, r):
    """(where, text) per broken invariant of one register. Fields may overlap: an SVD can describe
    one set of bits twice, as a read view (read-only) and a write view (AIRCR VECTKEYSTAT and
    VECTKEY). The write view decides the masks; two WRITABLE views with different identities
    contradict each other."""
    found = []
    z, o = r["zeroMask"], r["oneMask"]

    def say(w, text):
        found.append((w, f"{w}: {text}"))

    if z & o:
        say(where, f"zeroMask & oneMask = {z & o:#x}")
    if r["writeBackActsMask"] & ~z:
        say(where,
            f"writeBackActsMask {r['writeBackActsMask']:#x} not inside zeroMask {z:#x}")
    keys = 0
    readable = False
    for f in r["fields"]:
        m = field_mask(f)
        name = f"{where}.{f['name']}"
        conflicting = [k for k, g in enumerate(r["fields"]) if g is not f and writable(g) and writable(f)
                       and g["identity"] != f["identity"] and field_mask(g) & m]
        if conflicting:
            # once per overlap, by its first field
            if r["fields"].index(f) < conflicting[0]:
                names = ", ".join(r["fields"][k]["name"] for k in conflicting)
                say(name,
                    f"identity {f['identity']} overlaps writable field(s) {names} with another identity")
            continue
        write_view = 0
        for g in r["fields"]:
            if g is not f and writable(g):
                write_view |= field_mask(g)
        if f["identity"] == "none" and m & (z | o):
            say(name,
                f"identity none but {m & (z | o):#x} in zeroMask|oneMask")
        if f["identity"] == "zero":
            outside = m & ~z & (~write_view if not writable(f) else ~0)
            if outside:
                say(name, f"identity zero but {outside:#x} outside zeroMask")
        if f["identity"] == "one" and m & ~o:
            say(name, f"identity one but {m & ~o:#x} outside oneMask")
        if f.get("key"):
            keys |= m
        if f["access"] not in ("writeOnly", "writeOnce"):
            readable = True
    if r["mustSupplyMask"] & ~keys:
        say(where,
            f"mustSupplyMask {r['mustSupplyMask']:#x} outside the key fields {keys:#x}")
    if r["writeOnlyRegister"] and readable:
        say(where, "writeOnlyRegister with a readable field")
    wo = r.get("writeOnlyNoIdentityMask", 0)
    if wo & (z | o):
        say(where, f"writeOnlyNoIdentityMask {wo:#x} inside zeroMask|oneMask")
    covered = 0
    for f in r["fields"]:
        if f["access"] in ("writeOnly", "writeOnce") and f["identity"] == "none":
            covered |= field_mask(f)
    if wo & ~covered:
        say(where,
            f"writeOnlyNoIdentityMask bits {wo & ~covered:#x} belong to no write-only field without identity")
    return found


def json_check(side_dir, ids, known):
    """Prints every finding; returns the number of findings the known list does not name."""
    count = 0
    for i in ids:
        for path in sorted((side_dir / i / "json").glob("*.json")):
            p = json.loads(path.read_text())
            regs = [(f"{p['name']}.{r['name']}", r) for r in p["registers"]]
            for g in p["registerGroups"]:
                regs += [(f"{p['name']}.{g['name']}.{r['name']}", r)
                         for r in g["registers"]]
            for where, r in regs:
                for w, finding in register_findings(where, r):
                    reason = next((v for k, v in known.items() if k.startswith(f"{i}:")
                                   and (w == k[len(i) + 1:] or w.startswith(k[len(i) + 1:] + "."))), None)
                    if reason:
                        print(f"{i} {finding} (known: {reason})")
                    else:
                        print(f"{i} {finding}")
                        count += 1
    return count


# --- main --------------------------------------------------------------------------------------


def parse(argv):
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("svds", nargs="*",
                   help="explicit SVD files instead of --root")
    p.add_argument(
        "--old", help="the old converter: binary, git ref or `worktree`")
    p.add_argument("--new", help="the new converter")
    p.add_argument(
        "--converter", help="one converter for both sides (with --old-root)")
    p.add_argument("--root", help="the input root (new side)")
    p.add_argument(
        "--old-root", help="the input root of the old side (an SVD edit check)")
    p.add_argument("--no-variants", action="store_true",
                   help="do not run chip_*/scripts/make_*_svd.py")
    p.add_argument(
        "-o", "--out", help="out dir (kept; default: a temporary one, removed unless --keep)")
    p.add_argument("--keep", action="store_true")
    p.add_argument("--options", default=PROFILE,
                   help=f"converter options (default: {PROFILE})")
    p.add_argument("--named", action="store_true",
                   help="also convert with --named-registers=on")
    p.add_argument(
        "--deps", help="<dir>/<dep>-src for an offline converter build")
    p.add_argument("--compile", action="store_true")
    p.add_argument("--cxx", default="g++,clang++")
    p.add_argument("--sdk", help="Kvasir_SDK (or its src) for --compile")
    p.add_argument("--known", default=str(HERE / "regen_diff_known.txt"))
    p.add_argument("--json-check", action="store_true")
    p.add_argument("-j", "--jobs", type=int,
                   default=max(1, (os.cpu_count() or 2) // 2))
    p.add_argument("-v", "--verbose", action="store_true",
                   help="unified diffs of changed files")
    p.add_argument("--lines", type=int, default=60,
                   help="diff lines per file with -v")
    a = p.parse_args(argv)
    a.old = a.old or a.converter
    a.new = a.new or a.converter
    if not a.old or not a.new:
        p.error("--old and --new, or --converter")
    if bool(a.svds) == bool(a.root):
        p.error("--root <dir> or SVD files, not both")
    if a.old_root and not a.root:
        p.error("--old-root needs --root")
    if a.compile and not a.sdk:
        p.error("--compile needs --sdk")
    return a


def main(argv=None):
    a = parse(argv)
    if a.out:
        out = Path(a.out).resolve()
        if out.exists() and any(out.iterdir()):
            print(f"regen_diff.py: {out} is not empty", file=sys.stderr)
            return 2
        out.mkdir(parents=True, exist_ok=True)
    else:
        out = Path(tempfile.mkdtemp(prefix="regen_diff_"))
    try:
        return run_all(a, out)
    except ToolError as e:
        print(f"regen_diff.py: {e}", file=sys.stderr)
        return 2
    finally:
        if not a.out and not a.keep:
            shutil.rmtree(out, ignore_errors=True)
        else:
            print(f"output in {out}")


def run_all(a, out):
    if a.root:
        new_svds = collect(a.root, out / "inputs" / "new", not a.no_variants)
        old_svds = collect(a.old_root, out / "inputs" / "old",
                           not a.no_variants) if a.old_root else new_svds
    else:
        for s in a.svds:
            if not Path(s).is_file():
                raise ToolError(f"no such SVD: {s}")
        new_svds = old_svds = {
            svd_id(s, None): Path(s).resolve() for s in a.svds}
    old_bin = build_converter(a.old, out / "converter_old", a.deps)
    new_bin = old_bin if a.new == a.old else build_converter(
        a.new, out / "converter_new", a.deps)
    options = a.options.split()
    convert_side(old_bin, old_svds, out / "old", options, a.named, a.jobs)
    convert_side(new_bin, new_svds, out / "new", options, a.named, a.jobs)
    ids = sorted(set(old_svds) | set(new_svds))
    differing = compare(out / "old", out / "new", ids, a.verbose, a.lines)
    print(f"{len(ids) - differing}/{len(ids)} identical")
    status = 1 if differing else 0
    if a.compile:
        bad = compile_side(out / "new", sorted(new_svds), [c for c in a.cxx.split(",") if c], a.sdk,
                           read_known(a.known), a.jobs)
        print(f"compile: {bad} unexpected result(s)")
        status = status or (1 if bad else 0)
    if a.json_check:
        found = json_check(out / "new", sorted(new_svds), read_known(a.known))
        print(f"json-check: {found} finding(s)")
        status = status or (1 if found else 0)
    return status


if __name__ == "__main__":
    sys.exit(main())
