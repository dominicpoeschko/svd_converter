# SVD Converter

A C++ tool that converts ARM SVD (System View Description) files into C++ headers for the Kvasir register access library.

## Usage

To use this library, include it in your project as a git submodule and add the following to your `CMakeLists.txt`:

```cmake
add_subdirectory(svd_converter)
```

### Converting SVD Files

```cmake
svd_convert(my_chip
    SVD_FILE path/to/chip.svd
    OUTPUT_DIRECTORY chips
    GENERATOR kvasir_bit
)

target_link_libraries(${target_name} my_chip)
```

The folder `<build>/svd_converter_generated/<OUTPUT_DIRECTORY>` belongs to that one call (a second call with the same
folder is a configure error). A file is rewritten only when its text changed, so an unchanged header keeps its mtime
and nothing recompiles; a file the converter no longer writes (a peripheral that left the SVD) is removed. The headers
are the converter's raw text, not formatted: the same bytes on every machine and in every build folder. The step's
only declared output is a dependency dummy in the folder: to regenerate by hand, delete the folder or the dummy
(deleting a single header is not noticed).

### Write semantics: the write-only guard

The converter and `svd_convert()` share one set of defaults, Kvasir's write semantics:

| option | CLI | `svd_convert()` keyword | default | what it does |
|---|---|---|---|---|
| guard | `--write-only-guard=off\|warn\|error` | `WRITE_ONLY_GUARD` | `error` | reports each write-only field without a write identity that a partial write of a sibling field would read back and rewrite; `error` also fails (exit 2), `warn` only prints |
| registers | `--write-only-registers=derived\|off` | `WRITE_ONLY_REGISTERS` | `derived` | a register with no readable field is whole-register-only: never read, so its fields are no finding |
| mask | `--write-only-mask[=on\|off]` | `WRITE_ONLY_MASK ON\|OFF` | on | the template's `writeOnlyMask`: a read-modify-write that would rewrite a write-only field does not compile |

A field is classified in the SVD: `<modifiedWriteValues>oneToSet</modifiedWriteValues>` (a 0 does nothing), a
one-value `<writeConstraint>` (a key), or `<!-- Kvasir: write-only accepted: <why> -->` inside the `<field>`. An SVD
that is not classified yet opts out with all three off (`--write-only-guard=off --write-only-registers=off
--write-only-mask=off`, or `WRITE_ONLY_GUARD off WRITE_ONLY_REGISTERS off WRITE_ONLY_MASK OFF`). A survey of an
unclassified SVD: `--write-only-guard=warn --write-only-registers=off`. Every `svd_convert()` call that weakens a
default is listed in the global property `SVD_CONVERT_WEAKENED` (`<target>: guard=... registers=... mask=...`).

`NAMED_REGISTERS ON` (`--named-registers`): every register's Address names its register struct, so every register
diagnostic names its fields, at a compile-time cost (default off; the cache value `KVASIR_SVD_NAMED_REGISTERS`
overrides every call of a tree).

### Command Line Usage

```bash
./svd_converter [--write-only-guard=off|warn|error] [--write-only-registers=derived|off] [--write-only-mask[=on|off]]
                [--named-registers] [--clean-output] input.svd output_dir kvasir_bit|json
```

`--clean-output` removes every `*.hpp` (`*.json` for the json generator) and leftover `*.tmp` in `output_dir` that
this run did not write; other files and subfolders stay. Off by default, so a folder of your own keeps its files;
`svd_convert()` always passes it.

### Source layout

The SVD reader is a library of passes in `src/svd/` (namespace `Svd`, target `svd_converter_lib`, linked by the
converter and its tests): `model.hpp` (the types, no XML or json), `json.hpp` (their json form), `read` (the XML into
the model, as written), `semantics` (per register: masks, field identities, key and read-side-effect facts, the
write-only classification and its findings), `naming` (C++ names, instance groups of derived peripherals, prefix
stripping), `interrupts` (one name per vector, the C++ names and the layout) and `chip` (`chipFromSVD`: read ->
analyze -> findings -> group derived peripherals -> mode clusters -> names -> interrupts). A rule about how an SVD
element is read lives in `read.cpp`, what a write does in `semantics.cpp`.

### Tests

`cmake -S tests -B build && cmake --build build && ctest --test-dir build`. What they show:

- `golden_*`: the raw output of `tests/golden/features.svd` (every construct the generators handle) per mode, byte for
  byte; regenerate after an intended change with `cmake -DCONVERTER=<bin> -P tests/golden/update.cmake`.
- `cli_output_*`: `--clean-output` removes stale files and keeps foreign ones, nothing is removed without it, an
  unchanged file keeps its mtime, two folders get the same bytes; `cmake_duplicate_output_dir`: two `svd_convert`
  calls sharing a folder fail at configure time, `cmake_bad_write_only_mask` a mask that is not ON or OFF.
- `cli_write_only_*`, `cmake_write_only_keywords`: the guard's exit code and report per option (the defaults stop on
  the unclassified field), and the command line `svd_convert()` writes for its defaults, explicit keywords and the
  opt-out.
- `test_view_model`: each rendering decision of the kvasir_bit view model, one check each.
- `test_regen_diff`, `test_instantiate_all` (with Python): the tools below.

### Tools

- `tools/regen_diff.py`: converts a set of SVDs with two converters (binaries, git refs or `worktree`), or one
  converter over two input roots, and compares the raw output; `--compile` instantiates every register
  (`tools/instantiate_all.py`) and compiles it against Kvasir, `--json-check` checks the masks' invariants.
- `tools/instantiate_all.py`: one translation unit that instantiates every register of a converted SVD.

### Generators

- `kvasir_bit` - Generates Kvasir register access headers. The template (`src/generator_templates/kvasir_bit.hpp`) is a
  transcription: every decision - the Address's mode argument, which registers get `overrideDefaults` and what it
  writes, the dim-expanded field lists, the log text - lives in `src/view_model.hpp` and is unit-tested
  (`test_view_model`).
- `json` - Outputs structured JSON representation
