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

### Command Line Usage

```bash
./svd_converter input.svd output_dir kvasir_bit
```

### Generators

- `kvasir_bit` - Generates Kvasir register access headers
- `json` - Outputs structured JSON representation
- `custom_template` - Uses custom inja templates

### Custom Template Example

```cmake
svd_convert(my_chip
    SVD_FILE chip.svd
    OUTPUT_DIRECTORY output
    GENERATOR custom_template
    GENERATOR_ARGS templates/ hpp
)
```
