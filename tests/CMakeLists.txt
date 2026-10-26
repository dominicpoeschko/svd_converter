cmake_minimum_required(VERSION 3.28)
project(svd_converter_tests)

include(FetchContent)

FetchContent_Declare(
    cmake_helpers
    GIT_REPOSITORY https://github.com/dominicpoeschko/cmake_helpers.git
    GIT_TAG master)

FetchContent_MakeAvailable(cmake_helpers)

include(${cmake_helpers_SOURCE_DIR}/BuildOptions.cmake)

add_subdirectory(../ ${CMAKE_CURRENT_BINARY_DIR}/svd_converter)

# Dependencies found via find_package inside the subdirectory only exist as directory scoped imported targets there -
# resolve them again at this scope. Fetched dependencies are global targets and skip this.
if(NOT TARGET nlohmann_json::nlohmann_json)
    find_package(nlohmann_json REQUIRED)
endif()
if(NOT TARGET pugixml)
    find_package(pugixml REQUIRED)
endif()
if(NOT TARGET pantor::inja)
    find_package(inja REQUIRED)
endif()

# Propagate stdlib/sanitizer flags into fetched dependencies so libc++/debug-ABI builds link. The fetched pugixml target
# itself is an interface wrapper - flag the compiled libraries.
foreach(dep_target pugixml-static pugixml-shared)
    if(NOT TARGET ${dep_target})
        continue()
    endif()
    get_target_property(dep_aliased ${dep_target} ALIASED_TARGET)
    if(dep_aliased)
        set(dep_target ${dep_aliased})
    endif()
    get_target_property(dep_imported ${dep_target} IMPORTED)
    if(NOT dep_imported)
        target_add_stdlib_flags(${dep_target})
        target_add_stdlib_debug_flags(${dep_target})
        target_add_sanitizer_flags(${dep_target} PUBLIC)
        target_add_optimizer_flags(${dep_target} PUBLIC)
    endif()
endforeach()

enable_testing()

function(svd_converter_add_test name)
    add_executable(${name} ${ARGN})
    target_link_libraries(${name} PRIVATE svd_converter_lib)
    target_add_default_build_options(${name} PRIVATE)
endfunction()

svd_converter_add_test(test_svd_parser svd_parser_tests.cpp)
add_test(NAME test_svd_parser COMMAND test_svd_parser)

# the kvasir_bit view model (src/view_model.hpp): every rendering decision, one check each
svd_converter_add_test(test_view_model view_model_tests.cpp)
add_test(NAME test_view_model COMMAND test_view_model)

# Integration tests run against the real RP2350 svd from the kvasir-io/chip_rp2350 repo. Set SVD_CONVERTER_TEST_SVD_FILE
# to a local copy to skip the download.
set(SVD_CONVERTER_TEST_SVD_FILE
    ""
    CACHE FILEPATH "Local RP2350 svd file for the integration tests (downloaded when empty)")

if(SVD_CONVERTER_TEST_SVD_FILE)
    set(test_svd_file ${SVD_CONVERTER_TEST_SVD_FILE})
else()
    set(test_svd_file ${CMAKE_CURRENT_BINARY_DIR}/rp2350.svd)
    if(NOT EXISTS ${test_svd_file})
        message(STATUS "Downloading RP2350 svd")
        # pinned to a chip_rp2350 commit, so an SVD edit there does not break this CI; bump it together with
        # rp2350_tests.cpp's counts
        file(DOWNLOAD
             https://raw.githubusercontent.com/kvasir-io/chip_rp2350/d48a3f36eb3acc137fbc14f215e6b627f7a67712/chip.svd
             ${test_svd_file} STATUS download_status)
        list(GET download_status 0 download_code)
        if(NOT download_code EQUAL 0)
            file(REMOVE ${test_svd_file})
            list(GET download_status 1 download_error)
            message(FATAL_ERROR "RP2350 svd download failed: ${download_error}")
        endif()
    endif()
endif()

svd_converter_add_test(test_rp2350 rp2350_tests.cpp)

# End to end: run the svd_converter binary with both builtin generators, then let test_rp2350 compare the written json
# files against its in-process parse result (chipFromSVD(device): the SVD as written, so the opt-out options).
add_test(NAME e2e_make_directories
         COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/generated_json
                 ${CMAKE_CURRENT_BINARY_DIR}/generated_kvasir)
set_tests_properties(e2e_make_directories PROPERTIES FIXTURES_SETUP e2e_directories)

set(e2e_optout --write-only-guard=off --write-only-registers=off --write-only-mask=off)
add_test(NAME e2e_generate_json COMMAND svd_converter ${e2e_optout} ${test_svd_file}
                                        ${CMAKE_CURRENT_BINARY_DIR}/generated_json json)
set_tests_properties(e2e_generate_json PROPERTIES FIXTURES_REQUIRED e2e_directories FIXTURES_SETUP e2e_json)

add_test(NAME e2e_generate_kvasir_bit COMMAND svd_converter ${e2e_optout} ${test_svd_file}
                                              ${CMAKE_CURRENT_BINARY_DIR}/generated_kvasir kvasir_bit)
set_tests_properties(e2e_generate_kvasir_bit PROPERTIES FIXTURES_REQUIRED e2e_directories)

add_test(NAME test_rp2350 COMMAND test_rp2350 ${test_svd_file} ${CMAKE_CURRENT_BINARY_DIR}/generated_json)
set_tests_properties(test_rp2350 PROPERTIES FIXTURES_REQUIRED e2e_json)

# The write-only guard on the command line (tests/write_only/guard.svd: one unclassified field, one accepted, one
# register with no readable field): the exit code and the report per mode.
set(guard_svd ${CMAKE_CURRENT_SOURCE_DIR}/write_only/guard.svd)
set(guard_out ${CMAKE_CURRENT_BINARY_DIR}/generated_guard)
set(expect_exit ${CMAKE_CURRENT_SOURCE_DIR}/write_only/expect_exit.cmake)
file(MAKE_DIRECTORY ${guard_out})
function(svd_converter_guard_test name exit_code)
    cmake_parse_arguments(PARSE_ARGV 2 ARG "" "EXPECT;REJECT;OPTIONS" "")
    # a directory per test: ctest -j runs them at once, and the converter's tmp-and-rename writes would collide
    file(MAKE_DIRECTORY ${guard_out}/${name})
    set(defines -DCONVERTER=$<TARGET_FILE:svd_converter> -DSVD=${guard_svd} -DOUT=${guard_out}/${name}
                -DEXPECT_EXIT=${exit_code})
    if(ARG_OPTIONS)
        list(APPEND defines -DOPTIONS=${ARG_OPTIONS})
    endif()
    if(ARG_EXPECT)
        list(APPEND defines -DEXPECT_STDERR=${ARG_EXPECT})
    endif()
    if(ARG_REJECT)
        list(APPEND defines -DREJECT_STDERR=${ARG_REJECT})
    endif()
    add_test(NAME ${name} COMMAND ${CMAKE_COMMAND} ${defines} -P ${expect_exit})
endfunction()
# the defaults (error, registers derived, mask on): the unclassified field stops it, the register with no readable field
# is whole-register-only
svd_converter_guard_test(
    cli_write_only_guard_default 2 EXPECT
    "TC.CTRLA.SWRST \\(bit 0\\) is write-only.*1 write-only field\\(s\\) in 1 register\\(s\\).*--write-only-guard=error"
    REJECT "EVSYS|RTC")
svd_converter_guard_test(
    cli_write_only_guard_warn
    0
    OPTIONS
    --write-only-guard=warn,--write-only-registers=off
    EXPECT
    "TC.CTRLA.SWRST \\(bit 0\\) is write-only.*EVSYS.SWEVT.CHANNEL0.*3 write-only field\\(s\\) in 2 register\\(s\\)"
    REJECT
    "RTC")
svd_converter_guard_test(cli_write_only_guard_error 2 OPTIONS --write-only-guard=error EXPECT "TC.CTRLA.SWRST")
svd_converter_guard_test(cli_write_only_guard_off 0 OPTIONS --write-only-guard=off REJECT "write-only")
svd_converter_guard_test(cli_write_only_bad_mask 1 OPTIONS --write-only-mask=maybe EXPECT "bad --write-only-mask")
svd_converter_guard_test(
    cli_write_only_registers_derived
    2
    OPTIONS
    --write-only-guard=error,--write-only-registers=derived
    EXPECT
    "1 write-only field\\(s\\) in 1 register\\(s\\)"
    REJECT
    "EVSYS")
svd_converter_guard_test(cli_write_only_bad_option 1 OPTIONS --write-only-guard=loud EXPECT "bad --write-only-guard")

# svd_convert's keywords on the generator's command line. `all` builds the three steps: the default call converts the
# fully classified golden/features.svd (guard.svd's unclassified field would stop it, as it should).
svd_convert(
    guard_cmake_default
    SVD_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/golden/features.svd
    OUTPUT_DIRECTORY
    guard_cmake_default
    GENERATOR
    json)
svd_convert(
    guard_cmake_set
    SVD_FILE
    ${guard_svd}
    OUTPUT_DIRECTORY
    guard_cmake_set
    GENERATOR
    json
    WRITE_ONLY_GUARD
    warn
    WRITE_ONLY_REGISTERS
    derived
    WRITE_ONLY_MASK
    ON)
# the opt-out: an explicit off must stay off (an `if(<var>)` default test would take it for "not given")
svd_convert(
    guard_cmake_off
    SVD_FILE
    ${guard_svd}
    OUTPUT_DIRECTORY
    guard_cmake_off
    GENERATOR
    json
    WRITE_ONLY_GUARD
    off
    WRITE_ONLY_REGISTERS
    off
    WRITE_ONLY_MASK
    OFF)
# both weaken the defaults, so both are listed (a conformance tree requires the list empty); the default call is not
get_property(weakened GLOBAL PROPERTY SVD_CONVERT_WEAKENED)
foreach(entry "guard_cmake_set: guard=warn registers=derived mask=on"
              "guard_cmake_off: guard=off registers=off mask=off")
    if(NOT entry IN_LIST weakened)
        message(FATAL_ERROR "svd_convert: SVD_CONVERT_WEAKENED '${weakened}' lacks '${entry}'")
    endif()
endforeach()
if(weakened MATCHES "guard_cmake_default")
    message(FATAL_ERROR "svd_convert: the default call is in SVD_CONVERT_WEAKENED '${weakened}'")
endif()
if(CMAKE_GENERATOR MATCHES "Ninja")
    add_test(NAME cmake_write_only_keywords COMMAND ${CMAKE_COMMAND} -DNINJA_FILE=${CMAKE_BINARY_DIR}/build.ninja -P
                                                    ${CMAKE_CURRENT_SOURCE_DIR}/write_only/check_command_lines.cmake)
endif()

# The converter's output folder (tests/output_dir): files it no longer writes are removed with --clean-output (what
# svd_convert passes), nothing without it, an unchanged file keeps its mtime, two folders get the same bytes, the json
# generator removes only *.json.
foreach(case clean no_clean keeps_mtime location json_clean)
    add_test(
        NAME cli_output_${case}
        COMMAND
            ${CMAKE_COMMAND} -DCONVERTER=$<TARGET_FILE:svd_converter> -DSVD=${guard_svd}
            -DWORK=${CMAKE_CURRENT_BINARY_DIR}/output_dir/${case} -DCASE=${case} -P
            ${CMAKE_CURRENT_SOURCE_DIR}/output_dir/check.cmake)
endforeach()
set(configure_args -DFETCHCONTENT_FULLY_DISCONNECTED=ON)
foreach(dep cmake_helpers nlohmann_json pugixml inja)
    if(DEFINED ${dep}_SOURCE_DIR)
        string(TOUPPER ${dep} dep_upper)
        list(APPEND configure_args -DFETCHCONTENT_SOURCE_DIR_${dep_upper}=${${dep}_SOURCE_DIR})
    endif()
endforeach()
string(REPLACE ";" "|" configure_args "${configure_args}")
# configure errors of svd_convert (tests/configure_fails.cmake): two calls sharing a folder, a bad WRITE_ONLY_MASK
foreach(case duplicate_output_dir bad_write_only_mask)
    add_test(
        NAME cmake_${case}
        COMMAND
            ${CMAKE_COMMAND} -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/.. -DSVD=${guard_svd}
            -DWORK=${CMAKE_CURRENT_BINARY_DIR}/configure_fails/${case} -DCASE=${case} -DARGS=${configure_args} -P
            ${CMAKE_CURRENT_SOURCE_DIR}/configure_fails.cmake)
endforeach()
# one svd_convert folder, SVD A then SVD B in the same tree (a chip switch): A's headers are gone (tests/output_dir)
add_test(
    NAME cmake_output_dir_switch
    COMMAND
        ${CMAKE_COMMAND} -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/..
        -DSVD_A=${CMAKE_CURRENT_SOURCE_DIR}/golden/features.svd -DSVD_B=${guard_svd}
        -DWORK=${CMAKE_CURRENT_BINARY_DIR}/output_dir/switch -DARGS=${configure_args} -P
        ${CMAKE_CURRENT_SOURCE_DIR}/output_dir/switch.cmake)

# The survey tool over the json generator's output. Without Python these tests are left out locally; under CI (the
# environment variable CI, set by every runner) that is a configure error, so a job without Python cannot pass by
# skipping them.
find_package(Python3 COMPONENTS Interpreter)
if(NOT Python3_FOUND AND DEFINED ENV{CI})
    message(FATAL_ERROR "CI is set but no Python 3 interpreter was found: test_write_only_survey, test_instantiate_all "
                        "and test_regen_diff would be left out")
endif()
if(Python3_FOUND)
    add_test(NAME test_write_only_survey COMMAND ${Python3_EXECUTABLE} -B
                                                 ${CMAKE_CURRENT_SOURCE_DIR}/test_write_only_survey.py)
endif()

# The tools' own tests (tools/instantiate_all.py, tools/regen_diff.py), behind Python like the survey's.
if(Python3_FOUND)
    add_test(NAME test_instantiate_all COMMAND ${Python3_EXECUTABLE} -B
                                               ${CMAKE_CURRENT_SOURCE_DIR}/test_instantiate_all.py)
    add_test(NAME test_regen_diff COMMAND ${Python3_EXECUTABLE} -B ${CMAKE_CURRENT_SOURCE_DIR}/test_regen_diff.py)
endif()

# Golden output: every mode of golden/modes.cmake converts its SVD into a fresh folder and compares it byte for byte
# with the checked-in raw output (pure CMake, no diff binary). An intended generator change regenerates the expected
# files: cmake -DCONVERTER=<bin> -P tests/golden/update.cmake
include(${CMAKE_CURRENT_SOURCE_DIR}/golden/modes.cmake)
foreach(mode ${golden_modes})
    string(REPLACE "|" ";" fields "${mode}")
    list(GET fields 0 name)
    list(GET fields 1 svd)
    list(GET fields 2 generator)
    list(GET fields 3 options)
    get_filename_component(stem ${svd} NAME_WE)
    if(stem STREQUAL "features")
        set(test_name golden_${name})
    else()
        set(test_name golden_${stem}_${name})
    endif()
    add_test(
        NAME ${test_name}
        COMMAND
            ${CMAKE_COMMAND} -DCONVERTER=$<TARGET_FILE:svd_converter> -DSVD=${CMAKE_CURRENT_SOURCE_DIR}/${svd}
            -DGENERATOR=${generator} -DOPTIONS=${options} -DOUT=${CMAKE_CURRENT_BINARY_DIR}/golden_out/${stem}/${name}
            -DEXPECTED=${CMAKE_CURRENT_SOURCE_DIR}/golden/${stem}/${name} -P
            ${CMAKE_CURRENT_SOURCE_DIR}/golden/compare_dir.cmake)
endforeach()
