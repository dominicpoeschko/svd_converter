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
    target_include_directories(${name} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src)
    target_link_libraries(${name} PRIVATE nlohmann_json::nlohmann_json pantor::inja pugixml)
    target_add_default_build_options(${name} PRIVATE)
endfunction()

svd_converter_add_test(test_svd_parser svd_parser_tests.cpp)
add_test(NAME test_svd_parser COMMAND test_svd_parser)

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
        file(DOWNLOAD https://raw.githubusercontent.com/kvasir-io/chip_rp2350/master/chip.svd ${test_svd_file}
             STATUS download_status)
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
# files against its in-process parse result.
add_test(NAME e2e_make_directories
         COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/generated_json
                 ${CMAKE_CURRENT_BINARY_DIR}/generated_kvasir)
set_tests_properties(e2e_make_directories PROPERTIES FIXTURES_SETUP e2e_directories)

add_test(NAME e2e_generate_json COMMAND svd_converter ${test_svd_file} ${CMAKE_CURRENT_BINARY_DIR}/generated_json json)
set_tests_properties(e2e_generate_json PROPERTIES FIXTURES_REQUIRED e2e_directories FIXTURES_SETUP e2e_json)

add_test(NAME e2e_generate_kvasir_bit COMMAND svd_converter ${test_svd_file}
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
    set(defines -DCONVERTER=$<TARGET_FILE:svd_converter> -DSVD=${guard_svd} -DOUT=${guard_out}
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
svd_converter_guard_test(
    cli_write_only_guard_default 0 EXPECT
    "TC.CTRLA.SWRST \\(bit 0\\) is write-only.*EVSYS.SWEVT.CHANNEL0.*3 write-only field\\(s\\) in 2 register\\(s\\)"
    REJECT "RTC")
svd_converter_guard_test(cli_write_only_guard_error 2 OPTIONS --write-only-guard=error EXPECT "TC.CTRLA.SWRST")
svd_converter_guard_test(cli_write_only_guard_off 0 OPTIONS --write-only-guard=off REJECT "write-only")
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

# svd_convert's keywords on the generator's command line (the targets are never built: nothing consumes them).
svd_convert(
    guard_cmake_default
    SVD_FILE
    ${guard_svd}
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
if(CMAKE_GENERATOR MATCHES "Ninja")
    add_test(NAME cmake_write_only_keywords COMMAND ${CMAKE_COMMAND} -DNINJA_FILE=${CMAKE_BINARY_DIR}/build.ninja -P
                                                    ${CMAKE_CURRENT_SOURCE_DIR}/write_only/check_command_lines.cmake)
endif()

# The survey tool over the json generator's output.
find_package(Python3 COMPONENTS Interpreter)
if(Python3_FOUND)
    add_test(NAME test_write_only_survey COMMAND ${Python3_EXECUTABLE} -B
                                                 ${CMAKE_CURRENT_SOURCE_DIR}/test_write_only_survey.py)
endif()
