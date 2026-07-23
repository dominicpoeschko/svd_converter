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
