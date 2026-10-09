cmake_minimum_required(VERSION 3.28)

if(TARGET svd_converter)
    return()
endif()

include(FetchContent)
FetchContent_Declare(
    cmake_helpers
    GIT_REPOSITORY https://github.com/dominicpoeschko/cmake_helpers.git
    GIT_TAG master)
FetchContent_MakeAvailable(cmake_helpers)

if(NOT CMAKE_CROSSCOMPILING)

    project(
        svd_converter
        VERSION 0.2.0
        DESCRIPTION "SVD to C++ header generator tool for Kvasir register access"
        HOMEPAGE_URL "https://github.com/dominicpoeschko/svd_converter"
        LANGUAGES CXX)

    include(${cmake_helpers_SOURCE_DIR}/BuildOptions.cmake)
    include(${cmake_helpers_SOURCE_DIR}/FindOrFetch.cmake)

    find_or_fetch_package(
        nlohmann_json
        GIT_REPOSITORY
        https://github.com/nlohmann/json
        GIT_TAG
        v3.12.0
        GIT_BRANCH
        master)

    option(PUGIXML_INSTALL "" OFF)
    find_or_fetch_package(
        pugixml
        GIT_REPOSITORY
        https://github.com/zeux/pugixml
        GIT_TAG
        v1.16
        GIT_BRANCH
        master)

    option(INJA_USE_EMBEDDED_JSON "" OFF)
    option(INJA_INSTALL "" OFF)
    option(INJA_EXPORT "" OFF)
    option(INJA_BUILD_TESTS "" OFF)
    option(BUILD_BENCHMARK "" OFF)
    find_or_fetch_package(
        inja
        GIT_REPOSITORY
        https://github.com/pantor/inja
        GIT_TAG
        v3.5.0
        GIT_BRANCH
        main
        PATCH_FILE
        ${CMAKE_CURRENT_LIST_DIR}/inja.patch)

    add_executable(svd_converter src/main.cpp)

    target_include_directories(svd_converter PRIVATE "${CMAKE_CURRENT_LIST_DIR}/src")
    target_add_default_build_options(svd_converter PRIVATE)

    target_link_libraries(svd_converter PRIVATE nlohmann_json::nlohmann_json pantor::inja pugixml)
else()
    include(${cmake_helpers_SOURCE_DIR}/HostBuild.cmake)
    configure_host_build(svd_converter)
endif()

find_program(CLANG_FORMAT clang-format)
mark_as_advanced(FORCE CLANG_FORMAT)

# The write-only guard. A cache value overrides every svd_convert's keyword in this tree; empty = the keyword, else the
# default below.
set(KVASIR_SVD_WRITE_ONLY_GUARD
    ""
    CACHE STRING "override every svd_convert's WRITE_ONLY_GUARD (off|warn|error)")
set(KVASIR_SVD_WRITE_ONLY_REGISTERS
    ""
    CACHE STRING "override every svd_convert's WRITE_ONLY_REGISTERS (derived|off)")
set(KVASIR_SVD_WRITE_ONLY_MASK
    ""
    CACHE STRING "override every svd_convert's WRITE_ONLY_MASK (ON|OFF)")
# Named compile errors: every register's Address names its register struct, so every register diagnostic names its
# fields - at a compile-time cost, hence off.
set(KVASIR_SVD_NAMED_REGISTERS
    ""
    CACHE STRING "override every svd_convert's NAMED_REGISTERS (ON|OFF)")

# svd_convert(<target> SVD_FILE <svd> OUTPUT_DIRECTORY <dir> [GENERATOR kvasir_bit|json|custom_template] [GENERATOR_ARGS
# <template path> <extension>] [WRITE_ONLY_GUARD off|warn|error] [WRITE_ONLY_REGISTERS derived|off] [WRITE_ONLY_MASK
# ON|OFF] [NAMED_REGISTERS ON|OFF])
#
# WRITE_ONLY_GUARD: report write-only fields a partial write would read back and rewrite - `warn` prints them, `error`
# also fails the generator step. Default here `off`, not the converter's `warn`: the converter runs inside firmware
# builds, whose output must stay silent; a chip turns it on once its SVD is classified. WRITE_ONLY_REGISTERS derived: a
# register with no readable field is whole-register-only (default off). WRITE_ONLY_MASK ON: the template's
# `writeOnlyMask` switch (default OFF). NAMED_REGISTERS ON: every register's Address carries Register::Named<itself> for
# compile errors (default OFF; registers with an RmwHazard name themselves always).
function(svd_convert targetname)

    cmake_parse_arguments(
        PARSE_ARGV 1 PARSED_ARGS ""
        "SVD_FILE;OUTPUT_DIRECTORY;GENERATOR;WRITE_ONLY_GUARD;WRITE_ONLY_REGISTERS;WRITE_ONLY_MASK;NAMED_REGISTERS"
        "GENERATOR_ARGS")

    if(PARSED_ARGS_UNPARSED_ARGUMENTS)
        message(FATAL_ERROR "svd_convert: Unknown arguments: ${PARSED_ARGS_UNPARSED_ARGUMENTS}")
    endif()

    if(NOT PARSED_ARGS_SVD_FILE)
        message(FATAL_ERROR "svd_convert: SVD_FILE is required")
    endif()

    if(NOT PARSED_ARGS_OUTPUT_DIRECTORY)
        message(FATAL_ERROR "svd_convert: OUTPUT_DIRECTORY is required")
    endif()

    if(NOT PARSED_ARGS_GENERATOR)
        set(PARSED_ARGS_GENERATOR "kvasir_bit")
    endif()

    set(iname ${PARSED_ARGS_SVD_FILE})
    set(incname ${CMAKE_CURRENT_BINARY_DIR}/svd_converter_generated)
    set(dname ${incname}/${PARSED_ARGS_OUTPUT_DIRECTORY})
    set(oname ${dname}/${targetname}_dependency_dummy)

    get_target_property(build_dir svd_converter BINARY_DIR)
    if(NOT build_dir)
        message(FATAL_ERROR "svd_convert: Could not determine svd_converter binary directory")
    endif()

    if(NOT CMAKE_CROSSCOMPILING)
        set(svd_converter_bin ${build_dir}/svd_converter)
    else()
        get_target_property(svd_converter_bin svd_converter HOST_EXECUTABLE_PATH)
    endif()

    if(${PARSED_ARGS_GENERATOR} STREQUAL "kvasir_bit")
        file(GLOB_RECURSE outfiles ${dname}/*.hpp)
    elseif(${PARSED_ARGS_GENERATOR} STREQUAL "json")
        file(GLOB_RECURSE outfiles ${dname}/*.json)
    elseif(${PARSED_ARGS_GENERATOR} STREQUAL "custom_template")
        list(LENGTH PARSED_ARGS_GENERATOR_ARGS arg_size)
        if(NOT ${arg_size} EQUAL 2)
            message(
                FATAL_ERROR "custom_template generator needs GENERATOR_ARGS \"template_path\" and \"file_extension\"")
        endif()
        list(GET PARSED_ARGS_GENERATOR_ARGS 0 template_path)
        list(GET PARSED_ARGS_GENERATOR_ARGS 1 file_extension)
        file(GLOB_RECURSE outfiles ${dname}/*.${file_extension})
        string(PREPEND template_path "${CMAKE_CURRENT_LIST_DIR}/")
        file(GLOB_RECURSE template_files ${template_path}/*.inja)
    else()
        message(
            FATAL_ERROR
                "svd_convert: Invalid generator '${PARSED_ARGS_GENERATOR}'. Valid options: kvasir_bit, json, custom_template"
        )
    endif()

    # a non-empty cache value wins - compared as a string: `if(<var>)` would take "off"/"OFF" for "not set"
    foreach(option GUARD REGISTERS MASK)
        if(NOT "${KVASIR_SVD_WRITE_ONLY_${option}}" STREQUAL "")
            set(PARSED_ARGS_WRITE_ONLY_${option} ${KVASIR_SVD_WRITE_ONLY_${option}})
        endif()
    endforeach()
    if(NOT PARSED_ARGS_WRITE_ONLY_GUARD)
        set(PARSED_ARGS_WRITE_ONLY_GUARD off)
    endif()
    if(NOT PARSED_ARGS_WRITE_ONLY_REGISTERS)
        set(PARSED_ARGS_WRITE_ONLY_REGISTERS off)
    endif()
    if(NOT PARSED_ARGS_WRITE_ONLY_GUARD MATCHES "^(off|warn|error)$")
        message(
            FATAL_ERROR
                "svd_convert: WRITE_ONLY_GUARD must be off, warn or error, not '${PARSED_ARGS_WRITE_ONLY_GUARD}'")
    endif()
    if(NOT PARSED_ARGS_WRITE_ONLY_REGISTERS MATCHES "^(derived|off)$")
        message(
            FATAL_ERROR
                "svd_convert: WRITE_ONLY_REGISTERS must be derived or off, not '${PARSED_ARGS_WRITE_ONLY_REGISTERS}'")
    endif()
    set(write_only_options --write-only-guard=${PARSED_ARGS_WRITE_ONLY_GUARD}
                           --write-only-registers=${PARSED_ARGS_WRITE_ONLY_REGISTERS})
    if(PARSED_ARGS_WRITE_ONLY_MASK)
        list(APPEND write_only_options --write-only-mask=on)
    endif()
    if(NOT "${KVASIR_SVD_NAMED_REGISTERS}" STREQUAL "")
        set(PARSED_ARGS_NAMED_REGISTERS ${KVASIR_SVD_NAMED_REGISTERS})
    endif()
    if(PARSED_ARGS_NAMED_REGISTERS)
        list(APPEND write_only_options --named-registers=on)
    endif()

    # the options are on the command line: a changed option re-runs the generator
    set(command
        ${svd_converter_bin}
        ${write_only_options}
        ${iname}
        ${dname}
        ${PARSED_ARGS_GENERATOR}
        ${template_path}
        ${file_extension})

    if(NOT EXISTS ${CLANG_FORMAT} OR NOT ${PARSED_ARGS_GENERATOR} STREQUAL "kvasir_bit")
        add_custom_command(
            COMMAND ${CMAKE_COMMAND} -E make_directory ${dname}
            COMMAND ${command}
            COMMAND ${CMAKE_COMMAND} -E touch ${oname}
            DEPENDS svd_converter ${iname} ${svd_converter_bin} ${template_files}
            OUTPUT ${oname} ${outfiles}
            COMMENT "Converting ${iname} to ${PARSED_ARGS_GENERATOR} format: ${targetname}")
    else()
        add_custom_command(
            COMMAND ${CMAKE_COMMAND} -E make_directory ${dname}
            COMMAND ${command}
            COMMAND ${CLANG_FORMAT} --Wno-error=unknown -i ${dname}/*.hpp || true
            COMMAND ${CMAKE_COMMAND} -E touch ${oname}
            DEPENDS svd_converter ${iname} ${svd_converter_bin} ${template_files}
            OUTPUT ${oname} ${outfiles}
            COMMENT "Converting and formatting ${iname} to ${PARSED_ARGS_GENERATOR}: ${targetname}")
    endif()

    if(${CMAKE_VERSION} VERSION_LESS "3.19.0")
        set_source_files_properties(${oname} PROPERTIES GENERATED TRUE)
        add_custom_target(${targetname}_helper DEPENDS ${oname})
        add_library(${targetname} INTERFACE)
        add_dependencies(${targetname} ${targetname}_helper)
    else()
        add_library(${targetname} INTERFACE ${oname})
    endif()

    target_include_directories(${targetname} INTERFACE ${incname})

    if(${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.19.0")
        set_property(
            TARGET ${targetname}
            APPEND
            PROPERTY ADDITIONAL_CLEAN_FILES ${outfiles})
    endif()

endfunction()
