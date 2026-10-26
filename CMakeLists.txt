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

    # The SVD reader as passes (src/svd/): read -> analyze -> group, rename; the converter and its tests link it.
    add_library(svd_converter_lib STATIC src/svd/chip.cpp src/svd/interrupts.cpp src/svd/naming.cpp src/svd/read.cpp
                                         src/svd/semantics.cpp)
    target_include_directories(svd_converter_lib PUBLIC "${CMAKE_CURRENT_LIST_DIR}/src")
    target_add_default_build_options(svd_converter_lib PRIVATE)
    target_link_libraries(svd_converter_lib PUBLIC nlohmann_json::nlohmann_json pantor::inja pugixml)

    add_executable(svd_converter src/main.cpp)

    target_add_default_build_options(svd_converter PRIVATE)

    target_link_libraries(svd_converter PRIVATE svd_converter_lib)
else()
    include(${cmake_helpers_SOURCE_DIR}/HostBuild.cmake)
    configure_host_build(svd_converter)
endif()

# Named compile errors: every register's Address names its register struct, so every register diagnostic names its
# fields - at a compile-time cost, hence off.
set(KVASIR_SVD_NAMED_REGISTERS
    ""
    CACHE STRING "override every svd_convert's NAMED_REGISTERS (ON|OFF)")

# svd_convert(<target> SVD_FILE <svd> OUTPUT_DIRECTORY <dir> [GENERATOR kvasir_bit|json] [WRITE_ONLY_GUARD
# off|warn|error] [WRITE_ONLY_REGISTERS derived|off] [WRITE_ONLY_MASK ON|OFF] [NAMED_REGISTERS ON|OFF])
#
# The defaults are Kvasir's write semantics, the same as the converter's own: WRITE_ONLY_GUARD error - every write-only
# field of the SVD is classified (<modifiedWriteValues>oneToSet, a one-value <writeConstraint> key, or <!-- Kvasir:
# write-only accepted -->), so a new unclassified one stops the build (`warn` only prints it); WRITE_ONLY_REGISTERS
# derived - a register with no readable field is whole-register-only, never read; WRITE_ONLY_MASK ON - the template's
# `writeOnlyMask`, so a read-modify-write that would rewrite a write-only field does not compile. An SVD that is not
# classified yet opts out: WRITE_ONLY_GUARD off WRITE_ONLY_REGISTERS off WRITE_ONLY_MASK OFF. A call that weakens the
# defaults is listed in the global property SVD_CONVERT_WEAKENED (`<target>: guard=... registers=... mask=...`), which a
# conformance tree requires empty. NAMED_REGISTERS ON: every register's Address carries Register::Mode<..., itself> for
# compile errors (default OFF; registers with a key, a read side effect or write-only bits name themselves always).
function(svd_convert targetname)

    cmake_parse_arguments(
        PARSE_ARGV 1 PARSED_ARGS ""
        "SVD_FILE;OUTPUT_DIRECTORY;GENERATOR;WRITE_ONLY_GUARD;WRITE_ONLY_REGISTERS;WRITE_ONLY_MASK;NAMED_REGISTERS" "")

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

    if(NOT PARSED_ARGS_GENERATOR MATCHES "^(kvasir_bit|json)$")
        message(
            FATAL_ERROR "svd_convert: Invalid generator '${PARSED_ARGS_GENERATOR}'. Valid options: kvasir_bit, json")
    endif()

    # The folder belongs to this call: the converter removes what it no longer writes (--clean-output), so two calls
    # sharing one would delete each other's files.
    get_property(used_dirs GLOBAL PROPERTY SVD_CONVERT_OUTPUT_DIRS)
    if(dname IN_LIST used_dirs)
        message(FATAL_ERROR "svd_convert: ${dname} is already the output of another svd_convert call")
    endif()
    set_property(GLOBAL APPEND PROPERTY SVD_CONVERT_OUTPUT_DIRS ${dname})

    # compared as strings: `if(<var>)` takes an explicit "off"/"OFF" for "not given"
    if("${PARSED_ARGS_WRITE_ONLY_GUARD}" STREQUAL "")
        set(PARSED_ARGS_WRITE_ONLY_GUARD error)
    endif()
    if("${PARSED_ARGS_WRITE_ONLY_REGISTERS}" STREQUAL "")
        set(PARSED_ARGS_WRITE_ONLY_REGISTERS derived)
    endif()
    if("${PARSED_ARGS_WRITE_ONLY_MASK}" STREQUAL "")
        set(PARSED_ARGS_WRITE_ONLY_MASK ON)
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
    if(NOT PARSED_ARGS_WRITE_ONLY_MASK MATCHES "^(ON|OFF|on|off)$")
        message(FATAL_ERROR "svd_convert: WRITE_ONLY_MASK must be ON or OFF, not '${PARSED_ARGS_WRITE_ONLY_MASK}'")
    endif()
    string(TOLOWER ${PARSED_ARGS_WRITE_ONLY_MASK} write_only_mask)
    set(write_only_options
        --write-only-guard=${PARSED_ARGS_WRITE_ONLY_GUARD} --write-only-registers=${PARSED_ARGS_WRITE_ONLY_REGISTERS}
        --write-only-mask=${write_only_mask})
    if(NOT "${PARSED_ARGS_WRITE_ONLY_GUARD}/${PARSED_ARGS_WRITE_ONLY_REGISTERS}/${write_only_mask}" STREQUAL
       "error/derived/on")
        set_property(
            GLOBAL APPEND
            PROPERTY
                SVD_CONVERT_WEAKENED
                "${targetname}: guard=${PARSED_ARGS_WRITE_ONLY_GUARD} registers=${PARSED_ARGS_WRITE_ONLY_REGISTERS} mask=${write_only_mask}"
        )
    endif()
    if(NOT "${KVASIR_SVD_NAMED_REGISTERS}" STREQUAL "")
        set(PARSED_ARGS_NAMED_REGISTERS ${KVASIR_SVD_NAMED_REGISTERS})
    endif()
    if(PARSED_ARGS_NAMED_REGISTERS)
        list(APPEND write_only_options --named-registers=on)
    endif()

    # the options are on the command line: a changed option re-runs the generator
    set(command ${svd_converter_bin} ${write_only_options} --clean-output ${iname} ${dname} ${PARSED_ARGS_GENERATOR})

    # The step's one declared output is the dummy: a header listed as an output that the converter stops writing would
    # make ninja re-run the step on every build. The converter writes only changed files (an unchanged header keeps its
    # mtime) and its own text, unformatted: the same bytes on every machine and in every folder.
    add_custom_command(
        COMMAND ${CMAKE_COMMAND} -E make_directory ${dname}
        COMMAND ${command}
        COMMAND ${CMAKE_COMMAND} -E touch ${oname}
        DEPENDS svd_converter ${iname} ${svd_converter_bin}
        OUTPUT ${oname}
        COMMENT "Converting ${iname} to ${PARSED_ARGS_GENERATOR}: ${targetname}")

    add_library(${targetname} INTERFACE ${oname})

    target_include_directories(${targetname} INTERFACE ${incname})

    set_property(
        TARGET ${targetname}
        APPEND
        PROPERTY ADDITIONAL_CLEAN_FILES ${dname})

endfunction()
