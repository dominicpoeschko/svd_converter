cmake_minimum_required(VERSION 3.28)

if(TARGET svd_converter)
    return()
endif()

if(NOT CMAKE_CROSSCOMPILING)

    project(
        svd_converter
        VERSION 0.2.0
        DESCRIPTION "SVD to C++ header generator tool for Kvasir register access"
        HOMEPAGE_URL "https://github.com/dominicpoeschko/svd_converter"
        LANGUAGES CXX)

    include(cmake_helpers/BuildOptions.cmake)
    include(cmake_helpers/FindOrFetch.cmake)

    option(FMT_INSTALL "" OFF)
    find_or_fetch_package(fmt GIT_REPOSITORY https://github.com/fmtlib/fmt.git GIT_TAG master)

    find_or_fetch_package(nlohmann_json GIT_REPOSITORY https://github.com/nlohmann/json GIT_TAG master)

    option(PUGIXML_INSTALL "" OFF)
    find_or_fetch_package(pugixml GIT_REPOSITORY https://github.com/zeux/pugixml GIT_TAG master)

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
        main
        PATCH_COMMAND
        git
        apply
        ${CMAKE_CURRENT_LIST_DIR}/inja.patch)

    add_executable(svd_converter src/main.cpp)

    target_include_directories(svd_converter PRIVATE "${CMAKE_CURRENT_LIST_DIR}/src")
    target_add_default_build_options(svd_converter PRIVATE)

    target_link_libraries(svd_converter PRIVATE nlohmann_json::nlohmann_json pantor::inja pugixml fmt::fmt-header-only)
else()
    include(cmake_helpers/HostBuild.cmake)
    configure_host_build(svd_converter)
endif()

find_program(CLANG_FORMAT clang-format)
mark_as_advanced(FORCE CLANG_FORMAT)

function(svd_convert targetname)

    cmake_parse_arguments(PARSE_ARGV 1 PARSED_ARGS "" "SVD_FILE;OUTPUT_DIRECTORY;GENERATOR" "GENERATOR_ARGS")

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
        set(svd_converter_bin ${build_dir}/host_build/svd_converter)
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

    set(command ${svd_converter_bin} ${iname} ${dname} ${PARSED_ARGS_GENERATOR} ${template_path} ${file_extension})

    if(NOT EXISTS ${CLANG_FORMAT} OR NOT ${PARSED_ARGS_GENERATOR} STREQUAL "kvasir_bit")
        add_custom_command(
            COMMAND ${CMAKE_COMMAND} -E make_directory ${dname}
            COMMAND ${command}
            COMMAND ${CMAKE_COMMAND} -E touch ${oname}
            DEPENDS svd_converter ${iname} ${svd_converter_bin} ${template_files}
            OUTPUT ${oname} ${outfiles})
    else()
        add_custom_command(
            COMMAND ${CMAKE_COMMAND} -E make_directory ${dname}
            COMMAND ${command}
            COMMAND ${CLANG_FORMAT} --Wno-error=unknown -i ${dname}/*.hpp || true
            COMMAND ${CMAKE_COMMAND} -E touch ${oname}
            DEPENDS svd_converter ${iname} ${svd_converter_bin} ${template_files}
            OUTPUT ${oname} ${outfiles})
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
