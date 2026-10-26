# cmake -DCONVERTER=<svd_converter binary> -P tests/golden/update.cmake
#
# Regenerates every golden mode's expected output (modes.cmake) in the source tree. Run by hand after an intended
# generator change, never by ctest; the commit then shows the generated diff.
if(NOT CONVERTER)
    message(FATAL_ERROR "usage: cmake -DCONVERTER=<svd_converter binary> -P ${CMAKE_CURRENT_LIST_FILE}")
endif()
include(${CMAKE_CURRENT_LIST_DIR}/modes.cmake)
get_filename_component(tests_dir ${CMAKE_CURRENT_LIST_DIR} DIRECTORY)
foreach(mode ${golden_modes})
    string(REPLACE "|" ";" fields "${mode}")
    list(GET fields 0 name)
    list(GET fields 1 svd)
    list(GET fields 2 generator)
    list(GET fields 3 options)
    string(REPLACE "," ";" options "${options}")
    get_filename_component(stem ${svd} NAME_WE)
    set(dir ${CMAKE_CURRENT_LIST_DIR}/${stem}/${name})
    file(REMOVE_RECURSE ${dir})
    file(MAKE_DIRECTORY ${dir})
    execute_process(
        COMMAND ${CONVERTER} ${options} ${tests_dir}/${svd} ${dir} ${generator}
        RESULT_VARIABLE result
        ERROR_VARIABLE err
        OUTPUT_QUIET)
    if(NOT result EQUAL 0)
        message(FATAL_ERROR "${name} (${svd}): converter exit ${result}\n${err}")
    endif()
    message(STATUS "${stem}/${name} regenerated")
endforeach()
