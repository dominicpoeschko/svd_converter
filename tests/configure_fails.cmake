# cmake -DSOURCE_DIR=<svd_converter> -DSVD=<svd> -DWORK=<dir> -DCASE=<case> [-DARGS=a|b] -P configure_fails.cmake: a
# small project whose svd_convert calls must fail at configure time, with svd_convert's message. ARGS: the cache values
# that make its configure offline (dependency sources). Cases: duplicate_output_dir  two calls with one OUTPUT_DIRECTORY
# (the converter cleans its folder: they would delete each other's files) bad_write_only_mask   WRITE_ONLY_MASK maybe
file(REMOVE_RECURSE ${WORK})
file(MAKE_DIRECTORY ${WORK}/project)
if(CASE STREQUAL "duplicate_output_dir")
    set(calls "svd_convert(first SVD_FILE ${SVD} OUTPUT_DIRECTORY same GENERATOR json)
svd_convert(second SVD_FILE ${SVD} OUTPUT_DIRECTORY same GENERATOR json)")
    set(expected "is already the output of another svd_convert call")
elseif(CASE STREQUAL "bad_write_only_mask")
    set(calls "svd_convert(mask SVD_FILE ${SVD} OUTPUT_DIRECTORY mask GENERATOR json WRITE_ONLY_MASK maybe)")
    set(expected "WRITE_ONLY_MASK must be ON or OFF, not 'maybe'")
else()
    message(FATAL_ERROR "configure_fails.cmake: unknown CASE '${CASE}'")
endif()
file(
    WRITE ${WORK}/project/CMakeLists.txt
    "cmake_minimum_required(VERSION 3.28)
project(svd_convert_${CASE} LANGUAGES CXX)
add_subdirectory(${SOURCE_DIR} svd_converter)
${calls}
")
string(REPLACE "|" ";" args "${ARGS}")
execute_process(
    COMMAND ${CMAKE_COMMAND} -S ${WORK}/project -B ${WORK}/build ${args}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE out
    ERROR_VARIABLE err)
if(result EQUAL 0)
    message(FATAL_ERROR "the configure passed")
endif()
if(NOT err MATCHES "${expected}")
    message(FATAL_ERROR "the configure failed for another reason:\n${out}${err}")
endif()
