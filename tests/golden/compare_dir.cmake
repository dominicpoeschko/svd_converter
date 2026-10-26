# cmake -DCONVERTER=<bin> -DSVD=<svd> -DGENERATOR=<gen> [-DOPTIONS=a,b] -DOUT=<dir> -DEXPECTED=<dir> -P
# compare_dir.cmake
#
# Converts SVD into a fresh OUT (exit 0 expected) and compares it with EXPECTED: the same file names, every file byte
# for byte (cmake -E compare_files; no diff binary needed). On a mismatch it names the first differing file and the
# command that regenerates the expected output.
string(REPLACE "," ";" options "${OPTIONS}")
file(REMOVE_RECURSE ${OUT})
file(MAKE_DIRECTORY ${OUT})
execute_process(
    COMMAND ${CONVERTER} ${options} ${SVD} ${OUT} ${GENERATOR}
    RESULT_VARIABLE result
    OUTPUT_VARIABLE out
    ERROR_VARIABLE err)
if(NOT result EQUAL 0)
    message(FATAL_ERROR "converter exit ${result}:\n${out}${err}")
endif()
set(update "cmake -DCONVERTER=${CONVERTER} -P ${CMAKE_CURRENT_LIST_DIR}/update.cmake")
file(
    GLOB expected_files
    RELATIVE ${EXPECTED}
    ${EXPECTED}/*)
file(
    GLOB out_files
    RELATIVE ${OUT}
    ${OUT}/*)
list(SORT expected_files)
list(SORT out_files)
if(NOT expected_files STREQUAL out_files)
    message(FATAL_ERROR "file lists differ\n  expected: ${expected_files}\n  generated: ${out_files}\n"
                        "If the change is intended: ${update}")
endif()
foreach(f ${expected_files})
    execute_process(COMMAND ${CMAKE_COMMAND} -E compare_files ${EXPECTED}/${f} ${OUT}/${f} RESULT_VARIABLE differs)
    if(differs)
        message(FATAL_ERROR "${f} differs from the expected ${EXPECTED}/${f} (generated: ${OUT}/${f})\n"
                            "If the change is intended: ${update}")
    endif()
endforeach()
list(LENGTH expected_files n)
message(STATUS "${n} file(s) identical")
