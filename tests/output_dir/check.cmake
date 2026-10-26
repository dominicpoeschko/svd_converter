# cmake -DCONVERTER=<bin> -DSVD=<svd> -DWORK=<dir> -DCASE=<case> -P check.cmake: the converter's output folder. clean
# --clean-output removes stale *.hpp and leftover *.tmp, keeps other files, the dummy and subfolders no_clean without it
# nothing is removed keeps_mtime      an unchanged file is not rewritten (same mtime), a changed one is location two
# folders get the same bytes json_clean       the json generator removes stale *.json only
file(REMOVE_RECURSE ${WORK})
file(MAKE_DIRECTORY ${WORK}/out/sub)

function(convert dir generator)
    execute_process(
        COMMAND ${CONVERTER} --write-only-guard=off ${ARGN} ${SVD} ${dir} ${generator}
        RESULT_VARIABLE result
        ERROR_VARIABLE err
        OUTPUT_QUIET)
    if(NOT result EQUAL 0)
        message(FATAL_ERROR "converter exit ${result}: ${err}")
    endif()
endfunction()

function(expect_exists)
    foreach(f ${ARGN})
        if(NOT EXISTS ${WORK}/out/${f})
            message(FATAL_ERROR "${f} is missing")
        endif()
    endforeach()
endfunction()

function(expect_gone)
    foreach(f ${ARGN})
        if(EXISTS ${WORK}/out/${f})
            message(FATAL_ERROR "${f} is still there")
        endif()
    endforeach()
endfunction()

function(seed)
    foreach(f ${ARGN})
        file(WRITE ${WORK}/out/${f} "seed\n")
    endforeach()
endfunction()

if(CASE STREQUAL "clean")
    seed(STALE.hpp keep.txt x_dependency_dummy sub/A.hpp A.hpp.tmp)
    convert(${WORK}/out kvasir_bit --clean-output)
    expect_exists(TC.hpp RTC.hpp keep.txt x_dependency_dummy sub/A.hpp)
    expect_gone(STALE.hpp A.hpp.tmp)
elseif(CASE STREQUAL "no_clean")
    seed(STALE.hpp)
    convert(${WORK}/out kvasir_bit)
    expect_exists(TC.hpp STALE.hpp)
elseif(CASE STREQUAL "keeps_mtime")
    convert(${WORK}/out kvasir_bit --clean-output)
    file(TIMESTAMP ${WORK}/out/TC.hpp before "%s")
    file(TIMESTAMP ${WORK}/out/RTC.hpp rtc_before "%s")
    execute_process(COMMAND ${CMAKE_COMMAND} -E sleep 1.1)
    file(APPEND ${WORK}/out/RTC.hpp "changed by hand\n")
    file(TIMESTAMP ${WORK}/out/RTC.hpp rtc_touched "%s")
    execute_process(COMMAND ${CMAKE_COMMAND} -E sleep 1.1)
    convert(${WORK}/out kvasir_bit --clean-output)
    file(TIMESTAMP ${WORK}/out/TC.hpp after "%s")
    file(TIMESTAMP ${WORK}/out/RTC.hpp rtc_after "%s")
    if(NOT before STREQUAL after)
        message(FATAL_ERROR "an unchanged TC.hpp was rewritten (${before} -> ${after})")
    endif()
    if(rtc_after STREQUAL rtc_touched)
        message(FATAL_ERROR "a changed RTC.hpp was not rewritten")
    endif()
    file(READ ${WORK}/out/RTC.hpp rtc)
    if(rtc MATCHES "changed by hand")
        message(FATAL_ERROR "RTC.hpp kept the hand edit")
    endif()
elseif(CASE STREQUAL "location")
    convert(${WORK}/a kvasir_bit --clean-output)
    convert(${WORK}/x/y/b kvasir_bit --clean-output)
    file(
        GLOB files
        RELATIVE ${WORK}/a
        ${WORK}/a/*)
    file(
        GLOB files_b
        RELATIVE ${WORK}/x/y/b
        ${WORK}/x/y/b/*)
    if(NOT files STREQUAL files_b OR NOT files)
        message(FATAL_ERROR "file lists differ: ${files} / ${files_b}")
    endif()
    foreach(f ${files})
        execute_process(COMMAND ${CMAKE_COMMAND} -E compare_files ${WORK}/a/${f} ${WORK}/x/y/b/${f}
                        RESULT_VARIABLE differs)
        if(differs)
            message(FATAL_ERROR "${f} differs between the two folders")
        endif()
    endforeach()
elseif(CASE STREQUAL "json_clean")
    seed(STALE.json STALE.hpp)
    convert(${WORK}/out json --clean-output)
    expect_exists(TC.json STALE.hpp)
    expect_gone(STALE.json)
else()
    message(FATAL_ERROR "unknown CASE ${CASE}")
endif()
