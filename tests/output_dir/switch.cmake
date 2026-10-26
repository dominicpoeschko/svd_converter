# cmake -DSOURCE_DIR=<svd_converter> -DSVD_A=<svd> -DSVD_B=<svd> -DWORK=<dir> [-DARGS=a|b] -P switch.cmake: one
# svd_convert OUTPUT_DIRECTORY, configured and built with SVD A, then re-configured in the same tree with SVD B and
# built again (a chip or part switch in a warm tree). The changed SVD path re-runs the step, and --clean-output removes
# every header only A has before anything could compile against it. ARGS: the cache values that make the configure
# offline (dependency sources). A = golden/features.svd, B = write_only/guard.svd.
file(REMOVE_RECURSE ${WORK})
file(MAKE_DIRECTORY ${WORK}/project)
file(
    WRITE ${WORK}/project/CMakeLists.txt
    "cmake_minimum_required(VERSION 3.28)
project(svd_convert_switch LANGUAGES CXX)
add_subdirectory(${SOURCE_DIR} svd_converter)
set(CHIP_SVD \"\" CACHE FILEPATH \"the SVD to convert\")
svd_convert(chip_peripherals SVD_FILE \${CHIP_SVD} OUTPUT_DIRECTORY peripherals WRITE_ONLY_GUARD warn)
")
string(REPLACE "|" ";" args "${ARGS}")
set(out ${WORK}/build/svd_converter_generated/peripherals)

function(configure_and_build svd)
    execute_process(
        COMMAND ${CMAKE_COMMAND} -S ${WORK}/project -B ${WORK}/build ${args} -DCHIP_SVD=${svd}
        RESULT_VARIABLE result
        OUTPUT_VARIABLE log
        ERROR_VARIABLE log)
    if(NOT result EQUAL 0)
        message(FATAL_ERROR "configure with ${svd} failed:\n${log}")
    endif()
    execute_process(
        COMMAND ${CMAKE_COMMAND} --build ${WORK}/build --target chip_peripherals
        RESULT_VARIABLE result
        OUTPUT_VARIABLE log
        ERROR_VARIABLE log)
    if(NOT result EQUAL 0)
        message(FATAL_ERROR "build with ${svd} failed:\n${log}")
    endif()
endfunction()

function(expect state)
    foreach(f ${ARGN})
        if(state STREQUAL "present" AND NOT EXISTS ${out}/${f})
            message(FATAL_ERROR "${f} is missing")
        elseif(state STREQUAL "gone" AND EXISTS ${out}/${f})
            message(FATAL_ERROR "${f} of the first SVD is still there after the switch")
        endif()
    endforeach()
endfunction()

configure_and_build(${SVD_A})
expect(present CTRLP.hpp SERCOM_SPI.hpp InterruptNames.hpp Interrupts.hpp TC.hpp)
expect(gone EVSYS.hpp RTC.hpp)
configure_and_build(${SVD_B})
expect(
    gone
    CTRLP.hpp
    SERCOM_SPI.hpp
    SERCOM_UART.hpp
    SPARE.hpp
    InterruptNames.hpp
    Interrupts.hpp)
expect(present EVSYS.hpp RTC.hpp TC.hpp)
