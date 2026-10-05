# cmake -DCONVERTER=bin -DSVD=file -DOUT=dir [-DOPTIONS=opt1,opt2] -DEXPECT_EXIT=n [-DEXPECT_STDERR=regex]
# [-DREJECT_STDERR=regex] -P expect_exit.cmake Runs the converter (json generator) and checks its exit code and stderr
# (the write-only guard tests).
string(REPLACE "," ";" options "${OPTIONS}")
execute_process(
    COMMAND ${CONVERTER} ${options} ${SVD} ${OUT} json
    RESULT_VARIABLE result
    ERROR_VARIABLE err
    OUTPUT_QUIET)
if(NOT result STREQUAL EXPECT_EXIT)
    message(FATAL_ERROR "exit ${result}, expected ${EXPECT_EXIT}; stderr:\n${err}")
endif()
if(DEFINED EXPECT_STDERR AND NOT err MATCHES "${EXPECT_STDERR}")
    message(FATAL_ERROR "stderr does not match '${EXPECT_STDERR}':\n${err}")
endif()
if(DEFINED REJECT_STDERR AND err MATCHES "${REJECT_STDERR}")
    message(FATAL_ERROR "stderr matches '${REJECT_STDERR}':\n${err}")
endif()
