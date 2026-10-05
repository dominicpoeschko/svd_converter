# cmake -DNINJA_FILE=<build.ninja> -P check_command_lines.cmake: the generator commands svd_convert wrote for the two
# guard targets of tests/CMakeLists.txt carry the write-only options its keywords ask for.
file(READ ${NINJA_FILE} ninja)
if(NOT ninja MATCHES "--write-only-guard=off --write-only-registers=off [^\n]*guard.svd [^\n]*guard_cmake_default json")
    message(FATAL_ERROR "the default svd_convert does not pass --write-only-guard=off --write-only-registers=off")
endif()
if(NOT
   ninja
   MATCHES
   "--write-only-guard=warn --write-only-registers=derived --write-only-mask=on [^\n]*guard.svd [^\n]*guard_cmake_set json"
)
    message(FATAL_ERROR "svd_convert's WRITE_ONLY_* keywords do not reach the command line")
endif()
