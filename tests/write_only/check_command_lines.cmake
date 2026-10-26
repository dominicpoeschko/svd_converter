# cmake -DNINJA_FILE=<build.ninja> -P check_command_lines.cmake: the generator commands svd_convert wrote for the three
# guard targets of tests/CMakeLists.txt carry the write-only options its keywords (or its defaults) ask for.
file(READ ${NINJA_FILE} ninja)
if(NOT
   ninja
   MATCHES
   "--write-only-guard=error --write-only-registers=derived --write-only-mask=on [^\n]*features.svd [^\n]*guard_cmake_default json"
)
    message(FATAL_ERROR "the default svd_convert does not pass the defaults error/derived/on")
endif()
if(NOT
   ninja
   MATCHES
   "--write-only-guard=warn --write-only-registers=derived --write-only-mask=on [^\n]*guard.svd [^\n]*guard_cmake_set json"
)
    message(FATAL_ERROR "svd_convert's WRITE_ONLY_* keywords do not reach the command line")
endif()
if(NOT ninja MATCHES
   "--write-only-guard=off --write-only-registers=off --write-only-mask=off [^\n]*guard.svd [^\n]*guard_cmake_off json")
    message(FATAL_ERROR "an explicit WRITE_ONLY_* off does not stay off")
endif()
