# The golden modes: <name> <svd (below tests/)> <generator> <options, comma-separated>. Included by tests/CMakeLists.txt
# (one ctest golden_<name> each) and by update.cmake. The expected output of a mode is tests/golden/<svd stem>/<name>/.
set(golden_profile "--write-only-guard=error,--write-only-registers=derived,--write-only-mask=on")
set(golden_modes
    # the write-only guard profile, every firmware's options
    "kvasir_bit|golden/features.svd|kvasir_bit|${golden_profile}"
    "kvasir_bit_named|golden/features.svd|kvasir_bit|${golden_profile},--named-registers=on"
    # the opt-out: the only way a third-party SVD converts once the profile is the default
    "kvasir_bit_optout|golden/features.svd|kvasir_bit|--write-only-guard=off,--write-only-registers=off,--write-only-mask=off"
    "json|golden/features.svd|json|${golden_profile}"
    # an unclassified write-only field (guard only warns): the RmwHazard's write-only mask argument
    "kvasir_bit|write_only/guard.svd|kvasir_bit|--write-only-guard=warn,--write-only-registers=derived,--write-only-mask=on"
)
