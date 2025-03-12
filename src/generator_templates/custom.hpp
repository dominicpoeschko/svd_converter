#pragma once

#include "inja_wrapper.hpp"

#include <string>

namespace Generator { namespace Custom {

    inline inja::Environment getEnvironment(std::string const& template_path) {
        inja::Environment env{template_path};
        env.set_search_included_templates_in_files(true);
        return env;
    }

}}   // namespace Generator::Custom
