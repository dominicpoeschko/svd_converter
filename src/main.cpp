#include "fmt_wrapper.hpp"
#include "generator_templates/custom.hpp"
#include "generator_templates/kvasir_bit.hpp"
#include "inja_wrapper.hpp"
#include "svd_parser.hpp"

#include <exception>
#include <fstream>
#include <functional>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>

namespace {

std::function<void(inja::json const&)> getRenderer(std::string const& outpath,
                                                   std::string const& generator,
                                                   int                argc,
                                                   char const* const* argv) {
    auto getOutStream = [=](inja::json const& peripheral, std::string_view extension) {
        return std::ofstream{
          fmt::format("{}/{}.{}", outpath, peripheral["name"].get<std::string>(), extension)};
    };

    if(generator == "json") {
        return [=](inja::json const& peripheral) {
            getOutStream(peripheral, "json") << peripheral.dump(4);
        };
    }
    if(generator == "kvasir_bit") {
        return
          [=, env = Generator::Kvasir::getEnvironment()](inja::json const& peripheral) mutable {
              getOutStream(peripheral, "hpp")
                << env.render(Generator::Kvasir::PeripheralTemplate, peripheral);
          };
    }
    if(generator == "custom_template") {
        if(argc != 2) {
            throw std::runtime_error(
              "bad arguments for \"custom_template\" generator. \"template_path\" and "
              "\"file_extension\" required");
        }
        return
          [               =,
           file_extension = std::string{*std::next(argv, 1)},
           env = Generator::Custom::getEnvironment(argv[0])](inja::json const& peripheral) mutable {
              getOutStream(peripheral, file_extension)
                << env.render_file("/peripheral_template.inja", peripheral);
          };
    }

    throw std::runtime_error("bad generator");
}
}   // namespace

int main(int                argc,
         char const* const* argv) {
    try {
        using inja::json;
        if(argc < 4) {
            throw std::runtime_error(
              "wrong args use with \"svdfile\" \"outpath\" \"generator\""
              " \"[<template path for custom_template>]\""
              " \"[<file extension for custom_template>]\"");
        }
        std::string const svdfile   = *std::next(argv, 1);
        std::string const outpath   = *std::next(argv, 2);
        std::string const generator = *std::next(argv, 3);

        if(generator != "kvasir_bit" && generator != "json" && generator != "custom_template") {
            throw std::runtime_error("bad generator");
        }

        pugi::xml_document doc;
        if(!doc.load_file(svdfile.c_str())) {
            throw std::runtime_error("svdfile not found or invalid");
        }

        auto device = doc.child("device");
        if(device.empty()) {
            throw std::runtime_error("device empty");
        }

        inja::json chip = ChipFromSVD(device);

        auto render = getRenderer(outpath, generator, argc - 4, std::next(argv, 4));

        for(auto const& peripheral : chip["peripherals"]) {
            render(peripheral);
        }

        return 0;
    } catch(std::exception const& exception) {
        fmt::print(stderr, "caught {}\n", exception.what());
        return 1;
    }
}
