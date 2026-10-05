#include "generator_templates/custom.hpp"
#include "generator_templates/kvasir_bit.hpp"
#include "inja_wrapper.hpp"
#include "svd_parser.hpp"

#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <print>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace {

std::function<void(inja::json const&)> getRenderer(std::string const& outpath,
                                                   std::string const& generator,
                                                   int                argc,
                                                   char const* const* argv) {
    auto getOutStream = [=](inja::json const& peripheral, std::string_view extension) {
        return std::ofstream{
          std::format("{}/{}.{}", outpath, peripheral["name"].get<std::string>(), extension)};
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
          [=,
           file_extension = std::string{*std::next(argv, 1)},
           env = Generator::Custom::getEnvironment(argv[0])](inja::json const& peripheral) mutable {
              getOutStream(peripheral, file_extension)
                << env.render_file("/peripheral_template.inja", peripheral);
          };
    }

    throw std::runtime_error("bad generator");
}

// Options before the positional arguments: --write-only-guard=off|warn|error,
// --write-only-registers=derived|off, --write-only-mask[=on|off], --named-registers[=on|off].
ConvertOptions parseOptions(int&                argc,
                            char const* const*& argv) {
    ConvertOptions options;
    while(argc > 1 && std::string_view{*std::next(argv, 1)}.starts_with("--")) {
        std::string_view const arg = *std::next(argv, 1);
        auto const             eq  = arg.find('=');
        auto const             key = arg.substr(0, eq);
        auto const value = eq == std::string_view::npos ? std::string_view{} : arg.substr(eq + 1);
        if(key == "--write-only-guard") {
            if(value == "off") {
                options.guard = WriteOnlyGuard::off;
            } else if(value == "warn") {
                options.guard = WriteOnlyGuard::warn;
            } else if(value == "error") {
                options.guard = WriteOnlyGuard::error;
            } else {
                throw std::runtime_error(std::format("bad {}: off, warn or error", arg));
            }
        } else if(key == "--write-only-registers") {
            if(value == "derived") {
                options.writeOnlyRegisters = WriteOnlyRegisters::derived;
            } else if(value == "off") {
                options.writeOnlyRegisters = WriteOnlyRegisters::off;
            } else {
                throw std::runtime_error(std::format("bad {}: derived or off", arg));
            }
        } else if(key == "--write-only-mask") {
            if(value.empty() || value == "on" || value == "ON") {
                options.writeOnlyMask = true;
            } else if(value == "off" || value == "OFF") {
                options.writeOnlyMask = false;
            } else {
                throw std::runtime_error(std::format("bad {}: on or off", arg));
            }
        } else if(key == "--named-registers") {
            if(value.empty() || value == "on" || value == "ON") {
                options.namedRegisters = true;
            } else if(value == "off" || value == "OFF") {
                options.namedRegisters = false;
            } else {
                throw std::runtime_error(std::format("bad {}: on or off", arg));
            }
        } else {
            throw std::runtime_error(std::format("unknown option {}", arg));
        }
        --argc;
        argv = std::next(argv);
    }
    return options;
}

// The findings on stderr, one line each, and a summary; true when the build has to stop.
bool reportWriteOnly(std::string const&                   svdfile,
                     ConvertOptions const&                options,
                     std::vector<WriteOnlyFinding> const& findings) {
    if(options.guard == WriteOnlyGuard::off || findings.empty()) { return false; }
    auto const name = std::filesystem::path{svdfile}.filename().string();
    for(auto const& finding : findings) {
        std::print(stderr, "svd_converter: {}: {}\n", name, describe(finding));
    }
    std::size_t registers = 0;
    for(std::size_t i = 0; i < findings.size(); ++i) {
        bool const sameRegister = i != 0 && findings[i].where == findings[i - 1].where
                               && findings[i].reg == findings[i - 1].reg;
        if(!sameRegister) { ++registers; }
    }
    std::print(
      stderr,
      "svd_converter: {}: {} write-only field(s) in {} register(s) unclassified "
      "(--write-only-guard={}). Classify each in the SVD: "
      "<modifiedWriteValues>oneToSet</modifiedWriteValues> (a 0 does nothing), a one-value "
      "<writeConstraint> (a key), or <!-- Kvasir: write-only accepted: <why, datasheet "
      "line> -->\n",
      name,
      findings.size(),
      registers,
      options.guard == WriteOnlyGuard::error ? "error" : "warn");
    return options.guard == WriteOnlyGuard::error;
}
}   // namespace

int main(int                argc,
         char const* const* argv) {
    try {
        using inja::json;
        auto const options = parseOptions(argc, argv);
        if(argc < 4) {
            throw std::runtime_error(
              "wrong args use with [--write-only-guard=off|warn|error] "
              "[--write-only-registers=derived|off] [--write-only-mask] [--named-registers] "
              "\"svdfile\" \"outpath\" "
              "\"generator\" \"[<template path for custom_template>]\" "
              "\"[<file extension for custom_template>]\"");
        }
        std::string const svdfile   = *std::next(argv, 1);
        std::string const outpath   = *std::next(argv, 2);
        std::string const generator = *std::next(argv, 3);

        if(generator != "kvasir_bit" && generator != "json" && generator != "custom_template") {
            throw std::runtime_error("bad generator");
        }

        pugi::xml_document doc;
        // comments: the `<!-- Kvasir: write-only accepted -->` marker is one
        if(!doc.load_file(svdfile.c_str(), pugi::parse_default | pugi::parse_comments)) {
            throw std::runtime_error("svdfile not found or invalid");
        }

        auto device = doc.child("device");
        if(device.empty()) { throw std::runtime_error("device empty"); }

        std::vector<WriteOnlyFinding> findings;
        inja::json                    chip = ChipFromSVD(device, options, &findings);

        auto render = getRenderer(outpath, generator, argc - 4, std::next(argv, 4));

        // Rendered whatever the guard says: the headers are there to look at.
        for(auto& peripheral : chip["peripherals"]) {
            // the templates' switch; the json generator writes the model alone
            if(generator != "json") {
                peripheral["writeOnlyMask"]  = options.writeOnlyMask;
                peripheral["namedRegisters"] = options.namedRegisters;
            }
            render(peripheral);
        }
        // the vectors' names for compile errors (Kvasir_SDK StartUp diagnostics): a chip SVD has them, a core's not
        if(generator == "kvasir_bit" && !chip["interrupts"].empty()) {
            auto env = Generator::Kvasir::getEnvironment();
            std::ofstream{std::format("{}/InterruptNames.hpp", outpath)}
              << env.render(Generator::Kvasir::InterruptNamesTemplate, chip);
        }

        return reportWriteOnly(svdfile, options, findings) ? 2 : 0;
    } catch(std::exception const& exception) {
        std::print(stderr, "caught {}\n", exception.what());
        return 1;
    }
}
