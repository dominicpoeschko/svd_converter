#include "generator_templates/kvasir_bit.hpp"
#include "inja_wrapper.hpp"
#include "svd/chip.hpp"
#include "svd/interrupts.hpp"
#include "svd/json.hpp"
#include "svd/model.hpp"
#include "svd/semantics.hpp"

#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <print>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

namespace {

// The converter's output folder. A file is written only when its text changed (an unchanged
// header keeps its mtime, so nothing that includes it recompiles), through <name>.tmp and a rename
// (an interrupted run leaves no half-written header). With `clean`, finish() removes every regular
// file with the generator's extension that this run did not write, and leftover *.tmp files: a
// peripheral that left the SVD leaves the folder too. Other files (the CMake step's dependency
// dummy) and subfolders stay.
class OutputDir {
public:
    OutputDir(std::filesystem::path dir,
              std::string           extension,
              bool                  clean)
      : dir_{std::move(dir)}
      , extension_{std::move(extension)}
      , clean_{clean} {
        std::filesystem::create_directories(dir_);
    }

    void write(std::string const& fileName,
               std::string const& text) {
        written_.insert(fileName);
        auto const path = dir_ / fileName;
        if(holds(path, text)) { return; }
        auto tmp = path;
        tmp += ".tmp";
        {
            std::ofstream out{tmp, std::ios::binary | std::ios::trunc};
            out << text;
            out.close();
            if(!out) { throw std::runtime_error(std::format("cannot write {}", tmp.string())); }
        }
        std::filesystem::rename(tmp, path);
    }

    void finish() const {
        if(!clean_) { return; }
        std::vector<std::filesystem::path> stale;
        for(auto const& entry : std::filesystem::directory_iterator{dir_}) {
            if(!entry.is_regular_file()) { continue; }
            auto const name = entry.path().filename().string();
            if(name.ends_with(".tmp") || (name.ends_with(extension_) && !written_.contains(name))) {
                stale.push_back(entry.path());
            }
        }
        for(auto const& path : stale) { std::filesystem::remove(path); }
    }

    [[nodiscard]] std::string extension() const { return extension_; }

private:
    static bool holds(std::filesystem::path const& path,
                      std::string const&           text) {
        std::error_code error;
        if(std::filesystem::file_size(path, error) != text.size() || error) { return false; }
        std::ifstream in{path, std::ios::binary};
        // a read that fails gives other text: the file is written anew
        std::string const old{std::istreambuf_iterator<char>{in}, std::istreambuf_iterator<char>{}};
        return old == text;
    }

    std::filesystem::path dir_;
    std::string           extension_;   // ".hpp" / ".json"
    bool                  clean_;
    std::set<std::string> written_;
};

std::string_view extensionOf(std::string_view generator) {
    return generator == "json" ? ".json" : ".hpp";
}

std::function<void(OutputDir&,
                   inja::json const&)>
getRenderer(std::string const&                      generator,
            Generator::Kvasir::RenderOptions const& options) {
    auto fileName = [](OutputDir const& out, inja::json const& peripheral) {
        return peripheral["name"].get<std::string>() + out.extension();
    };

    if(generator == "json") {
        return [=](OutputDir& out, inja::json const& peripheral) {
            out.write(fileName(out, peripheral), peripheral.dump(4));
        };
    }
    if(generator == "kvasir_bit") {
        return
          [=, env = Generator::Kvasir::getEnvironment()](OutputDir&        out,
                                                         inja::json const& peripheral) mutable {
              out.write(fileName(out, peripheral),
                        Generator::Kvasir::render(env, peripheral, options));
          };
    }
    throw std::runtime_error("bad generator");
}

struct CliOptions {
    Svd::ConvertOptions convert;
    // --clean-output: the folder belongs to this run (svd_convert() passes it); off by default, so
    // a folder of one's own keeps its files
    bool cleanOutput = false;
};

// Options before the positional arguments: --write-only-guard=off|warn|error (default error),
// --write-only-registers=derived|off (default derived), --write-only-mask[=on|off] (default on),
// --named-registers[=on|off], --clean-output[=on|off]. An SVD that is not classified yet:
// --write-only-guard=off --write-only-registers=off --write-only-mask=off.
CliOptions parseOptions(int&                argc,
                        char const* const*& argv) {
    CliOptions           cli;
    Svd::ConvertOptions& options = cli.convert;
    while(argc > 1 && std::string_view{*std::next(argv, 1)}.starts_with("--")) {
        std::string_view const arg = *std::next(argv, 1);
        auto const             eq  = arg.find('=');
        auto const             key = arg.substr(0, eq);
        auto const value = eq == std::string_view::npos ? std::string_view{} : arg.substr(eq + 1);
        if(key == "--write-only-guard") {
            if(value == "off") {
                options.guard = Svd::WriteOnlyGuard::off;
            } else if(value == "warn") {
                options.guard = Svd::WriteOnlyGuard::warn;
            } else if(value == "error") {
                options.guard = Svd::WriteOnlyGuard::error;
            } else {
                throw std::runtime_error(std::format("bad {}: off, warn or error", arg));
            }
        } else if(key == "--write-only-registers") {
            if(value == "derived") {
                options.writeOnlyRegisters = Svd::WriteOnlyRegisters::derived;
            } else if(value == "off") {
                options.writeOnlyRegisters = Svd::WriteOnlyRegisters::off;
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
        } else if(key == "--clean-output") {
            if(value.empty() || value == "on" || value == "ON") {
                cli.cleanOutput = true;
            } else if(value == "off" || value == "OFF") {
                cli.cleanOutput = false;
            } else {
                throw std::runtime_error(std::format("bad {}: on or off", arg));
            }
        } else {
            throw std::runtime_error(std::format("unknown option {}", arg));
        }
        --argc;
        argv = std::next(argv);
    }
    return cli;
}

// The findings on stderr, one line each, and a summary; true when the build has to stop.
bool reportWriteOnly(std::string const&                        svdfile,
                     Svd::ConvertOptions const&                options,
                     std::vector<Svd::WriteOnlyFinding> const& findings) {
    if(options.guard == Svd::WriteOnlyGuard::off || findings.empty()) { return false; }
    auto const name = std::filesystem::path{svdfile}.filename().string();
    for(auto const& finding : findings) {
        std::print(stderr, "svd_converter: {}: {}\n", name, Svd::describe(finding));
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
      options.guard == Svd::WriteOnlyGuard::error ? "error" : "warn");
    return options.guard == Svd::WriteOnlyGuard::error;
}
}   // namespace

int main(int                argc,
         char const* const* argv) {
    try {
        using inja::json;
        auto const  cli     = parseOptions(argc, argv);
        auto const& options = cli.convert;
        if(argc < 4) {
            throw std::runtime_error(
              "wrong args use with [--write-only-guard=off|warn|error] "
              "[--write-only-registers=derived|off] [--write-only-mask[=on|off]] "
              "[--named-registers] "
              "[--clean-output] "
              "\"svdfile\" \"outpath\" \"kvasir_bit|json\"");
        }
        std::string const svdfile   = *std::next(argv, 1);
        std::string const outpath   = *std::next(argv, 2);
        std::string const generator = *std::next(argv, 3);

        if(generator != "kvasir_bit" && generator != "json") {
            throw std::runtime_error("bad generator");
        }

        pugi::xml_document doc;
        // comments: the `<!-- Kvasir: write-only accepted -->` marker is one
        if(!doc.load_file(svdfile.c_str(), pugi::parse_default | pugi::parse_comments)) {
            throw std::runtime_error("svdfile not found or invalid");
        }

        auto device = doc.child("device");
        if(device.empty()) { throw std::runtime_error("device empty"); }

        std::vector<Svd::WriteOnlyFinding> findings;
        inja::json                         chip = Svd::chipFromSVD(device, options, &findings);

        OutputDir out{outpath, std::string{extensionOf(generator)}, cli.cleanOutput};
        auto      render = getRenderer(
          generator,
          {.writeOnlyMask = options.writeOnlyMask, .namedRegisters = options.namedRegisters});

        // Rendered whatever the guard says: the headers are there to look at.
        for(auto const& peripheral : chip["peripherals"]) { render(out, peripheral); }
        // the vectors' names for compile errors (Kvasir_SDK StartUp diagnostics): a chip SVD has them, a core's not
        if(generator == "kvasir_bit" && !chip["interrupts"].empty()) {
            auto env = Generator::Kvasir::getEnvironment();
            out.write("InterruptNames.hpp",
                      env.render(Generator::Kvasir::InterruptNamesTemplate, chip));
            // the vectors as objects for the chip's Kvasir::Interrupt (peripherals/Interrupts.hpp)
            auto const interrupts = chip["interrupts"].get<std::vector<Svd::Interrupt>>();
            Svd::checkInterruptCppNames(interrupts);
            auto const layout = Svd::interruptLayout(interrupts);
            inja::json model  = {
              {            "name",        chip["name"]},
              {    "interruptEnd",          layout.end},
              {   "interruptGaps",         layout.gaps},
              {"deviceInterrupts", inja::json::array()}
            };
            for(auto const& irq : interrupts) {
                model["deviceInterrupts"].push_back({
                  {   "name",                        irq.name},
                  {  "value",                       irq.value},
                  {"cppName", Svd::interruptCppName(irq.name)}
                });
            }
            out.write("Interrupts.hpp",
                      env.render(Generator::Kvasir::DeviceInterruptsTemplate, model));
        }
        // before the guard's verdict: an exit 2 has written and cleaned the folder all the same
        out.finish();

        return reportWriteOnly(svdfile, options, findings) ? 2 : 0;
    } catch(std::exception const& exception) {
        std::print(stderr, "caught {}\n", exception.what());
        return 1;
    }
}
