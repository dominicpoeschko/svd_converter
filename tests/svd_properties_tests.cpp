// What every converted SVD holds, whatever it describes: the model chipFromSVD builds (the SVD as written, loaded as
// the converter loads it) equals the json the svd_converter binary wrote for it, and every peripheral, register, field
// and enumerated value has a valid C++ name and fits where it sits. No fact of one chip's register map: the converter's
// behaviours (derived peripherals into instance tables, clusters, dims, prefix stripping, enums, masks, write keys) are
// svd_parser_tests.cpp's and the golden tests'. Run on the golden fixtures and on a real vendor SVD (tests/CMakeLists.txt).
#include "svd/chip.hpp"
#include "svd/json.hpp"
#include "svd/model.hpp"

#include <cctype>
#include <cstdio>
#include <exception>
#include <fstream>
#include <iterator>
#include <pugixml.hpp>
#include <string>
#include <string_view>

using namespace Svd;

namespace {

int failures = 0;

#define CHECK(cond, msg)                                        \
    do {                                                        \
        if(!(cond)) {                                           \
            std::printf("FAIL: %s (line %d)\n", msg, __LINE__); \
            ++failures;                                         \
        }                                                       \
    } while(0)

bool validIdentifier(std::string_view name) {
    if(name.empty()) { return false; }
    if(std::isdigit(static_cast<unsigned char>(name.front())) != 0) { return false; }
    for(auto const character : name) {
        if(std::isalnum(static_cast<unsigned char>(character)) == 0 && character != '_') {
            return false;
        }
    }
    return true;
}

void checkRegisterInvariants(Register const& reg) {
    auto const width = dataTypeSize(reg.dataType);
    CHECK(validIdentifier(reg.name), "register name is valid identifier");
    for(auto const& field : reg.fields) {
        CHECK(validIdentifier(field.name), "field name is valid identifier");
        CHECK(field.startBit <= field.stopBit, "field bit order");
        CHECK(field.stopBit < width, "field fits register width");
        auto const fieldWidth = field.stopBit - field.startBit + 1;
        CHECK(field.resetValue <= maskFromRange(fieldWidth - 1, 0), "field reset value fits");
        for(auto const& value : field.values) {
            CHECK(validIdentifier(value.name), "enum name is valid identifier");
            CHECK(value.value <= maskFromRange(fieldWidth - 1, 0), "enum value fits field");
        }
    }
}

void testInvariants(Chip const& chip) {
    CHECK(!chip.peripherals.empty(), "the SVD has peripherals");
    for(auto const& peripheral : chip.peripherals) {
        CHECK(validIdentifier(peripheral.name), "peripheral name is valid identifier");
        CHECK(!peripheral.baseAddresses.empty(), "peripheral has base address");
        for(auto const& reg : peripheral.registers) { checkRegisterInvariants(reg); }
        for(auto const& group : peripheral.registerGroups) {
            CHECK(group.dim != 0, "register group dim not zero");
            for(auto const& reg : group.registers) { checkRegisterInvariants(reg); }
        }
    }
}

void testGeneratedJson(Chip const&        chip,
                       std::string const& jsonDirectory) {
    for(auto const& peripheral : chip.peripherals) {
        auto const    path = jsonDirectory + "/" + peripheral.name + ".json";
        std::ifstream file{path};
        if(!file) {
            std::printf("FAIL: missing generated file %s\n", path.c_str());
            ++failures;
            continue;
        }
        auto const generated = nlohmann::json::parse(file, nullptr, false);
        if(generated.is_discarded()) {
            std::printf("FAIL: invalid json in %s\n", path.c_str());
            ++failures;
            continue;
        }
        if(nlohmann::json(peripheral) != generated) {
            std::printf("FAIL: generated json differs for %s\n", peripheral.name.c_str());
            ++failures;
        }
    }
}

}   // namespace

int main(int                argc,
         char const* const* argv) {
    try {
        if(argc < 2) {
            std::printf("usage: %s svdfile [generated_json_directory]\n", *argv);
            return 1;
        }
        std::string const svdfile = *std::next(argv, 1);

        // with its comments, as the converter loads it (src/main.cpp): the `Kvasir:` markers are comments
        pugi::xml_document doc;
        if(!doc.load_file(svdfile.c_str(), pugi::parse_default | pugi::parse_comments)) {
            std::printf("FAIL: could not load %s\n", svdfile.c_str());
            return 1;
        }

        auto const chip = chipFromSVD(doc.child("device"));

        testInvariants(chip);
        if(argc > 2) { testGeneratedJson(chip, *std::next(argv, 2)); }

        if(failures == 0) {
            std::printf("all tests passed\n");
            return 0;
        }
        std::printf("%d checks failed\n", failures);
        return 1;
    } catch(std::exception const& exception) {
        std::printf("FAIL: caught %s\n", exception.what());
        return 1;
    }
}
