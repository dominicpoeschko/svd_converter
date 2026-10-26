// Unit tests of the kvasir_bit view model (src/view_model.hpp): one check per rendering decision -
// the mode argument's alternatives, which registers get overrideDefaults, the
// dim-expanded field lists and the log text. The template only prints these.
#include "view_model.hpp"

#include <cstdio>
#include <string>
#include <vector>

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

namespace VM = Generator::Kvasir::ViewModel;
using Generator::Kvasir::RenderOptions;
using Lines = std::vector<std::string>;

Field field(std::string name,
            unsigned    bit,
            Access      access = Access::readWrite) {
    Field f;
    f.name     = std::move(name);
    f.startBit = bit;
    f.stopBit  = bit;
    f.access   = access;
    return f;
}

Field enumField(std::string name,
                unsigned    bit,
                unsigned    reset) {
    Field f      = field(std::move(name), bit);
    f.type       = FieldType::typeEnum;
    f.resetValue = reset;
    f.values     = {
      {.name = "OFF", .description = "", .value = 0},
      { .name = "ON", .description = "", .value = 1}
    };
    return f;
}

Register reg(std::vector<Field> fields) {
    Register r;
    r.name   = "Ctrl";
    r.fields = std::move(fields);
    return r;
}

constexpr RenderOptions plain{.writeOnlyMask = false, .namedRegisters = false};

void testModeLines() {
    auto r = reg({field("EN", 0)});
    CHECK(VM::modeLines(r, plain).empty(), "a plain register has no mode");
    CHECK((VM::modeLines(r, {.writeOnlyMask = false, .namedRegisters = true})
           == Lines{", Kvasir::Register::Mode<{}, CTRL>"}),
          "--named-registers names it");

    r.zeroMask = 0xFFFF0100;
    CHECK(VM::modeLines(r, plain).empty(),
          "a write-back mask of 0 (Semantics' default) is left out");
    r.writeBackActsMask = 0x100;
    CHECK(
      (VM::modeLines(r, plain) == Lines{", Kvasir::Register::Mode<{.writeBackActs = 0x00000100}>"}),
      "a non-zero write-back mask, no register name");
    r.zeroMask = 0x100;
    CHECK(
      (VM::modeLines(r, plain) == Lines{", Kvasir::Register::Mode<{.writeBackActs = 0x00000100}>"}),
      "a write-back mask equal to the zero mask is emitted too: only 0 is the default");

    auto key           = reg({field("EN", 0)});
    key.mustSupplyMask = 0xFFFF0000;
    CHECK((VM::modeLines(key, plain)
           == Lines{", Kvasir::Register::Mode<{.mustSupply = 0xFFFF0000}, CTRL>"}),
          "a key names the register");
    key.writeBackActsMask = 0x100;
    CHECK((VM::modeLines(key, plain)
           == Lines{", Kvasir::Register::Mode<{.mustSupply = 0xFFFF0000, .writeBackActs = "
                    "0x00000100}, CTRL>"}),
          "the facts in declaration order");

    auto pop              = reg({field("DAT", 0)});
    pop.readHasSideEffect = true;
    pop.writeBackActsMask = 0x100;
    CHECK((VM::modeLines(pop, plain)
           == Lines{", Kvasir::Register::Mode<{.readSideEffect = true}, CTRL>"}),
          "a read side effect, and no write-back mask (whole writes only)");

    auto wo                    = reg({field("EN", 0)});
    wo.writeOnlyNoIdentityMask = 0x2;
    CHECK(VM::modeLines(wo, plain).empty(), "the write-only mask only with --write-only-mask");
    CHECK((VM::modeLines(wo, {.writeOnlyMask = true, .namedRegisters = true})
           == Lines{", Kvasir::Register::Mode<{.writeOnlyNoIdentity = 0x00000002}, CTRL>"}),
          "--write-only-mask: writeOnlyNoIdentity");

    auto all              = reg({field("GO", 0, Access::writeOnly)});
    all.writeOnlyRegister = true;
    all.writeBackActsMask = 0x1;
    CHECK((VM::modeLines(all, plain)
           == Lines{", Kvasir::Register::Mode<{.writeOnlyRegister = true}, CTRL>"}),
          "a write-only register: no write-back mask");
    all.mustSupplyMask = 0xFF000000;
    CHECK((VM::modeLines(all, plain)
           == Lines{", Kvasir::Register::Mode<{.mustSupply = 0xFF000000, .writeOnlyRegister = "
                    "true}, CTRL>"}),
          "a key in a write-only register stays a fact: the SDK makes a write that misses it a "
          "compile error");
}

void testDefaults() {
    CHECK(!VM::hasDefaults(reg({field("RO", 0, Access::readOnly)})),
          "nothing writable: no defaults");
    CHECK(!VM::hasDefaults(reg({enumField("MODE", 0, 2)})),
          "an enum resetting to an unnamed value: no defaults");
    CHECK(VM::resetName(enumField("MODE", 0, 2)) == "???", "the unnamed reset value's comment");

    auto r = reg({field("RO", 0, Access::readOnly), enumField("MODE", 1, 1), field("EN", 2)});
    r.fields[2].resetValue = 1;
    CHECK(VM::hasDefaults(r), "writable fields with named resets: defaults");

    // a writable field outside <resetMask> has no reset value to default to
    r.resetMask = 0b011;
    CHECK(!VM::hasDefaults(r),
          "a writable field without a reset value (outside resetMask): no defaults");
    auto ro      = reg({field("RO", 0, Access::readOnly), field("EN", 1)});
    ro.resetMask = 0b010;
    CHECK(VM::hasDefaults(ro), "a read-only field without a reset value does not matter");
}

void testResetValue() {
    auto r       = reg({field("EN", 0)});
    r.dataType   = DataType::u8;
    r.resetValue = 0xE00;
    CHECK(VM::resetValue(r) == 0x00, "a reset value wider than the register is cut to its width");
    r.dataType = DataType::u32;
    CHECK(VM::resetValue(r) == 0xE00, "one that fits stays");
}

void testElements() {
    auto pri         = field("PRI", 0);
    pri.repType      = RepeatType::cluster;
    pri.dim          = 2;
    pri.dimIncrement = 8;
    auto       r     = reg({enumField("MODE", 31, 0), pri});
    auto const e     = VM::elements(r);
    CHECK(e.size() == 3, "a field cluster expands per element");
    CHECK(e[0].name == "mode" && e[0].expr == "mode" && e[0].isEnum, "a plain enum field");
    CHECK(e[1].name == "pri[0]" && e[1].expr == "PRI<0>::pri" && !e[1].isEnum
            && e[2].expr == "PRI<1>::pri",
          "cluster elements");
}

void testFmtString() {
    auto pri    = field("PRI", 0);
    pri.repType = RepeatType::cluster;
    pri.dim     = 2;
    auto r      = reg({enumField("MODE", 31, 0), pri});
    r.type      = RepeatType::cluster;
    CHECK(VM::fmtString({.peripheral = "Nvic", .group = "Grp"}, r)
            == R"(nvic::grp[{}]::ctrl[{}](\n\"mode\": {}\n\"pri[0]\": 0x{:x}\n\"pri[1]\": 0x{:x}))",
          "a group plus cluster register: both indexes, an enum as {}, numbers as 0x{:x}");
    CHECK(VM::dimsCount({.peripheral = "Nvic", .group = "Grp"}, r) == "0+ 1+ 1", "two dimensions");
    CHECK(VM::fmtString({.peripheral = "P", .group = {}}, reg({})) == R"(p::ctrl(\n))",
          "a register without fields");
}

void testAccessText() {
    CHECK(VM::accessText(Access::readWrite, ModifiedWriteValues::empty, ReadAction::empty)
            == "ReadWriteAccess",
          "plain kinds keep their short names");
    CHECK(VM::accessText(Access::readWrite, ModifiedWriteValues::oneToClear, ReadAction::empty)
            == "Kvasir::Register::Access<Kvasir::Register::AccessType::readWrite, "
               "Kvasir::Register::ReadActionType::normal, "
               "Kvasir::Register::ModifiedWriteValueType::oneToClear>",
          "anything else is the qualified Access<>");
}
}   // namespace

int main() {
    testModeLines();
    testDefaults();
    testResetValue();
    testElements();
    testFmtString();
    testAccessText();
    if(failures != 0) {
        std::printf("%d checks failed\n", failures);
        return 1;
    }
    std::printf("all view model checks passed\n");
    return 0;
}
