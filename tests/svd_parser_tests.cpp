// Unit tests for the svd parser: string/number/bit-range conversions, name and description
// sanitizing and chipFromSVD on small in-memory svd documents covering derived registers,
// derived peripherals, clusters, enums and error handling.
#include "generator_templates/kvasir_bit.hpp"
#include "svd/chip.hpp"
#include "svd/interrupts.hpp"
#include "svd/model.hpp"
#include "svd/naming.hpp"
#include "svd/read.hpp"
#include "svd/semantics.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <exception>
#include <format>
#include <pugixml.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
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

#define CHECK_THROWS(expr, msg)                                               \
    do {                                                                      \
        bool caught = false;                                                  \
        try {                                                                 \
            (void)(expr);                                                     \
        } catch(std::exception const&) { caught = true; }                     \
        if(!caught) {                                                         \
            std::printf("FAIL: %s did not throw (line %d)\n", msg, __LINE__); \
            ++failures;                                                       \
        }                                                                     \
    } while(0)

Chip parseChip(std::string const& peripheralsXml) {
    std::string const xml
      = "<device><name>TESTCHIP</name><width>32</width><size>32</size>"
        "<access>read-write</access><peripherals>"
      + peripheralsXml + "</peripherals></device>";
    pugi::xml_document doc;
    if(!doc.load_string(xml.c_str())) { throw std::runtime_error("test xml invalid"); }
    return chipFromSVD(doc.child("device"));
}

void testMasks() {
    CHECK(maskFromRange(28, 28) == 0x10000000ULL, "single bit mask");
    CHECK(maskFromRange(31, 0) == 0xFFFFFFFFULL, "full 32 bit mask");
    CHECK(maskFromRange(63, 0) == 0xFFFFFFFFFFFFFFFFULL, "full 64 bit mask");
    CHECK(maskFromRange(5, 4) == 0x30ULL, "two bit mask");
    CHECK(makeResetValue(0x1fffffff, 28, 28) == 1, "reset value single bit");
    CHECK(makeResetValue(0x56, 4, 5) == 1, "reset value bit range");
    CHECK(makeResetValue(0x12345678, 4, 7) == 7, "reset value nibble");
    CHECK(clearBits(0xFFFFFFFF, 0, 3) == 0xFFFFFFF0, "clear low nibble");
}

void testNumberParsing() {
    CHECK(fromSVDString<std::uint64_t>("42") == 42, "decimal number");
    CHECK(fromSVDString<std::uint64_t>("0x2A") == 42, "hex number");
    CHECK(fromSVDString<std::uint64_t>("0xFFFFFFFFFFFFFFFF") == 0xFFFFFFFFFFFFFFFFULL,
          "64 bit number");
    CHECK_THROWS(fromSVDString<std::uint64_t>(""), "empty number");
    CHECK_THROWS(fromSVDString<std::uint64_t>("abc"), "invalid number");
}

void testBitRangeParsing() {
    auto const single = fromSVDString<BitRange>("[28:28]");
    CHECK(single.start == 28 && single.stop == 28, "single bit range");
    auto const full = fromSVDString<BitRange>("[31:0]");
    CHECK(full.start == 0 && full.stop == 31, "full bit range");
    CHECK_THROWS(fromSVDString<BitRange>(""), "empty bit range");
    CHECK_THROWS(fromSVDString<BitRange>("28:28"), "bit range without brackets");
    CHECK_THROWS(fromSVDString<BitRange>("[28]"), "bit range without colon");
    CHECK_THROWS(fromSVDString<BitRange>("[1:2]"), "bit range lsb greater msb");
}

void testEnumStringParsing() {
    CHECK(fromSVDString<Access>("read-only") == Access::readOnly, "access read-only");
    CHECK(fromSVDString<Access>("write-only") == Access::writeOnly, "access write-only");
    CHECK(fromSVDString<Access>("read-write") == Access::readWrite, "access read-write");
    CHECK(fromSVDString<Access>("writeOnce") == Access::writeOnce, "access writeOnce");
    CHECK(fromSVDString<Access>("read-writeOnce") == Access::readWriteOnce,
          "access read-writeOnce (CMSIS-SVD.xsd)");
    CHECK_THROWS(fromSVDString<Access>("bad"), "bad access");

    CHECK(fromSVDString<DataType>("8") == DataType::u8, "size 8");
    CHECK(fromSVDString<DataType>("16") == DataType::u16, "size 16");
    CHECK(fromSVDString<DataType>("32") == DataType::u32, "size 32");
    CHECK(fromSVDString<DataType>("64") == DataType::u64, "size 64");
    CHECK_THROWS(fromSVDString<DataType>("7"), "bad size");

    CHECK(fromSVDString<ModifiedWriteValues>("oneToClear") == ModifiedWriteValues::oneToClear,
          "modifiedWriteValues oneToClear");
    CHECK(fromSVDString<ModifiedWriteValues>("") == ModifiedWriteValues::empty,
          "modifiedWriteValues empty");
    CHECK_THROWS(fromSVDString<ModifiedWriteValues>("bad"), "bad modifiedWriteValues");

    CHECK(fromSVDString<ReadAction>("clear") == ReadAction::clear, "readAction clear");
    CHECK(fromSVDString<ReadAction>("") == ReadAction::empty, "readAction empty");
    CHECK_THROWS(fromSVDString<ReadAction>("bad"), "bad readAction");
}

void testNameSanitizing() {
    CHECK(sanitizeName("CTRL") == "CTRL", "plain name untouched");
    CHECK(sanitizeName("if") == "_if", "c++ keyword prefixed");
    CHECK(sanitizeName("VALUE") == "_VALUE", "kvasir keyword prefixed case insensitive");
    CHECK(sanitizeName("9BIT") == "_9BIT", "leading digit prefixed");
    CHECK(sanitizeName("A-B") == "A_B", "punctuation replaced");
    CHECK(sanitizeName("A B") == "A_B", "space replaced");
    CHECK(sanitizeName("GPIO[%s]") == "GPIO", "dim placeholder brackets removed");
    CHECK(sanitizeName("CH_%s") == "CH", "dim placeholder underscore removed");
    CHECK(sanitizeName("A__B") == "A_B", "double underscore collapsed");
}

void testDescriptionSanitizing() {
    CHECK(sanitizeDescription("a\nb") == "a b", "newline replaced");
    CHECK(sanitizeDescription("a\tb") == "a b", "tab replaced");
    CHECK(sanitizeDescription("a  b") == "a b", "double space collapsed");
    CHECK(sanitizeDescription(" x ") == "x", "surrounding spaces trimmed");
    CHECK(sanitizeDescription("a\\nb") == "a b", "escaped newline replaced");
}

void testBasicParsing() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <description>peripheral  a</description>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CTRL</name>
                    <description>control</description>
                    <addressOffset>0x4</addressOffset>
                    <resetValue>0x12345678</resetValue>
                    <fields>
                        <field>
                            <name>EN</name>
                            <bitRange>[0:0]</bitRange>
                        </field>
                        <field>
                            <name>MODE</name>
                            <bitOffset>4</bitOffset>
                            <bitWidth>4</bitWidth>
                            <access>read-only</access>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");

    CHECK(chip.name == "TESTCHIP", "chip name");
    CHECK(chip.peripherals.size() == 1, "one peripheral");
    auto const& peripheral = chip.peripherals.front();
    CHECK(peripheral.name == "PERIA", "peripheral name");
    CHECK(peripheral.description == "peripheral a", "peripheral description sanitized");
    CHECK(peripheral.type == RepeatType::normal, "peripheral type normal");
    CHECK(peripheral.baseAddresses.size() == 1, "one base address");
    CHECK(peripheral.baseAddresses.front().index == 0, "base address index");
    CHECK(peripheral.baseAddresses.front().address == 0x40000000, "base address value");
    CHECK(peripheral.registers.size() == 1, "one register");

    auto const& reg = peripheral.registers.front();
    CHECK(reg.name == "CTRL", "register name");
    CHECK(reg.addressOffset == 4, "register address offset");
    CHECK(reg.resetValue == 0x12345678, "register reset value");
    CHECK(reg.dataType == DataType::u32, "register data type from device size");
    CHECK(reg.fields.size() == 2, "two fields");

    auto const& enField = reg.fields[0];
    CHECK(enField.name == "EN", "field name from bitRange variant");
    CHECK(enField.startBit == 0 && enField.stopBit == 0, "field bitRange bits");
    CHECK(enField.access == Access::readWrite, "field access inherited from device");
    CHECK(enField.resetValue == 0, "field reset value extracted");

    auto const& modeField = reg.fields[1];
    CHECK(modeField.startBit == 4 && modeField.stopBit == 7, "field bitOffset/bitWidth bits");
    CHECK(modeField.access == Access::readOnly, "field access override");
    CHECK(modeField.resetValue == 7, "field reset value from register reset");
}

void testSizeOverrideAndDisplayName() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>UGLY_NAME</name>
                    <displayName>NICE_NAME</displayName>
                    <addressOffset>0x0</addressOffset>
                    <size>16</size>
                    <fields>
                        <field><name>F</name><bitRange>[0:0]</bitRange></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& reg  = chip.peripherals.front().registers.front();
    CHECK(reg.name == "NICE_NAME", "displayName overrides name");
    CHECK(reg.dataType == DataType::u16, "register size override");
    CHECK(reg.zeroMask == 0xFFFE, "zeroMask matches register width minus fields");
}

void testWriteMasks() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>IRQ</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>LEVEL</name><bitRange>[3:0]</bitRange></field>
                        <field>
                            <name>DONE</name>
                            <bitRange>[8:8]</bitRange>
                            <modifiedWriteValues>oneToClear</modifiedWriteValues>
                        </field>
                        <field>
                            <name>ARM</name>
                            <bitRange>[13:12]</bitRange>
                            <modifiedWriteValues>zeroToClear</modifiedWriteValues>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& reg  = chip.peripherals.front().registers.front();
    CHECK(reg.fields[1].modifiedWriteValues == ModifiedWriteValues::oneToClear,
          "modifiedWriteValues parsed");
    // zeroMask: every bit not covered by a plain field, oneToClear fields stay set
    CHECK(reg.zeroMask == 0xFFFFCFF0, "zeroMask keeps oneToClear bits");
    // writeBackActsMask: of zeroMask only the oneToClear field, reserved bits are kept as read
    CHECK(reg.writeBackActsMask == 0x00000100, "writeBackActsMask is the oneToClear field alone");
    // oneMask: bits where writing one is the no-op, i.e. zeroTo* fields
    CHECK(reg.oneMask == 0x00003000, "oneMask covers zeroToClear bits");
}

// Every element of a dim field leaves zeroMask; a one-to-* dim field stays in it.
void testZeroMaskCoversDimFieldElements() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>NVIC</name>
            <baseAddress>0xE000E100</baseAddress>
            <registers>
                <register>
                    <name>IPR</name>
                    <addressOffset>0x400</addressOffset>
                    <fields>
                        <field>
                            <name>PRI_%s</name>
                            <bitOffset>4</bitOffset>
                            <bitWidth>4</bitWidth>
                            <dim>4</dim>
                            <dimIncrement>8</dimIncrement>
                        </field>
                    </fields>
                </register>
                <register>
                    <name>ICER</name>
                    <addressOffset>0x80</addressOffset>
                    <fields>
                        <field>
                            <name>CLRENA_%s</name>
                            <bitRange>[0:0]</bitRange>
                            <dim>32</dim>
                            <dimIncrement>1</dimIncrement>
                            <modifiedWriteValues>oneToClear</modifiedWriteValues>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& regs = chip.peripherals.front().registers;
    CHECK(regs[0].zeroMask == 0x0F0F0F0F, "every element of a dim field leaves zeroMask");
    CHECK(regs[1].zeroMask == 0xFFFFFFFF, "a one-to-clear dim field stays in zeroMask whole");
    CHECK(regs[0].writeBackActsMask == 0, "plain dim fields: nothing to write as 0");
    CHECK(regs[1].writeBackActsMask == 0xFFFFFFFF,
          "every element of a one-to-clear dim field acts");
}

// Every element of a zeroTo* dim field is in oneMask; a field past the register is rejected.
void testDimFieldOneMaskAndWidth() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>FLAGS</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>ARM</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field>
                            <name>ARM_%s</name>
                            <bitRange>[1:0]</bitRange>
                            <dim>3</dim>
                            <dimIncrement>4</dimIncrement>
                            <modifiedWriteValues>zeroToClear</modifiedWriteValues>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& reg  = chip.peripherals.front().registers.front();
    CHECK(reg.oneMask == 0x00000333, "every element of a zeroTo* dim field is in oneMask");
    CHECK(reg.zeroMask == 0xFFFFFCCC, "and none of them in zeroMask");

    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>NVIC</name>
            <baseAddress>0xE000E100</baseAddress>
            <registers>
                <register>
                    <name>IPR</name>
                    <addressOffset>0x400</addressOffset>
                    <fields>
                        <field>
                            <name>PRI_%s</name>
                            <bitOffset>4</bitOffset>
                            <bitWidth>4</bitWidth>
                            <dim>5</dim>
                            <dimIncrement>8</dimIncrement>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)"),
                 "a dim field whose fifth element is past the 32-bit register");

    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERI</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>SMALL</name>
                    <addressOffset>0x0</addressOffset>
                    <size>8</size>
                    <fields>
                        <field>
                            <name>WIDE</name>
                            <bitRange>[11:4]</bitRange>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)"),
                 "a field past an 8-bit register");
}

// A read-only field ignores every write, so its bits stay in zeroMask the way reserved bits
// do: a write of the register's other fields needs no read-modify-write for it. The field
// itself is still emitted (it is readable), only the SDK's DefaultValues leaves it out.
void testReadOnlyFieldMask() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>STAT</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        <field>
                            <name>BUSY</name>
                            <bitRange>[4:4]</bitRange>
                            <access>read-only</access>
                        </field>
                        <field>
                            <name>COUNT</name>
                            <bitRange>[15:8]</bitRange>
                            <access>read-only</access>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& reg  = chip.peripherals.front().registers.front();
    CHECK(reg.fields.size() == 3, "read-only fields are kept");
    CHECK(reg.fields[1].access == Access::readOnly, "read-only access parsed");
    CHECK(reg.zeroMask == 0xFFFFFFFE, "zeroMask keeps read-only bits");
    CHECK(reg.writeBackActsMask == 0, "read-only bits are written back as read");
    CHECK(reg.oneMask == 0x00000000, "oneMask untouched by read-only fields");
}

void testDerivedRegister() {
    auto const  chip      = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CTRL</name>
                    <addressOffset>0x0</addressOffset>
                    <resetValue>0x5</resetValue>
                    <fields>
                        <field><name>EN</name><bitRange>[2:0]</bitRange></field>
                    </fields>
                </register>
                <register derivedFrom="CTRL">
                    <name>CTRL2</name>
                    <addressOffset>0x8</addressOffset>
                </register>
            </registers>
        </peripheral>)");
    auto const& registers = chip.peripherals.front().registers;
    CHECK(registers.size() == 2, "derived register added");
    CHECK(registers[1].name == "CTRL2", "derived register name");
    CHECK(registers[1].addressOffset == 8, "derived register offset");
    CHECK(registers[1].resetValue == 0x5, "derived register copies reset value");
    CHECK(registers[1].fields.size() == 1, "derived register copies fields");

    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register derivedFrom="MISSING">
                    <name>CTRL2</name>
                    <addressOffset>0x8</addressOffset>
                </register>
            </registers>
        </peripheral>)"),
                 "derived register with missing base");
}

// The register properties pass down: device -> peripheral -> cluster -> register, each level
// overriding them with its own <size>, <access>, <resetValue> and <resetMask>.
void testInheritedProperties() {
    auto const  chip   = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <size>16</size>
            <access>read-only</access>
            <resetValue>0x0003</resetValue>
            <registers>
                <register>
                    <name>STATUS</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>A</name><bitRange>[1:0]</bitRange></field>
                    </fields>
                </register>
                <register>
                    <name>CTRL</name>
                    <addressOffset>0x4</addressOffset>
                    <size>32</size>
                    <access>read-write</access>
                    <resetMask>0x0000FFFF</resetMask>
                    <fields>
                        <field><name>B</name><bitRange>[3:0]</bitRange></field>
                    </fields>
                </register>
                <cluster>
                    <dim>2</dim>
                    <dimIncrement>0x8</dimIncrement>
                    <name>CH[%s]</name>
                    <addressOffset>0x10</addressOffset>
                    <size>8</size>
                    <register>
                        <name>CFG</name>
                        <addressOffset>0x0</addressOffset>
                        <fields>
                            <field><name>C</name><bitRange>[0:0]</bitRange></field>
                        </fields>
                    </register>
                </cluster>
            </registers>
        </peripheral>)");
    auto const& p      = chip.peripherals.front();
    auto const& status = p.registers[0];
    CHECK(status.dataType == DataType::u16, "a register inherits the peripheral's size");
    CHECK(status.svdAccess == Access::readOnly, "a register inherits the peripheral's access");
    CHECK(status.fields.front().access == Access::readOnly, "its field inherits it too");
    CHECK(status.resetValue == 3, "a register inherits the peripheral's reset value");
    CHECK(status.resetMask == 0xFFFF, "resetMask: the device default, cut to the register's width");
    auto const& ctrl = p.registers[1];
    CHECK(ctrl.dataType == DataType::u32 && ctrl.svdAccess == Access::readWrite,
          "a register's own size and access override the peripheral's");
    CHECK(ctrl.resetMask == 0xFFFF, "a register's own resetMask");
    auto const& cfg = p.registerGroups.front().registers.front();
    CHECK(cfg.dataType == DataType::u8, "a dim cluster's own size reaches its registers");
    CHECK(cfg.svdAccess == Access::readOnly, "the cluster passes the peripheral's access on");

    // a mode cluster (no dim) is a level too
    auto const modes = parseChip(R"(
        <peripheral>
            <name>SER</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <cluster>
                    <name>UART</name>
                    <addressOffset>0x0</addressOffset>
                    <size>8</size>
                    <register>
                        <name>CTRL</name>
                        <addressOffset>0x0</addressOffset>
                        <fields>
                            <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        </fields>
                    </register>
                </cluster>
            </registers>
        </peripheral>)");
    CHECK(modes.peripherals.size() == 1
            && modes.peripherals.front().registers.front().dataType == DataType::u8,
          "a mode cluster's own size reaches its registers");
}

// derivedFrom names the base by its SVD <name>; the base's <displayName> is the C name only.
void testDerivedFromDisplayNamedBase() {
    auto const  chip      = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CTRL_A</name>
                    <displayName>CTRLA</displayName>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                    </fields>
                </register>
                <register derivedFrom="CTRL_A">
                    <name>CTRL_B</name>
                    <displayName>CTRLB</displayName>
                    <addressOffset>0x4</addressOffset>
                </register>
                <register derivedFrom="CTRL_A">
                    <name>CTRL_C</name>
                    <addressOffset>0x8</addressOffset>
                </register>
            </registers>
        </peripheral>)");
    auto const& registers = chip.peripherals.front().registers;
    CHECK(registers.size() == 3, "both derived registers found their base by its SVD name");
    CHECK(registers.size() == 3 && registers[1].name == "CTRLB",
          "a derived register's displayName");
    CHECK(registers.size() == 3 && registers[2].name == "CTRL_C", "a derived register's own name");
    CHECK(registers.size() == 3 && registers[2].svdName == "CTRL_C", "and its SVD name");

    // the displayName is no SVD name: deriving from it is a missing base, and the message names it
    bool named = false;
    try {
        (void)parseChip(R"(
            <peripheral>
                <name>PERIA</name>
                <baseAddress>0x40000000</baseAddress>
                <registers>
                    <register>
                        <name>CTRL_A</name>
                        <displayName>CTRLA</displayName>
                        <addressOffset>0x0</addressOffset>
                    </register>
                    <register derivedFrom="CTRLA">
                        <name>CTRL_B</name>
                        <addressOffset>0x4</addressOffset>
                    </register>
                </registers>
            </peripheral>)");
    } catch(std::exception const& e) {
        named = std::string_view{e.what()}
             == "register PERIA::CTRL_B derives from CTRLA, which PERIA does not have";
    }
    CHECK(named, "a missing base throws, naming the register and the base");
}

// derivedFrom chains: C from B from A resolves to A's fields; each derived register's own
// <description>, <dim>, <size>, <access>, <resetValue> and <resetMask> override its base's.
void testDerivedRegisterChain() {
    auto const  chip      = parseChip(R"(
        <peripheral>
            <name>PC</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register derivedFrom="B">
                    <name>C</name>
                    <addressOffset>0x8</addressOffset>
                </register>
                <register derivedFrom="A">
                    <name>B</name>
                    <description>B's own</description>
                    <addressOffset>0x4</addressOffset>
                    <resetValue>0x2</resetValue>
                </register>
                <register>
                    <name>A</name>
                    <description>the base</description>
                    <addressOffset>0x0</addressOffset>
                    <resetValue>0x1</resetValue>
                    <fields>
                        <field><name>EN</name><bitRange>[1:0]</bitRange></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& registers = chip.peripherals.front().registers;
    CHECK(registers.size() == 3, "a chained derivedFrom resolves");
    if(registers.size() != 3) { return; }
    // the underived first, then the derived in document order
    CHECK(registers[0].name == "A" && registers[1].name == "C" && registers[2].name == "B",
          "register order: underived, then derived");
    auto const& c = registers[1];
    CHECK(c.addressOffset == 8 && c.fields.size() == 1 && c.fields.front().name == "EN",
          "C has A's fields at its own offset");
    CHECK(c.description == "B's own", "C takes B's description (its nearest base that has one)");
    CHECK(c.resetValue == 0x2 && c.fields.front().resetValue == 0x2, "and B's reset value");
    CHECK(registers[2].description == "B's own" && registers[0].description == "the base",
          "B and A keep their own");

    {
        // a derived register's own <dim>, <size>, <access> and <resetMask> over its base's
        auto const  own  = parseChip(R"(
            <peripheral>
                <name>PD</name>
                <baseAddress>0x40000000</baseAddress>
                <registers>
                    <register>
                        <name>A</name>
                        <addressOffset>0x0</addressOffset>
                        <access>read-write</access>
                        <resetValue>0x1</resetValue>
                        <fields>
                            <field><name>EN</name><bitRange>[1:0]</bitRange></field>
                        </fields>
                    </register>
                    <register derivedFrom="A">
                        <dim>2</dim>
                        <dimIncrement>0x2</dimIncrement>
                        <name>D%s</name>
                        <addressOffset>0x4</addressOffset>
                        <size>16</size>
                        <access>read-only</access>
                        <resetMask>0x1</resetMask>
                    </register>
                </registers>
            </peripheral>)");
        auto const& regs = own.peripherals.front().registers;
        CHECK(regs.size() == 2, "a derived register with properties of its own");
        if(regs.size() == 2) {
            auto const& d = regs[1];
            CHECK(d.dim == 2 && d.dimIncrement == 2 && d.type == RepeatType::cluster,
                  "takes its own dim");
            CHECK(d.dataType == DataType::u16 && d.fields.front().dataType == DataType::u16,
                  "its own size");
            CHECK(d.svdAccess == Access::readOnly && d.fields.front().access == Access::readOnly,
                  "its own access");
            CHECK(d.resetMask == 0x1 && regs[0].resetMask == 0xFFFFFFFF, "its own resetMask");
            CHECK(regs[0].dim == 0 && regs[0].dataType == DataType::u32
                    && regs[0].svdAccess == Access::readWrite,
                  "and the base keeps its own");
        }
    }

    bool named = false;
    try {
        (void)parseChip(R"(
            <peripheral>
                <name>PC</name>
                <baseAddress>0x40000000</baseAddress>
                <registers>
                    <register derivedFrom="B"><name>A</name><addressOffset>0x0</addressOffset></register>
                    <register derivedFrom="A"><name>B</name><addressOffset>0x4</addressOffset></register>
                </registers>
            </peripheral>)");
    } catch(std::exception const& e) {
        named = std::string_view{e.what()} == "register A: derivedFrom loops back to A";
    }
    CHECK(named, "a derivedFrom loop throws, naming it");
}

void testDerivedRegisterOwnElements() {
    auto const  chip      = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <dim>4</dim>
                    <dimIncrement>0x1</dimIncrement>
                    <name>CFG%s</name>
                    <description>Group 0</description>
                    <addressOffset>0x0</addressOffset>
                    <size>8</size>
                    <resetValue>0x5</resetValue>
                    <fields>
                        <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        <field><name>MODE</name><bitRange>[3:1]</bitRange></field>
                    </fields>
                </register>
                <register derivedFrom="CFG%s">
                    <name>SAME%s</name>
                    <addressOffset>0x10</addressOffset>
                </register>
                <register derivedFrom="CFG%s">
                    <dim>2</dim>
                    <dimIncrement>0x2</dimIncrement>
                    <name>WIDE%s</name>
                    <description>Group 1</description>
                    <addressOffset>0x20</addressOffset>
                    <size>16</size>
                    <access>read-only</access>
                    <resetValue>0x3</resetValue>
                    <resetMask>0x1</resetMask>
                </register>
            </registers>
        </peripheral>)");
    auto const& registers = chip.peripherals.front().registers;
    CHECK(registers.size() == 3, "both derived registers");
    if(registers.size() != 3) { return; }
    auto const& same = registers[1];
    CHECK(same.name == "SAME" && same.dim == 4 && same.dimIncrement == 1
            && same.type == RepeatType::cluster,
          "no <dim> of its own: the base's");
    CHECK(same.description == "Group 0" && same.dataType == DataType::u8 && same.resetValue == 0x5
            && same.resetMask == 0xFF && same.svdAccess == Access::readWrite,
          "and the base's description and properties");
    auto const& wide = registers[2];
    CHECK(wide.name == "WIDE" && wide.dim == 2 && wide.dimIncrement == 2, "its own <dim>");
    CHECK(wide.description == "Group 1", "its own <description>");
    CHECK(wide.dataType == DataType::u16 && wide.svdAccess == Access::readOnly,
          "its own size and access");
    CHECK(wide.resetValue == 0x3 && wide.resetMask == 0x1, "its own resetValue and resetMask");
    CHECK(wide.fields.size() == 2 && wide.fields[0].dataType == DataType::u16
            && wide.fields[0].access == Access::readOnly && wide.fields[0].resetValue == 1
            && wide.fields[1].resetValue == 1,
          "the base's fields under the derived register's properties");
    CHECK(!hasResetValue(wide.fields[1], wide.resetMask), "MODE lies outside its resetMask");
}

// A dim cluster's name goes through sanitizeName like every other name.
void testGroupNameSanitized() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <cluster>
                    <dim>2</dim>
                    <dimIncrement>0x4</dimIncrement>
                    <name>EP-OUT[%s]</name>
                    <addressOffset>0x0</addressOffset>
                    <register>
                        <name>CFG</name>
                        <addressOffset>0x0</addressOffset>
                        <fields>
                            <field><name>C</name><bitRange>[0:0]</bitRange></field>
                        </fields>
                    </register>
                </cluster>
            </registers>
        </peripheral>)");
    CHECK(chip.peripherals.front().registerGroups.front().name == "EP_OUT", "group name sanitized");
}

// <resetMask>: a field wholly outside it has no reset value (no check, no defaults), a field partly
// inside it is checked on its defined bits only.
void testResetMask() {
    auto const  chip      = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>DIV</name>
                    <addressOffset>0x0</addressOffset>
                    <resetValue>0x0</resetValue>
                    <resetMask>0x0</resetMask>
                    <fields>
                        <field><name>MODE</name><bitRange>[1:0]</bitRange>
                            <enumeratedValues>
                                <enumeratedValue><name>SLOW</name><value>1</value></enumeratedValue>
                                <enumeratedValue><name>FAST</name><value>2</value></enumeratedValue>
                            </enumeratedValues>
                        </field>
                    </fields>
                </register>
                <register>
                    <name>PART</name>
                    <addressOffset>0x4</addressOffset>
                    <resetValue>0x00000002</resetValue>
                    <resetMask>0x00000002</resetMask>
                    <fields>
                        <field><name>MODE</name><bitRange>[1:0]</bitRange>
                            <enumeratedValues>
                                <enumeratedValue><name>B</name><value>3</value></enumeratedValue>
                            </enumeratedValues>
                        </field>
                        <field><name>EN</name><bitRange>[4:4]</bitRange></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& registers = chip.peripherals.front().registers;
    CHECK(registers[0].resetMask == 0, "resetMask read");
    CHECK(!hasResetValue(registers[0].fields[0], registers[0].resetMask),
          "a field wholly outside resetMask has no reset value");
    CHECK(hasResetValue(registers[1].fields[0], registers[1].resetMask),
          "a field partly inside resetMask has one");
    CHECK(!hasResetValue(registers[1].fields[1], registers[1].resetMask), "EN lies outside");
    CHECK(registers[1].resetMask == 2, "partial resetMask read");
}

// FULLREGISTER's access (Register::access): a field-less register's own <access>, write-only when no field
// can be read, read-only when every field is read-only, read-write otherwise.
void testFullRegisterAccess() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register><name>NF_RO</name><addressOffset>0x00</addressOffset><access>read-only</access></register>
                <register><name>NF_WO</name><addressOffset>0x04</addressOffset><access>write-only</access></register>
                <register><name>NF_WONCE</name><addressOffset>0x08</addressOffset><access>writeOnce</access></register>
                <register><name>NF_RWONCE</name><addressOffset>0x0C</addressOffset><access>read-writeOnce</access></register>
                <register><name>NF_RW</name><addressOffset>0x10</addressOffset></register>
                <register>
                    <name>ALL_WO</name><addressOffset>0x14</addressOffset><access>write-only</access>
                    <fields>
                        <field><name>A</name><bitRange>[0:0]</bitRange><modifiedWriteValues>oneToSet</modifiedWriteValues></field>
                        <field><name>B</name><bitRange>[1:1]</bitRange><access>writeOnce</access><modifiedWriteValues>oneToSet</modifiedWriteValues></field>
                    </fields>
                </register>
                <register>
                    <name>ALL_RO</name><addressOffset>0x18</addressOffset><access>read-only</access>
                    <fields>
                        <field><name>A</name><bitRange>[0:0]</bitRange></field>
                    </fields>
                </register>
                <register>
                    <name>MIXED</name><addressOffset>0x1C</addressOffset>
                    <fields>
                        <field><name>A</name><bitRange>[0:0]</bitRange><access>read-only</access></field>
                        <field><name>B</name><bitRange>[1:1]</bitRange><access>write-only</access><modifiedWriteValues>oneToSet</modifiedWriteValues></field>
                    </fields>
                </register>
                <register>
                    <name>RWONCE</name><addressOffset>0x20</addressOffset>
                    <fields>
                        <field><name>A</name><bitRange>[0:0]</bitRange><access>read-writeOnce</access></field>
                        <field><name>B</name><bitRange>[1:1]</bitRange><access>write-only</access><modifiedWriteValues>oneToSet</modifiedWriteValues></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& r    = chip.peripherals.front().registers;
    CHECK(r[0].access == Access::readOnly, "a field-less read-only register");
    CHECK(r[1].access == Access::writeOnly, "a field-less write-only register");
    CHECK(r[2].access == Access::writeOnly, "a field-less writeOnce register is write-only");
    CHECK(r[3].access == Access::readWrite, "a field-less read-writeOnce register is read-write");
    CHECK(r[4].access == Access::readWrite,
          "a field-less register without <access>: the device's read-write");
    CHECK(r[5].access == Access::writeOnly, "no field readable: write-only");
    CHECK(r[6].access == Access::readOnly, "every field read-only: read-only");
    CHECK(r[7].access == Access::readWrite, "read-only and write-only fields: read-write");
    CHECK(r[8].access == Access::readWrite, "a read-writeOnce field can be read: read-write");
}

void testDerivedPeripheral() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>UART0</name>
            <baseAddress>0x40070000</baseAddress>
            <registers>
                <register>
                    <name>DATA</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>D</name><bitRange>[7:0]</bitRange></field>
                    </fields>
                </register>
            </registers>
        </peripheral>
        <peripheral derivedFrom="UART0">
            <name>UART1</name>
            <baseAddress>0x40078000</baseAddress>
        </peripheral>)");
    CHECK(chip.peripherals.size() == 1, "derived peripherals merged");
    auto const& peripheral = chip.peripherals.front();
    CHECK(peripheral.name == "UART", "merged peripheral common prefix name");
    CHECK(peripheral.type == RepeatType::cluster, "merged peripheral is cluster");
    CHECK(peripheral.baseAddresses.size() == 2, "merged peripheral instance count");
    CHECK(peripheral.baseAddresses[0].index == 0
            && peripheral.baseAddresses[0].address == 0x40070000,
          "instance 0 address");
    CHECK(peripheral.baseAddresses[1].index == 1
            && peripheral.baseAddresses[1].address == 0x40078000,
          "instance 1 address");
}

// A derived peripheral of a derived peripheral (UART2 <- UART1 <- UART0) joins the first one's
// group; a derived peripheral with register properties of its own is a peripheral of its own, the
// base's registers under them, with its own description.
void testDerivedPeripheralChainAndOwnProperties() {
    auto const base = std::string{R"(
        <peripheral>
            <name>UART0</name>
            <description>the base</description>
            <baseAddress>0x40070000</baseAddress>
            <registers>
                <register>
                    <name>DATA</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>D</name><bitRange>[7:0]</bitRange></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)"};
    {
        auto const chip = parseChip(base + R"(
            <peripheral derivedFrom="UART1"><name>UART2</name><baseAddress>0x40080000</baseAddress></peripheral>
            <peripheral derivedFrom="UART0"><name>UART1</name><baseAddress>0x40078000</baseAddress></peripheral>)");
        CHECK(chip.peripherals.size() == 1 && chip.peripherals.front().baseAddresses.size() == 3,
              "a chained derived peripheral is an instance of the first base's group");
    }
    {
        auto const chip = parseChip(base + R"(
            <peripheral derivedFrom="UART0">
                <name>WIDE1</name>
                <description>its own</description>
                <baseAddress>0x40078000</baseAddress>
                <size>16</size>
                <resetValue>0x12</resetValue>
            </peripheral>
            <peripheral derivedFrom="WIDE1"><name>WIDE2</name><baseAddress>0x40080000</baseAddress></peripheral>)");
        auto const find = [&](std::string_view name) -> Peripheral const* {
            auto const it = std::ranges::find(chip.peripherals, name, &Peripheral::name);
            return it == chip.peripherals.end() ? nullptr : &*it;
        };
        auto const* uart = find("UART0");
        CHECK(uart != nullptr && uart->type == RepeatType::normal
                && uart->registers.front().dataType == DataType::u32,
              "the base keeps its registers, alone");
        auto const* wide = find("WIDE");
        CHECK(wide != nullptr && wide->type == RepeatType::cluster
                && wide->baseAddresses.size() == 2,
              "the derived peripheral with properties of its own is a group of its own (WIDE2 "
              "joins it)");
        CHECK(wide && wide->description == "its own", "with its own description");
        CHECK(wide && wide->registers.size() == 1 && wide->registers.front().name == "DATA"
                && wide->registers.front().dataType == DataType::u16
                && wide->registers.front().resetValue == 0x12
                && wide->registers.front().fields.front().dataType == DataType::u16,
              "and the base's registers under its own size and reset value");
    }
    {
        // a derived peripheral with its own <registers> (and no register property) is a peripheral
        // of its own with those registers, not an instance of the base that drops them
        auto const chip = parseChip(base + R"(
            <peripheral derivedFrom="UART0">
                <name>OWN1</name>
                <baseAddress>0x40078000</baseAddress>
                <registers>
                    <register>
                        <name>CTRL</name>
                        <addressOffset>0x4</addressOffset>
                        <fields>
                            <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        </fields>
                    </register>
                </registers>
            </peripheral>)");
        auto const own  = std::ranges::find_if(chip.peripherals, [](Peripheral const& p) {
            return p.name.starts_with("OWN");
        });
        CHECK(chip.peripherals.size() == 2 && own != chip.peripherals.end()
                && own->registers.size() == 1 && own->registers.front().name == "CTRL"
                && own->description == "the base",
              "a derived peripheral's own <registers> replace its base's");
    }
    {
        // a derived peripheral without an instance number is said once (left out), not again as
        // "have not found"
        std::vector<DerivedRef> derived{
          {.name = "PORT_IOBUS", .base = "PORT", .address = 0x60000000, .processed = false}
        };
        auto const groups = groupInstances("PORT", 0x41004400, derived);
        CHECK(groups.empty() && derived.front().processed,
              "a left-out member is processed: its stderr line is the only one");
    }
    {
        // a derivedFrom loop of peripherals (also one that derives from itself) throws, naming it
        auto const loopMessage = [&](std::string const& xml) {
            try {
                (void)parseChip(xml);
            } catch(std::exception const& e) { return std::string{e.what()}; }
            return std::string{};
        };
        CHECK(loopMessage(base + R"(
            <peripheral derivedFrom="UART2"><name>UART1</name><baseAddress>0x40078000</baseAddress></peripheral>
            <peripheral derivedFrom="UART1"><name>UART2</name><baseAddress>0x40080000</baseAddress></peripheral>)")
                == "peripheral UART1: derivedFrom loops back to UART1",
              "a peripheral derivedFrom loop throws");
        CHECK(loopMessage(base + R"(
            <peripheral derivedFrom="UART1"><name>UART1</name><baseAddress>0x40078000</baseAddress></peripheral>)")
                == "peripheral UART1: derivedFrom loops back to UART1",
              "a peripheral that derives from itself throws");
    }
}

// The derived peripheral shapes of ST's SVDs (STM32F413.svd V2.0): the base is not instance 0,
// port letters, a family with its own stem derived from another, and two groups with one stem.
void testDerivedPeripheralInstanceNames() {
    auto const peripheral
      = [](std::string_view name, std::string_view address, std::string_view base = {}) {
            std::string xml = base.empty() ? "<peripheral>"
                                           : std::format("<peripheral derivedFrom=\"{}\">", base);
            xml += std::format("<name>{}</name><baseAddress>{}</baseAddress>", name, address);
            if(base.empty()) {
                xml += "<registers><register><name>SR</name><addressOffset>0x0</addressOffset>"
                   "<fields><field><name>F</name><bitRange>[0:0]</bitRange></field></fields>"
                   "</register></registers>";
            }
            return xml + "</peripheral>";
        };
    auto const chip = parseChip(
      peripheral("USART3", "0x40004800") + peripheral("UART4", "0x40004C00", "USART3")
      + peripheral("USART1", "0x40011000", "USART3") + peripheral("UART10", "0x40011C00", "USART3")
      + peripheral("GPIOF", "0x40021400") + peripheral("GPIOC", "0x40020800", "GPIOF")
      + peripheral("GPIOH", "0x40021C00", "GPIOF") + peripheral("TIM1", "0x40010000")
      + peripheral("TIM8", "0x40010400", "TIM1") + peripheral("TIM7", "0x40001400")
      + peripheral("TIM6", "0x40001000", "TIM7") + peripheral("TIM2", "0x40000000")
      + peripheral("PORT", "0x41004400") + peripheral("PORT_IOBUS", "0x60000000", "PORT"));

    auto const find = [&](std::string_view name) -> Peripheral const* {
        auto const it = std::ranges::find(chip.peripherals, name, &Peripheral::name);
        return it == chip.peripherals.end() ? nullptr : &*it;
    };
    auto const instances = [](Peripheral const* p) {
        std::vector<std::pair<std::uint64_t, std::uint64_t>> result;
        result.reserve(p->baseAddresses.size());
        for(auto const& a : p->baseAddresses) { result.emplace_back(a.index, a.address); }
        return result;
    };
    using V = std::vector<std::pair<std::uint64_t, std::uint64_t>>;

    auto const* usart = find("USART");
    CHECK(usart != nullptr && usart->type == RepeatType::cluster, "USART group");
    CHECK(usart && (instances(usart) == V{{3, 0x40004800}, {1, 0x40011000}}),
          "base USART3 keeps its own number");
    auto const* uart = find("UART");
    CHECK(uart != nullptr && uart->type == RepeatType::cluster, "UART group of its own");
    CHECK(uart && (instances(uart) == V{{4, 0x40004C00}, {10, 0x40011C00}}), "UART instances");
    CHECK(uart && uart->registers.size() == 1 && uart->registers.front().name == "SR",
          "UART has the base's registers");

    auto const* gpio = find("GPIO");
    CHECK(gpio && (instances(gpio) == V{{5, 0x40021400}, {2, 0x40020800}, {7, 0x40021C00}}),
          "port letters as instances");

    CHECK(find("TIM") == nullptr, "no ambiguous TIM group");
    auto const* tim18 = find("TIM1_8");
    CHECK(tim18 && (instances(tim18) == V{{1, 0x40010000}, {8, 0x40010400}}), "TIM1_8");
    auto const* tim67 = find("TIM6_7");
    CHECK(tim67 && (instances(tim67) == V{{7, 0x40001400}, {6, 0x40001000}}), "TIM6_7 named sorted");
    CHECK(find("TIM2") != nullptr && find("TIM2")->type == RepeatType::normal,
          "underived TIM2 stays itself");

    CHECK(find("PORT") != nullptr && find("PORT")->type == RepeatType::normal,
          "a derived name without an instance is not merged");
}

void testRegisterDim() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CH[%s]</name>
                    <dim>4</dim>
                    <dimIncrement>0x4</dimIncrement>
                    <addressOffset>0x10</addressOffset>
                    <fields>
                        <field><name>V</name><bitRange>[7:0]</bitRange></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& reg  = chip.peripherals.front().registers.front();
    CHECK(reg.name == "CH", "dim register placeholder removed");
    CHECK(reg.type == RepeatType::cluster, "dim register is cluster");
    CHECK(reg.dim == 4, "dim count");
    CHECK(reg.dimIncrement == 4, "dim increment");

    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CH</name>
                    <dim>0</dim>
                    <dimIncrement>0x4</dimIncrement>
                    <addressOffset>0x10</addressOffset>
                </register>
            </registers>
        </peripheral>)"),
                 "register dim zero");
}

void testClusterWithDim() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>PADS</name>
            <baseAddress>0x40038000</baseAddress>
            <registers>
                <cluster>
                    <dim>48</dim>
                    <dimIncrement>0x4</dimIncrement>
                    <name>GPIO[%s]</name>
                    <addressOffset>0x4</addressOffset>
                    <register>
                        <name>PAD</name>
                        <addressOffset>0x0</addressOffset>
                        <resetValue>0x56</resetValue>
                        <fields>
                            <field><name>DRIVE</name><bitRange>[5:4]</bitRange></field>
                        </fields>
                    </register>
                </cluster>
            </registers>
        </peripheral>)");
    CHECK(chip.peripherals.size() == 1, "dim cluster stays inside peripheral");
    auto const& peripheral = chip.peripherals.front();
    CHECK(peripheral.registerGroups.size() == 1, "dim cluster becomes register group");
    auto const& group = peripheral.registerGroups.front();
    CHECK(group.name == "GPIO", "register group placeholder removed");
    CHECK(group.dim == 48, "register group dim");
    CHECK(group.dimIncrement == 4, "register group increment");
    CHECK(group.addressOffset == 4, "register group offset");
    CHECK(group.registers.size() == 1, "register group register count");
    CHECK(group.registers.front().fields.front().resetValue == 1,
          "register group field reset value");
}

void testRegisterNamedLikeItsGroup() {
    auto const  chip       = parseChip(R"(
        <peripheral>
            <name>EVSYS</name>
            <baseAddress>0x4100E000</baseAddress>
            <registers>
                <cluster>
                    <dim>2</dim>
                    <dimIncrement>0x8</dimIncrement>
                    <name>CHANNEL[%s]</name>
                    <addressOffset>0x20</addressOffset>
                    <register>
                        <name>Channel</name>
                        <addressOffset>0x0</addressOffset>
                        <fields><field><name>EVGEN</name><bitRange>[6:0]</bitRange></field></fields>
                    </register>
                    <register>
                        <name>CHINTFLAG</name>
                        <addressOffset>0x6</addressOffset>
                        <size>8</size>
                        <fields><field><name>OVR</name><bitRange>[0:0]</bitRange></field></fields>
                    </register>
                </cluster>
                <register>
                    <name>CHANNEL</name>
                    <addressOffset>0x0</addressOffset>
                    <fields><field><name>X</name><bitRange>[0:0]</bitRange></field></fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& peripheral = chip.peripherals.front();
    auto const& group      = peripheral.registerGroups.front();
    CHECK(group.name == "CHANNEL", "group name kept");
    CHECK(group.registers.size() == 2 && group.registers[0].name == "Channel_",
          "a register named like its group (any case) gets a _ suffix");
    CHECK(group.registers.size() == 2 && group.registers[1].name == "CHINTFLAG",
          "its sibling keeps its name");
    CHECK(peripheral.registers.size() == 1 && peripheral.registers.front().name == "CHANNEL",
          "a peripheral-level register of that name is no member of the group: kept");
}

void testClusterWithoutDim() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>PARENT</name>
            <baseAddress>0xA0000000</baseAddress>
            <registers>
                <cluster>
                    <name>SUB</name>
                    <register>
                        <name>CTRL</name>
                        <addressOffset>0x0</addressOffset>
                        <fields>
                            <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        </fields>
                    </register>
                </cluster>
            </registers>
        </peripheral>)");
    CHECK(chip.peripherals.size() == 1, "cluster without dim splits into peripheral");
    auto const& peripheral = chip.peripherals.front();
    CHECK(peripheral.name == "PARENT_SUB", "split peripheral name");
    CHECK(peripheral.baseAddresses.front().address == 0xA0000000,
          "split peripheral inherits base address");
    CHECK(peripheral.registers.size() == 1, "split peripheral register count");
}

// The shape of Microchip's USB peripheral: two mode clusters without dim, and inside DEVICE the
// endpoint registers as a cluster with dim.
void testClusterNestedInCluster() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>USB</name>
            <baseAddress>0x41005000</baseAddress>
            <registers>
                <cluster>
                    <name>DEVICE</name>
                    <addressOffset>0x0</addressOffset>
                    <register>
                        <name>CTRLA</name>
                        <addressOffset>0x0</addressOffset>
                        <size>8</size>
                        <fields>
                            <field><name>ENABLE</name><bitRange>[1:1]</bitRange></field>
                        </fields>
                    </register>
                    <cluster>
                        <dim>8</dim>
                        <dimIncrement>0x20</dimIncrement>
                        <name>DEVICE_ENDPOINT[%s]</name>
                        <addressOffset>0x100</addressOffset>
                        <register>
                            <name>EPCFG</name>
                            <addressOffset>0x0</addressOffset>
                            <size>8</size>
                            <fields>
                                <field><name>EPTYPE0</name><bitRange>[2:0]</bitRange></field>
                            </fields>
                        </register>
                        <register>
                            <name>EPSTATUSSET</name>
                            <addressOffset>0x5</addressOffset>
                            <size>8</size>
                            <fields>
                                <field><name>BK0RDY</name><bitRange>[6:6]</bitRange></field>
                            </fields>
                        </register>
                    </cluster>
                </cluster>
                <cluster>
                    <name>HOST</name>
                    <addressOffset>0x0</addressOffset>
                    <register>
                        <name>CTRLA</name>
                        <addressOffset>0x0</addressOffset>
                        <size>8</size>
                        <fields>
                            <field><name>ENABLE</name><bitRange>[1:1]</bitRange></field>
                        </fields>
                    </register>
                </cluster>
            </registers>
        </peripheral>)");
    CHECK(chip.peripherals.size() == 2, "both mode clusters split into peripherals");
    auto const find = [&](std::string_view name) -> Peripheral const* {
        for(auto const& p : chip.peripherals) {
            if(p.name == name) { return &p; }
        }
        return nullptr;
    };
    auto const* device = find("USB_DEVICE");
    auto const* host   = find("USB_HOST");
    CHECK(device != nullptr && host != nullptr, "named USB_DEVICE and USB_HOST");
    if(device == nullptr || host == nullptr) { return; }
    CHECK(device->registers.size() == 1, "the mode's own registers");
    CHECK(device->registerGroups.size() == 1, "nested dim cluster becomes its register group");
    auto const& group = device->registerGroups.front();
    CHECK(group.name == "DEVICE_ENDPOINT", "nested group placeholder removed");
    CHECK(group.dim == 8, "nested group dim");
    CHECK(group.dimIncrement == 0x20, "nested group increment");
    CHECK(group.addressOffset == 0x100, "nested group offset");
    CHECK(group.registers.size() == 2, "nested group registers");
    CHECK(host->registerGroups.empty(), "the sibling mode does not get DEVICE's group");
}

// A mode cluster that does not start at the peripheral's base: its registers, and a group nested
// in it, move with it.
void testClusterAddressOffset() {
    auto const chip = parseChip(R"(
        <peripheral>
            <name>PARENT</name>
            <baseAddress>0xA0000000</baseAddress>
            <registers>
                <cluster>
                    <name>SUB</name>
                    <addressOffset>0x40</addressOffset>
                    <register>
                        <name>CTRL</name>
                        <addressOffset>0x8</addressOffset>
                        <fields>
                            <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        </fields>
                    </register>
                    <cluster>
                        <dim>2</dim>
                        <dimIncrement>0x10</dimIncrement>
                        <name>CH[%s]</name>
                        <addressOffset>0x20</addressOffset>
                        <register>
                            <name>CFG</name>
                            <addressOffset>0x0</addressOffset>
                            <fields>
                                <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                            </fields>
                        </register>
                    </cluster>
                </cluster>
            </registers>
        </peripheral>)");
    CHECK(chip.peripherals.size() == 1, "one split peripheral");
    auto const& peripheral = chip.peripherals.front();
    CHECK(peripheral.registers.size() == 1 && peripheral.registers.front().addressOffset == 0x48,
          "register offset includes the cluster's");
    CHECK(peripheral.registerGroups.size() == 1
            && peripheral.registerGroups.front().addressOffset == 0x60,
          "nested group offset includes the cluster's");
}

void testEnumValues() {
    auto const  chip  = parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>PAD</name>
                    <addressOffset>0x0</addressOffset>
                    <resetValue>0x1</resetValue>
                    <fields>
                        <field>
                            <name>DRIVE</name>
                            <bitRange>[1:0]</bitRange>
                            <enumeratedValues>
                                <enumeratedValue>
                                    <name>2mA</name>
                                    <value>0</value>
                                </enumeratedValue>
                                <enumeratedValue>
                                    <description>four milli amps</description>
                                    <value>1</value>
                                </enumeratedValue>
                                <enumeratedValue>
                                    <value>2</value>
                                </enumeratedValue>
                            </enumeratedValues>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& field = chip.peripherals.front().registers.front().fields.front();
    CHECK(field.type == FieldType::typeEnum, "field with enumeratedValues is enum");
    CHECK(field.values.size() == 3, "enum value count");
    CHECK(field.values[0].name == "_2mA", "enum name leading digit prefixed");
    CHECK(field.values[0].value == 0, "enum value 0");
    CHECK(field.values[1].name == "four_milli_amps", "enum name from description");
    CHECK(field.values[2].name == "_2", "enum name from value");
}

void testPeripheralPrefixStripping() {
    auto const  chip = parseChip(R"(
        <peripheral>
            <name>GPIO</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>GPIO_CTRL</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field>
                            <name>GPIO_EN</name>
                            <bitRange>[0:0]</bitRange>
                            <enumeratedValues>
                                <enumeratedValue>
                                    <name>GPIO_OFF</name>
                                    <value>0</value>
                                </enumeratedValue>
                            </enumeratedValues>
                        </field>
                    </fields>
                </register>
            </registers>
        </peripheral>)");
    auto const& reg  = chip.peripherals.front().registers.front();
    CHECK(reg.name == "CTRL", "peripheral prefix stripped from register name");
    CHECK(reg.fields.front().name == "EN", "peripheral prefix stripped from field name");
    CHECK(reg.fields.front().values.front().name == "OFF",
          "peripheral prefix stripped from enum name");

    // The RP2040's RTC has registers RTC_1 and RTC_0: stripping the prefix must not leave a
    // bare digit behind.
    auto const rtc = parseChip(R"(
        <peripheral>
            <name>RTC</name>
            <baseAddress>0x4005C000</baseAddress>
            <registers>
                <register>
                    <name>RTC_1</name>
                    <addressOffset>0x18</addressOffset>
                </register>
                <register>
                    <name>RTC_0</name>
                    <addressOffset>0x1C</addressOffset>
                </register>
            </registers>
        </peripheral>)");
    CHECK(rtc.peripherals.front().registers[0].name == "_1", "stripped name with leading digit");
    CHECK(rtc.peripherals.front().registers[1].name == "_0", "stripped name with leading digit");
}

void testErrors() {
    {
        pugi::xml_document doc;
        doc.load_string("<device><name>TESTCHIP</name></device>");
        CHECK_THROWS(chipFromSVD(doc.child("device")), "device without peripherals element");
    }
    CHECK(parseChip("").peripherals.empty(), "empty peripherals element parses empty");
    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERIA</name>
        </peripheral>)"),
                 "peripheral without base address");
    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <addressOffset>0x0</addressOffset>
                </register>
            </registers>
        </peripheral>)"),
                 "register without name");
    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CTRL</name>
                </register>
            </registers>
        </peripheral>)"),
                 "register without address offset");
    CHECK_THROWS(parseChip(R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CTRL</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>F</name></field>
                    </fields>
                </register>
            </registers>
        </peripheral>)"),
                 "field without bit position");
}

// Write semantics from standard SVD elements: a self-clearing trigger written oneToSet joins
// zeroMask, a writeConstraint range of one value is a key (Semantics::mustSupply), a readAction
// makes the register's read a side effect; a range of several values and SVD `clear` change nothing.
void testWriteSemantics() {
    auto const  xml  = R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <registers>
                <register>
                    <name>CTRL</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        <field><name>GO</name><bitRange>[1:1]</bitRange><access>write-only</access>
                            <modifiedWriteValues>oneToSet</modifiedWriteValues></field>
                        <field><name>LOAD</name><bitRange>[2:2]</bitRange>
                            <modifiedWriteValues>clear</modifiedWriteValues></field>
                        <field><name>DIV</name><bitRange>[11:8]</bitRange>
                            <writeConstraint><range><minimum>1</minimum><maximum>9</maximum></range></writeConstraint></field>
                        <field><name>KEY</name><bitRange>[31:16]</bitRange>
                            <writeConstraint><range><minimum>0x05FA</minimum><maximum>0x05FA</maximum></range></writeConstraint></field>
                    </fields>
                </register>
                <register>
                    <name>DATA</name>
                    <addressOffset>0x4</addressOffset>
                    <fields><field><name>DAT</name><bitRange>[7:0]</bitRange>
                        <readAction>modify</readAction></field></fields>
                </register>
            </registers>
        </peripheral>)";
    Chip const  chip = parseChip(xml);
    auto const& ctrl = chip.peripherals.front().registers[0];
    auto const& data = chip.peripherals.front().registers[1];
    CHECK(ctrl.zeroMask == 0x0000F0FA, "the oneToSet trigger joins zeroMask, `clear` does not");
    CHECK(ctrl.writeBackActsMask == 0x00000002,
          "the write-only oneToSet trigger must be written 0");
    CHECK(ctrl.fields[1].identity == Identity::zero && ctrl.fields[2].identity == Identity::none,
          "identities");
    CHECK(ctrl.mustSupplyMask == 0xFFFF0000, "only the one-value range is a key");
    CHECK(!ctrl.readHasSideEffect, "no readAction in CTRL");
    CHECK(data.readHasSideEffect && data.mustSupplyMask == 0, "DATA's read pops");
}

// `<!-- Kvasir: write key NAME[hi:lo] = 0xVALUE: ... -->` in a <register>: a key over read-only fields (RP2350
// POWMAN TIMER) becomes a write-only key field of its own; the read-only fields stay readable, every write must
// carry the key, nothing else changes. A register without the marker is untouched; a marker it cannot read throws.
void testWriteKeyOverReadOnly() {
    auto const parse = [](std::string const& registerXml) {
        std::string const xml
          = "<device><name>TESTCHIP</name><width>32</width><size>32</size>"
            "<access>read-write</access><peripherals><peripheral><name>PERIA</name>"
            "<baseAddress>0x40000000</baseAddress><registers>"
          + registerXml + "</registers></peripheral></peripherals></device>";
        pugi::xml_document doc;
        if(!doc.load_string(xml.c_str(), pugi::parse_default | pugi::parse_comments)) {
            throw std::runtime_error("test xml invalid");
        }
        return chipFromSVD(doc.child("device"));
    };
    std::string const fields = R"(<fields>
            <field><name>USING_XOSC</name><bitRange>[16:16]</bitRange><access>read-only</access></field>
            <field><name>ALARM</name><bitRange>[6:6]</bitRange><modifiedWriteValues>oneToClear</modifiedWriteValues></field>
            <field><name>RUN</name><bitRange>[1:1]</bitRange></field>
        </fields>)";
    Chip const plain = parse("<register><name>TIMER</name><addressOffset>0x88</addressOffset>"
                             + fields + "</register>");
    Chip const keyed = parse(
      "<register><name>TIMER</name><addressOffset>0x88</addressOffset>"
      "<!-- Kvasir: write key PASSWORD[31:16] = 0x5AFE: read-only fields up there -->"
      + fields + "</register>");
    auto const& p = plain.peripherals.front().registers.front();
    auto const& k = keyed.peripherals.front().registers.front();
    CHECK(p.fields.size() == 3 && p.mustSupplyMask == 0, "no marker: no key");
    CHECK(k.fields.size() == 4, "the key is a field of its own, after the SVD's");
    auto const& key = k.fields.back();
    CHECK(key.name == "PASSWORD" && key.startBit == 16 && key.stopBit == 31,
          "the key's name and bits");
    CHECK(key.access == Access::writeOnly && key.key && key.type == FieldType::typeEnum,
          "a write-only key");
    CHECK(key.values.size() == 1 && key.values.front().value == 0x5AFE, "its one value");
    CHECK(k.mustSupplyMask == 0xFFFF0000, "every write must carry it");
    CHECK(k.writeBackActsMask == p.writeBackActsMask && k.writeBackActsMask == 0x40,
          "only the W1C flag is written 0, as without the key");
    CHECK(k.zeroMask == (p.zeroMask & ~0xFFFF0000ULL),
          "the key's bits are written, not kept as read");
    CHECK(k.fields[0].access == Access::readOnly && k.fields[0].name == "USING_XOSC",
          "the read-only field stays");
    CHECK_THROWS(parse("<register><name>TIMER</name><addressOffset>0x88</addressOffset>"
                       "<!-- Kvasir: write key PASSWORD 0x5AFE -->"
                       + fields + "</register>"),
                 "a marker without bits");
    CHECK_THROWS(parse("<register><name>TIMER</name><addressOffset>0x88</addressOffset>"
                       "<!-- Kvasir: write key PASSWORD[32:16] = 0x5AFE: too wide -->"
                       + fields + "</register>"),
                 "a key past the register's width");
}

// The device interrupts as C++ objects (peripherals/Interrupts.hpp): the name rule, end and gaps, the refusals, and
// the rendered text.
void testDeviceInterrupts() {
    CHECK(interruptCppName("TIMER_IRQ_0") == "timer_0" && interruptCppName("IRQ_X") == "x"
            && interruptCppName("PM") == "pm"
            && interruptCppName("IO_IRQ_BANK0_NS") == "io_bank0_ns"
            && interruptCppName("SPARE_IRQ_5") == "spare_5"
            && interruptCppName("I2CFMP1event") == "i2cfmp1event"
            && interruptCppName("lptim1_OR_it_eit_23") == "lptim1_or_it_eit_23",
          "names: lower-case, every IRQ token dropped");
    auto const layout    = interruptLayout(std::vector<Interrupt>{
      {"A", 3},
      {"B", 7}
    });
    bool const gapsRight = layout.gaps == std::vector<std::uint64_t>{0, 1, 2, 4, 5, 6};
    CHECK(layout.end == 8 && gapsRight, "end = max + 1, the gaps");
    auto const none = interruptLayout(std::vector<Interrupt>{
      {"A", 0},
      {"B", 1}
    });
    CHECK(none.end == 2 && none.gaps.empty(), "no gaps");
    CHECK(interruptLayout(std::vector<Interrupt>{}).end == 0, "no interrupts");
    auto const throws = [](std::vector<Interrupt> const& irqs) {
        try {
            checkInterruptCppNames(irqs);
        } catch(std::exception const&) { return true; }
        return false;
    };
    using L            = std::vector<Interrupt>;
    bool const twice   = throws(L{
      {"TIMER_IRQ_0", 0},
      {    "TIMER_0", 1}
    });
    bool const empty   = throws(L{
      {"IRQ", 0}
    });
    bool const digit   = throws(L{
      {"0_TIMER", 0}
    });
    bool const keyword = throws(L{
      {"DELETE", 0}
    });
    bool const fine    = !throws(L{
      {"TIMER_IRQ_0", 0},
      {"TIMER_IRQ_1", 1}
    });
    CHECK(twice, "two vectors with one name throw");
    CHECK(empty, "an empty name throws");
    CHECK(digit, "a leading digit throws");
    CHECK(keyword, "a keyword throws");
    CHECK(fine, "distinct names pass");

    auto             env   = Generator::Kvasir::getEnvironment();
    inja::json const model = {
      {            "name",                             "TESTCHIP"                          },
      {    "interruptEnd",                                                                8},
      {   "interruptGaps",                                 std::vector<std::uint64_t>{0, 1}},
      {"deviceInterrupts",
       inja::json::array({{{"name", "TIMER_IRQ_0"}, {"value", 2}, {"cppName", "timer_0"}}})}
    };
    auto const text = env.render(Generator::Kvasir::DeviceInterruptsTemplate, model);
    CHECK(text.contains("static constexpr ::Kvasir::Nvic::Index<2> timer_0{};   // TIMER_IRQ_0")
            && text.contains("inline constexpr int interruptEnd = 8;")
            && text.contains("std::array<int, 2> interruptGaps{ 0, 1 }"),
          "the rendered Interrupts.hpp");
}

// Named diagnostics: interrupts from the SVD (sorted, one per vector), the register type in Mode,
// every register named with --named-registers, and the InterruptNames.hpp the generator writes.
void testDiagnosticNames() {
    auto const xml  = R"(
        <peripheral>
            <name>PERIA</name>
            <baseAddress>0x40000000</baseAddress>
            <interrupt><name>PERIA_IRQ_1</name><value>7</value></interrupt>
            <interrupt><name>PERIA_IRQ_0</name><value>3</value></interrupt>
            <registers>
                <register>
                    <name>CTRL</name>
                    <addressOffset>0x0</addressOffset>
                    <fields>
                        <field><name>EN</name><bitRange>[0:0]</bitRange></field>
                        <field><name>KEY</name><bitRange>[31:16]</bitRange>
                            <writeConstraint><range><minimum>0x05FA</minimum><maximum>0x05FA</maximum></range></writeConstraint></field>
                    </fields>
                </register>
                <register>
                    <name>PLAIN</name>
                    <addressOffset>0x4</addressOffset>
                    <fields><field><name>DAT</name><bitRange>[7:0]</bitRange></field></fields>
                </register>
            </registers>
        </peripheral>
        <peripheral derivedFrom="PERIA">
            <name>PERIB</name>
            <baseAddress>0x40001000</baseAddress>
            <interrupt><name>PERIA_IRQ_1</name><value>7</value></interrupt>
        </peripheral>)";
    Chip const chip = parseChip(xml);
    CHECK(chip.interrupts.size() == 2 && chip.interrupts[0].value == 3
            && chip.interrupts[0].name == "PERIA_IRQ_0" && chip.interrupts[1].value == 7,
          "interrupts sorted by value, a repeated one once");

    auto                   env        = Generator::Kvasir::getEnvironment();
    inja::json const       peripheral = inja::json(chip).at("peripherals").at(0);
    auto const             plain      = Generator::Kvasir::render(env, peripheral, {});
    std::string_view const ctrl       = "Kvasir::Register::Mode<{.mustSupply = 0xFFFF0000}, CTRL>";
    CHECK(plain.contains(ctrl), "a hazard register names itself");
    CHECK(!plain.contains(", PLAIN>"), "no name for a plain register without --named-registers");
    auto const named = Generator::Kvasir::render(env,
                                                 peripheral,
                                                 {.writeOnlyMask = false, .namedRegisters = true});
    CHECK(named.contains(", PLAIN>") && named.contains(ctrl),
          "--named-registers: every other register names itself");

    auto const irqs = env.render(Generator::Kvasir::InterruptNamesTemplate, inja::json(chip));
    CHECK(irqs.contains("std::array<InterruptName, 2>") && irqs.contains("{ 3, \"PERIA_IRQ_0\" }"),
          "InterruptNames.hpp");
    CHECK(irqs.contains(
            "struct Kvasir::Startup::Diagnostics::DeviceInterruptNames<Kvasir::Tag::User, Ts...>")
            && irqs.contains("#include \"kvasir/StartUp/InterruptNames.hpp\""),
          "InterruptNames.hpp specialises the SDK's names hook");
}

// parseChip with the write-only guard's options, comments kept (the accepted marker is one)
Chip parseChipGuarded(std::string const&             peripheralsXml,
                      ConvertOptions const&          options,
                      std::vector<WriteOnlyFinding>& findings) {
    std::string const xml
      = "<device><name>TESTCHIP</name><width>32</width><size>32</size>"
        "<access>read-write</access><peripherals>"
      + peripheralsXml + "</peripherals></device>";
    pugi::xml_document doc;
    if(!doc.load_string(xml.c_str(), pugi::parse_default | pugi::parse_comments)) {
        throw std::runtime_error("test xml invalid");
    }
    return chipFromSVD(doc.child("device"), options, &findings);
}

std::string oneRegister(std::string const& name,
                        std::string const& fields,
                        std::string const& extra = "") {
    return "<peripheral><name>" + name
         + "</name><baseAddress>0x40000000</baseAddress><registers><register><name>CTRL</name>"
           "<addressOffset>0x0</addressOffset>"
         + extra + "<fields>" + fields + "</fields></register></registers></peripheral>";
}

constexpr auto readWriteEn   = "<field><name>EN</name><bitRange>[1:1]</bitRange></field>";
constexpr auto readWriteMode = "<field><name>MODE</name><bitRange>[5:4]</bitRange></field>";
constexpr auto plainSwrst
  = "<field><name>SWRST</name><bitRange>[0:0]</bitRange><access>write-only</access></field>";

// The write-only guard: a write-only field without a write identity next to a writable
// field whose partial write reads the register is a finding; every way to classify it is not.
void testWriteOnlyGuard() {
    // guard warn, registers off: findings reported, the model as without classification
    ConvertOptions const off{.guard              = WriteOnlyGuard::warn,
                             .writeOnlyMask      = false,
                             .writeOnlyRegisters = WriteOnlyRegisters::off};
    {
        ConvertOptions const defaults{};
        CHECK(defaults.guard == WriteOnlyGuard::error && defaults.writeOnlyMask
                && defaults.writeOnlyRegisters == WriteOnlyRegisters::derived,
              "the defaults are Kvasir's write semantics: error, mask on, registers derived");
    }
    {
        std::vector<WriteOnlyFinding> f;
        auto const                    chip = parseChipGuarded(
          oneRegister("TC", std::string{plainSwrst} + readWriteEn + readWriteMode),
          off,
          f);
        auto const& r = chip.peripherals.front().registers.front();
        CHECK(f.size() == 1 && f[0].field == "SWRST" && f[0].reg == "CTRL" && f[0].where == "TC",
              "one finding for SWRST");
        CHECK(f.size() == 1 && f[0].rewrittenBy == (std::vector<std::string>{"EN", "MODE"}),
              "rewritten by both siblings");
        CHECK(f.size() == 1 && f[0].bits == "bit 0" && f[0].readableSibling, "bits and kind");
        CHECK(r.writeOnlyNoIdentityMask == 0x1 && !r.writeOnlyRegister, "the mask is SWRST's bit");
        CHECK(f.size() == 1
                && describe(f[0])
                     == "TC.CTRL.SWRST (bit 0) is write-only with no write identity: a write of EN, "
                        "MODE reads the register and writes SWRST back as read",
              "the report line");
    }
    {
        std::vector<WriteOnlyFinding> f;
        auto const                    chip = parseChipGuarded(
          oneRegister("TC",
                      std::string{"<field><name>SWRST</name><bitRange>[0:0]</bitRange><access>"
                                  "write-only</access><modifiedWriteValues>oneToSet"
                                  "</modifiedWriteValues></field>"}
                        + readWriteEn),
          off,
          f);
        CHECK(f.empty() && chip.peripherals.front().registers.front().writeOnlyNoIdentityMask == 0,
              "oneToSet: a strobe, classified");
    }
    {
        std::vector<WriteOnlyFinding> f;
        (void)parseChipGuarded(
          oneRegister("SCB",
                      std::string{"<field><name>KEY</name><bitRange>[31:16]</bitRange><access>"
                                  "write-only</access><writeConstraint><range><minimum>0x5FA"
                                  "</minimum><maximum>0x5FA</maximum></range></writeConstraint>"
                                  "</field>"}
                        + readWriteEn),
          off,
          f);
        CHECK(f.empty(), "a one-value writeConstraint: a key, classified");
    }
    {
        std::vector<WriteOnlyFinding> f;
        auto const                    chip = parseChipGuarded(
          oneRegister("TC",
                      std::string{"<field><name>SWRST</name><bitRange>[0:0]</bitRange><access>"
                                  "write-only</access><!-- Kvasir: write-only accepted: reads 0, "
                                  "DS l.1 --></field>"}
                        + readWriteEn),
          off,
          f);
        auto const& r = chip.peripherals.front().registers.front();
        CHECK(f.empty() && r.fields[0].writeOnlyAccepted && r.writeOnlyNoIdentityMask == 0,
              "the accepted comment silences the field");
        std::vector<WriteOnlyFinding> g;
        (void)parseChipGuarded(oneRegister("TC",
                                           std::string{"<field><name>SWRST</name><bitRange>[0:0]"
                                                       "</bitRange><access>write-only</access>"
                                                       "<!-- Kvasir: something else --></field>"}
                                             + readWriteEn),
                               off,
                               g);
        CHECK(g.size() == 1, "another Kvasir comment does not");
    }
    {
        auto const fields
          = std::string{plainSwrst}
          + "<field><name>GO</name><bitRange>[1:1]</bitRange><access>write-only</access></field>";
        std::vector<WriteOnlyFinding> f;
        auto const                    chip = parseChipGuarded(oneRegister("EV", fields), off, f);
        CHECK(f.size() == 2 && !f[0].readableSibling, "registers off: no readable field, findings");
        CHECK(!chip.peripherals.front().registers.front().writeOnlyRegister, "and not derived");
        std::vector<WriteOnlyFinding> g;
        ConvertOptions const          derived{};   // the default: registers derived
        auto const  chip2 = parseChipGuarded(oneRegister("EV", fields), derived, g);
        auto const& r     = chip2.peripherals.front().registers.front();
        CHECK(g.empty() && r.writeOnlyRegister && r.writeOnlyNoIdentityMask == 0,
              "derived: a whole-register-only register, no finding");
        std::vector<WriteOnlyFinding> h;
        (void)parseChipGuarded(oneRegister("TC", std::string{plainSwrst} + readWriteEn),
                               derived,
                               h);
        CHECK(h.size() == 1, "derived keeps the mixed register's finding");
    }
    {
        std::vector<WriteOnlyFinding> f;
        (void)parseChipGuarded(
          oneRegister("FIFO",
                      std::string{plainSwrst}
                        + "<field><name>DATA</name><bitRange>[15:8]</bitRange><readAction>modify"
                          "</readAction></field>"),
          off,
          f);
        CHECK(f.empty(), "a register whose read has a side effect is never read-modify-written");
    }
    {
        std::vector<WriteOnlyFinding> f;
        (void)parseChipGuarded(
          oneRegister("ST",
                      std::string{plainSwrst}
                        + "<field><name>BUSY</name><bitRange>[1:1]</bitRange><access>read-only"
                          "</access></field>"),
          off,
          f);
        CHECK(f.empty(), "only read-only siblings: nothing writes SWRST back");
    }
    {
        std::vector<WriteOnlyFinding> f;
        auto const                    chip = parseChipGuarded(
          oneRegister("PIO",
                      std::string{"<field><name>STROBE%s</name><bitRange>[0:0]</bitRange><access>"
                                  "write-only</access><dim>4</dim><dimIncrement>2</dimIncrement>"
                                  "</field>"}
                        + "<field><name>MODE</name><bitRange>[9:8]</bitRange></field>"),
          off,
          f);
        CHECK(f.size() == 1
                && chip.peripherals.front().registers.front().writeOnlyNoIdentityMask == 0x55,
              "a dim field: the mask covers every element");
        CHECK(f.size() == 1 && f[0].bits == "bit 0 x4 every 2", "and the report says so");
    }
    {
        std::vector<WriteOnlyFinding> f;
        auto const                    xml
          = oneRegister("TC3", std::string{plainSwrst} + readWriteEn)
          + "<peripheral derivedFrom=\"TC3\"><name>TC4</name><baseAddress>0x40001000</baseAddress>"
            "</peripheral>";
        auto const chip = parseChipGuarded(xml, off, f);
        CHECK(f.size() == 1 && f[0].where == "TC3", "a derived peripheral is reported once");
        CHECK(chip.peripherals.size() == 1
                && chip.peripherals.front().registers.front().writeOnlyNoIdentityMask == 0x1,
              "and shares the result");
    }
    {
        std::vector<WriteOnlyFinding> f;
        (void)parseChipGuarded(oneRegister("TC", std::string{plainSwrst} + readWriteEn),
                               ConvertOptions{.guard = WriteOnlyGuard::off},
                               f);
        CHECK(f.size() == 1, "the model is the same with the guard off; only the report is not");
    }
}

}   // namespace

int main() {
    testDiagnosticNames();
    testDeviceInterrupts();
    testWriteOnlyGuard();
    testWriteSemantics();
    testWriteKeyOverReadOnly();
    testMasks();
    testNumberParsing();
    testBitRangeParsing();
    testEnumStringParsing();
    testNameSanitizing();
    testDescriptionSanitizing();
    testBasicParsing();
    testSizeOverrideAndDisplayName();
    testWriteMasks();
    testReadOnlyFieldMask();
    testZeroMaskCoversDimFieldElements();
    testDimFieldOneMaskAndWidth();
    testDerivedRegister();
    testInheritedProperties();
    testDerivedFromDisplayNamedBase();
    testDerivedRegisterChain();
    testDerivedRegisterOwnElements();
    testGroupNameSanitized();
    testResetMask();
    testFullRegisterAccess();
    testDerivedPeripheral();
    testDerivedPeripheralInstanceNames();
    testDerivedPeripheralChainAndOwnProperties();
    testRegisterDim();
    testClusterWithDim();
    testRegisterNamedLikeItsGroup();
    testClusterWithoutDim();
    testClusterNestedInCluster();
    testClusterAddressOffset();
    testEnumValues();
    testPeripheralPrefixStripping();
    testErrors();

    if(failures == 0) {
        std::printf("all tests passed\n");
        return 0;
    }
    std::printf("%d checks failed\n", failures);
    return 1;
}
