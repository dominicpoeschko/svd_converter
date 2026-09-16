// Unit tests for the svd parser: string/number/bit-range conversions, name and description
// sanitizing and ChipFromSVD on small in-memory svd documents covering derived registers,
// derived peripherals, clusters, enums and error handling.
#include "svd_parser.hpp"

#include <cstdio>
#include <exception>
#include <pugixml.hpp>
#include <stdexcept>
#include <string>
#include <string_view>

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
        } catch(std::exception const&) {                                      \
            caught = true;                                                    \
        }                                                                     \
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
    return ChipFromSVD(doc.child("device"));
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
// itself is still emitted (it is readable), only default_values leaves it out.
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
    CHECK(field.type == FieldType::enum_, "field with enumeratedValues is enum");
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
        CHECK_THROWS(ChipFromSVD(doc.child("device")), "device without peripherals element");
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

}   // namespace

int main() {
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
    testDerivedPeripheral();
    testRegisterDim();
    testClusterWithDim();
    testClusterWithoutDim();
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
