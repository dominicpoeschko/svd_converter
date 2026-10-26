#pragma once

// Reading an SVD into the model, as written: the device's peripherals with their registers, register
// groups (clusters with <dim>) and fields, the interrupts, the derived peripherals and the mode
// clusters. Names are sanitized; nothing is analyzed (semantics.hpp) or grouped (chip.hpp) yet.

#include "svd/model.hpp"

#include <concepts>
#include <cstdint>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace Svd {

template<typename T>
concept SvdValue = std::same_as<T, std::uint64_t> || std::same_as<T, std::string>
                || std::same_as<T, BitRange> || std::same_as<T, DataType> || std::same_as<T, Access>
                || std::same_as<T, ModifiedWriteValues> || std::same_as<T, ReadAction>;

// One SVD element's text as a value: a number (decimal, 0x hex or 0 octal), a string, a bitRange
// `[msb:lsb]`, a size (8, 16, 32, 64), an access, modifiedWriteValues or readAction. Throws on
// anything else.
template<SvdValue T>
T fromSVDString(std::string_view svdString);

extern template std::uint64_t       fromSVDString<std::uint64_t>(std::string_view);
extern template std::string         fromSVDString<std::string>(std::string_view);
extern template BitRange            fromSVDString<BitRange>(std::string_view);
extern template DataType            fromSVDString<DataType>(std::string_view);
extern template Access              fromSVDString<Access>(std::string_view);
extern template ModifiedWriteValues fromSVDString<ModifiedWriteValues>(std::string_view);
extern template ReadAction          fromSVDString<ReadAction>(std::string_view);

// The device as read: the chip (its peripherals that are neither derived nor split into modes,
// and every <interrupt> in document order), the derived peripherals, and the peripherals split
// into their mode clusters.
struct RawDevice {
    Chip                      chip;
    std::vector<DerivedRef>   derived;
    std::vector<ModeClusters> modeClusters;
};

[[nodiscard]] RawDevice readDevice(pugi::xml_node const& device);

}   // namespace Svd
