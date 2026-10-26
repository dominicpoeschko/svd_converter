
#pragma once
#include "kvasir/Common/Tags.hpp"
#include "kvasir/StartUp/InterruptNames.hpp"

#include <array>
#include <cstdint>
#include <string_view>

// the device interrupts of FEATURES from its SVD: index (0 = the first after the core's exceptions) and name
namespace Kvasir::Peripheral {
struct InterruptName {
    std::uint32_t    index;
    std::string_view name;
};

inline constexpr std::array<InterruptName, 4> interruptNames{ {
    { 0, "CTRLP" },
    { 1, "TC" },
    { 3, "SERCOM" },
    { 4, "SPARE" },
} };
}

// the names for Startup's reports (Kvasir_SDK StartUp/InterruptNames.hpp's hook)
template<typename... Ts>
struct Kvasir::Startup::Diagnostics::DeviceInterruptNames<Kvasir::Tag::User, Ts...> {
    static consteval std::string_view of(int index) {
        for(auto const& irq : ::Kvasir::Peripheral::interruptNames) {
            if(static_cast<int>(irq.index) == index) { return irq.name; }
        }
        return {};
    }
};
