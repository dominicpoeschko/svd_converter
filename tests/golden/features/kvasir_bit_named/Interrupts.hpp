
#pragma once
#include "InterruptNames.hpp"
#include "kvasir/Common/Interrupt.hpp"

#include <array>

// the device interrupts of FEATURES from its SVD, as Nvic indexes (0 = the first after the core's exceptions); a
// name is the SVD's, lower-cased, every IRQ token dropped
namespace Kvasir::Peripheral {
struct DeviceInterrupts {
    static constexpr ::Kvasir::Nvic::Index<0> ctrlp{};   // CTRLP
    static constexpr ::Kvasir::Nvic::Index<1> tc{};   // TC
    static constexpr ::Kvasir::Nvic::Index<3> sercom{};   // SERCOM
    static constexpr ::Kvasir::Nvic::Index<4> spare{};   // SPARE
};

// one past the highest line
inline constexpr int interruptEnd = 5;
// the indexes below interruptEnd that no line names
inline constexpr std::array<int, 1> interruptGaps{ 2 };
}
