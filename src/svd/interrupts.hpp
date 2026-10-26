#pragma once

// A chip's device interrupts: one name per vector, the C++ names of peripherals/Interrupts.hpp and
// the layout of the vector table they describe.

#include "svd/model.hpp"

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace Svd {

// One entry per vector, sorted by value: SVDs repeat an interrupt in every peripheral that shares it
// (RP2350 TIMER0/1 ...); the first one read keeps its name.
void dedupeInterrupts(std::vector<Interrupt>& interrupts);

// The C++ name of a device interrupt (peripherals/Interrupts.hpp): the SVD's name lower-cased, split on '_', every
// "irq" token dropped, joined with '_' again (TIMER_IRQ_0 -> timer_0, IO_IRQ_BANK0 -> io_bank0, SPARE_IRQ_5 ->
// spare_5, PM -> pm).
[[nodiscard]] std::string interruptCppName(std::string_view svdName);

// The device interrupts' layout: one past the highest line, and the indexes below it no line names.
struct InterruptLayout {
    std::uint64_t              end{};
    std::vector<std::uint64_t> gaps;
};

// `interrupts` is sorted and has one entry per vector (dedupeInterrupts).
[[nodiscard]] InterruptLayout interruptLayout(std::vector<Interrupt> const& interrupts);

// Every C++ name a valid identifier, no keyword, none twice: throws naming the vectors.
void checkInterruptCppNames(std::vector<Interrupt> const& interrupts);

}   // namespace Svd
