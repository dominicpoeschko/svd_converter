#include "svd/interrupts.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <cstddef>
#include <format>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace Svd {

void dedupeInterrupts(std::vector<Interrupt>& interrupts) {
    std::ranges::stable_sort(interrupts, {}, &Interrupt::value);
    auto const dup = std::ranges::unique(interrupts, {}, &Interrupt::value);
    interrupts.erase(dup.begin(), dup.end());
}

std::string interruptCppName(std::string_view svdName) {
    std::string out;
    std::string token;
    auto        flush = [&] {
        if(!token.empty() && token != "irq") {
            if(!out.empty()) { out += '_'; }
            out += token;
        }
        token.clear();
    };
    for(char const c : svdName) {
        if(c == '_') {
            flush();
        } else {
            token += static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }
    }
    flush();
    return out;
}

InterruptLayout interruptLayout(std::vector<Interrupt> const& interrupts) {
    InterruptLayout layout;
    if(interrupts.empty()) { return layout; }
    layout.end    = interrupts.back().value + 1;
    std::size_t i = 0;
    for(std::uint64_t index = 0; index != layout.end; ++index) {
        if(i != interrupts.size() && interrupts[i].value == index) {
            ++i;
        } else {
            layout.gaps.push_back(index);
        }
    }
    return layout;
}

void checkInterruptCppNames(std::vector<Interrupt> const& interrupts) {
    static constexpr std::array<std::string_view, 24> keywords{
      "and",   "asm",     "auto",   "bool", "break",  "case",   "char",  "class",
      "const", "default", "delete", "do",   "else",   "enum",   "for",   "if",
      "int",   "long",    "new",    "or",   "return", "switch", "while", "xor"};
    std::map<std::string, std::uint64_t> seen;
    for(auto const& irq : interrupts) {
        std::string const name  = interruptCppName(irq.name);
        bool const        valid = !name.empty()
                               && std::isdigit(static_cast<unsigned char>(name.front())) == 0
                               && std::ranges::all_of(name, [](char c) {
                               return std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '_';
                                  });
        if(!valid || std::ranges::find(keywords, name) != keywords.end()) {
            throw std::runtime_error(std::format("interrupt {} ({}): '{}' is no usable C++ name",
                                                 irq.value,
                                                 irq.name,
                                                 name));
        }
        if(auto const [it, fresh] = seen.emplace(name, irq.value); !fresh) {
            throw std::runtime_error(
              std::format("interrupts {} and {} both become '{}'", it->second, irq.value, name));
        }
    }
}

}   // namespace Svd
