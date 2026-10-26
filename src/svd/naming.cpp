#include "svd/naming.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <format>
#include <iterator>
#include <print>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace Svd {

std::string removePercentS(std::string name) noexcept {
    if(auto const pos = name.find("[%s]"); pos != std::string::npos) {
        name.erase(pos, 4);
    } else if(auto const pos2 = name.find("_%s"); pos2 != std::string::npos) {
        name.erase(pos2, 3);
    } else if(auto const pos3 = name.find("%s"); pos3 != std::string::npos) {
        name.erase(pos3, 2);
    }
    return name;
}

std::string sanitizeName(std::string name,
                         bool        full) {
    static constexpr std::array cppKeywords{
      "alignas",   "alignof",  "and",      "asm",      "auto",    "bitand",  "bitor",     "bool",
      "break",     "case",     "catch",    "char",     "class",   "compl",   "concept",   "const",
      "constexpr", "continue", "decltype", "default",  "delete",  "do",      "double",    "else",
      "enum",      "explicit", "export",   "extern",   "false",   "float",   "for",       "friend",
      "goto",      "if",       "inline",   "int",      "long",    "mutable", "namespace", "new",
      "noexcept",  "not",      "nullptr",  "operator", "or",      "private", "protected", "public",
      "register",  "requires", "return",   "short",    "signed",  "sizeof",  "static",    "struct",
      "switch",    "template", "this",     "throw",    "true",    "try",     "typedef",   "typeid",
      "typename",  "union",    "unsigned", "using",    "virtual", "void",    "volatile",  "while",
      "xor"};

    static constexpr std::array kvasirKeywords{"value"};
    for(auto& character : name) {
        if(full) {
            if(std::ispunct(character) != 0) { character = '_'; }
        } else {
            if(std::ispunct(character) != 0 && character != '[' && character != ']'
               && character != '%')
            {
                character = '_';
            }
        }
    }

    auto sanitizeKeyword = [&](auto const& keywords) {
        for(auto const& keyword : keywords) {
            std::string lowercaseValue = name;
            std::ranges::transform(lowercaseValue, lowercaseValue.begin(), [](auto character) {
                return std::tolower(character);
            });
            if(keyword == lowercaseValue) {
                name.insert(0, "_");
                break;
            }
        }
    };

    sanitizeKeyword(cppKeywords);
    sanitizeKeyword(kvasirKeywords);

    if(!name.empty()) {
        if(std::isdigit(name.front()) != 0) { name.insert(0, "_"); }
    }

    for(auto& character : name) {
        if(std::isspace(character) != 0) { character = '_'; }
    }

    name = removePercentS(name);

    auto position = name.find("__");
    while(position != std::string::npos) {
        name.erase(position, 1);
        position = name.find("__");
    }

    return name;
}

std::string sanitizeDescription(std::string description) noexcept {
    auto replace = [&](std::string_view needle) {
        while(true) {
            auto position = std::ranges::search(description, needle).begin();
            if(position == description.end()) { break; }
            auto erasePosition
              = description.erase(position,
                                  std::next(position, static_cast<int>(needle.size()) - 1));
            *erasePosition = ' ';
        }
    };
    replace("\n");
    replace("\r");
    replace("\t");
    replace("\\n");
    replace("\\r");
    replace("\\t");
    replace("  ");
    if(description.starts_with(' ')) { description.erase(0, 1); }
    if(description.ends_with(' ')) {
        description.erase(std::next(description.end(), -1), description.end());
    }

    return description;
}

std::optional<InstanceName> splitInstanceName(std::string_view name,
                                              bool             letterInstances) {
    if(letterInstances) {
        if(name.size() < 2 || name.back() < 'A' || name.back() > 'Z') { return std::nullopt; }
        return InstanceName{std::string{name.substr(0, name.size() - 1)},
                            static_cast<std::uint64_t>(name.back() - 'A')};
    }
    auto const firstDigit = name.find_last_not_of("0123456789") + 1;
    if(firstDigit == 0 || firstDigit == name.size()) { return std::nullopt; }
    return InstanceName{std::string{name.substr(0, firstDigit)},
                        std::stoull(std::string{name.substr(firstDigit)})};
}

std::vector<InstanceGroup> groupInstances(std::string const&       baseName,
                                          std::uint64_t            baseAddress,
                                          std::vector<DerivedRef>& derived) {
    std::vector<DerivedRef*> members;
    for(auto& derivedRef : derived) {
        if(derivedRef.base != baseName) { continue; }
        if(derivedRef.processed) {
            throw std::runtime_error("found derived peripheral multiple times " + derivedRef.name);
        }
        members.push_back(&derivedRef);
    }
    if(members.empty()) { return {}; }

    // Port letters only when there are no digits and every name is the same stem plus a capital.
    bool const letterInstances = [&] {
        if(splitInstanceName(baseName, false)) { return false; }
        auto const base = splitInstanceName(baseName, true);
        return base && std::ranges::all_of(members, [&](DerivedRef const* member) {
                   auto const instance = splitInstanceName(member->name, true);
                   return instance && instance->stem == base->stem;
               });
    }();
    auto const base
      = splitInstanceName(baseName, letterInstances).value_or(InstanceName{baseName, 0});

    std::vector<InstanceGroup> groups{
      {base.stem, {AddressType{base.index, baseAddress}}}
    };
    for(auto* member : members) {
        auto const instance = splitInstanceName(member->name, letterInstances);
        if(!instance) {
            std::print(
              stderr,
              "peripheral {} derives from {} but its name has no instance number: left out\n",
              member->name,
              baseName);
            member->processed = true;   // said: not "have not found" once more
            continue;
        }
        member->processed = true;
        auto group        = std::ranges::find(groups, instance->stem, &InstanceGroup::stem);
        if(group == groups.end()) {
            groups.push_back({instance->stem, {}});
            group = std::prev(groups.end());
        }
        group->instances.push_back(AddressType{instance->index, member->address});
    }
    if(groups.size() == 1 && groups.front().instances.size() == 1) { return {}; }
    return groups;
}

void nameClashingGroups(std::vector<Peripheral>&        peripherals,
                        std::vector<MergedGroup> const& merged) {
    auto const nameCount = [&](std::string const& name) {
        return std::ranges::count(peripherals, name, &Peripheral::name);
    };
    std::vector<std::string> newNames;
    for(auto const& [index, instances] : merged) {
        auto const& name = peripherals[index].name;
        if(nameCount(name) < 2) {
            newNames.push_back(name);
            continue;
        }
        std::vector<std::uint64_t> numbers;
        std::ranges::transform(instances, std::back_inserter(numbers), &AddressType::index);
        std::ranges::sort(numbers);
        std::string newName = name;
        for(auto const number : numbers) {
            newName += std::format("{}{}", newName == name ? "" : "_", number);
        }
        newNames.push_back(std::move(newName));
    }
    for(std::size_t i = 0; i < merged.size(); ++i) {
        peripherals[merged[i].index].name = newNames[i];
    }
}

void stripPeripheralPrefixes(std::vector<Peripheral>& peripherals) {
    for(auto& peripheralRef : peripherals) {
        auto pname = peripheralRef.name + "_";

        auto removePeripheral = [&](std::string& strRef) {
            if(strRef.starts_with(pname) && strRef.size() > pname.size()) {
                strRef.erase(0, pname.size());
                // RTC_1 -> 1 is no identifier: keep the underscore, as sanitizeName does for
                // names that start with a digit.
                if(std::isdigit(static_cast<unsigned char>(strRef.front())) != 0) {
                    strRef.insert(strRef.begin(), '_');
                }
            }
        };

        auto removePeripheralReg = [&](Register& reg) {
            removePeripheral(reg.name);
            for(auto& fieldRef : reg.fields) {
                removePeripheral(fieldRef.name);
                for(auto& valueRef : fieldRef.values) { removePeripheral(valueRef.name); }
            }
        };
        for(auto& registerRef : peripheralRef.registers) { removePeripheralReg(registerRef); }
        for(auto& registerGroup : peripheralRef.registerGroups) {
            for(auto& registerRef : registerGroup.registers) { removePeripheralReg(registerRef); }
        }
    }
}

void renameGroupMembers(std::vector<Peripheral>& peripherals) {
    auto const upperName = [](std::string name) {
        std::ranges::transform(name, name.begin(), [](char c) {
            return static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
        });
        return name;
    };
    for(auto& peripheralRef : peripherals) {
        for(auto& registerGroup : peripheralRef.registerGroups) {
            for(auto& registerRef : registerGroup.registers) {
                if(upperName(registerRef.name) == upperName(registerGroup.name)) {
                    registerRef.name += '_';
                }
            }
        }
    }
}

}   // namespace Svd
