#pragma once

// An SVD's <device> as the generators' chip: read (read.hpp), analyzed (semantics.hpp), derived
// peripherals grouped into instances, mode clusters made peripherals, names settled (naming.hpp),
// one interrupt per vector (interrupts.hpp).

#include "svd/model.hpp"

#include <pugixml.hpp>
#include <vector>

namespace Svd {

// With the write-only guard's options; the findings (one per unclassified write-only field, each SVD
// peripheral once: derived peripherals share their base's registers) go to `findings` when given.
[[nodiscard]] Chip chipFromSVD(pugi::xml_node const&          device,
                               ConvertOptions const&          options,
                               std::vector<WriteOnlyFinding>* findings);

// The SVD as written: no write-only classification (the opt-out, every option off).
[[nodiscard]] Chip chipFromSVD(pugi::xml_node const& device);

}   // namespace Svd
