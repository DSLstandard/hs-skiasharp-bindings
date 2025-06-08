#pragma once

#include "core/SkImageInfo.h"
#include "c/sk_types.h"

extern "C" {
    int hsskia_SkColorTypeBytesPerPixel(sk_colortype_t ct) {
        return SkColorTypeBytesPerPixel((SkColorType) ct);
    }

    bool hsskia_SkColorTypeIsAlwaysOpaque(sk_colortype_t ct) {
        return SkColorTypeIsAlwaysOpaque((SkColorType) ct);
    }
}