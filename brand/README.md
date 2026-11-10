# ZIO Prelude — logo system

One solid Z with its base bar in the accent colour. The base bar leads from the
left: it is the first thing, the prelude, and everything above it builds on it.

In the full logo the mark is the Z of "ZIO". It stands at the cap height of the
wordmark, on the baseline, followed by "IO Prelude" in Bold.

## Files

| File | Use |
| --- | --- |
| `zio-prelude-logo.svg` | Primary horizontal logo, light backgrounds. Transparent. |
| `zio-prelude-logo-on-dark.svg` | Same logo, lighter ink and accent for dark surfaces. Transparent. |
| `zio-prelude-logo-mono-dark.svg` | Single colour (espresso), light background |
| `zio-prelude-logo-mono-white.svg` | Single colour (white), dark background |
| `zio-prelude-mark.svg` | Mark on a rounded tile: GitHub org avatar, Discord icon, app icon |
| `zio-prelude-mark-favicon.svg` | Same tile, Z enlarged for 16–48 px |
| `zio-prelude-mark-bare.svg` | The Z alone, no tile, for light backgrounds |
| `zio-prelude-mark-bare-on-dark.svg` | The Z alone, no tile, for dark backgrounds |
| `zio-prelude-mark-knockout.svg` | One colour, the Z is a real hole in the tile |
| `zio-prelude-social-github.svg` / `.png` | GitHub repo social preview, 1280 × 640, light |
| `zio-prelude-social-github-dark.svg` / `.png` | Same, dark |
| `zio-prelude-social-og.svg` / `.png` | Open Graph (Slack, Discord, Facebook, LinkedIn, X), 1200 × 630, light |
| `zio-prelude-social-og-dark.svg` / `.png` | Same, dark |
| `zio-prelude-brand-sheet.svg` | Reference sheet: variants, palette, construction |

All type is converted to outlines, so nothing depends on a webfont loading. The
knockout file uses a mask, so whatever is behind the logo shows through the Z.

There is no stacked lockup. The mark is already the Z of the name, so stacking it
over "ZIO Prelude" would say Z twice. For square spaces use `zio-prelude-mark.svg`.

## Palette

| Name | Hex | Role |
| --- | --- | --- |
| Espresso | `#2B1D1A` | Mark body and text on light; the tile |
| Terracotta | `#C8553D` | The base bar on light (4.4:1 on white: bar, rules and large text only) |
| Terracotta light | `#E57A5F` | The base bar on dark |
| Umber | `#5E4F49` | secondary text on light |
| Paper | `#F6F1EC` | Panels and backgrounds on light; the mark on dark |
| Night | `#1F1512` | Page background on dark |
| Warm muted | `#B7A59C` | Secondary text on dark |

## Construction

The Z is drawn on a 120-unit grid: a top bar, a diagonal and a base bar, with a
4-unit round-joined outline. Its visible box is 100 × 96 units.

In the full logo the Z is scaled so its height equals the cap height of the
wordmark (700 units at 1000 units per em). It sits on the baseline, with 0.08 em
of space before "IO". Tracking is −0.01 em. Don't re-space it.

Clear space is one base-bar height on every side of the mark, and the same
distance around the full logo, measured from the Z.

## Sizing

The mark alone holds down to 16 px; use `zio-prelude-mark-favicon.svg` below 48 px.
The full logo holds down to a Z that is 16 px tall (23 px type). Below that, use
the mark alone.

## Don't

- Stretch, squash or rotate the logo.
- Recolour the base bar. In single-colour files it takes the same colour as the rest.
- Add a gradient, shadow or outline.
- Put the mark beside the full name. It is already the Z.
- Place the logo on a busy image. Use a flat panel behind it.
- Rebuild the wordmark by typing it. Use the outlined files.

## Social preview

Light and dark cards share one layout: lockup at the top left, the headline,
the description, the address at the bottom right, and the base bar along the bottom edge from the left.
The Z is enlarged and bleeds off the right edge at low contrast.

Upload the PNG. GitHub's **Settings → General → Social preview** field does not
accept SVG, and neither do most link unfurlers. If a platform shows only one mode,
use the light card.

The cards are generated from one layout, so edit the copy in the SVG and re-export
if the tagline changes. The headline carries no end punctuation.

## Typeface

Space Grotesk Bold for "IO Prelude". SIL Open Font License, so it
also works for docs and site headings. Code uses JetBrains Mono.
