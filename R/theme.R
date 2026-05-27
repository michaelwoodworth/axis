# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/theme.R
# AXIS_THEME and MDRO_COLORS — sourced by app.R before the UI is built.
# ─────────────────────────────────────────────────────────────────────────────

# Categorical MDRO palette (copy from HANDOFF.md §6)
MDRO_COLORS <- c(
  "MRSA"      = "#c2410c",
  "VRE"       = "#7c3aed",
  "ESBL"      = "#0891b2",
  "CRE"       = "#dc2626",
  "CRAB"      = "#ca8a04",
  "CRPA"      = "#15803d",
  "MDRP"      = "#15803d",
  "MDRA"      = "#a16207",
  "C. auris"  = "#be185d",
  "Non-MDRO"  = "#6b7280",
  "MDRO positive (unspecified)" = "#64748b"
)

# bslib theme (HANDOFF.md §6)
AXIS_THEME <- bslib::bs_theme(
  version      = 5,
  bg           = "#f7f7f5",
  fg           = "#1a1d24",
  primary      = "#1f3a5f",
  secondary    = "#d4a017",
  success      = "#15803d",
  warning      = "#b45309",
  danger       = "#b91c1c",
  base_font    = bslib::font_google("IBM Plex Sans"),
  heading_font = bslib::font_google("IBM Plex Serif"),
  code_font    = bslib::font_google("IBM Plex Mono"),
  font_scale   = 0.95
) |>
  # Fine-tune card appearance to match design tokens
  bslib::bs_add_rules("
    .card {
      border: 1px solid #e8e6e0 !important;
      border-radius: 10px !important;
      box-shadow: 0 1px 3px rgba(0,0,0,.06) !important;
    }
    .navbar {
      border-bottom: 1px solid #e8e6e0;
    }
    .nav-link {
      font-size: 13.5px;
      font-weight: 500;
    }
    .bslib-value-box .value-box-value {
      font-family: 'IBM Plex Mono', monospace;
    }
  ")
