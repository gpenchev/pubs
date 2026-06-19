# make_reference.R
# Creates reference.pptx for the Quarto presentation.
# Uses officer to build a minimal PPTX with correct layouts and 16:9 canvas.
# Run once: Rscript presentation/make_reference.R

library(officer)
library(here)

out_path <- here("presentation", "reference.pptx")

# ── Colour definitions ────────────────────────────────────────────────────────
BG      <- "#1C2333"
SURFACE <- "#243044"
TEXT    <- "#E8EDF5"
TEXT2   <- "#9AABB8"
ACCENT1 <- "#4A90D9"
ACCENT2 <- "#52B788"
ACCENT3 <- "#E8863A"
ACCENT4 <- "#D64545"

# ── Create presentation object ────────────────────────────────────────────────
prs <- read_pptx()   # starts from built-in default

# ── Apply theme colours to all slide layouts ──────────────────────────────────
# officer cannot directly edit the slide master XML colour scheme easily,
# but we can set slide background and text on each layout via ph_with.
# The most reliable approach: add a blank slide, set background, save as reference.

# Get layout names
layouts <- layout_summary(prs)
cat("Available layouts:\n")
print(layouts)

# Add one slide per required layout to "prime" the reference doc
# Quarto PPTX uses: "Title Slide", "Title and Content", "Two Content", "Blank"

# We'll modify the XML directly for the theme colours
# First: export the default pptx to inspect it
tmp <- tempfile(fileext = ".pptx")
print(prs, target = tmp)

# Re-read and modify slide master background colour
prs2 <- read_pptx(tmp)

# Add a title slide to ensure layout exists
prs2 <- add_slide(prs2, layout = "Title Slide", master = "Office Theme")
prs2 <- ph_with(prs2,
  value     = "Threat Proximity and Defence Spending",
  location  = ph_location_type(type = "ctrTitle"))
prs2 <- ph_with(prs2,
  value     = "[Author] · [Institution] · 2025",
  location  = ph_location_type(type = "subTitle"))

# Add a content slide
prs2 <- add_slide(prs2, layout = "Title and Content", master = "Office Theme")
prs2 <- ph_with(prs2,
  value    = "Slide title",
  location = ph_location_type(type = "title"))

# Add a Two Content slide
tryCatch({
  prs2 <- add_slide(prs2, layout = "Two Content", master = "Office Theme")
  prs2 <- ph_with(prs2,
    value    = "Two column slide",
    location = ph_location_type(type = "title"))
}, error = function(e) message("Two Content layout not found: ", e$message))

# Add a Blank slide
prs2 <- add_slide(prs2, layout = "Blank", master = "Office Theme")

# ── Set canvas to 16:9 widescreen (13.333 x 7.5 inches = 12192000 x 6858000 EMU) ──
# officer 0.7.x exposes the presentation XML via $presentation$get()
sz_node <- xml2::xml_find_first(prs2$presentation$get(),
                                '//*[local-name()="sldSz"]')
if (!inherits(sz_node, "xml_missing")) {
  xml2::xml_set_attr(sz_node, "cx", "12192000")   # 13.333 in × 914400
  xml2::xml_set_attr(sz_node, "cy", "6858000")    # 7.5   in × 914400
  xml2::xml_set_attr(sz_node, "type", "screen16x9")
  cat("Canvas set to 16:9 (13.333 × 7.5 in)\n")
} else {
  cat("WARNING: sldSz node not found — canvas NOT updated\n")
}

# Save
print(prs2, target = out_path)
cat("reference.pptx written to:", out_path, "\n")
cat("\nNOTE: The reference.pptx uses the default Office Theme colours (light).\n")
cat("For a true dark theme, open", out_path, "in LibreOffice Impress or\n")
cat("PowerPoint and:\n")
cat("  1. Slide Master view\n")
cat("  2. Set background fill to #1C2333\n")
cat("  3. Set all text colours to #E8EDF5\n")
cat("  4. Save and close\n")
cat("\nAlternatively the slides render correctly with the light theme —\n")
cat("figure PNGs already have dark backgrounds and will display correctly\n")
cat("as embedded images even on a light PPTX background.\n")
