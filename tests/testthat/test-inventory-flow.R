library(dplyr)
library(tibble)
library(purrr)

source("../../R/mod_inventory.R")

# Synthetic records shaped like the REACT site labels: the site lives in
# characters 2-3 of the participant id (rML01 -> ML).
.inv_specimens <- function() {
  tibble::tibble(
    os_identifier = sprintf("OS-%03d", 1:5),
    participant_id = c("rML01", "aML001", "aEM037", "rML02", "rML03"),
    specimen_label = c("rML01PRaD00V1", "aML001PRaV1", "aEM037PRaV1",
                       "rML02PRaD00V1", "rML03PRaD00V1"),
    cp_short_title = "REACT",
    project_id = "REACT",
    type = "Cryopreserved Cells",
    custom_organism = c("Enterococcus faecium", "Enterococcus faecalis",
                        "Enterococcus faecium", "Enterococcus spp",
                        "Escherichia coli"),
    custom_mdro = c("VRE", "VRE", "VRE", NA_character_, "ESBL"),
    custom_parent_specimen_type = "Perirectal eSwab",
    custom_collection_date = as.Date("2026-01-01"),
    collection_dt = as.POSIXct("2026-01-01", tz = "UTC")
  )
}

test_that("OpenSpecimen-only rows resolve the same site as the linked path", {
  sp <- .inv_specimens()
  out <- os_enterococcus_flow_rows(sp)

  # rML/aML participants belong to RML Specialty Hospital, not to the
  # collection protocol. Falling back to "REACT" is what hid these records
  # whenever an analyst filtered the Sankey by site.
  expect_true(all(out$flow_site != "REACT"))
  expect_setequal(
    unique(out$flow_site),
    unique(canonical_flow_site(c("RML Specialty Hospital",
                                 "Emory Long Term Acute Care Hospital")))
  )
})

test_that("the site derived for an OpenSpecimen row matches the linked derivation", {
  sp <- .inv_specimens()
  os_site <- os_specimen_site_label(sp$participant_id, sp$specimen_label,
                                    sp$cp_short_title)

  dict <- inventory_site_dictionary()
  linked_codes <- purrr::pmap_chr(
    list(sp$participant_id, NA_character_, sp$specimen_label,
         NA_character_, sp$cp_short_title),
    derive_site_code
  )
  linked_site <- dict$site_label[match(linked_codes, dict$site_code)]

  expect_equal(os_site, linked_site)
})

test_that("a site filter keeps the OpenSpecimen-only rows for that site", {
  sp <- .inv_specimens()
  site <- canonical_flow_site("RML Specialty Hospital")
  filtered <- os_enterococcus_flow_rows(
    sp,
    input = list(f_site = site, f_study = "All", f_specimen = "All",
                 f_mdro = "All", f_species = "All", f_range = NULL)
  )
  expect_gt(nrow(filtered), 0L)
  expect_true(all(filtered$flow_site == site))
})

test_that("every Enterococcus species is included, not two hand-listed ones", {
  sp <- .inv_specimens()
  out <- os_enterococcus_flow_rows(sp)

  expect_setequal(
    out$flow_species,
    c("Enterococcus faecium", "Enterococcus faecalis",
      "Enterococcus faecium", "Enterococcus spp")
  )
  # Non-Enterococcus records are still excluded.
  expect_false(any(grepl("coli", out$flow_species)))
})

test_that("a genus-only record still gets a species label", {
  sp <- .inv_specimens()
  sp$custom_organism[4] <- NA_character_
  out <- os_enterococcus_flow_rows(sp)
  # An organism recorded as NA is not Enterococcus as far as the filter knows.
  expect_false(any(is.na(out$flow_species)))
})

test_that("non-cryopreserved records are still excluded", {
  sp <- .inv_specimens()
  sp$type <- "Perirectal eSwab"
  expect_equal(nrow(os_enterococcus_flow_rows(sp)), 0L)
})

test_that("os_enterococcus_flow_rows tolerates empty and missing input", {
  expect_equal(nrow(os_enterococcus_flow_rows(NULL)), 0L)
  expect_equal(nrow(os_enterococcus_flow_rows(tibble::tibble())), 0L)
})
