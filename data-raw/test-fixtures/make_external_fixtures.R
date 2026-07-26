# Regenerate external test fixtures for specification-first EDA.
#
# This script is intentionally manual and is not run by package tests. It uses
# public package datasets as external sources and computes expected outputs with
# simple base R code only. Do not use the package under test here.

fixture_source_packages <- c("medicaldata", "palmerpenguins")
missing_source_packages <- fixture_source_packages[
  !vapply(fixture_source_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_source_packages) > 0) {
  stop(
    "Packages required to regenerate external fixtures are missing: ",
    paste(missing_source_packages, collapse = ", "),
    ". Install them before running this script.",
    call. = FALSE
  )
}

fixture_observed_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.factor(x)) return("categorical")
  if (is.numeric(x) || is.integer(x)) return("numeric")
  if (is.character(x)) return("text")
  if (is.logical(x)) return("binary")
  class(x)[1]
}

make_expected_schema <- function(data, spec) {
  data.frame(
    name = spec$name,
    expected_type = spec$type,
    observed_type = unname(vapply(data[spec$name], fixture_observed_type, character(1))),
    expected_present = TRUE,
    observed_present = spec$name %in% names(data),
    status = ifelse(spec$name %in% names(data), "present", "missing"),
    stringsAsFactors = FALSE
  )
}

make_expected_missing <- function(data, spec) {
  data.frame(
    name = spec$name,
    n = nrow(data),
    n_missing = unname(vapply(data[spec$name], function(x) sum(is.na(x)), integer(1))),
    p_missing = unname(vapply(data[spec$name], function(x) mean(is.na(x)), numeric(1))),
    stringsAsFactors = FALSE
  )
}

make_expected_numeric_summary <- function(data, spec) {
  names_to_summarise <- spec$name[spec$type %in% c("numeric", "integer")]
  rows <- lapply(names_to_summarise, function(name) {
    values <- data[[name]]
    observed <- values[!is.na(values)]
    data.frame(
      name = name,
      n = length(values),
      n_missing = sum(is.na(values)),
      mean = mean(observed),
      sd = stats::sd(observed),
      median = stats::median(observed),
      min = min(observed),
      max = max(observed),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

split_fixture_levels <- function(value) {
  trimws(strsplit(value, ";", fixed = TRUE)[[1]])
}

make_expected_categorical <- function(data, spec) {
  categorical_spec <- spec[spec$type %in% c("categorical", "binary"), , drop = FALSE]
  rows <- lapply(seq_len(nrow(categorical_spec)), function(i) {
    name <- categorical_spec$name[[i]]
    values <- as.character(data[[name]])
    observed <- values[!is.na(values)]
    levels <- split_fixture_levels(categorical_spec$levels[[i]])
    counts <- vapply(levels, function(level) sum(observed == level), integer(1))
    data.frame(
      name = name,
      level = levels,
      n = as.integer(counts),
      p = as.numeric(counts) / length(values),
      p_observed = as.numeric(counts) / length(observed),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

make_expected_plot_inventory <- function(spec) {
  binned <- spec$type %in% c("numeric", "integer", "date", "datetime")
  data.frame(
    name = spec$name,
    type = spec$type,
    layer_geom = rep("GeomBar", nrow(spec)),
    layer_stat = ifelse(binned, "StatBin", "StatCount"),
    stringsAsFactors = FALSE
  )
}

fixture_dir <- file.path("tests", "testthat", "fixtures", "blood_storage")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

medicaldata_version <- as.character(utils::packageVersion("medicaldata"))

data("blood_storage", package = "medicaldata", envir = environment())
blood_storage <- get("blood_storage", envir = environment())


source_lines <- c(
  "# blood_storage fixture provenance",
  "",
  "## Source",
  "",
  "- Dataset: `blood_storage`",
  "- Source package: `medicaldata`",
  paste0("- Source package version used for fixture generation: ", medicaldata_version),
  paste0("- Observations: ", nrow(blood_storage)),
  paste0("- Variables: ", ncol(blood_storage)),
  "- Study type: retrospective cohort",
  "- Clinical area: prostate cancer recurrence after perioperative transfusion",
  "",
  "## Citation from source documentation",
  "",
  "Cata et al. Blood Storage Duration and Biochemical Recurrence of Cancer after",
  "Radical Prostatectomy. Mayo Clinic Proceedings. 2011;86(2):120-127.",
  "",
  "## Fixture files",
  "",
  "- `blood_storage.csv`: pinned data exported from `medicaldata::blood_storage`.",
  "- `blood_storage_spec.csv`: manually reviewed fixture data dictionary for the",
  "  specification-first EDA workflow.",
  "- `expected_schema.csv`: independently computed expected schema result for the",
  "  unmodified fixture data.",
  "- `expected_missing.csv`: independently computed expected missingness result for",
  "  the unmodified fixture data.",
  "",
  "## Regeneration",
  "",
  "Run from the repository root:",
  "",
  "```sh",
  "scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R",
  "```",
  "",
  "The script computes expected outputs with base R and does not call the",
  "package under test."
)
writeLines(source_lines, file.path(fixture_dir, "SOURCE.md"))

write.csv(
  blood_storage,
  file.path(fixture_dir, "blood_storage.csv"),
  row.names = FALSE,
  na = ""
)

spec <- data.frame(
  name = c(
    "RBC.Age.Group", "Median.RBC.Age", "Age", "AA", "FamHx", "PVol",
    "TVol", "T.Stage", "bGS", "BN+", "OrganConfined", "PreopPSA",
    "PreopTherapy", "Units", "sGS", "AnyAdjTherapy", "AdjRadTherapy",
    "Recurrence", "Censor", "TimeToRecurrence"
  ),
  label = c(
    "Red blood cell storage age group",
    "Median red blood cell storage age",
    "Age at prostatectomy",
    "African American race indicator",
    "Family history of prostate cancer indicator",
    "Prostate volume",
    "Tumour volume category",
    "Clinical tumour stage category",
    "Biopsy Gleason score category",
    "Bladder neck involvement indicator",
    "Organ-confined disease indicator",
    "Preoperative prostate-specific antigen",
    "Preoperative therapy indicator",
    "Number of transfused red blood cell units",
    "Surgical Gleason score category",
    "Any adjuvant therapy indicator",
    "Adjuvant radiation therapy indicator",
    "Biochemical prostate cancer recurrence indicator",
    "Censoring indicator",
    "Time to recurrence or censoring"
  ),
  type = c(
    "categorical", "numeric", "numeric", "binary", "binary", "numeric",
    "categorical", "categorical", "categorical", "binary", "binary",
    "numeric", "binary", "integer", "categorical", "binary", "binary",
    "binary", "binary", "numeric"
  ),
  role = c(
    "exposure", "exposure", "covariate", "covariate", "covariate",
    "covariate", "covariate", "covariate", "covariate", "covariate",
    "covariate", "covariate", "covariate", "exposure", "covariate",
    "covariate", "covariate", "outcome", "outcome", "outcome"
  ),
  units = c(
    "group", "days", "years", "", "", "mL", "category", "category",
    "category", "", "", "ng/mL", "", "units", "category", "", "", "",
    "", "months"
  ),
  levels = c(
    "1;2;3", "", "", "0;1", "0;1", "", "1;2;3", "1;2", "1;2;3",
    "0;1", "0;1", "", "0;1", "", "1;2;3;4", "0;1", "0;1",
    "0;1", "0;1", ""
  ),
  min = c(1, 10, 38.4, 0, 0, 19.4, 1, 1, 1, 0, 0, 1.3, 0, 1, 1, 0, 0, 0, 0, 0.27),
  max = c(3, 25, 79, 1, 1, 274, 3, 2, 3, 1, 1, 40, 1, 19, 4, 1, 1, 1, 1, 104),
  missing_codes = rep("", 20),
  required = rep(TRUE, 20),
  group = c(
    "transfusion", "transfusion", "demographics", "demographics",
    "history", "clinical", "clinical", "clinical", "clinical", "clinical",
    "clinical", "clinical", "treatment", "transfusion", "clinical",
    "treatment", "treatment", "outcomes", "outcomes", "outcomes"
  ),
  description = c(
    "RBC storage duration exposure group based on terciles.",
    "Median storage age of transfused red blood cells.",
    "Patient age at radical prostatectomy.",
    "Binary indicator for African American race.",
    "Binary indicator for family history of prostate cancer.",
    "Measured prostate volume.",
    "Tumour volume category.",
    "Clinical tumour stage category.",
    "Biopsy Gleason score category.",
    "Binary indicator for bladder neck involvement.",
    "Binary indicator for organ-confined disease.",
    "Preoperative PSA value.",
    "Binary indicator for preoperative therapy.",
    "Number of transfused allogeneic RBC units.",
    "Surgical Gleason score category.",
    "Binary indicator for any adjuvant therapy.",
    "Binary indicator for adjuvant radiation therapy.",
    "Binary indicator for biochemical recurrence.",
    "Binary censoring indicator.",
    "Follow-up time to recurrence or censoring."
  ),
  stringsAsFactors = FALSE
)

write.csv(spec, file.path(fixture_dir, "blood_storage_spec.csv"), row.names = FALSE, na = "")

blood_storage_serialized <- read.csv(
  file.path(fixture_dir, "blood_storage.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
expected_schema <- make_expected_schema(blood_storage_serialized, spec)
write.csv(expected_schema, file.path(fixture_dir, "expected_schema.csv"), row.names = FALSE, na = "")

expected_missing <- make_expected_missing(blood_storage_serialized, spec)
write.csv(expected_missing, file.path(fixture_dir, "expected_missing.csv"), row.names = FALSE, na = "")

penguins_fixture_dir <- file.path("tests", "testthat", "fixtures", "penguins_raw")
dir.create(penguins_fixture_dir, recursive = TRUE, showWarnings = FALSE)

palmerpenguins_version <- as.character(utils::packageVersion("palmerpenguins"))
penguins_raw <- palmerpenguins::penguins_raw

penguins_source_lines <- c(
  "# penguins_raw fixture provenance",
  "",
  "## Source",
  "",
  "- Dataset: `penguins_raw`",
  "- Source package: `palmerpenguins`",
  paste0("- Source package version used for fixture generation: ", palmerpenguins_version),
  paste0("- Observations: ", nrow(penguins_raw)),
  paste0("- Variables: ", ncol(penguins_raw)),
  "- Licence: CC0",
  "- Dataset documentation: https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html",
  "- Package website: https://allisonhorst.github.io/palmerpenguins/",
  "",
  "## Citation",
  "",
  "Gorman KB, Williams TD, Fraser WR (2014). Ecological Sexual Dimorphism and",
  "Environmental Variability within a Community of Antarctic Penguins (Genus",
  "Pygoscelis). PLoS ONE 9(3): e90081.",
  "https://doi.org/10.1371/journal.pone.0090081",
  "",
  "## Fixture files",
  "",
  "- `penguins_raw.csv`: pinned data exported from `palmerpenguins::penguins_raw`.",
  "- `penguins_raw_spec.csv`: manually reviewed EDA data dictionary.",
  "- `expected_schema.csv`: independently computed schema contract.",
  "- `expected_missing.csv`: independently computed missingness contract.",
  "- `expected_summary_numeric.csv`: independently computed numeric summaries.",
  "- `expected_summary_categorical.csv`: independently computed categorical summaries.",
  "- `expected_plot_inventory.csv`: independently defined non-visual plot dispatch.",
  "",
  "## Regeneration",
  "",
  "Run from the repository root:",
  "",
  "```sh",
  "scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R",
  "```",
  "",
  "The script computes expected outputs with base R and does not call the",
  "package under test."
)
writeLines(penguins_source_lines, file.path(penguins_fixture_dir, "SOURCE.md"))

write.csv(
  penguins_raw,
  file.path(penguins_fixture_dir, "penguins_raw.csv"),
  row.names = FALSE,
  na = "NA"
)

penguins_spec <- data.frame(
  name = c(
    "studyName", "Sample Number", "Species", "Region", "Island", "Stage",
    "Individual ID", "Clutch Completion", "Date Egg", "Culmen Length (mm)",
    "Culmen Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Sex",
    "Delta 15 N (o/oo)", "Delta 13 C (o/oo)", "Comments"
  ),
  label = c(
    "Study name", "Sample number", "Penguin species", "Sampling region",
    "Sampling island", "Reproductive stage", "Individual identifier",
    "Clutch completion", "Date egg observed", "Culmen length", "Culmen depth",
    "Flipper length", "Body mass", "Sex", "Delta 15 N isotope ratio",
    "Delta 13 C isotope ratio", "Comments"
  ),
  type = c(
    "categorical", "integer", "categorical", "categorical", "categorical",
    "categorical", "text", "binary", "date", "numeric", "numeric",
    "integer", "integer", "binary", "numeric", "numeric", "text"
  ),
  role = c(
    "metadata", "identifier", "covariate", "covariate", "covariate",
    "covariate", "identifier", "covariate", "covariate", "covariate",
    "covariate", "covariate", "covariate", "covariate", "covariate",
    "covariate", "metadata"
  ),
  units = c(
    "", "", "", "", "", "", "", "", "", "mm", "mm", "mm", "g",
    "", "o/oo", "o/oo", ""
  ),
  levels = c(
    "PAL0708;PAL0809;PAL0910",
    "",
    paste(
      c(
        "Adelie Penguin (Pygoscelis adeliae)",
        "Chinstrap penguin (Pygoscelis antarctica)",
        "Gentoo penguin (Pygoscelis papua)"
      ),
      collapse = ";"
    ),
    "Anvers",
    "Biscoe;Dream;Torgersen",
    "Adult, 1 Egg Stage",
    "",
    "No;Yes",
    "",
    "",
    "",
    "",
    "",
    "FEMALE;MALE",
    "",
    "",
    ""
  ),
  min = c(
    "", "1", "", "", "", "", "", "", "2007-11-09", "32.1", "13.1",
    "172", "2700", "", "7.6322", "-27.01854", ""
  ),
  max = c(
    "", "152", "", "", "", "", "", "", "2009-12-01", "59.6", "21.5",
    "231", "6300", "", "10.02544", "-23.78767", ""
  ),
  missing_codes = rep("", 17),
  required = rep(TRUE, 17),
  group = c(
    "study", "identifiers", "biology", "location", "location", "reproduction",
    "identifiers", "reproduction", "reproduction", "morphology", "morphology",
    "morphology", "morphology", "biology", "isotopes", "isotopes", "metadata"
  ),
  description = c(
    "Sampling expedition in which data were collected.",
    "Continuous sample numbering sequence within the source study.",
    "Penguin species including common and scientific names.",
    "Region of the Palmer LTER sampling grid.",
    "Island near Palmer Station where the sample was collected.",
    "Reproductive stage at sampling.",
    "Source identifier for the sampled individual.",
    "Whether the observed nest had a full clutch of two eggs.",
    "Date the study nest was observed with one egg.",
    "Length of the dorsal ridge of the bill in millimetres.",
    "Depth of the dorsal ridge of the bill in millimetres.",
    "Penguin flipper length in millimetres.",
    "Penguin body mass in grams.",
    "Recorded sex of the sampled penguin.",
    "Ratio of stable nitrogen isotopes 15N to 14N.",
    "Ratio of stable carbon isotopes 13C to 12C.",
    "Additional source comments about sampling or measurements."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  penguins_spec,
  file.path(penguins_fixture_dir, "penguins_raw_spec.csv"),
  row.names = FALSE,
  na = ""
)

penguins_serialized <- read.csv(
  file.path(penguins_fixture_dir, "penguins_raw.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

write.csv(
  make_expected_schema(penguins_serialized, penguins_spec),
  file.path(penguins_fixture_dir, "expected_schema.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  make_expected_missing(penguins_serialized, penguins_spec),
  file.path(penguins_fixture_dir, "expected_missing.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  make_expected_numeric_summary(penguins_serialized, penguins_spec),
  file.path(penguins_fixture_dir, "expected_summary_numeric.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  make_expected_categorical(penguins_serialized, penguins_spec),
  file.path(penguins_fixture_dir, "expected_summary_categorical.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  make_expected_plot_inventory(penguins_spec),
  file.path(penguins_fixture_dir, "expected_plot_inventory.csv"),
  row.names = FALSE,
  na = ""
)
