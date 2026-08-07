# Regenerate external test fixtures for specification-first EDA.
#
# This script is intentionally manual and is not run by package tests. It uses
# verified public source archives and computes expected outputs with simple base
# R code only. The schema output is a regression projection because its
# classifier mirrors historical production logic; it is not independent
# compatibility evidence. Do not use the package under test here.

fixture_sources <- list(
  medicaldata = list(
    version = "0.2.0",
    url = "https://cran.r-project.org/src/contrib/medicaldata_0.2.0.tar.gz",
    sha256 = "56dab0c6078e6f9a9f183427a4481c5497e5d107b795bf965cc7ce4ac4c39236"
  ),
  palmerpenguins = list(
    version = "0.1.1",
    url = "https://cran.r-project.org/src/contrib/palmerpenguins_0.1.1.tar.gz",
    sha256 = "2a40d48ba6c7978fdf2a6daf647ccb39cd17590680138931d11194d3dd1a30b4"
  )
)

fixture_sha256 <- c(
  blood_storage = "e3a1c6b83de9ddae8380ef2a92ce995fe927c5a176c589039d8b6089dae812b9",
  penguins_raw = "a634e85f0676c74c4cd73f94ff8cbf9ec12540d01797434cf1fd0ba8d9af663f"
)

sha256_file <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste0(openssl::sha256(connection))
}

verify_sha256 <- function(path, expected, label) {
  actual <- sha256_file(path)
  if (!identical(actual, expected)) {
    stop(
      label,
      " SHA-256 mismatch; expected ",
      expected,
      " but found ",
      actual,
      ". No committed fixture was overwritten.",
      call. = FALSE
    )
  }
  invisible(path)
}

prepare_fixture_sources <- function() {
  work_dir <- tempfile("episcout-fixture-sources-")
  source_library <- file.path(work_dir, "library")
  dir.create(source_library, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  archives <- character(length(fixture_sources))
  names(archives) <- names(fixture_sources)
  for (package in names(fixture_sources)) {
    source <- fixture_sources[[package]]
    archive <- file.path(work_dir, basename(source$url))
    utils::download.file(source$url, archive, mode = "wb", quiet = TRUE)
    verify_sha256(archive, source$sha256, paste(package, "source archive"))
    archives[[package]] <- archive
  }

  utils::install.packages(
    unname(archives),
    lib = source_library,
    repos = NULL,
    type = "source",
    dependencies = FALSE,
    quiet = TRUE
  )

  for (package in names(fixture_sources)) {
    actual_version <- utils::packageDescription(
      package,
      lib.loc = source_library,
      fields = "Version"
    )
    if (!identical(actual_version, fixture_sources[[package]]$version)) {
      stop("Unexpected installed source version for ", package, call. = FALSE)
    }
  }

  blood_env <- new.env(parent = emptyenv())
  utils::data(
    "blood_storage",
    package = "medicaldata",
    lib.loc = source_library,
    envir = blood_env
  )
  loadNamespace(
    "palmerpenguins",
    lib.loc = source_library
  )
  penguins_raw <- getExportedValue("palmerpenguins", "penguins_raw")
  unloadNamespace("palmerpenguins")

  list(
    blood_storage = blood_env$blood_storage,
    penguins_raw = penguins_raw,
    medicaldata_license = readLines(
      file.path(source_library, "medicaldata", "LICENSE"),
      warn = FALSE
    )
  )
}

write_verified_csv <- function(data, path, na, expected_sha256, label) {
  candidate <- tempfile(paste0(basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(candidate, force = TRUE), add = TRUE)
  write.csv(data, candidate, row.names = FALSE, na = na)
  verify_sha256(candidate, expected_sha256, label)
  if (!file.copy(candidate, path, overwrite = TRUE)) {
    stop("Unable to replace verified fixture: ", path, call. = FALSE)
  }
  invisible(path)
}

verify_serialized_fixture <- function(data, na, expected_sha256, label) {
  candidate <- tempfile(paste0(label, "-"))
  on.exit(unlink(candidate, force = TRUE), add = TRUE)
  write.csv(data, candidate, row.names = FALSE, na = na)
  verify_sha256(candidate, expected_sha256, label)
}

write_fixture_checksums <- function(directory) {
  files <- sort(list.files(directory, full.names = TRUE))
  files <- files[basename(files) != "CHECKSUMS.sha256"]
  hashes <- vapply(files, sha256_file, character(1))
  writeLines(
    paste(hashes, basename(files), sep = "  "),
    file.path(directory, "CHECKSUMS.sha256")
  )
}

source_objects <- prepare_fixture_sources()

if (!identical(
  source_objects$medicaldata_license,
  c("YEAR: 2021", "COPYRIGHT HOLDER: medicaldata authors")
)) {
  stop("Unexpected medicaldata licence notice; no fixture was overwritten.", call. = FALSE)
}
verify_serialized_fixture(
  source_objects$blood_storage,
  na = "",
  expected_sha256 = fixture_sha256[["blood_storage"]],
  label = "blood_storage fixture"
)
verify_serialized_fixture(
  source_objects$penguins_raw,
  na = "NA",
  expected_sha256 = fixture_sha256[["penguins_raw"]],
  label = "penguins_raw fixture"
)

fixture_observed_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("datetime")
  }
  if (inherits(x, "Date")) {
    return("date")
  }
  if (is.factor(x)) {
    return("categorical")
  }
  if (is.numeric(x) || is.integer(x)) {
    return("numeric")
  }
  if (is.character(x)) {
    return("text")
  }
  if (is.logical(x)) {
    return("binary")
  }
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
  identifier <- spec$role %in% c("id", "identifier")
  names_to_summarise <- spec$name[
    spec$type %in% c("numeric", "integer") & !identifier
  ]
  rows <- lapply(names_to_summarise, function(name) {
    values <- data[[name]]
    observed <- values[!is.na(values)]
    data.frame(
      name = name,
      n = length(values),
      n_missing = sum(is.na(values)),
      mean = signif(mean(observed), 13L),
      sd = signif(stats::sd(observed), 13L),
      median = signif(stats::median(observed), 13L),
      min = signif(min(observed), 13L),
      max = signif(max(observed), 13L),
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
  identifier <- spec$role %in% c("id", "identifier")
  data.frame(
    name = spec$name,
    type = spec$type,
    layer_geom = ifelse(identifier, NA_character_, "GeomCol"),
    layer_stat = ifelse(identifier, NA_character_, "StatIdentity"),
    stringsAsFactors = FALSE
  )
}

fixture_dir <- file.path("tests", "testthat", "fixtures", "blood_storage")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

blood_storage <- source_objects$blood_storage
medicaldata_source <- fixture_sources$medicaldata

source_lines <- c(
  "# blood_storage fixture provenance",
  "",
  "## Source",
  "",
  "- Dataset: `blood_storage`",
  "- Source package: `medicaldata`",
  paste0("- Source package version used for fixture generation: ", medicaldata_source$version),
  paste0("- Canonical source archive: ", medicaldata_source$url),
  paste0("- Source archive SHA-256: `", medicaldata_source$sha256, "`"),
  paste0("- Serialized fixture SHA-256: `", fixture_sha256[["blood_storage"]], "`"),
  paste0("- Observations: ", nrow(blood_storage)),
  paste0("- Variables: ", ncol(blood_storage)),
  "- Licence: MIT (`LICENSE.medicaldata` reproduces the package notice)",
  "- Redistribution basis: the fixture is an exact serialization of the dataset distributed in the MIT-licensed `medicaldata` source package.",
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
  "- `blood_storage_spec.csv`: manually reviewed fixture data dictionary for the specification-first EDA workflow.",
  "- `expected_schema.csv`: generator-produced regression projection of the historical presence and observed-class fields; it is not independent evidence of type compatibility.",
  "- `expected_missing.csv`: independently computed expected missingness result for the unmodified fixture data.",
  "- `LICENSE.medicaldata`: source-package MIT copyright notice.",
  "- `CHECKSUMS.sha256`: offline drift guard for every committed file in this fixture family.",
  "",
  "## Extraction and transformation",
  "",
  "The verified source archive is installed into a temporary library and `medicaldata::blood_storage` is loaded from that isolated installation. The object is serialized with `write.csv(row.names = FALSE, na = \"\")`. No row, column or value is transformed or excluded.",
  "",
  "## Regeneration",
  "",
  "Run from the repository root:",
  "",
  "```sh",
  "scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R",
  "```",
  "",
  "The script computes expected outputs with base R and does not call the package under test. The generated schema classifier mirrors the package's historical classifier, so hand-authored tests provide the independent evidence for type compatibility."
)

write_verified_csv(
  blood_storage,
  file.path(fixture_dir, "blood_storage.csv"),
  na = "",
  expected_sha256 = fixture_sha256[["blood_storage"]],
  label = "blood_storage fixture"
)
writeLines(source_lines, file.path(fixture_dir, "SOURCE.md"))
writeLines(
  source_objects$medicaldata_license,
  file.path(fixture_dir, "LICENSE.medicaldata")
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
write_fixture_checksums(fixture_dir)

penguins_fixture_dir <- file.path("tests", "testthat", "fixtures", "penguins_raw")
dir.create(penguins_fixture_dir, recursive = TRUE, showWarnings = FALSE)

penguins_raw <- source_objects$penguins_raw
palmerpenguins_source <- fixture_sources$palmerpenguins

penguins_source_lines <- c(
  "# penguins_raw fixture provenance",
  "",
  "## Source",
  "",
  "- Dataset: `penguins_raw`",
  "- Source package: `palmerpenguins`",
  paste0("- Source package version used for fixture generation: ", palmerpenguins_source$version),
  paste0("- Canonical source archive: ", palmerpenguins_source$url),
  paste0("- Source archive SHA-256: `", palmerpenguins_source$sha256, "`"),
  paste0("- Serialized fixture SHA-256: `", fixture_sha256[["penguins_raw"]], "`"),
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
  "- `expected_schema.csv`: generator-produced regression projection of the historical presence and observed-class fields; it is not independent evidence of type compatibility.",
  "- `expected_missing.csv`: independently computed missingness contract.",
  "- `expected_summary_numeric.csv`: independently computed numeric summaries.",
  "- `expected_summary_categorical.csv`: independently computed categorical summaries.",
  "- `expected_plot_inventory.csv`: independently defined non-visual compact-plot dispatch, including named no-plot rows for both reviewed identifiers.",
  "- `CHECKSUMS.sha256`: offline drift guard for every committed file in this fixture family.",
  "",
  "## Extraction and transformation",
  "",
  "The verified source archive is installed into a temporary library and `palmerpenguins::penguins_raw` is loaded from that isolated installation. The object is serialized with `write.csv(row.names = FALSE, na = \"NA\")`. No row, column or value is transformed or excluded.",
  "",
  "## Regeneration",
  "",
  "Run from the repository root:",
  "",
  "```sh",
  "scripts/rscript_env_caller.R data-raw/test-fixtures/make_external_fixtures.R",
  "```",
  "",
  "The script computes expected outputs with base R and does not call the package under test. The generated schema classifier mirrors the package's historical classifier, so hand-authored tests provide the independent evidence for type compatibility."
)

write_verified_csv(
  penguins_raw,
  file.path(penguins_fixture_dir, "penguins_raw.csv"),
  na = "NA",
  expected_sha256 = fixture_sha256[["penguins_raw"]],
  label = "penguins_raw fixture"
)
writeLines(
  penguins_source_lines,
  file.path(penguins_fixture_dir, "SOURCE.md")
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
  na = "NA"
)
write_fixture_checksums(penguins_fixture_dir)
