linkage_foundation_sources <- function() {
  x <- data.frame(
    record_id = paste0("x", 1:4),
    full_name = c(
      "José Luis Hernández García",
      "María del Carmen de la Cruz",
      "Juan Pérez López",
      "Ana Sofía Torres"
    ),
    birth_date = as.Date(c("1980-05-12", "1975-02-20", "1990-01-01", NA)),
    age = c(46, 51, 36, 30),
    sex = c("M", "F", "M", "F"),
    geography = c("01", "02", "03", "04"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    source_key = paste0("y", 1:5),
    name_text = c(
      "JOSE-LUIS  HERNANDEZ, GARCIA",
      "Maria del Carmen Cruz de la",
      "Juan Perez Lpoez",
      "Juan Perez Lopez",
      "Ana Torres"
    ),
    dob = as.Date(c("1980-05-12", "1975-02-21", "1990-01-01", "1987-07-08", NA)),
    age_years = c(46, 51, 36, 39, 30),
    recorded_sex = c("M", "F", "M", "M", "F"),
    area_code = c("01", "02", "03", "03", "04"),
    stringsAsFactors = FALSE
  )
  list(x = x, y = y)
}

linkage_foundation_spec <- function(max_candidates = 100) {
  blocks <- data.frame(
    pass = c(1L, 1L, 2L),
    x_field = c("geography", "sex", "birth_date"),
    y_field = c("area_code", "recorded_sex", "dob"),
    profile = rep("identity", 3L),
    stringsAsFactors = FALSE
  )
  comparisons <- data.frame(
    comparison = c(
      "name_tokens", "name_jw", "birth_date", "age", "sex", "geography"
    ),
    x_field = c(
      "full_name", "full_name", "birth_date", "age", "sex", "geography"
    ),
    y_field = c(
      "name_text", "name_text", "dob", "age_years", "recorded_sex",
      "area_code"
    ),
    profile = c("latin_sorted", "latin", rep("identity", 4L)),
    method = c(
      "token_jaccard", "jaro_winkler", "date_tolerance",
      "numeric_tolerance", "exact", "exact"
    ),
    parameter = c(0.75, 0.85, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  epi_linkage_spec(
    x_id = "record_id",
    y_id = "source_key",
    profiles = list(
      latin = epi_linkage_text_profile(diacritics = "strip"),
      latin_sorted = epi_linkage_text_profile(
        diacritics = "strip", token_order = "sort"
      )
    ),
    blocks = blocks,
    comparisons = comparisons,
    max_candidates = max_candidates
  )
}

linkage_scoring_spec <- function(blocks = NULL,
                                 nonmatch_max = 0.05,
                                 match_min = 0.9) {
  base <- linkage_foundation_spec()
  if (is.null(blocks)) blocks <- base$blocks
  parameters <- data.frame(
    comparison = base$comparisons$comparison,
    m_probability = c(0.8, 0.85, 0.99, 0.9, 0.95, 0.9),
    u_probability = c(0.1, 0.2, 0.02, 0.2, 0.5, 0.25),
    stringsAsFactors = FALSE
  )
  epi_linkage_spec(
    x_id = base$x_id,
    y_id = base$y_id,
    profiles = base$profiles[names(base$profiles) != "identity"],
    blocks = blocks,
    comparisons = base$comparisons,
    max_candidates = 100,
    model = list(parameters = parameters, match_prevalence = 0.05),
    thresholds = list(
      metric = "model_posterior",
      nonmatch_max = nonmatch_max,
      match_min = match_min
    )
  )
}

linkage_complete_truth <- function(n_x = 4L, n_y = 5L) {
  truth <- expand.grid(
    x_index = seq_len(n_x),
    y_index = seq_len(n_y),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  keys <- paste(truth$x_index, truth$y_index, sep = ":")
  truth$is_match <- keys %in% c("1:1", "2:2", "3:3", "4:5")
  truth
}
