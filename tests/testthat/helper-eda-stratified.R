make_stratified_fixture <- function() {
  data <- data.frame(
    arm = c("A", "A", "B", "C", "MISS", NA),
    value = c(1, 3, 2, Inf, 999, NA_real_),
    status = c("yes", "no", "yes", "other", "NA", NA),
    note = c("secret-a", "secret-b", "secret-c", "secret-d", "secret-e", NA),
    visit = as.Date(c("2024-01-01", "2024-01-03", "2024-01-02", "2024-01-04", NA, NA)),
    participant_id = 1:6,
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    name = c("arm", "value", "status", "note", "visit", "participant_id", "absent"),
    label = c("Study arm", "Value", "Status", "Note", "Visit", "Participant", "Absent"),
    database_type = "text",
    analysis_type = c("categorical", "numeric", "categorical", "text", "date", "integer", "text"),
    role = c("exposure", "measure", "measure", "measure", "measure", "identifier", "measure"),
    levels = c("B;A;D", "", "no;yes;unused", "", "", "", ""),
    missing_codes = c("MISS", "999", "", "", "", "", ""),
    required = c(rep(TRUE, 6), FALSE),
    stringsAsFactors = FALSE
  )
  list(data = data, spec = spec)
}
