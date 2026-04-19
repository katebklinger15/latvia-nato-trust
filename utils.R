# =============================================================================
# utils.R — Shared helper functions
# =============================================================================

# Significance stars
p_stars <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ "\u2020",
    TRUE      ~ ""
  )
}

# Format p-values
fmt_p <- function(p) {
  ifelse(is.na(p), "", scales::pvalue(p, accuracy = 0.001))
}

# Format coefficient + SE + stars into a single cell string
fmt_b_se <- function(b, se, p) {
  star <- p_stars(p)
  if (is.na(b) | is.na(se)) return("")
  glue::glue("{round(b, 3)} ({round(se, 3)}){star}")
}

# Statistical mode (most frequent value)
Mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Row mean with a minimum non-missing threshold
# Returns NA if fewer than MIN_NONMISS values are present
MIN_NONMISS <- 2L

row_mean_min_nonmiss <- function(mat, min_k = MIN_NONMISS) {
  nn  <- rowSums(!is.na(mat))
  out <- rowMeans(mat, na.rm = TRUE)
  out[nn < min_k] <- NA_real_
  out
}

# =============================================================================
# Descriptive frequency functions
# =============================================================================

# freq_by_lang: frequency table broken down by language group
# Returns counts and two percentage columns:
#   pct_within_cat  — share of each language within a variable category
#   pct_within_lang — share of each category within a language group
freq_by_lang <- function(data, var, lang_var = lang) {
  data %>%
    filter(!is.na({{ lang_var }})) %>%
    count({{ var }}, {{ lang_var }}, name = "n") %>%
    group_by({{ var }}) %>%
    mutate(pct_within_cat = 100 * n / sum(n)) %>%
    ungroup() %>%
    group_by({{ lang_var }}) %>%
    mutate(
      total_in_lang   = sum(n),
      pct_within_lang = 100 * n / total_in_lang
    ) %>%
    ungroup()
}

# trust_by_lang: frequency table of NATO trust responses
# broken down by a covariate and language group
# Set include_na = TRUE to retain missing trust categories in counts
trust_by_lang <- function(data, var,
                          trust_var  = trust_nato_ord,
                          lang_var   = lang,
                          include_na = FALSE) {
  data %>%
    filter(!is.na({{ lang_var }}), !is.na({{ trust_var }})) %>%
    count({{ var }}, {{ lang_var }}, {{ trust_var }},
          name = "n", .drop = !include_na) %>%
    group_by({{ var }}, {{ lang_var }}) %>%
    mutate(
      n_var_lang          = sum(n),
      pct_within_var_lang = 100 * n / n_var_lang
    ) %>%
    ungroup() %>%
    group_by({{ lang_var }}) %>%
    mutate(
      total_in_lang       = sum(n_var_lang),
      pct_var_within_lang = 100 * n_var_lang / total_in_lang
    ) %>%
    ungroup()
}