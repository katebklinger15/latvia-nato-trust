# =============================================================================
# 02_analysis.R — OLS, Ordered Logistic Regression, and SEM
# =============================================================================
# Input:  latvia_data (from 01_load_clean.R)
# Output: figures/ and tables/ directories with all plots and tables
# Sections:
#   1.  Setup and helper functions
#   2.  Analysis variables and OLS dataset
#   3.  EDA plots
#   4.  OLS models (M1-M5)
#   5.  Robust SEs and attenuation
#   6.  VIF checks
#   7.  Predicted mean trust by language
#   8.  Influence and outlier diagnostics
#   9.  Bootstrap: significance of coefficient attenuation
#   10. Block tests and Wald tests
#   11. Effect sizes (partial R² and Cohen's f²)
#   12. Predicted values: threat by language
#   13. Indexed threat and response models
#   14. OLR models (M1-M5)
#   15. SEM: latent mediation model
#   16. SEM: indexed mediation model
#   17. Export tables
# =============================================================================

source("utils.R")

library(tidyverse)
library(car)
library(lmtest)
library(sandwich)
library(ordinal)
library(broom)
library(dotwhisker)
library(ggeffects)
library(lavaan)
library(glue)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(kableExtra)
library(gt)
library(scales)
library(boot)
library(showtext)
library(extrafont)

# Create output folders if they don't exist
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("tables"))  dir.create("tables")

fig_dir <- "figures"

# =============================================================================
# 1. Helper functions
# =============================================================================

# Typical row for marginal predictions: holds all controls at modal/median values
typical_row <- function(d) {
  data.frame(
    pol_interest_cat = factor(Mode(d$pol_interest_cat), levels = levels(d$pol_interest_cat)),
    qe1_1_num        = median(d$qe1_1_num,  na.rm = TRUE),
    qe2_2_num        = median(d$qe2_2_num,  na.rm = TRUE),
    qe4_1_num        = median(d$qe4_1_num,  na.rm = TRUE),
    qe4_2_num        = median(d$qe4_2_num,  na.rm = TRUE),
    qe4_8_num        = median(d$qe4_8_num,  na.rm = TRUE),
    qe4_12_num       = median(d$qe4_12_num, na.rm = TRUE),
    qe4_14_num       = median(d$qe4_14_num, na.rm = TRUE),
    qe3.4_num        = 0,
    qe3.5_num        = 0,
    qe3.7_num        = 0,
    edu_4            = factor(Mode(d$edu_4),         levels = levels(d$edu_4)),
    age_cat          = factor(Mode(d$age_cat),        levels = levels(d$age_cat)),
    gender           = factor(Mode(d$gender),         levels = levels(d$gender)),
    soc_class_cat    = factor(Mode(d$soc_class_cat),  levels = levels(d$soc_class_cat))
  )
}

# Robust tidy: extracts HC1 robust SEs from an lm object
tidy_robust <- function(fit, type = "HC1") {
  ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = type))
  tibble(
    term      = rownames(ct),
    estimate  = as.numeric(ct[, 1]),
    std.error = as.numeric(ct[, 2]),
    statistic = as.numeric(ct[, 3]),
    p.value   = as.numeric(ct[, 4])
  )
}

# Robust joint test for a factor block (e.g., all age_cat dummies jointly = 0)
factor_joint_p <- function(fit, factor_prefix, type = "HC1") {
  cn   <- names(coef(fit))
  trms <- cn[cn != "(Intercept)" & str_detect(cn, paste0("^", fixed(factor_prefix)))]
  if (length(trms) == 0) return(NA_real_)
  hyp <- paste0(trms, " = 0")
  wt  <- tryCatch(
    car::linearHypothesis(fit, hyp,
                          vcov. = sandwich::vcovHC(fit, type = type),
                          test  = "Chisq"),
    error = function(e) NULL
  )
  if (is.null(wt)) return(NA_real_)
  as.numeric(wt[2, ncol(wt)])
}

# Robust point estimate extractor (returns NA if term not in model)
get_robust_estimate <- function(fit, term, type = "HC1") {
  ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = type))
  rn <- rownames(ct)
  if (is.null(rn) || !(term %in% rn)) return(NA_real_)
  as.numeric(ct[term, "Estimate"])
}

# Influence table: leverage, Cook's D, studentized residuals per observation
influence_table <- function(fit, data) {
  mf  <- model.frame(fit)
  idx <- as.integer(rownames(mf))
  as_tibble(mf) %>%
    mutate(
      model_row  = seq_len(nrow(mf)),
      idx_in_dat = idx,
      serialid   = data$serialid[idx],
      orig_row   = data$orig_row[idx],
      fitted     = fitted(fit),
      rstandard  = rstandard(fit),
      rstudent   = rstudent(fit),
      hat        = hatvalues(fit),
      cooks      = cooks.distance(fit),
      dffits     = dffits(fit)
    )
}

# Prediction grid helper: builds newdata varying one or more predictors
# while holding everything else at typical values
pred_grid_ols <- function(model, data, vary, hold = typical_row(data)) {
  grid     <- do.call(crossing, vary)
  hold_rep <- hold[rep(1, nrow(grid)), , drop = FALSE]
  add_cols <- setdiff(names(hold_rep), names(grid))
  newdat   <- bind_cols(grid, hold_rep[, add_cols, drop = FALSE])
  p        <- predict(model, newdata = newdat, se.fit = TRUE)
  newdat$pred <- p$fit
  newdat$se   <- p$se.fit
  newdat$lwr  <- newdat$pred - 1.96 * newdat$se
  newdat$upr  <- newdat$pred + 1.96 * newdat$se
  newdat
}

# Grid sequence helper for prediction plots
# Handles factors, z-scored numerics, and raw-scale numerics
make_grid_seq <- function(x, k = 5,
                          z_tol_mean    = 0.15,
                          z_tol_sd      = 0.15,
                          sd_window     = 2,
                          use_quantiles = FALSE) {
  x <- x[!is.na(x)]
  if (is.factor(x) || is.ordered(x)) {
    ux  <- sort(unique(as.numeric(as.character(x))))
    if (length(ux) <= k) return(ux)
    idx <- round(seq(1, length(ux), length.out = k))
    return(ux[idx])
  }
  x_num <- as.numeric(x)
  mu    <- mean(x_num, na.rm = TRUE)
  s     <- sd(x_num,   na.rm = TRUE)
  is_z  <- (abs(mu - 0) <= z_tol_mean) && (abs(s - 1) <= z_tol_sd)
  if (is_z) return(seq(-sd_window, sd_window, length.out = k))
  if (use_quantiles) {
    qs <- quantile(x_num, probs = seq(0, 1, length.out = k),
                   na.rm = TRUE, names = FALSE)
    return(as.numeric(round(qs, 3)))
  }
  r <- range(x_num, na.rm = TRUE)
  seq(r[1], r[2], length.out = k)
}

# lavaan-safe variable name cleaner
make_lavaan_safe <- function(x) {
  x <- make.names(x, unique = TRUE)
  x <- gsub("\\.", "_", x)
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x
}

# Treatment dummy coder that preserves NA rows (required for lavaan)
safe_treatment_dummies <- function(df, formula_rhs) {
  mf   <- model.frame(as.formula(paste("~", formula_rhs)),
                      data = df, na.action = na.pass,
                      drop.unused.levels = FALSE)
  mm   <- model.matrix(as.formula(paste("~", formula_rhs)), data = mf)
  mm   <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  kept <- as.integer(rownames(mm))
  out  <- matrix(NA_real_, nrow = nrow(df), ncol = ncol(mm))
  out[kept, ] <- mm
  out  <- as.data.frame(out)
  ok   <- vapply(out, function(v) any(!is.na(v)) && sd(v, na.rm = TRUE) > 0, logical(1))
  out  <- out[, ok, drop = FALSE]
  colnames(out) <- make_lavaan_safe(colnames(out))
  out
}

# Table formatting helpers
fmt_num   <- function(x, digs = 3) ifelse(is.na(x), "", sprintf(paste0("%.", digs, "f"), x))
fmt_joint <- function(p) { if (is.na(p)) return(""); glue("p = {fmt_num(p, 3)}{p_stars(p)}") }
sig_code  <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < .001  ~ "***",
    p < .01   ~ "**",
    p < .05   ~ "*",
    p < .10   ~ "\u2020",
    TRUE      ~ ""
  )
}

# =============================================================================
# 2. Analysis variables and OLS dataset
# =============================================================================

# Ensure social class naming is consistent across scripts
if (!"soc_class_cat" %in% names(latvia_data) && "soc_class4_cat" %in% names(latvia_data)) {
  latvia_data <- latvia_data %>%
    dplyr::mutate(soc_class_cat = soc_class4_cat) %>%
    droplevels()
}

# Variable blocks
controls  <- c("age_cat", "gender", "soc_class_cat", "pol_interest_cat", "edu_4")
threats   <- c("qe4_1_num", "qe4_2_num", "qe3.4_num", "qe3.5_num", "qe3.7_num")
responses <- c("qe4_8_num", "qe4_12_num", "qe4_14_num", "qe2_2_num", "qe1_1_num")
keep_vars <- c("serialid", "trust_nato_num", "trust_nato_ord", "lang",
               controls, threats, responses)

# Build OLS-ready dataset: unordered factors, complete DV
analysis_df_ols <- latvia_data %>%
  mutate(orig_row = row_number()) %>%
  select(any_of(keep_vars), orig_row) %>%
  filter(!is.na(trust_nato_num)) %>%
  mutate(
    lang             = factor(lang, levels = c("Latvian", "Russian")),
    gender           = factor(gender),
    age_cat          = factor(age_cat, ordered = FALSE),
    pol_interest_cat = factor(pol_interest_cat, ordered = FALSE),
    edu_4            = factor(edu_4, ordered = FALSE)
  ) %>%
  droplevels()

message("OLS dataset: ", nrow(analysis_df_ols), " observations")

# =============================================================================
# 3. EDA plots
# =============================================================================

# Overall NATO trust distribution
p_trust_overall <- analysis_df_ols %>%
  count(trust_nato_num) %>%
  ggplot(aes(x = factor(trust_nato_num), y = n)) +
  geom_col(fill = "#4472C4") +
  labs(
    x     = "Trust in NATO (1 = No trust, 2 = DK, 3 = Trust)",
    y     = "Count",
    title = "Distribution of trust in NATO (Latvia sample)"
  ) +
  theme_minimal()
p_trust_overall

ggsave(file.path(fig_dir, "trust_overall.png"),
       p_trust_overall, width = 6, height = 4, dpi = 300)

# Trust by language group (percent stacked bar)
p_trust_by_lang <- analysis_df_ols %>%
  filter(!is.na(lang)) %>%
  count(lang, trust_nato_num) %>%
  group_by(lang) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = lang, y = pct, fill = factor(trust_nato_num))) +
  geom_col() +
  scale_fill_brewer(
    type    = "qual", palette = "Set2",
    name    = "Trust level",
    labels  = c("1 No trust", "2 DK", "3 Trust")
  ) +
  labs(x = "Language", y = "Percent", title = "Trust in NATO by language") +
  theme_minimal()
p_trust_by_lang

ggsave(file.path(fig_dir, "trust_by_language.png"),
       p_trust_by_lang, width = 6, height = 4, dpi = 300)

# =============================================================================
# 4. OLS models (M1-M5)
# =============================================================================
# M1: language only
# M2: + demographic controls
# M3: + threat perceptions
# M4: + policy response attitudes
# M5: full model (controls + threats + responses)

rhs_m1 <- c("lang")
rhs_m2 <- c(rhs_m1, controls)
rhs_m3 <- c(rhs_m2, threats)
rhs_m4 <- c(rhs_m2, responses)
rhs_m5 <- c(rhs_m2, threats, responses)

m1_ols <- lm(reformulate(rhs_m1, "trust_nato_num"), data = analysis_df_ols)
m2_ols <- lm(reformulate(rhs_m2, "trust_nato_num"), data = analysis_df_ols)
m3_ols <- lm(reformulate(rhs_m3, "trust_nato_num"), data = analysis_df_ols)
m4_ols <- lm(reformulate(rhs_m4, "trust_nato_num"), data = analysis_df_ols)
m5_ols <- lm(reformulate(rhs_m5, "trust_nato_num"), data = analysis_df_ols)

models_ols <- list(
  "Model 1: Lang"              = m1_ols,
  "Model 2: + Controls"        = m2_ols,
  "Model 3: + Threat"          = m3_ols,
  "Model 4: + Response"        = m4_ols,
  "Model 5: + Threat+Response" = m5_ols
)

# =============================================================================
# 5. Robust SEs and coefficient attenuation
# =============================================================================
# How much of the raw language gap (M1) is explained by adding covariates (M5)?

lang_rows <- lapply(models_ols, function(fit) {
  ct <- coeftest(fit, vcov = vcovHC(fit, type = "HC1"))
  if ("langRussian" %in% rownames(ct)) ct["langRussian", , drop = FALSE] else NULL
})

lang_df <- do.call(rbind, lang_rows)
message("langRussian coefficients across models:")
print(lang_df)

beta1 <- lang_df[1, "Estimate"]
beta5 <- lang_df[5, "Estimate"]
attenuation_share <- (beta1 - beta5) / beta1
message("Attenuation share M1 to M5: ", round(attenuation_share, 3))

# =============================================================================
# 6. VIF checks (M2-M5)
# =============================================================================
# Threshold: VIF < 5 indicates acceptable multicollinearity

vifs <- lapply(models_ols[-1], car::vif)
names(vifs) <- names(models_ols)[-1]
vifs

# =============================================================================
# 7. Predicted mean trust by language (M5, controls at typical values)
# =============================================================================

newdat_base <- typical_row(analysis_df_ols)
newdat_lang <- rbind(
  cbind(lang = factor("Latvian", levels = levels(analysis_df_ols$lang)), newdat_base),
  cbind(lang = factor("Russian", levels = levels(analysis_df_ols$lang)), newdat_base)
)
newdat_lang$pred_mean <- predict(m5_ols, newdata = newdat_lang)

message("Predicted mean trust by language (M5):")
print(newdat_lang[, c("lang", "pred_mean")])

# =============================================================================
# 8. Influence and outlier diagnostics (M5)
# =============================================================================

infl_m5  <- influence_table(m5_ols, analysis_df_ols)
p_coefs  <- length(coef(m5_ols))
n_obs    <- nobs(m5_ols)
lev_cut  <- 2 * p_coefs / n_obs  # leverage threshold
cook_cut <- 4 / n_obs             # Cook's D threshold

# Flag influential observations
suspects_m5 <- infl_m5 %>%
  filter(abs(rstudent) > 2 | hat > lev_cut | cooks > cook_cut) %>%
  arrange(desc(cooks))

# Sensitivity check: refit M5 dropping top-5 influential cases
compare_lang_after_dropk <- function(fit, data, suspects, k = 5, term = "langRussian") {
  drop_ids <- suspects$serialid[seq_len(min(k, nrow(suspects)))]
  fit_k    <- update(fit, data = data %>% dplyr::filter(!serialid %in% drop_ids))
  list(
    coefs        = c(full = coef(fit)[term], dropk = coef(fit_k)[term]),
    full_robust  = lmtest::coeftest(fit,   vcov = sandwich::vcovHC(fit,   type = "HC1"))[term, ],
    dropk_robust = lmtest::coeftest(fit_k, vcov = sandwich::vcovHC(fit_k, type = "HC1"))[term, ]
  )
}

message("Sensitivity: language coefficient before and after dropping top-5 influential cases")
print(compare_lang_after_dropk(m5_ols, analysis_df_ols, suspects_m5, k = 5))

# Influence plot: leverage vs Cook's distance
aug <- augment(m5_ols)

p_influence <- ggplot(aug, aes(.hat, .cooksd)) +
  geom_point(alpha = 0.55, size = 2) +
  geom_vline(xintercept = lev_cut,  color = "#2C6FED", linewidth = 0.5) +
  geom_hline(yintercept = cook_cut, color = "#E23B3B", linewidth = 0.5) +
  annotate("text",
           x = lev_cut, y = max(aug$.cooksd, na.rm = TRUE),
           label = "Leverage threshold", color = "#2C6FED",
           vjust = 0.9, hjust = -0.1, size = 2.5) +
  annotate("text",
           x = max(aug$.hat, na.rm = TRUE), y = cook_cut,
           label = "Cook's D threshold", color = "#E23B3B",
           hjust = 1.2, vjust = -0.6, size = 2.5) +
  labs(
    title = "Influence diagnostics: leverage vs. Cook's distance (M5)",
    x     = "Leverage (hat value)",
    y     = "Cook's distance"
  ) +
  theme_minimal(base_size = 12)
p_influence

ggsave(file.path(fig_dir, "influence_plot_m5.png"),
       p_influence, width = 6, height = 5, dpi = 300)

# =============================================================================
# 9. Bootstrap: significance of coefficient attenuation (M1 vs M5)
# =============================================================================
# Tests whether the drop in the language coefficient from M1 to M5
# is statistically meaningful using 1000 bootstrap resamples

rows_m5   <- as.integer(rownames(model.frame(m5_ols)))
df_m5rows <- analysis_df_ols[rows_m5, , drop = FALSE]

boot_fun_aligned <- function(data, idx) {
  d    <- data[idx, , drop = FALSE]
  m1   <- lm(trust_nato_num ~ lang, data = d)
  m5   <- lm(trust_nato_num ~ lang + age_cat + gender + soc_class_cat +
               pol_interest_cat + edu_4 +
               qe4_1_num + qe4_2_num + qe3.4_num + qe3.5_num + qe3.7_num +
               qe4_8_num + qe4_12_num + qe4_14_num + qe2_2_num + qe1_1_num,
             data = d)
  beta_m1 <- unname(coef(m1)[["langRussian"]])
  beta_m5 <- unname(coef(m5)[["langRussian"]])
  c(beta_m1 = beta_m1, beta_m5 = beta_m5, diff = beta_m1 - beta_m5)
}

set.seed(42)
b_aligned <- boot(df_m5rows, statistic = boot_fun_aligned, R = 1000)
ci_diff   <- boot.ci(b_aligned, type = "perc", index = 3)

message("Bootstrap 95% CI for M1–M5 language coefficient attenuation:")
print(ci_diff)

# =============================================================================
# 10. Block tests and Wald tests
# =============================================================================

# Joint significance of threat block in M5
car::linearHypothesis(
  m5_ols,
  c("qe4_1_num = 0", "qe4_2_num = 0",
    "qe3.4_num = 0", "qe3.5_num = 0", "qe3.7_num = 0"),
  vcov. = sandwich::vcovHC(m5_ols, type = "HC1"),
  test  = "Chisq"
)

# Joint significance of response block in M5
car::linearHypothesis(
  m5_ols,
  c("qe4_8_num = 0", "qe4_12_num = 0",
    "qe4_14_num = 0", "qe2_2_num = 0", "qe1_1_num = 0"),
  vcov. = sandwich::vcovHC(m5_ols, type = "HC1"),
  test  = "Chisq"
)

# Wald tests for block additions (aligned rows)
rows_m3  <- as.integer(rownames(model.frame(m3_ols)))
rows_m4  <- as.integer(rownames(model.frame(m4_ols)))
rows_m5  <- as.integer(rownames(model.frame(m5_ols)))

m2_on_m3 <- lm(reformulate(rhs_m2, "trust_nato_num"), data = analysis_df_ols[rows_m3, ])
m2_on_m4 <- lm(reformulate(rhs_m2, "trust_nato_num"), data = analysis_df_ols[rows_m4, ])
m3_on_m5 <- lm(reformulate(rhs_m3, "trust_nato_num"), data = analysis_df_ols[rows_m5, ])
m4_on_m5 <- lm(reformulate(rhs_m4, "trust_nato_num"), data = analysis_df_ols[rows_m5, ])

vcov_fn <- function(x) sandwich::vcovHC(x, type = "HC1")

message("Wald test: M2 vs M3 (adding threats)")
lmtest::waldtest(m2_on_m3, m3_ols, vcov = vcov_fn, test = "Chisq")

message("Wald test: M2 vs M4 (adding responses)")
lmtest::waldtest(m2_on_m4, m4_ols, vcov = vcov_fn, test = "Chisq")

message("Wald test: M3 vs M5 (adding responses to threat model)")
lmtest::waldtest(m3_on_m5, m5_ols, vcov = vcov_fn, test = "Chisq")

message("Wald test: M4 vs M5 (adding threats to response model)")
lmtest::waldtest(m4_on_m5, m5_ols, vcov = vcov_fn, test = "Chisq")

# =============================================================================
# 11. Effect sizes: partial R² and Cohen's f²
# =============================================================================

block_effect_sizes <- function(data, rhs_small, rhs_big,
                               response   = "trust_nato_num",
                               use_common = c("union_cc", "big_rows")) {
  use_common <- match.arg(use_common)
  if (use_common == "union_cc") {
    vars_union <- unique(c(response, rhs_small, rhs_big))
    df <- data[complete.cases(data[, vars_union, drop = FALSE]), , drop = FALSE]
  } else {
    fit_big_tmp <- lm(reformulate(rhs_big, response), data = data)
    rows_big    <- as.integer(rownames(model.frame(fit_big_tmp)))
    df          <- data[rows_big, , drop = FALSE]
  }
  fit_small <- lm(reformulate(rhs_small, response), data = df)
  fit_big   <- lm(reformulate(rhs_big,   response), data = df)
  r2_small  <- summary(fit_small)$r.squared
  r2_big    <- summary(fit_big)$r.squared
  delta_r2  <- r2_big - r2_small
  wt        <- lmtest::waldtest(fit_small, fit_big,
                                vcov = function(x) sandwich::vcovHC(x, type = "HC1"),
                                test = "F")
  Fstat      <- unname(wt[2, "F"])
  df1        <- unname(wt[2, "Df"])
  df2        <- unname(wt[2, "Res.Df"])
  pval       <- unname(wt[2, "Pr(>F)"])
  partial_r2 <- (Fstat * df1) / (Fstat * df1 + df2)
  f2         <- partial_r2 / (1 - partial_r2)
  
  tibble(N_aligned  = nrow(df), df1, df2,
         F_robust   = Fstat, p_robust = pval,
         delta_R2   = delta_r2, partial_R2 = partial_r2, f2)
}

eff_tbl <- bind_rows(
  block_effect_sizes(analysis_df_ols, rhs_m2, rhs_m3, use_common = "big_rows") %>%
    mutate(comparison = "Controls → +Threat (M2 vs M3)"),
  block_effect_sizes(analysis_df_ols, rhs_m2, rhs_m4, use_common = "big_rows") %>%
    mutate(comparison = "Controls → +Response (M2 vs M4)"),
  block_effect_sizes(analysis_df_ols, rhs_m3, rhs_m5, use_common = "big_rows") %>%
    mutate(comparison = "+Threat → Full (M3 vs M5)"),
  block_effect_sizes(analysis_df_ols, rhs_m4, rhs_m5, use_common = "big_rows") %>%
    mutate(comparison = "+Response → Full (M4 vs M5)")
) %>%
  mutate(
    delta_R2   = round(delta_R2,   4),
    partial_R2 = round(partial_R2, 4),
    f2         = round(f2,         3),
    F_robust   = round(F_robust,   2),
    p_robust   = format.pval(p_robust, digits = 3, eps = 1e-3)
  ) %>%
  select(comparison, N_aligned, df1, df2, F_robust, p_robust, delta_R2, partial_R2, f2)

print(eff_tbl)

# =============================================================================
# 12. Predicted values: threat perception by language (M5 OLS)
# =============================================================================

pg <- pred_grid_ols(
  model = m5_ols,
  data  = analysis_df_ols,
  vary  = list(lang = levels(analysis_df_ols$lang), qe4_2_num = 1:4)
)
pg

p_pred <- ggplot(pg, aes(x = qe4_2_num, y = pred, color = lang, group = lang)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = lang), alpha = .15, color = NA) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    x     = "Perceived threat to Latvia (1–4)",
    y     = "Predicted mean trust in NATO (1–3)",
    title = "Predicted mean trust across threat levels, by language (M5 OLS)"
  ) +
  theme_minimal()
p_pred

ggsave(file.path(fig_dir, "predicted_means_qe4_2_by_lang_M5_OLS.png"),
       p_pred, width = 7, height = 4.5, dpi = 300)

# =============================================================================
# 13. Indexed threat and response models (M3i–M5i)
# =============================================================================
# Alternative specifications using composite mean indexes instead of
# individual items — reduces model complexity and multicollinearity

# Set to TRUE to z-score items before averaging (recommended if mixing scales)
use_zscores <- FALSE

threat_items   <- c("qe4_1_num", "qe4_2_num", "qe3.4_num", "qe3.5_num", "qe3.7_num")
response_items <- c("qe4_8_num", "qe4_12_num", "qe4_14_num", "qe2_2_num", "qe1_1_num")

# Row mean with minimum non-missing threshold
row_mean_min_nonmiss_mat <- function(mat, min_k = 2L) {
  nn  <- rowSums(!is.na(mat))
  out <- rowMeans(mat, na.rm = TRUE)
  out[nn < min_k] <- NA_real_
  out
}

analysis_df_ols <- analysis_df_ols %>%
  mutate(
    across(
      all_of(c(threat_items, response_items)),
      ~ if (use_zscores) as.numeric(scale(.)) else .,
      .names = "z_{.col}"
    )
  ) %>%
  mutate(
    threat_index = {
      mat <- as.matrix(pick(all_of(paste0("z_", threat_items))))
      row_mean_min_nonmiss_mat(mat)
    },
    response_index = {
      mat <- as.matrix(pick(all_of(paste0("z_", response_items))))
      row_mean_min_nonmiss_mat(mat)
    }
  )

rhs_m3_idx <- c(rhs_m2, "threat_index")
rhs_m4_idx <- c(rhs_m2, "response_index")
rhs_m5_idx <- c(rhs_m2, "threat_index", "response_index")

m3i_ols <- lm(reformulate(rhs_m3_idx, "trust_nato_num"), data = analysis_df_ols)
m4i_ols <- lm(reformulate(rhs_m4_idx, "trust_nato_num"), data = analysis_df_ols)
m5i_ols <- lm(reformulate(rhs_m5_idx, "trust_nato_num"), data = analysis_df_ols)

models_idx <- list(
  "M3i: + ThreatIndex"       = m3i_ols,
  "M4i: + ResponseIndex"     = m4i_ols,
  "M5i: + Threat+ResponseIdx" = m5i_ols
)

# Robust language coefficient in indexed models
lang_rows_idx <- lapply(models_idx, function(fit) {
  ct <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type = "HC1"))
  if ("langRussian" %in% rownames(ct)) ct["langRussian", , drop = FALSE] else NULL
})
lang_df_idx <- do.call(rbind, lang_rows_idx)
message("langRussian in indexed models:")
print(lang_df_idx)

# Attenuation in indexed spec
beta5i             <- get_robust_estimate(m5i_ols, term = "langRussian")
attenuation_idx    <- (get_robust_estimate(m1_ols, "langRussian") - beta5i) /
  get_robust_estimate(m1_ols, "langRussian")
message("Attenuation share M1 to M5i: ", round(attenuation_idx, 3))

# VIF for indexed models
vifs_idx <- lapply(models_idx, car::vif)
names(vifs_idx) <- names(models_idx)
vifs_idx

# Predicted trust across threat index by language (M5i)
resp_hold   <- mean(analysis_df_ols$response_index, na.rm = TRUE)
grid_threat <- make_grid_seq(analysis_df_ols$threat_index, k = 5)
hold        <- typical_row(analysis_df_ols)

newdat_idx <- tidyr::crossing(lang = levels(analysis_df_ols$lang),
                              threat_index = grid_threat) %>%
  mutate(response_index = resp_hold) %>%
  bind_cols(hold[, setdiff(names(hold), names(.)), drop = FALSE]) %>%
  mutate(
    lang             = factor(lang,             levels = levels(analysis_df_ols$lang)),
    gender           = factor(gender,           levels = levels(analysis_df_ols$gender)),
    age_cat          = factor(age_cat,          levels = levels(analysis_df_ols$age_cat)),
    pol_interest_cat = factor(pol_interest_cat, levels = levels(analysis_df_ols$pol_interest_cat)),
    edu_4            = factor(edu_4,            levels = levels(analysis_df_ols$edu_4)),
    soc_class_cat    = factor(soc_class_cat,    levels = levels(analysis_df_ols$soc_class_cat))
  )

pp_idx          <- predict(m5i_ols, newdata = newdat_idx, se.fit = TRUE)
newdat_idx$pred <- pp_idx$fit
newdat_idx$lwr  <- pp_idx$fit - 1.96 * pp_idx$se.fit
newdat_idx$upr  <- pp_idx$fit + 1.96 * pp_idx$se.fit

p_pred_idx <- ggplot(newdat_idx,
                     aes(x = threat_index, y = pred, color = lang, group = lang)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = lang), alpha = .15, color = NA) +
  labs(
    x     = "Threat index (row mean)",
    y     = "Predicted mean trust in NATO (1–3)",
    title = "Predicted trust by threat index and language (M5i; response index held constant)"
  ) +
  theme_minimal()
p_pred_idx

ggsave(file.path(fig_dir, "predicted_means_threatIndex_by_lang_M5i_OLS.png"),
       p_pred_idx, width = 7, height = 4.5, dpi = 300)

# Predicted trust across response index by language (M5i)
threat_hold  <- mean(analysis_df_ols$threat_index, na.rm = TRUE)
grid_resp    <- make_grid_seq(analysis_df_ols$response_index, k = 5)

newdat_resp <- tidyr::crossing(lang = levels(analysis_df_ols$lang),
                               response_index = grid_resp) %>%
  mutate(threat_index = threat_hold) %>%
  bind_cols(hold[, setdiff(names(hold), names(.)), drop = FALSE]) %>%
  mutate(
    lang             = factor(lang,             levels = levels(analysis_df_ols$lang)),
    gender           = factor(gender,           levels = levels(analysis_df_ols$gender)),
    age_cat          = factor(age_cat,          levels = levels(analysis_df_ols$age_cat)),
    pol_interest_cat = factor(pol_interest_cat, levels = levels(analysis_df_ols$pol_interest_cat)),
    edu_4            = factor(edu_4,            levels = levels(analysis_df_ols$edu_4)),
    soc_class_cat    = factor(soc_class_cat,    levels = levels(analysis_df_ols$soc_class_cat))
  )

pp_resp           <- predict(m5i_ols, newdata = newdat_resp, se.fit = TRUE)
newdat_resp$pred  <- pp_resp$fit
newdat_resp$lwr   <- pp_resp$fit - 1.96 * pp_resp$se.fit
newdat_resp$upr   <- pp_resp$fit + 1.96 * pp_resp$se.fit

p_pred_resp <- ggplot(newdat_resp,
                      aes(x = response_index, y = pred, color = lang, group = lang)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = lang), alpha = .15, color = NA) +
  labs(
    x     = "Response index (row mean)",
    y     = "Predicted mean trust in NATO (1–3)",
    title = "Predicted trust by response index and language (M5i; threat index held constant)"
  ) +
  theme_minimal()
p_pred_resp

ggsave(file.path(fig_dir, "predicted_means_responseIndex_by_lang_M5i_OLS.png"),
       p_pred_resp, width = 7, height = 4.5, dpi = 300)

# =============================================================================
# 14. OLR models (M1-M5)
# =============================================================================
# Ordered logistic regression as a robustness check on the OLS results
# Uses ordered factor DV and keeps factor encodings as ordered

analysis_df_olr <- latvia_data %>%
  select(any_of(keep_vars)) %>%
  filter(!is.na(trust_nato_ord)) %>%
  mutate(
    lang             = factor(lang, levels = c("Latvian", "Russian")),
    gender           = factor(gender),
    age_cat          = factor(age_cat,          ordered = TRUE),
    pol_interest_cat = factor(pol_interest_cat, ordered = TRUE),
    edu_4            = factor(edu_4,            ordered = TRUE)
  ) %>%
  droplevels()

# Complete case subsets for each model specification
vars_m1 <- unique(c("trust_nato_ord", "lang"))
vars_m2 <- unique(c("trust_nato_ord", "lang", controls))
vars_m3 <- unique(c("trust_nato_ord", "lang", controls, threats))
vars_m4 <- unique(c("trust_nato_ord", "lang", controls, responses))
vars_m5 <- unique(c("trust_nato_ord", "lang", controls, threats, responses))

common_m1 <- analysis_df_olr[complete.cases(analysis_df_olr[, vars_m1]), ]
common_m2 <- analysis_df_olr[complete.cases(analysis_df_olr[, vars_m2]), ]
common_m3 <- analysis_df_olr[complete.cases(analysis_df_olr[, vars_m3]), ]
common_m4 <- analysis_df_olr[complete.cases(analysis_df_olr[, vars_m4]), ]
common_m5 <- analysis_df_olr[complete.cases(analysis_df_olr[, vars_m5]), ]

m1_olr <- ordinal::clm(reformulate(rhs_m1, "trust_nato_ord"), data = common_m1, link = "logit", Hess = TRUE)
m2_olr <- ordinal::clm(reformulate(rhs_m2, "trust_nato_ord"), data = common_m2, link = "logit", Hess = TRUE)
m3_olr <- ordinal::clm(reformulate(rhs_m3, "trust_nato_ord"), data = common_m3, link = "logit", Hess = TRUE)
m4_olr <- ordinal::clm(reformulate(rhs_m4, "trust_nato_ord"), data = common_m4, link = "logit", Hess = TRUE)
m5_olr <- ordinal::clm(reformulate(rhs_m5, "trust_nato_ord"), data = common_m5, link = "logit", Hess = TRUE)

models_olr <- list(
  "M1: Language"            = m1_olr,
  "M2: + Controls"          = m2_olr,
  "M3: + Threats"           = m3_olr,
  "M4: + Responses"         = m4_olr,
  "M5: Threats + Responses" = m5_olr
)

# Proportional odds assumption check on M5
message("Proportional odds assumption test (M5 OLR):")
print(ordinal::nominal_test(m5_olr))

# Language coefficient across OLR models
lang_olr_df <- purrr::map_dfr(names(models_olr), function(nm) {
  s <- summary(models_olr[[nm]])$coefficients
  if ("langRussian" %in% rownames(s)) {
    data.frame(
      model    = nm,
      estimate = s["langRussian", "Estimate"],
      se       = s["langRussian", "Std. Error"],
      z        = s["langRussian", "z value"],
      p        = s["langRussian", "Pr(>|z|)"]
    )
  }
})

message("langRussian across OLR models:")
print(lang_olr_df)

# Predicted probabilities by language (controls at typical values)
get_pp_table <- function(model, model_name, data) {
  hold   <- typical_row(data)
  newdat <- tidyr::crossing(lang = c("Latvian", "Russian")) %>%
    dplyr::bind_cols(hold[, setdiff(names(hold), "lang"), drop = FALSE]) %>%
    dplyr::mutate(
      lang             = factor(lang,             levels = levels(data$lang)),
      gender           = factor(gender,           levels = levels(data$gender)),
      age_cat          = factor(age_cat,          levels = levels(data$age_cat),          ordered = TRUE),
      pol_interest_cat = factor(pol_interest_cat, levels = levels(data$pol_interest_cat), ordered = TRUE),
      edu_4            = factor(edu_4,            levels = levels(data$edu_4),            ordered = TRUE)
    )
  pp <- predict(model, newdata = newdat, type = "prob")$fit
  data.frame(
    model  = model_name,
    lang   = c("Latvian", "Russian"),
    P_low  = round(pp[, 1], 3),
    P_mid  = round(pp[, 2], 3),
    P_high = round(pp[, 3], 3)
  )
}

pp_table <- purrr::map2_dfr(models_olr, names(models_olr), ~ get_pp_table(.x, .y, analysis_df_olr))

message("Predicted probabilities by language across OLR models:")
print(pp_table)

# Predicted probability plot (M5 OLR, varies threat perception by language)
eff  <- ggeffects::ggpredict(m5_olr, terms = c("qe4_2_num [1:4]", "lang"))
p_pp <- plot(eff) +
  labs(
    x     = "Perceived threat to Latvia (1–4)",
    y     = "Predicted probability",
    title = "Predicted probability of trust categories by threat level (M5 OLR)"
  )
p_pp

ggsave(file.path(fig_dir, "predicted_probs_qe4_2_by_lang_M5_OLR.png"),
       p_pp, width = 7, height = 5, dpi = 300)


# OLS coefficient table (M1-M5) saved to HTML
rows_def <- tribble(
  ~id,               ~label,                                          ~block,     ~kind,
  "(Intercept)",     "Constant / Intercept",                          "none",     "coef",
  "langRussian",     "Lang: Russian (vs Latvian)",                    "none",     "coef",
  "HEADER_CONTROLS", "Controls",                                      "header",   "header",
  "age_cat",         "Age (joint test)",                              "control",  "joint",
  "gender",          "Gender (joint test)",                           "control",  "joint",
  "edu_4",           "Education (joint test)",                        "control",  "joint",
  "pol_interest_cat","Political interest (joint test)",               "control",  "joint",
  "soc_class_cat",   "Social class (joint test)",                     "control",  "joint",
  "HEADER_THREAT",   "Threat perceptions",                            "header",   "header",
  "qe4_1_num",       "Invasion: threat to EU (1–4)",                  "threat",   "coef",
  "qe4_2_num",       "Invasion: threat to Latvia (1–4)",              "threat",   "coef",
  "qe3.4_num",       "War spreading to EU (1/0)",                     "threat",   "coef",
  "qe3.5_num",       "War spreading to Latvia (1/0)",                 "threat",   "coef",
  "qe3.7_num",       "Problems in energy/goods supply (1/0)",         "threat",   "coef",
  "HEADER_RESP",     "Policy responses",                              "header",   "header",
  "qe4_8_num",       "EU reduce energy dependence from Russia (1–4)", "response", "coef",
  "qe4_12_num",      "Reduce oil imports/invest in renewables (1–4)", "response", "coef",
  "qe4_14_num",      "Fill gas storage for winter (1–4)",             "response", "coef",
  "qe2_2_num",       "Ban Russian state-owned media (1–4)",           "response", "coef",
  "qe1_1_num",       "Satisfaction with Latvia's Ukraine response (1–4)", "response", "coef"
)

cell_for_model <- function(fit, id, kind) {
  if (kind == "header") return("")
  if (kind == "joint") return(fmt_joint(factor_joint_p(fit, id)))
  if (kind == "coef") {
    td  <- tidy_robust(fit)
    row <- td %>% filter(term == id)
    if (nrow(row) == 0) return("")
    return(fmt_b_se(row$estimate, row$std.error, row$p.value))
  }
  ""
}

models_for_table <- list(
  `Model 1` = m1_ols, `Model 2` = m2_ols, `Model 3` = m3_ols,
  `Model 4` = m4_ols, `Model 5` = m5_ols
)

wide_tbl <- rows_def %>%
  mutate(
    `Model 1` = map2_chr(id, kind, ~ cell_for_model(models_for_table[[1]], .x, .y)),
    `Model 2` = map2_chr(id, kind, ~ cell_for_model(models_for_table[[2]], .x, .y)),
    `Model 3` = map2_chr(id, kind, ~ cell_for_model(models_for_table[[3]], .x, .y)),
    `Model 4` = map2_chr(id, kind, ~ cell_for_model(models_for_table[[4]], .x, .y)),
    `Model 5` = map2_chr(id, kind, ~ cell_for_model(models_for_table[[5]], .x, .y))
  ) %>%
  select(Variable = label, `Model 1`:`Model 5`, block)

adj_r2  <- sapply(models_for_table, function(m) summary(m)$adj.r.squared)
adj_row <- tibble(Variable = "Adjusted R²",
                  `Model 1` = sprintf("%.3f", adj_r2[1]),
                  `Model 2` = sprintf("%.3f", adj_r2[2]),
                  `Model 3` = sprintf("%.3f", adj_r2[3]),
                  `Model 4` = sprintf("%.3f", adj_r2[4]),
                  `Model 5` = sprintf("%.3f", adj_r2[5]))

n_row <- tibble(Variable = "Observations",
                `Model 1` = as.character(nobs(models_for_table[[1]])),
                `Model 2` = as.character(nobs(models_for_table[[2]])),
                `Model 3` = as.character(nobs(models_for_table[[3]])),
                `Model 4` = as.character(nobs(models_for_table[[4]])),
                `Model 5` = as.character(nobs(models_for_table[[5]])))

wide_tbl_final <- bind_rows(wide_tbl %>% select(-block), adj_row, n_row)

block_vec    <- wide_tbl$block
header_idx   <- which(block_vec == "header")
control_idx  <- which(block_vec == "control")
threat_idx   <- which(block_vec == "threat")
response_idx <- which(block_vec == "response")
fit_start    <- nrow(wide_tbl) + 1
fit_end      <- nrow(wide_tbl_final)

kbl_out <- wide_tbl_final %>%
  kbl(align = c("l", rep("c", 5)), booktabs = TRUE,
      caption = "OLS coefficients with robust (HC1) SEs and model fit statistics") %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover"),
                html_font = "Times New Roman") %>%
  row_spec(control_idx,  background = "#F5F5F5") %>%
  row_spec(threat_idx,   background = "#FFFFFF") %>%
  row_spec(response_idx, background = "#F5F5F5") %>%
  row_spec(header_idx,   background = "#D9D9D9", bold = TRUE,
           extra_css = "border-top: 2px solid #999;") %>%
  row_spec(fit_start:fit_end, background = "#FFFFFF") %>%
  footnote(
    general       = "*** p < .001; ** p < .01; * p < .05; † p < .10. Robust (HC1) standard errors in parentheses.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

save_kable(kbl_out, file = file.path(fig_dir, "coef_table_M1toM5.html"))
message("OLS coefficient table saved to figures/coef_table_M1toM5.html")

# OLS diagnostics (residual plots and component-plus-residual plots)
png(file.path(fig_dir, "diagnostics_M5_OLS.png"), width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 2)); plot(m5_ols)
dev.off()

png(file.path(fig_dir, "component_plus_residuals_M5_OLS.png"), width = 1400, height = 1000, res = 150)
car::crPlots(m5_ols, terms = ~ qe4_1_num + qe4_2_num + qe4_8_num + qe4_12_num + qe4_14_num)
dev.off()

# =============================================================================
# 15. SEM: latent mediation model (WLSMV)
# =============================================================================
# Language → [Latent Threat, Latent Response] → Trust in NATO
# Estimator: WLSMV (robust to ordinal DV)
# Controls included in all structural equations

# SEM variable blocks (qe4_1_num dropped from threats after model comparison)
controls_sem  <- c("age_cat", "gender", "soc_class_cat", "pol_interest_cat", "edu_4")
threats_sem   <- c("qe4_2_num", "qe3.4_num", "qe3.5_num", "qe3.7_num")
responses_sem <- c("qe4_8_num", "qe4_12_num", "qe4_14_num", "qe2_2_num", "qe1_1_num")
ordinals      <- "trust_nato_ord"

# Build SEM base frame
sem_base <- analysis_df_ols %>%
  transmute(
    trust_nato_ord,
    lang_Russian = ifelse(lang == "Russian", 1, 0),
    across(all_of(threats_sem)),
    across(all_of(responses_sem)),
    across(all_of(controls_sem))
  )

# Dummy-code controls for lavaan
ctrl_df     <- safe_treatment_dummies(sem_base,
                                      "age_cat + gender + soc_class_cat + pol_interest_cat + edu_4")
stopifnot(nrow(ctrl_df) == nrow(sem_base))
control_rhs <- paste(colnames(ctrl_df), collapse = " + ")

sem_df <- dplyr::bind_cols(
  sem_base %>% select(trust_nato_ord, lang_Russian,
                      all_of(threats_sem), all_of(responses_sem)),
  ctrl_df
)

# Residual covariances within factors (guided by modification indices and theory)
extra_resids <- '
  qe4_12_num ~~ qe4_14_num
  qe3.4_num  ~~ qe3.5_num
  qe3.5_num  ~~ qe3.7_num
'

model_wls <- glue('
  # Measurement model
  THREAT   =~ {paste(threats_sem,   collapse = " + ")}
  RESPONSE =~ {paste(responses_sem, collapse = " + ")}

  # Within-factor residual covariances
  {extra_resids}

  # Structural equations
  THREAT         ~ a1*lang_Russian + {control_rhs}
  RESPONSE       ~ a2*lang_Russian + d*THREAT + {control_rhs}
  trust_nato_ord ~ b1*THREAT + b2*RESPONSE + cp*lang_Russian + {control_rhs}

  # Defined indirect and total effects
  ind_lang_via_THREAT   := a1*b1
  ind_lang_via_RESPONSE := a2*b2
  ind_lang_chain        := a1*d*b2
  ind_lang_total        := ind_lang_via_THREAT + ind_lang_via_RESPONSE + ind_lang_chain
  total_lang_to_trust   := cp + ind_lang_total
')

fit_wls <- sem(model_wls, data = sem_df, estimator = "WLSMV",
               ordered = ordinals, std.lv = TRUE)

message("SEM latent model summary:")
print(summary(fit_wls, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))

# Path diagram for latent SEM
draw_path_map <- function(fit,
                          estimator_label = "WLSMV",
                          file_stub       = "path_map_structural_WLSMV",
                          pos_col = "#1b7837", neg_col = "#b2182b", gray = "#888888") {
  pe <- parameterEstimates(fit, standardized = TRUE)
  
  get_edge <- function(lhs, rhs) {
    r <- pe %>% dplyr::filter(op == "~", lhs == !!lhs, rhs == !!rhs)
    if (nrow(r) == 0L) return(list(lbl = "NA", col = "#555555", p = NA_real_))
    b <- r$std.all[1]; p <- r$pvalue[1]
    list(lbl = sprintf("%.2f%s", b, p_stars(p)),
         col = ifelse(is.finite(b) && b >= 0, pos_col, neg_col), p = p)
  }
  
  dv     <- if (any(pe$lhs == "trust_nato_ord")) "trust_nato_ord" else "trust_nato_num"
  A      <- get_edge("THREAT",   "lang_Russian")
  D      <- get_edge("RESPONSE", "THREAT")
  B2     <- get_edge(dv,         "RESPONSE")
  CP     <- get_edge(dv,         "lang_Russian")
  B1     <- get_edge(dv,         "THREAT")
  L2     <- get_edge("RESPONSE", "lang_Russian")
  r2v    <- unlist(inspect(fit, "r2"))
  r2_note <- sprintf("R²: Threat = %.2f; Response = %.2f; Trust = %.2f",
                     as.numeric(r2v["THREAT"]),
                     as.numeric(r2v["RESPONSE"]),
                     as.numeric(r2v[dv]))
  if (!is.na(CP$p) && CP$p >= .05) CP$col <- gray
  
  g <- DiagrammeR::grViz(glue('
    digraph {{
      graph [layout = dot, rankdir = LR, nodesep = 0.6, ranksep = 0.7]
      node  [shape = box, style = "rounded,filled", fillcolor = "white",
             fontsize = 11, fontname = "Helvetica"]
      edge  [fontsize = 11, fontname = "Helvetica", color = "#333333", penwidth = 1.2]
      Language [label = "Language\\n(Russian = 1)"];
      Threat   [label = "Threat (latent)"];
      Response [label = "Response (latent)"];
      Trust    [label = "Trust in NATO"];
      Controls [label = "Controls:\\nAge, Gender, Class,\\nInterest, Education"];
      Language -> Threat   [label = "{A$lbl}",  color = "{A$col}"];
      Threat   -> Response [label = "{D$lbl}",  color = "{D$col}"];
      Response -> Trust    [label = "{B2$lbl}", color = "{B2$col}"];
      Language -> Trust    [label = "{CP$lbl}", color = "{CP$col}"];
      Threat   -> Trust    [label = "{B1$lbl}", color = "{B1$col}"];
      Language -> Response [label = "{L2$lbl}", color = "{L2$col}",
                            style = {ifelse(!is.na(L2$p) && L2$p >= .05, "dashed", "solid")}];
      Controls -> Threat   [label = "included", color = "{gray}"];
      Controls -> Response [label = "included", color = "{gray}"];
      Controls -> Trust    [label = "included", color = "{gray}"];
      labelloc = "t";
      label = "Structural mediation model (standardized coefficients)";
      subgraph cluster_legend {{
        label = ""; fontsize = 10; color = "white";
        Legend [shape = note, fontname = "Helvetica", fontsize = 10,
                fillcolor = "white", style = "filled",
                label = "Notes:\\n• Estimator: {estimator_label}\\n• {r2_note}\\n• *** p<.001; ** p<.01; * p<.05"];
      }}
    }}
  '))
  
  print(g)
  svg_txt <- DiagrammeRsvg::export_svg(g)
  writeLines(svg_txt, file.path("figures", paste0(file_stub, ".svg")))
  if (requireNamespace("rsvg", quietly = TRUE)) {
    rsvg::rsvg_pdf(charToRaw(svg_txt),
                   file.path("figures", paste0(file_stub, ".pdf")))
    rsvg::rsvg_png(charToRaw(svg_txt),
                   file.path("figures", paste0(file_stub, ".png")),
                   width = 1800, height = 1050)
  }
}

draw_path_map(fit_wls, estimator_label = "WLSMV",
              file_stub = "path_map_structural_WLSMV")

# SEM structural paths table
pe     <- parameterEstimates(fit_wls, standardized = TRUE)
stdsol <- standardizedSolution(fit_wls, se = TRUE, z = TRUE)

rows_str <- stdsol %>%
  filter(op == "~",
         lhs %in% c("THREAT", "RESPONSE", "trust_nato_ord"),
         rhs %in% c("lang_Russian", "THREAT", "RESPONSE")) %>%
  mutate(p_calc = ifelse(is.finite(z), 2 * pnorm(abs(z), lower.tail = FALSE), NA_real_)) %>%
  transmute(
    Path = case_when(
      lhs == "THREAT"          & rhs == "lang_Russian" ~ "THREAT \u2190 Language",
      lhs == "RESPONSE"        & rhs == "THREAT"       ~ "RESPONSE \u2190 THREAT",
      lhs == "RESPONSE"        & rhs == "lang_Russian" ~ "RESPONSE \u2190 Language",
      lhs == "trust_nato_ord"  & rhs == "RESPONSE"     ~ "TRUST \u2190 RESPONSE",
      lhs == "trust_nato_ord"  & rhs == "THREAT"       ~ "TRUST \u2190 THREAT",
      lhs == "trust_nato_ord"  & rhs == "lang_Russian" ~ "TRUST \u2190 Language",
      TRUE ~ paste(lhs, "<-", rhs)
    ),
    `Std. Beta` = round(est.std, 3),
    SE          = round(se, 3),
    z           = round(z, 2),
    p           = ifelse(is.na(p_calc), "", scales::pvalue(p_calc, accuracy = 0.001))
  ) %>%
  unique()

# SEM indirects table
label_candidates <- c("ind_lang_via_THREAT", "ind_lang_via_RESPONSE",
                      "ind_lang_chain", "ind_lang_total", "total_lang_to_trust")
labs_present <- intersect(pe$label, label_candidates)

rows_ind <- pe %>%
  filter(op == ":=", label %in% labs_present) %>%
  transmute(
    Effect = case_when(
      label == "ind_lang_via_THREAT"   ~ "Indirect via THREAT",
      label == "ind_lang_via_RESPONSE" ~ "Indirect via RESPONSE",
      label == "ind_lang_chain"        ~ "Chain (Lang \u2192 Threat \u2192 Response \u2192 Trust)",
      label == "ind_lang_total"        ~ "Total indirect",
      label == "total_lang_to_trust"   ~ "Total effect",
      TRUE ~ label
    ),
    Estimate = round(std.all, 3),
    SE       = round(se, 3),
    z        = round(z, 2),
    p        = ifelse(is.na(pvalue), "", scales::pvalue(pvalue, accuracy = 0.001))
  )

# Save SEM tables
gt(rows_str) %>%
  tab_header(title = md("**Structural paths (standardized)**")) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  gtsave("figures/sem_structural_table.html")

gt(rows_ind) %>%
  tab_header(title = md("**Indirect and total effects (standardized)**")) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  gtsave("figures/sem_indirects_table.html")

# =============================================================================
# 16. SEM: indexed mediation model (WLSMV)
# =============================================================================
# Alternative to the latent model using observed row-mean indexes

sem_df_idx <- bind_cols(
  sem_base %>% select(trust_nato_ord, lang_Russian),
  tibble(
    threat_index   = rowMeans(sem_base[, threats_sem],   na.rm = TRUE),
    response_index = rowMeans(sem_base[, responses_sem], na.rm = TRUE)
  ),
  ctrl_df
)

model_indexed <- glue('
  threat_index   ~ a1*lang_Russian + {control_rhs}
  response_index ~ a2*lang_Russian + d*threat_index + {control_rhs}
  trust_nato_ord ~ b1*threat_index + b2*response_index + cp*lang_Russian + {control_rhs}

  ind_lang_via_THREAT   := a1*b1
  ind_lang_via_RESPONSE := a2*b2
  ind_lang_chain        := a1*d*b2
  ind_lang_total        := ind_lang_via_THREAT + ind_lang_via_RESPONSE + ind_lang_chain
  total_lang_to_trust   := cp + ind_lang_total
')

fit_idx <- sem(model_indexed, data = sem_df_idx, estimator = "WLSMV",
               ordered = ordinals, std.lv = TRUE)

message("SEM indexed model summary:")
print(summary(fit_idx, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))

# Path diagram for indexed SEM
draw_path_map_index <- function(fit,
                                estimator_label = "WLSMV (indexed)",
                                file_stub       = "path_map_structural_INDEXED",
                                pos_col = "#1b7837", neg_col = "#b2182b", gray = "#888888") {
  pe <- parameterEstimates(fit, standardized = TRUE)
  
  get_edge <- function(lhs, rhs) {
    r <- pe %>% dplyr::filter(op == "~", lhs == !!lhs, rhs == !!rhs)
    if (nrow(r) == 0L) return(list(lbl = "NA", col = "#555555", p = NA_real_))
    b <- r$std.all[1]; p <- r$pvalue[1]
    list(lbl = sprintf("%.2f%s", b, p_stars(p)),
         col = ifelse(is.finite(b) && b >= 0, pos_col, neg_col), p = p)
  }
  
  dv     <- if (any(pe$lhs == "trust_nato_ord")) "trust_nato_ord" else "trust_nato_num"
  A      <- get_edge("threat_index",   "lang_Russian")
  D      <- get_edge("response_index", "threat_index")
  B2     <- get_edge(dv,               "response_index")
  CP     <- get_edge(dv,               "lang_Russian")
  B1     <- get_edge(dv,               "threat_index")
  L2     <- get_edge("response_index", "lang_Russian")
  r2v    <- unlist(inspect(fit, "r2"))
  r2_note <- sprintf("R²: ThreatIdx = %.2f; ResponseIdx = %.2f; Trust = %.2f",
                     as.numeric(r2v["threat_index"]),
                     as.numeric(r2v["response_index"]),
                     as.numeric(r2v[dv]))
  if (!is.na(CP$p) && CP$p >= .05) CP$col <- gray
  
  g <- DiagrammeR::grViz(glue('
    digraph {{
      graph [layout = dot, rankdir = LR, nodesep = 0.6, ranksep = 0.7]
      node  [shape = box, style = "rounded,filled", fillcolor = "white",
             fontsize = 11, fontname = "Helvetica"]
      edge  [fontsize = 11, fontname = "Helvetica", color = "#333333", penwidth = 1.2]
      Language [label = "Language\\n(Russian = 1)"];
      Threat   [label = "Threat (index)"];
      Response [label = "Response (index)"];
      Trust    [label = "Trust in NATO"];
      Controls [label = "Controls:\\nAge, Gender, Class,\\nInterest, Education"];
      Language -> Threat   [label = "{A$lbl}",  color = "{A$col}"];
      Threat   -> Response [label = "{D$lbl}",  color = "{D$col}"];
      Response -> Trust    [label = "{B2$lbl}", color = "{B2$col}"];
      Language -> Trust    [label = "{CP$lbl}", color = "{CP$col}"];
      Threat   -> Trust    [label = "{B1$lbl}", color = "{B1$col}"];
      Language -> Response [label = "{L2$lbl}", color = "{L2$col}",
                            style = {ifelse(!is.na(L2$p) && L2$p >= .05, "dashed", "solid")}];
      Controls -> Threat   [label = "included", color = "{gray}"];
      Controls -> Response [label = "included", color = "{gray}"];
      Controls -> Trust    [label = "included", color = "{gray}"];
      labelloc = "t";
      label = "Structural mediation indexed model (standardized coefficients)";
      subgraph cluster_legend {{
        label = ""; fontsize = 10; color = "white";
        Legend [shape = note, fontname = "Helvetica", fontsize = 10,
                fillcolor = "white", style = "filled",
                label = "Notes:\\n• Estimator: {estimator_label}\\n• {r2_note}\\n• *** p<.001; ** p<.01; * p<.05"];
      }}
    }}
  '))
  
  print(g)
  svg_txt <- DiagrammeRsvg::export_svg(g)
  if (!dir.exists("figures")) dir.create("figures")
  if (requireNamespace("rsvg", quietly = TRUE)) {
    rsvg::rsvg_pdf(charToRaw(svg_txt),
                   file.path("figures", paste0(file_stub, ".pdf")))
    rsvg::rsvg_png(charToRaw(svg_txt),
                   file.path("figures", paste0(file_stub, ".png")),
                   width = 1800, height = 1050)
  }
}

draw_path_map_index(fit_idx, estimator_label = "WLSMV (indexed)",
                    file_stub = "path_map_structural_INDEXED")

# =============================================================================
# 17. Export SEM tables (RTF and LaTeX)
# =============================================================================

fmt_p_val <- function(p) ifelse(is.na(p), "", scales::pvalue(p, accuracy = 0.001))

# Recompute structural paths and indirects from latent model for export
stdsol_exp <- standardizedSolution(fit_wls, se = TRUE, z = TRUE)
pe_exp     <- parameterEstimates(fit_wls, standardized = TRUE)

rows_str_exp <- stdsol_exp %>%
  filter(op == "~",
         lhs %in% c("THREAT", "RESPONSE", "trust_nato_ord"),
         rhs %in% c("lang_Russian", "THREAT", "RESPONSE")) %>%
  mutate(
    p_calc = ifelse(is.finite(z), 2 * pnorm(abs(z), lower.tail = FALSE), NA_real_),
    Sig    = sig_code(p_calc),
    Path   = case_when(
      lhs == "THREAT"         & rhs == "lang_Russian" ~ "THREAT \u2190 Language",
      lhs == "RESPONSE"       & rhs == "THREAT"       ~ "RESPONSE \u2190 THREAT",
      lhs == "RESPONSE"       & rhs == "lang_Russian" ~ "RESPONSE \u2190 Language",
      lhs == "trust_nato_ord" & rhs == "RESPONSE"     ~ "TRUST \u2190 RESPONSE",
      lhs == "trust_nato_ord" & rhs == "THREAT"       ~ "TRUST \u2190 THREAT",
      lhs == "trust_nato_ord" & rhs == "lang_Russian" ~ "TRUST \u2190 Language",
      TRUE ~ paste(lhs, "<-", rhs)
    )
  ) %>%
  transmute(Path,
            `Std. Beta` = round(est.std, 3),
            SE          = round(se, 3),
            z           = round(z, 2),
            p           = fmt_p_val(p_calc),
            Sig) %>%
  unique()

rows_ind_exp <- pe_exp %>%
  filter(op == ":=", label %in% labs_present) %>%
  mutate(
    Sig    = sig_code(pvalue),
    Effect = case_when(
      label == "ind_lang_via_THREAT"   ~ "Indirect via THREAT",
      label == "ind_lang_via_RESPONSE" ~ "Indirect via RESPONSE",
      label == "ind_lang_chain"        ~ "Chain (Lang \u2192 Threat \u2192 Response \u2192 Trust)",
      label == "ind_lang_total"        ~ "Total indirect",
      label == "total_lang_to_trust"   ~ "Total effect",
      TRUE ~ label
    )
  ) %>%
  transmute(Effect,
            Estimate = round(std.all, 3),
            SE       = round(se, 3),
            z        = round(z, 2),
            p        = fmt_p_val(pvalue),
            Sig)

note_text <- md("**Bold** = p < .05; † = p < .10. *** p < .001; ** p < .01; * p < .05.")

gt_str_exp <- gt(rows_str_exp) %>%
  tab_header(title = md("**Structural paths (standardized)**")) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  tab_source_note(note_text)

gt_ind_exp <- gt(rows_ind_exp) %>%
  tab_header(title = md("**Indirect and total effects (standardized)**")) %>%
  fmt_missing(columns = everything(), missing_text = "") %>%
  tab_source_note(note_text)

gtsave(gt_str_exp, "tables/sem_structural_table.rtf")
gtsave(gt_ind_exp, "tables/sem_indirects_table.rtf")

# LaTeX export with fallback
for (obj_name in c("gt_str_exp", "gt_ind_exp")) {
  stub <- ifelse(obj_name == "gt_str_exp", "sem_structural_table", "sem_indirects_table")
  tryCatch(
    gtsave(get(obj_name), paste0("tables/", stub, ".tex")),
    error = function(e) {
      writeLines(as.character(gt::as_latex(get(obj_name))),
                 paste0("tables/", stub, ".tex"))
    }
  )
}

message("02_analysis.R complete — all figures saved to figures/ and tables saved to tables/")