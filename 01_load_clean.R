# =============================================================================
# 01_load_clean.R — Load data, filter to Latvia, define IV and DV
# =============================================================================
# Input:  data/eb_data.sav  (Eurobarometer; download from GESIS)
# Output: latvia_data (data frame in environment)
# =============================================================================

source("utils.R")

library(tidyverse)
library(haven)
library(conflicted)

conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("recode", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("count",  "dplyr", quiet = TRUE)
conflicted::conflict_prefer("sum",    "base",  quiet = TRUE)

# -----------------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------------
# Use a relative path — never paste your local desktop path here
all_euro <- haven::read_sav("ZA7902_v1-0-0.sav")

# Convert labelled variables to factors
all_euro <- all_euro %>%
  mutate(across(where(haven::is.labelled), ~ haven::as_factor(.x)))

# -----------------------------------------------------------------------------
# 2. Filter to Latvia
# -----------------------------------------------------------------------------
keep_vars <- c(
  "serialid",                                         # individual identifier
  "country", "p13", "qa6a_12",                        # IV (language), DV (NATO trust)
  "d11r2", "d10", "d63", "d9b",                       # controls: age, gender, class, education
  "polintr_1", "polintr_2", "polintr_3", "polintr_4", # political interest
  "qe1_1", "qe2_2", "qe4_1", "qe4_2",                 # Ukraine attitudes
  "qe4_8", "qe4_12", "qe4_14",                        # energy policy
  "qe3.4", "qe3.5", "qe3.7"                           # binary fear items
)

latvia_data <- all_euro %>%
  filter(country == "LV - Latvia") %>%
  select(any_of(keep_vars)) %>%
  droplevels()

# Verify: expect 1027 rows (802 Latvian + 225 Russian)
stopifnot(nrow(latvia_data) > 0)
message("Rows after Latvia filter: ", nrow(latvia_data))

# -----------------------------------------------------------------------------
# 3. Independent variable — language of interview (p13)
# -----------------------------------------------------------------------------
latvia_data <- latvia_data %>%
  mutate(lang = case_when(
    p13 == "Latvian (Latvia)"  ~ "Latvian",
    p13 == "Russian (Belarus)" ~ "Russian",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(lang))

# Verify language split
stopifnot(all(c("Latvian", "Russian") %in% latvia_data$lang))
message("Language distribution:\n")
print(table(latvia_data$lang))

# -----------------------------------------------------------------------------
# 4. Dependent variable — trust in NATO (qa6a_12), recoded to 1–3
# -----------------------------------------------------------------------------
# Original: 1 = Tend to trust, 2 = Tend not to trust, 3 = Don't know
# Recoded:  1 = Tend not to trust, 2 = Don't know, 3 = Tend to trust
# Higher values = more trust

latvia_data <- latvia_data %>%
  mutate(
    trust_nato_num = case_when(
      qa6a_12 == "Tend not to trust"        ~ 1,
      qa6a_12 == "Don't know (SPONTANEOUS)" ~ 2,
      qa6a_12 == "Tend to trust"            ~ 3,
      TRUE ~ NA_real_
    ),
    trust_nato_ord = factor(
      trust_nato_num,
      levels = c(1, 2, 3),
      labels = c("Tend not to trust", "Don't know", "Tend to trust"),
      ordered = TRUE
    )
  )

# Verify no unexpected NAs introduced
message("NATO trust distribution:\n")
print(table(latvia_data$trust_nato_ord, useNA = "ifany"))



# =============================================================================
# 01a_recode — Recode control variables and covariates
# =============================================================================
# Input:  latvia_data (from 01_load_clean.R)
# Output: latvia_data with recoded controls, covariates, and frequency tables
# =============================================================================

source("utils.R")

library(tidyverse)
library(conflicted)

conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("count",  "dplyr", quiet = TRUE)

# =============================================================================
# 1. Education (d9b)
# =============================================================================
# Original Eurobarometer coding: d9b — Education level, country-specific, 11 levels
# Recoded to numeric (1–11), ordered factor (11-level), and collapsed 4-level factor
# Higher number = more education

latvia_data <- latvia_data %>%
  mutate(
    d9b_chr = as.character(d9b),
    edu_num = case_when(
      str_detect(d9b_chr, "^LV:No school")                  ~ 1,  # less than 4 years
      str_detect(d9b_chr, "^LV:Primary")                    ~ 2,  # 4–6 years depending on cohort
      str_detect(d9b_chr, "^LV:Basic education")            ~ 3,  # 7–9 years depending on cohort
      str_detect(d9b_chr, "^LV:Vocational")                 ~ 4,
      str_detect(d9b_chr, "^LV:Secondary education$")       ~ 5,
      str_detect(d9b_chr, "^LV:Secondary professional")     ~ 6,
      str_detect(d9b_chr, "^LV:Post-secondary professional") ~ 7,
      str_detect(d9b_chr, "^LV:Professional higher education") ~ 8,
      str_detect(d9b_chr, "^LV:Bachelor")                   ~ 9,
      str_detect(d9b_chr, "^LV:Master")                     ~ 10,
      str_detect(d9b_chr, "^LV:PhD")                        ~ 11, # includes candidate of science
      TRUE ~ NA_real_
    ),
    # 11-level ordered factor
    edu_cat = factor(edu_num, levels = 1:11, ordered = TRUE),
    # 4-level ordered factor aligned with Latvian education system
    edu_4 = case_when(
      edu_num %in% c(1, 2, 3)      ~ "Lower secondary or less",
      edu_num %in% c(4, 5, 6)      ~ "Upper secondary",
      edu_num == 7                  ~ "Post-secondary (non-tertiary)",
      edu_num %in% c(8, 9, 10, 11) ~ "Tertiary",
      TRUE                          ~ NA_character_
    ),
    edu_4 = factor(
      edu_4,
      levels = c(
        "Lower secondary or less",
        "Upper secondary",
        "Post-secondary (non-tertiary)",
        "Tertiary"
      ),
      ordered = TRUE
    ),
    # Explicit NA versions for descriptive tables
    edu_cat_wNA = fct_explicit_na(edu_cat, na_level = "Missing (NA)"),
    edu_4_wNA   = fct_explicit_na(edu_4,   na_level = "Missing (NA)"),
    edu_raw_wNA = fct_explicit_na(factor(d9b_chr), na_level = "Missing (NA)")
  )

# Verify education distributions
message("Education (4-level):")
print(latvia_data %>% count(edu_4_wNA))

# Frequency tables by language
edu_raw_lang     <- freq_by_lang(latvia_data, edu_raw_wNA)
edu11_lang       <- freq_by_lang(latvia_data, edu_cat_wNA)
edu11_trust_lang <- trust_by_lang(latvia_data, edu_cat_wNA)
edu4_lang        <- freq_by_lang(latvia_data, edu_4_wNA)
edu4_trust_lang  <- trust_by_lang(latvia_data, edu_4_wNA)

# =============================================================================
# 2. Age, Gender, and Social Class
# =============================================================================
# Original Eurobarometer coding:
# d11r2 — Age recoded, 6 categories: 15-24, 25-34, 35-44, 45-54, 55-64, 65+
# d10   — Gender: 1 Man, 2 Woman, 3 Non-binary/other
# d63   — Social class self-assessment, 5 categories

latvia_data <- latvia_data %>%
  mutate(
    # Age: collapse 15-24 and 25-34 into 15-34 due to low frequency counts
    age_cat = factor(
      d11r2,
      levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      ordered = TRUE
    ),
    age_cat = fct_collapse(age_cat, "15-34" = c("15-24", "25-34")),
    age_cat = factor(
      age_cat,
      levels = c("15-34", "35-44", "45-54", "55-64", "65+"),
      ordered = TRUE
    ),
    
    # Gender
    gender = factor(d10),
    
    # Social class: set non-substantive responses to NA before recoding
    d63_clean = d63 %>%
      na_if("None (SPONT.)") %>%
      na_if("Refusal (SPONT.)") %>%
      na_if("DK (SPONT.)"),
    
    # 5-level ordered factor
    soc_class_ord = factor(
      d63_clean,
      levels = c(
        "The working class of society",
        "The lower middle class of society",
        "The middle class of society",
        "The upper middle class of society",
        "The higher class of society"
      ),
      ordered = TRUE
    ),
    
    # 4-level collapsed: upper middle and higher combined due to low cell counts
    soc_class4 = fct_collapse(
      soc_class_ord,
      "Upper middle/higher" = c(
        "The upper middle class of society",
        "The higher class of society"
      )
    ),
    soc_class4 = factor(
      soc_class4,
      levels = c(
        "The working class of society",
        "The lower middle class of society",
        "The middle class of society",
        "The upper middle/higher class of society"
      ),
      ordered = TRUE
    ),
    
    # Unordered version for OLS dummy coding
    soc_class4_cat = factor(soc_class4, ordered = FALSE),
    
    # Explicit NA versions for descriptive tables
    age_cat_wNA        = fct_explicit_na(age_cat,        na_level = "Missing (NA)"),
    gender_wNA         = fct_explicit_na(gender,         na_level = "Missing (NA)"),
    soc_class4_wNA     = fct_explicit_na(soc_class4,     na_level = "Missing/DK/Ref"),
    soc_class4_cat_wNA = fct_explicit_na(soc_class4_cat, na_level = "Missing/DK/Ref")
  ) %>%
  droplevels()

# Verify: check how many social class responses were set to NA by cleaning
message("Social class NA check:")
print(table(latvia_data$d63_clean, useNA = "ifany"))

# Frequency tables by language
age_lang          <- freq_by_lang(latvia_data, age_cat_wNA)
age_trust_lang    <- trust_by_lang(latvia_data, age_cat_wNA)
gender_lang       <- freq_by_lang(latvia_data, gender_wNA)
gender_trust_lang <- trust_by_lang(latvia_data, gender_wNA)
social_lang       <- freq_by_lang(latvia_data, soc_class4_cat_wNA)
social_trust_lang <- trust_by_lang(latvia_data, soc_class4_cat_wNA)

# =============================================================================
# 3. Political Interest
# =============================================================================
# Original Eurobarometer coding: polintr_1 through polintr_4
# One-hot dummies collapsed into a single ordered variable
# Higher number = more interest (inverted from Eurobarometer scaling)

pol_vars <- c("polintr_1", "polintr_2", "polintr_3", "polintr_4")

latvia_data <- latvia_data %>%
  mutate(
    pol_interest_cat = case_when(
      polintr_4 == "Yes" ~ "Not at all",
      polintr_3 == "Yes" ~ "Low",
      polintr_2 == "Yes" ~ "Medium",
      polintr_1 == "Yes" ~ "Strong",
      TRUE ~ NA_character_
    ),
    pol_interest_cat = factor(
      pol_interest_cat,
      levels = c("Not at all", "Low", "Medium", "Strong"),
      ordered = TRUE
    ),
    pol_interest_num  = as.numeric(pol_interest_cat),
    pol_interest_cat_wNA = fct_explicit_na(pol_interest_cat, na_level = "Missing (NA)")
  ) %>%
  droplevels()

# Verify: each row should have exactly one "Yes" across the four dummies
check_pol <- latvia_data %>%
  transmute(
    pol_yes_count = rowSums(
      across(all_of(pol_vars), ~ .x == "Yes"),
      na.rm = TRUE
    )
  ) %>%
  count(pol_yes_count)

message("Political interest one-hot check (all should be 1):")
print(check_pol)

# Frequency tables by language
pol_interest_lang      <- freq_by_lang(latvia_data, pol_interest_cat_wNA)
pol_int_trust_lang     <- trust_by_lang(latvia_data, pol_interest_cat_wNA)

# =============================================================================
# 4. Likert-Style Ukraine Attitude Variables
# =============================================================================
# Original Eurobarometer coding (higher = more agreement/satisfaction):
# qe1_1  — Satisfaction with Latvia's response to Ukraine invasion (4-point)
# qe2_2  — Agreement: ban Russian state-owned media (4-point)
# qe4_1  — Agreement: invasion is security threat to EU (4-point)
# qe4_2  — Agreement: invasion is security threat to Latvia (4-point)
# qe4_8  — Agreement: EU should reduce energy dependence from Russia (4-point)
# qe4_12 — Agreement: reduce oil imports/invest in renewables for security (4-point)
# qe4_14 — Agreement: gas storage should be filled for winter supply (4-point)
# NOTE: higher values = more agreement, inverted from Eurobarometer scaling

likert_agree_map <- c(
  "Totally disagree" = 1,
  "Tend to disagree" = 2,
  "Tend to agree"    = 3,
  "Totally agree"    = 4
)

likert_sat_map <- c(
  "Not at all satisfied" = 1,
  "Rather not satisfied" = 2,
  "Rather satisfied"     = 3,
  "Very satisfied"       = 4
)

# Non-substantive responses to be set to NA
non_substantive <- c("Don't know (SPONTANEOUS)", "Inap.", NA)

# Recode function: maps response labels to numeric, NAs anything not in map
recode_likert <- function(x, map, non_sub = non_substantive) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% non_sub] <- NA_character_
  unname(map[x_chr])
}

agree_vars <- c("qe2_2", "qe4_1", "qe4_2", "qe4_8", "qe4_12", "qe4_14")
sat_vars   <- c("qe1_1")

latvia_data <- latvia_data %>%
  mutate(
    across(all_of(sat_vars),   ~ recode_likert(.x, likert_sat_map),   .names = "{.col}_num"),
    across(all_of(agree_vars), ~ recode_likert(.x, likert_agree_map), .names = "{.col}_num")
  )

# Convert numeric Likert variables to ordered factors for frequency tables
likert_nums <- c(
  "qe1_1_num", "qe2_2_num", "qe4_1_num", "qe4_2_num",
  "qe4_8_num", "qe4_12_num", "qe4_14_num"
)

latvia_data <- latvia_data %>%
  mutate(
    across(
      all_of(likert_nums),
      ~ fct_explicit_na(factor(.x, levels = 1:4, ordered = TRUE), na_level = "Missing (NA)"),
      .names = "{.col}_f"
    )
  )

# Frequency tables by language
qe1_1_lang  <- freq_by_lang(latvia_data, qe1_1_num_f)
qe2_2_lang  <- freq_by_lang(latvia_data, qe2_2_num_f)
qe4_1_lang  <- freq_by_lang(latvia_data, qe4_1_num_f)
qe4_2_lang  <- freq_by_lang(latvia_data, qe4_2_num_f)
qe4_8_lang  <- freq_by_lang(latvia_data, qe4_8_num_f)
qe4_12_lang <- freq_by_lang(latvia_data, qe4_12_num_f)
qe4_14_lang <- freq_by_lang(latvia_data, qe4_14_num_f)

qe1_1_trust  <- trust_by_lang(latvia_data, qe1_1_num_f)
qe2_2_trust  <- trust_by_lang(latvia_data, qe2_2_num_f)
qe4_1_trust  <- trust_by_lang(latvia_data, qe4_1_num_f)
qe4_2_trust  <- trust_by_lang(latvia_data, qe4_2_num_f)  # note: was a typo in original
qe4_8_trust  <- trust_by_lang(latvia_data, qe4_8_num_f)
qe4_12_trust <- trust_by_lang(latvia_data, qe4_12_num_f)
qe4_14_trust <- trust_by_lang(latvia_data, qe4_14_num_f)

# =============================================================================
# 5. Binary Fear Variables
# =============================================================================
# Original Eurobarometer coding:
# qe3.4 — Fear: war spreading to more countries in Europe (mentioned/not mentioned)
# qe3.5 — Fear: war spreading to Latvia (mentioned/not mentioned)
# qe3.7 — Fear: problems in energy/goods supply (mentioned/not mentioned)
# Recoded to 1 = mentioned, 0 = not mentioned, NA = other

binary_vars <- c("qe3.4", "qe3.5", "qe3.7")

mentioned_labels <- c(
  "qe3.4" = "The war spreading to more countries in Europe",
  "qe3.5" = "The war spreading to our own country",
  "qe3.7" = "Problems in supply of energy or goods"
)

latvia_data <- latvia_data %>%
  mutate(
    across(
      all_of(binary_vars),
      ~ {
        x <- trimws(as.character(.x))
        v <- cur_column()
        case_when(
          is.na(x)                      ~ NA_real_,
          x == "Not mentioned"          ~ 0,
          x == mentioned_labels[[v]]    ~ 1,
          TRUE                          ~ NA_real_  # unexpected labels set to NA
        )
      },
      .names = "{.col}_num"
    )
  )

# Frequency tables by language
qe34_lang       <- freq_by_lang(latvia_data, qe3.4_num)
qe35_lang       <- freq_by_lang(latvia_data, qe3.5_num)
qe37_lang       <- freq_by_lang(latvia_data, qe3.7_num)
qe34_trust_lang <- trust_by_lang(latvia_data, qe3.4_num)
qe35_trust_lang <- trust_by_lang(latvia_data, qe3.5_num)
qe37_trust_lang <- trust_by_lang(latvia_data, qe3.7_num)

message("02_recode.R complete — latvia_data ready for analysis.")