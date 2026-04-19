# The Language of Trust: The Mechanisms of Institutional Trust in NATO among

Russian and Latvian Speakers in Latvia

**Author:** Kate Klinger\
**Date:** April 01, 2026\
**Context:** Senior Honors Thesis — Curriculum in Peace, War and Defense
at the University of North Carolina at Chapel Hill

## Overview

Using language as a proxy for ethnicity, this quantitative analysis
leverages Eurobarometer 97.2 (2022) to examine differences in levels of
institutional trust in NATO between Russian and Latvian speakers in
Latvia after Russia’s full-scale invasion of Ukraine. A set of
cross-domain theoretical assumptions underpin the empirical methods.
Social Identity Theory, Reactive Ethnicity, and Protective Motivation
Theory explain what motivates Russian and Latvian speakers to strengthen
group cohesion and engage in self-protective behaviors. This is modeled
empirically through multiple linear regression through Ordinary Least
Squares (OLS) and a Weighted Least Squares Means Variance
(WLSMV)-estimated Covariance-Based Structural Equation Model (CB-SEM) to
examine how language, demographic and social controls, war-related
threat perceptions, and policy response preferences associate with trust
in NATO independently and through latent variable causal mechanisms.

## Methods

-   OLS regression with robust (HC1) standard errors across 5 model
    blocks
-   Ordered logistic regression (`ordinal::clm`) as a robustness check
-   Structural mediation via WLSMV SEM (`lavaan`)
-   Bootstrap confidence intervals for coefficient attenuation

## Data

Eurobarometer 97.5, June-July 2022. Due to licensing restrictions, raw
data is not included in this repository. Download from
[GESIS](https://www.gesis.org) and place the `.sav` file at
`data/eb_data.sav`.

## How to Run

1.  Clone this repository
2.  Run `renv::restore()` in R to install all required packages
3.  Run scripts in order:
    -   `R/01_load_clean.R`
    -   `R/02_recode.R`
    -   `R/03_ols_models.R`
    -   `R/04_olr_models.R`
    -   `R/05_sem.R`

## Key Findings

-   Across models, the findings suggest casual mediation between
    language, threat perception, policy response preferences, and trust
    in NATO.
-   The results further indicate that language strongly attenuates as
    more variables are introduced, with policy responses as the
    strongest associational predictors of trust in NATO.
-   These findings situate linguistic identity in a volatile Eastern
    European security context, advancing understanding of how
    institutional trust operates as an individual risk-assessment in an
    evolving global order.

## Repository Structure

```         
latvia-nato-trust/
├── README.Rmd
├── utils.R
├── 01_load_clean.R
├── 02_analysis.R
├── figures/
├── tables/
└── .gitignore
```
