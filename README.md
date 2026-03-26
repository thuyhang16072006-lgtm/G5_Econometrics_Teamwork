# Econometrics Project

## The Moderating Role of Work Experience on the Wage Penalty of Horizontal Mismatch

---

## Research Overview

This project investigates whether work experience mitigates (remedies) or amplifies (widens) the wage penalty associated with horizontal mismatch—a situation where a worker’s job is not aligned with their field of education.

A key focus is on gender differences, particularly whether the wage penalty evolves differently for women, potentially due to career interruptions.

---

## Research Question

Does work experience reduce the wage penalty caused by horizontal mismatch?
Or does the penalty persist or even increase over time—especially for women?

---

## Data

* Source: Vietnam Labour Force Survey (LFS) / VHLSS 2018
* Initial sample size: ~800,000 observations
* Final analytical sample (after filtering): ~8,000–20,000 observations

---

## Data Preparation

### Sample Restrictions

The dataset is filtered based on the following criteria:

* Age: 15–65
* Industry: Manufacturing (VSIC codes 10–33)
* Education: College degree or higher
* Employment type: Wage workers
* Income: Positive wage
* Region: Red River Delta
* Employment status: Currently working

---

### Key Variables

#### Dependent Variable

* `ln_wage`: Log of monthly income

#### Main Explanatory Variable

* `mismatch`:

  * 1 = Job not matched with field of study
  * 0 = Matched

#### Experience (Mincer Specification)

* `exp = age − years_of_schooling − 6`
* `exp² = exp^2`

#### Control Variables

* Gender (`female`)
* Education level
* Urban/rural
* Economic sector
* Marital status
* Industry fixed effects (VSIC 2-digit)

#### Interaction Terms

* `mismatch × exp`
* `mismatch × female`
* `exp × female`
* `mismatch × exp × female`

---

## Methodology

### Baseline Model

A standard Mincer wage equation is estimated using OLS to identify:

* Wage penalty of mismatch
* Gender wage gap

### Main Model (with Interactions)

Includes interaction terms to test whether:

* Experience reduces mismatch penalties
* Effects differ by gender

### Key Interpretation

* `β₅`: Effect of experience on mismatch penalty (men)
* `β₈`: Additional effect for women
* If `β₈ < 0`: Women benefit less from experience

---

## Empirical Strategy

### Descriptive Analysis

* Compare matched vs. mismatched workers
* Gender comparisons within mismatch groups
* Wage distribution analysis

### Econometric Models

1. Baseline OLS
2. Interaction model
3. Industry fixed effects model

### Marginal Effects

Estimate how mismatch penalties evolve with experience:

* For men:
  `∂ln(wage)/∂mismatch = β₁ + β₅·exp`
* For women:
  `∂ln(wage)/∂mismatch = β₁ + β₆ + (β₅ + β₈)·exp`

---

## Robustness Checks

* Alternative mismatch proxy
* Restrict to full-time workers
* Exclude new labor market entrants
* Gender-specific regressions
* Heckman selection correction (implemented in R)

---

## Expected Contribution

This study contributes to the literature by:

* Examining dynamic effects of mismatch over the career lifecycle
* Providing gender-specific insights
* Offering evidence from a large-scale developing country dataset (Vietnam)

---

## Project Structure (Suggested)

```id="k0d1s9"
├── data/              # Raw and cleaned datasets
├── scripts/           # R scripts for data cleaning & analysis
├── output/            # Regression tables & figures
├── docs/              # Paper drafts and notes
└── README.md
```

---

## Tools

* R (tidyverse, fixest, ggplot2, sampleSelection)

---

## Author

* [Group 4]
* Econometrics Project – 2026

---

## License

This project is for academic and research purposes only.
