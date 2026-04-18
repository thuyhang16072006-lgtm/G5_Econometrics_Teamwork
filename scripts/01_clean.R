# ============================================================
# 01_clean.R — Sample Filtering & Variable Construction
# Project : Horizontal Mismatch & Wage Penalty
# Data    : Labour Force Survey (LFS) 2018 — Vietnam
# ============================================================
# PIPELINE:
#   Raw LFS → [filter] → [construct variables] → [winsorize]
#   → df_clean (full) + df_male + df_reentrant + train/test
# ============================================================

library(tidyverse)
library(haven)

# ── 0. HELPER ────────────────────────────────────────────────
log_n <- function(label, data) {
  cat(sprintf("  %-35s N = %d\n", label, nrow(data)))
}

# ── 1. LOAD DATA ─────────────────────────────────────────────
cat("=== LOAD DATA ===\n")
df_raw <- read_csv("data/raw/LFS_2018.csv", show_col_types = FALSE)
log_n("Raw", df_raw)

# ── 2. SAMPLE FILTERING ──────────────────────────────────────
cat("\n=== SAMPLE FILTERING ===\n")
df <- df_raw |>
  # Step 1: Working-age population (15–65)
  filter(C5 >= 15, C5 <= 65) |>
  (\(d) { log_n("After age filter (15–65)", d); d })() |>
  
  # Step 2: Currently employed (any of C21/C22/C23 == 1)
  filter(C21 == 1 | C22 == 1 | C23 == 1) |>
  (\(d) { log_n("After employment filter", d); d })() |>
  
  # Step 3: Wage workers only (C35 == 5)
  filter(C35 == 5) |>
  (\(d) { log_n("After wage-worker filter (C35=5)", d); d })() |>
  
  # Step 4: Manufacturing sector — VSIC Division C (10–33)
  mutate(vsic2 = C30C %/% 100) |>
  filter(vsic2 >= 10, vsic2 <= 33) |>
  (\(d) { log_n("After manufacturing filter (VSIC C)", d); d })() |>
  
  # Step 5: Post-secondary education (College or above, C17 >= 7)
  filter(C17 >= 7) |>
  (\(d) { log_n("After education filter (C17 >= 7)", d); d })() |>
  
  # Step 6: Positive monthly wage
  filter(C44 > 0, !is.na(C44)) |>
  (\(d) { log_n("After wage > 0 filter", d); d })() |>
  
  # Step 7: Valid field-of-study match indicator (C51 in {1, 2})
  filter(C51 %in% c(1, 2)) |>
  (\(d) { log_n("After C51 validity filter", d); d })()

# Cell-size check before proceeding
cat("\n--- Cell size: mismatch × female ---\n")
print(table(
  mismatch = ifelse(df$C51 == 2, 1, 0),
  female   = ifelse(df$C3  == 2, 1, 0)
))

# ── 3. VARIABLE CONSTRUCTION ─────────────────────────────────
cat("\n=== VARIABLE CONSTRUCTION ===\n")

df <- df |>
  mutate(
    # ── Dependent variable ───────────────────────────────────
    ln_wage      = log(C44 / 1000),       # log monthly wage (thousand VND)
    wage_monthly = C44,                   # raw wage — kept for robustness A3
    
    # ── Main independent variable ────────────────────────────
    mismatch     = as.integer(C51 == 2),  # 1 = field-of-study mismatch
    
    # ── Demographics ─────────────────────────────────────────
    female       = as.integer(C3 == 2),   # 1 = female
    marital      = as.integer(C9 == 2),   # 1 = currently married
    
    # ── Education → years of schooling ───────────────────────
    # C17: 7 = College (CĐ), 8 = University (ĐH), 9 = Postgraduate
    # FIX: dùng TRUE ~ thay .default (tương thích dplyr < 1.1.0)
    schooling = case_when(
      C17 == 7 ~ 14,
      C17 == 8 ~ 16,
      C17 == 9 ~ 18,
      TRUE     ~ NA_real_
    ),
    
    # ── Mincer experience ────────────────────────────────────
    exp  = C5 - schooling - 6,            # potential experience (years)
    exp2 = exp^2,
    
    # ── Geography & sector ───────────────────────────────────
    urban        = as.integer(TTNT == 1),
    industry_sub = vsic2,                 # VSIC 2-digit (10–33)
    edu_level    = C17,
    
    # FIX: dùng TRUE ~ thay .default (tương thích dplyr < 1.1.0)
    sector = case_when(
      C31 %in% 7:10 ~ "state",
      C31 == 11     ~ "fdi",
      TRUE          ~ "private"
    ),
    
    # ── Interaction terms ────────────────────────────────────
    mismatch_exp    = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female      = exp * female,
    triple          = mismatch * exp * female,  # 3-way interaction
    
    # ── Re-entrant proxy ─────────────────────────────────────
    # Definition: women ≥ 30 who have/had a family (C9 ∈ {2,3,4})
    # but have low actual experience (exp ≤ 10) relative to their age.
    # Proxy for workers who left the labour market for family reasons
    # and returned. Uses raw C9 (not the binary marital flag).
    # C9: 2 = married, 3 = widowed, 4 = divorced/separated
    re_entrant = as.integer(
      female == 1 &
        C5    >= 30 &
        C9    %in% c(2, 3, 4) &
        exp   <= 10
    ),
    
    # ── Labels (for plots) ───────────────────────────────────
    mismatch_label = factor(mismatch, levels = c(0, 1),
                            labels = c("Matched", "Mismatched")),
    gender_lab     = factor(female,   levels = c(0, 1),
                            labels = c("Male", "Female"))
  ) |>
  
  # Remove observations with negative potential experience
  filter(exp >= 0, !is.na(exp))

log_n("After removing exp < 0", df)

# ── 4. WINSORIZE ln_wage (1st – 99th percentile) ─────────────
q <- quantile(df$ln_wage, c(0.01, 0.99), na.rm = TRUE)
df <- df |>
  mutate(ln_wage = pmax(pmin(ln_wage, q[2]), q[1]))

cat(sprintf("  ln_wage winsorized at [%.3f, %.3f]\n", q[1], q[2]))

# ── 5. SELECT FINAL VARIABLES ─────────────────────────────────
df <- df |>
  select(
    # Identifiers (for merging / debugging)
    TINH, HOSO, STT,
    
    # Dependent variable
    ln_wage, wage_monthly,
    
    # Main independent variable & key controls
    mismatch, exp, exp2, female,
    
    # Interaction terms
    mismatch_exp, mismatch_female, exp_female, triple,
    
    # Control variables
    edu_level, marital, urban, sector, industry_sub,
    
    # Subsample flag
    re_entrant,
    
    # Raw source variables (robustness checks & verification)
    C5,        # age — used in re-entrant filter & summary stats
    C9,        # raw marital status — used in 05_robust.R
    C17,       # raw education code — robustness A1
    schooling, # years of schooling (14/16/18) — robustness A1
    C30C,      # VSIC 4-digit — used to verify industry_sub
    C31,       # establishment type — verify sector
    C35,       # employment status — verify wage-worker filter
    C44,       # raw monthly wage (thousand VND)
    C46,       # usual hours worked — robustness A4 (full-time filter)
    C51,       # raw mismatch indicator — verify mismatch
    vsic2,     # VSIC 2-digit (= industry_sub, kept for cross-check)
    
    # Survey weight (for weighted regressions if needed)
    Cal_weigh,
    
    # Labels & region
    mismatch_label, gender_lab, Region
  )

# ── 6. FINAL DIAGNOSTICS ─────────────────────────────────────
cat("\n=== FINAL SAMPLE DIAGNOSTICS ===\n")
cat(sprintf("  Final N (df_clean)    : %d\n", nrow(df)))
cat(sprintf("  Missing ln_wage       : %d\n", sum(is.na(df$ln_wage))))
cat(sprintf("  Missing mismatch      : %d\n", sum(is.na(df$mismatch))))
cat(sprintf("  Missing exp           : %d\n", sum(is.na(df$exp))))

cat("\nMismatch distribution:\n")
print(prop.table(table(df$mismatch)) |> round(3))

cat("\nGender distribution:\n")
print(prop.table(table(df$female)) |> round(3))

cat("\nSummary — ln_wage:\n");  print(summary(df$ln_wage))
cat("\nSummary — exp:\n");      print(summary(df$exp))

# ── 7. SUBSAMPLES ────────────────────────────────────────────
cat("\n=== SUBSAMPLES ===\n")

# Subsample 1: Male — baseline without career-gap confound
df_male <- df |> filter(female == 0)
log_n("Male subsample", df_male)

# Subsample 2: Re-entrants — women returning after a career break
df_reentrant <- df |> filter(re_entrant == 1)
log_n("Re-entrant subsample (exp <= 10)", df_reentrant)

# Fallback: relax experience threshold if N is too small
if (nrow(df_reentrant) < 100) {
  cat("  ⚠ Re-entrant N < 100 → relaxing threshold to exp ≤ 15\n")
  df_reentrant <- df |>
    filter(female == 1, C5 >= 30, C9 %in% c(2, 3, 4), exp <= 15)
  log_n("Re-entrant subsample (exp <= 15)", df_reentrant)
}

# Quick 3-group comparison
cat("\n--- 3-group comparison ---\n")
df |>
  mutate(group = case_when(
    re_entrant == 1 ~ "Re-entrant",
    female == 0     ~ "Male",
    TRUE            ~ "Female (other)"
  )) |>
  group_by(group) |>
  summarise(
    N            = n(),
    pct_mismatch = round(mean(mismatch) * 100, 1),
    mean_lnwage  = round(mean(ln_wage),  3),
    mean_exp     = round(mean(exp),      1),
    mean_age     = round(mean(C5),       1),
    .groups = "drop"
  ) |>
  print()

# ── 8. TRAIN / TEST SPLIT (70 / 30) ──────────────────────────
cat("\n=== TRAIN / TEST SPLIT ===\n")
set.seed(123)
train_idx <- sample(nrow(df), size = floor(0.7 * nrow(df)))
df_train  <- df[ train_idx, ]
df_test   <- df[-train_idx, ]
log_n("Train set (70%)", df_train)
log_n("Test  set (30%)", df_test)

# ── 9. EXPORT ─────────────────────────────────────────────────
cat("\n=== EXPORT ===\n")
saveRDS(df,           "data/processed/data_processed.rds")
saveRDS(df_male,      "data/processed/df_male.rds")
saveRDS(df_reentrant, "data/processed/df_reentrant.rds")
saveRDS(df_train,     "data/processed/df_train.rds")
saveRDS(df_test,      "data/processed/df_test.rds")

cat("  ✓ data_processed.rds   — full analytic sample\n")
cat("  ✓ df_male.rds          — subsample 1: Male\n")
cat("  ✓ df_reentrant.rds     — subsample 2: Re-entrants\n")
cat("  ✓ df_train.rds         — 70% training set\n")
cat("  ✓ df_test.rds          — 30% test set\n")
cat("\n✓ 01_clean.R complete — next: run 02_eda.R\n")
