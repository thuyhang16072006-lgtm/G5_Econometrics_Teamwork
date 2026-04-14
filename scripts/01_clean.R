# ============================================================
# 01_clean.R — Sample filtering & variable construction
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

library(tidyverse)
library(haven)

# ── 1. LOAD DATA ─────────────────────────────────────────────
df_raw <- read_csv("data/raw/LFS_2018.csv")
cat("Raw N:", nrow(df_raw), "\n")

# ── 2. SAMPLE FILTERING ──────────────────────────────────────
df <- df_raw

df <- df |> filter(C5 >= 15, C5 <= 65)
cat("Sau filter tuổi:", nrow(df), "\n")

df <- df |> filter(C21 == 1 | C22 == 1 | C23 == 1)
cat("Sau filter có việc:", nrow(df), "\n")

df <- df |> filter(C35 == 5)
cat("Sau filter làm công:", nrow(df), "\n")

df <- df |>
  mutate(vsic2 = C30C %/% 100) |>
  filter(vsic2 >= 10, vsic2 <= 33)
cat("Sau filter ngành CB-CT:", nrow(df), "\n")

df <- df |> filter(C17 >= 7)
cat("Sau filter trình độ:", nrow(df), "\n")

df <- df |> filter(C44 > 0, !is.na(C44))
cat("Sau filter thu nhập:", nrow(df), "\n")

df <- df |> filter(C51 %in% c(1, 2))
cat("Sau filter C51:", nrow(df), "\n")

# ── 3. VARIABLE CONSTRUCTION ─────────────────────────────────
df <- df |>
  mutate(
    ln_wage      = log(C44 / 1000),
    mismatch     = ifelse(C51 == 2, 1, 0),
    female       = ifelse(C3 == 2, 1, 0),
    
    schooling = case_when(
      C17 == 7 ~ 14,
      C17 == 8 ~ 16,
      C17 == 9 ~ 18,
      TRUE     ~ NA_real_
    ),
    
    exp          = C5 - schooling - 6,
    exp2         = exp^2,
    
    marital      = ifelse(C9 == 2, 1, 0),
    urban        = ifelse(TTNT == 1, 1, 0),
    edu_level    = C17,
    industry_sub = vsic2,
    
    sector = case_when(
      C31 %in% 7:10 ~ "state",
      C31 == 11     ~ "fdi",
      TRUE          ~ "private"
    ),
    
    mismatch_exp    = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female      = exp * female,
    triple          = mismatch * exp * female,
    
    re_entrant = as.integer(
      female == 1 &
        C5 >= 30 &
        C9 %in% c(2, 3, 4) &
        exp <= 10
    )
  ) |>
  filter(exp >= 0, !is.na(exp))

# ── 4. WINSORIZE ln_wage ──────────────────────────────────────
q  <- quantile(df$ln_wage, c(0.01, 0.99), na.rm = TRUE)
df <- df |>
  mutate(ln_wage = pmax(pmin(ln_wage, q[2]), q[1]))

# ── 5. SUBSAMPLES ─────────────────────────────────────────────
df_male      <- df |> filter(female == 0)
df_reentrant <- df |> filter(re_entrant == 1)

# ── 6. TRAIN/TEST SPLIT ───────────────────────────────────────
set.seed(123)
train_idx <- sample(nrow(df), size = floor(0.7 * nrow(df)))
df_train  <- df[ train_idx, ]
df_test   <- df[-train_idx, ]

# ── 7. EXPORT ─────────────────────────────────────────────────
saveRDS(df,           "data/processed/data_processed.rds")
saveRDS(df_male,      "data/processed/df_male.rds")
saveRDS(df_reentrant, "data/processed/df_reentrant.rds")
saveRDS(df_train,     "data/processed/df_train.rds")
saveRDS(df_test,      "data/processed/df_test.rds")

cat("\n✓ 01_clean.R hoàn thành!\n")
cat("  N toàn mẫu  :", nrow(df), "\n")
cat("  N male      :", nrow(df_male), "\n")
cat("  N re-entrant:", nrow(df_reentrant), "\n")
cat("  N train     :", nrow(df_train), "\n")
cat("  N test      :", nrow(df_test), "\n")
cat("\nTiếp theo chạy 02_eda.R\n")