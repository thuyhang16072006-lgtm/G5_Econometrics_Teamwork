# ============================================================
# 01_clean.R — Sample filtering & variable construction
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# ============================================================

library(tidyverse)
library(haven)

# ── 1. LOAD DATA ─────────────────────────────────────────────
df_raw <- read_csv("data/raw/LFS_2018.csv")

cat("Raw N:", nrow(df_raw), "\n")  # 823,899

# ── 2. SAMPLE FILTERING ──────────────────────────────────────
# Chạy từng bước, in N để theo dõi

df <- df_raw

# Bước 1: Tuổi lao động 15–65
df <- df |> filter(C5 >= 15, C5 <= 65)
cat("Sau filter tuổi:", nrow(df), "\n")

# Bước 2: Đang có việc làm (ít nhất 1 trong 3 câu = 1)
df <- df |> filter(C21 == 1 | C22 == 1 | C23 == 1)
cat("Sau filter có việc:", nrow(df), "\n")

# Bước 3: Làm công ăn lương
df <- df |> filter(C35 == 5)
cat("Sau filter làm công:", nrow(df), "\n")

# Bước 4: Ngành chế biến chế tạo VSIC C (1000–3399)
# C30C là VSIC 4 chữ số → lấy 2 chữ số đầu = C30C %/% 100
df <- df |>
  mutate(vsic2 = C30C %/% 100) |>
  filter(vsic2 >= 10, vsic2 <= 33)
cat("Sau filter ngành CB-CT:", nrow(df), "\n")

# Bước 5: Trình độ Cao đẳng trở lên
df <- df |> filter(C17 >= 7)
cat("Sau filter trình độ:", nrow(df), "\n")

# Bước 6: Thu nhập > 0
df <- df |> filter(C44 > 0, !is.na(C44))
cat("Sau filter thu nhập:", nrow(df), "\n")

# Bước 7: Vùng Đồng bằng Sông Hồng
#df <- df |> filter(Region == 1)
#cat("Sau filter vùng:", nrow(df), "\n")

# Bước 8: C51 hợp lệ (chỉ giữ 1 và 2, loại 0/3/4)
df <- df |> filter(C51 %in% c(1, 2))
cat("Sau filter C51:", nrow(df), "\n")

# ── KIỂM TRA CELL SIZE NGAY ──────────────────────────────────
cat("\n--- Cell size check ---\n")
cat("mismatch × female:\n")
print(table(
  mismatch = ifelse(df$C51 == 2, 1, 0),
  female   = ifelse(df$C3  == 2, 1, 0)
))
# Nếu ô mismatch=1, female=1 < 200 → báo A để điều chỉnh scope

# ── 3. VARIABLE CONSTRUCTION ─────────────────────────────────
df <- df |>
  mutate(
    # Biến phụ thuộc
    ln_wage = log(C44 / 1000),
    
    # Biến mismatch
    mismatch = ifelse(C51 == 2, 1, 0),
    
    # Biến giới tính
    female = ifelse(C3 == 2, 1, 0),
    
    # Số năm học proxy theo C17
    schooling = case_when(
      C17 == 7 ~ 14,   # Cao đẳng
      C17 == 8 ~ 16,   # Đại học
      C17 == 9 ~ 18,   # Trên đại học
      TRUE     ~ NA_real_
    ),
    
    # Kinh nghiệm Mincer
    exp  = C5 - schooling - 6,
    exp2 = exp^2,
    
    # Các biến kiểm soát
    marital      = ifelse(C9 == 2, 1, 0),
    urban        = ifelse(TTNT == 1, 1, 0),
    edu_level    = C17,
    industry_sub = vsic2,   # VSIC 2 chữ số (đã tạo ở bước filter)
    
    sector = case_when(
      C31 %in% 7:10 ~ "state",
      C31 == 11     ~ "fdi",
      TRUE          ~ "private"
    ),
    
    # Interaction terms
    mismatch_exp    = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female      = exp * female,
    triple          = mismatch * exp * female
  ) |>
  
  # Loại exp âm (mâu thuẫn logic)
  filter(exp >= 0, !is.na(exp))

cat("\nSau tạo biến, loại exp < 0:", nrow(df), "\n")

# ── 4. WINSORIZE ln_wage ──────────────────────────────────────
q <- quantile(df$ln_wage, c(0.01, 0.99), na.rm = TRUE)
df <- df |>
  mutate(ln_wage = pmax(pmin(ln_wage, q[2]), q[1]))

# ── 5. KIỂM TRA CUỐI ─────────────────────────────────────────
cat("\n--- Final sample summary ---\n")
cat("N cuối:", nrow(df), "\n")
cat("Missing ln_wage:", sum(is.na(df$ln_wage)), "\n")
cat("Missing mismatch:", sum(is.na(df$mismatch)), "\n")
cat("Missing exp:", sum(is.na(df$exp)), "\n")

cat("\nPhân phối mismatch:\n")
print(prop.table(table(df$mismatch)))

cat("\nPhân phối female:\n")
print(prop.table(table(df$female)))

cat("\nSummary ln_wage:\n")
print(summary(df$ln_wage))

# ── 6. EXPORT ────────────────────────────────────────────────
saveRDS(df, "data/processed/data_processed.rds")
cat("\nXong! Đã export data_processed.rds\n")
# _______________________________________________________________
# 1. Biến re_entrant 
df <- df |>
  mutate(re_entrant = as.integer(
    female == 1 & C5 >= 30 &
      marital %in% c(1) &   # đã/đang có gia đình
      exp <= 10
  ))
cat("Re-entrant N:", sum(df$re_entrant), "\n")

# 2. Subsamples theo giới
df_male   <- filter(df, female == 0)
df_female <- filter(df, female == 1)

# 3. Train/Test split 
set.seed(123)
train_idx <- sample(nrow(df), size = floor(0.7 * nrow(df)))
df_train  <- df[ train_idx, ]
df_test   <- df[-train_idx, ]

# 4. Export 
saveRDS(df_male,   "data/processed/df_male.rds")
saveRDS(df_female, "data/processed/df_female.rds")
saveRDS(df_train,  "data/processed/df_train.rds")
saveRDS(df_test,   "data/processed/df_test.rds")
cat("Đã export đủ 5 files processed!\n")