# ============================================================
# 04_margins.R — Marginal Effects · M3 Full Model
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Mục đích:
#   - Tính marginal effect của mismatch tại các mức exp khác nhau
#   - Tính marginal effect của mismatch theo female
#   - Xuất bảng số + plot ggplot2
# Requires: data/processed/m3.rds, df_train.rds
# ============================================================

source("scripts/00_setup.R")
library(ggplot2)
library(dplyr)
library(sandwich)
library(lmtest)

# ── 1. LOAD ──────────────────────────────────────────────────
m3 <- readRDS("data/processed/m3.rds")        
df_train <- readRDS("data/processed/df_train.rds")

cat("====================================\n")
cat("   MARGINAL EFFECTS — M3 FULL       \n")
cat("====================================\n")

# ── 2. ROBUST VCOV ───────────────────────────────────────────
vcov_hc1 <- vcovHC(m3, type = "HC1")

# ── 3. LẤY HỆ SỐ CẦN THIẾT ──────────────────────────────────
# ln_wage = β₀ + β₁·mismatch + β₂·exp + β₃·exp²
#         + β₄·female + β₅·(mismatch×exp)
#         + β₆·(mismatch×female) + β₇·(exp×female)
#         + β₈·(mismatch×exp×female) + controls
#
# ∂ln_wage/∂mismatch = β₁ + β₅·exp + β₆·female + β₈·exp·female
# → Đây là marginal effect của mismatch

b  <- coef(m3)
b1 <- b["mismatch"]
b5 <- b["mismatch_exp"]
b6 <- b["mismatch_female"]
b8 <- b["triple"]

cat("\nHệ số liên quan đến mismatch:\n")
cat(sprintf("  β₁ (mismatch)            : %8.5f\n", b1))
cat(sprintf("  β₅ (mismatch × exp)      : %8.5f\n", b5))
cat(sprintf("  β₆ (mismatch × female)   : %8.5f\n", b6))
cat(sprintf("  β₈ (mismatch×exp×female) : %8.5f\n", b8))

# ════════════════════════════════════════════════════════════
# A. MARGINAL EFFECT CỦA MISMATCH THEO EXP
#    Tính riêng cho Nam (female=0) và Nữ (female=1)
#    ME = β₁ + β₅·exp + β₆·female + β₈·exp·female
# ════════════════════════════════════════════════════════════
cat("\n\n--- A. Marginal Effect của Mismatch theo Kinh nghiệm ---\n")

exp_seq <- seq(0, 30, by = 1)   # 0–30 năm kinh nghiệm

me_table <- bind_rows(
  # Nam (female = 0)
  data.frame(
    exp    = exp_seq,
    female = 0,
    group  = "Nam",
    ME     = b1 + b5 * exp_seq + b6 * 0 + b8 * exp_seq * 0
  ),
  # Nữ (female = 1)
  data.frame(
    exp    = exp_seq,
    female = 1,
    group  = "Nữ",
    ME     = b1 + b5 * exp_seq + b6 * 1 + b8 * exp_seq * 1
  )
) |>
  mutate(
    # Chuyển sang % (semi-elasticity: base::exp(ME)-1)
    ME_pct = (base::exp(ME) - 1) * 100
  )

# ── In bảng tóm tắt tại các mốc exp quan trọng ───────────────
key_exp <- c(0, 5, 10, 15, 20, 25, 30)
cat(sprintf("\n%-5s | %-12s | %-12s | %-12s | %-12s\n",
            "Exp", "ME Nam (β)", "ME Nam (%)", "ME Nữ (β)", "ME Nữ (%)"))
cat(strrep("-", 60), "\n")

for (e in key_exp) {
  me_m <- me_table |> filter(exp == e, group == "Nam")
  me_f <- me_table |> filter(exp == e, group == "Nữ")
  cat(sprintf("%-5d | %+12.5f | %+11.1f%% | %+12.5f | %+11.1f%%\n",
              e,
              me_m$ME, me_m$ME_pct,
              me_f$ME, me_f$ME_pct))
}

# ── SE của ME theo exp dùng Delta Method ─────────────────────
# Var(ME) tại female=f, exp=e:
#   Var = Var(β₁) + e²·Var(β₅) + f²·Var(β₆) + (e·f)²·Var(β₈)
#       + 2e·Cov(β₁,β₅) + 2f·Cov(β₁,β₆) + 2ef·Cov(β₁,β₈)
#       + 2ef²·Cov(β₅,β₆) + 2e²f·Cov(β₅,β₈) + 2ef·Cov(β₆,β₈)

compute_me_se <- function(exp_val, female_val, vcov_mat) {
  # Gradient vector: dME/dβ = [1, 0, 0, 0, exp, female, 0, exp*female, ...]
  # Chỉ cần 4 hệ số: mismatch, mismatch_exp, mismatch_female, triple
  idx <- c("mismatch", "mismatch_exp", "mismatch_female", "triple")
  V   <- vcov_mat[idx, idx]
  g   <- c(1, exp_val, female_val, exp_val * female_val)
  se  <- sqrt(as.numeric(t(g) %*% V %*% g))
  return(se)
}

me_table <- me_table |>
  rowwise() |>
  mutate(
    SE   = compute_me_se(exp, female, vcov_hc1),
    CI_lo = ME - 1.96 * SE,
    CI_hi = ME + 1.96 * SE,
    CI_lo_pct = (base::exp(CI_lo) - 1) * 100,
    CI_hi_pct = (base::exp(CI_hi) - 1) * 100
  ) |>
  ungroup()

# ════════════════════════════════════════════════════════════
# B. MARGINAL EFFECT TẠI GIÁ TRỊ TRUNG BÌNH (AME)
#    Average Marginal Effect: trung bình ME trên toàn mẫu
# ════════════════════════════════════════════════════════════
cat("\n\n--- B. Average Marginal Effect (AME) ---\n")

ame_df <- df_train |>
  mutate(
    ME_i = b1 + b5 * exp + b6 * female + b8 * exp * female
  )

ame_male   <- mean(ame_df$ME_i[ame_df$female == 0])
ame_female <- mean(ame_df$ME_i[ame_df$female == 1])
ame_all    <- mean(ame_df$ME_i)

cat(sprintf("  AME (toàn mẫu) : %+.5f → %+.1f%%\n",
            ame_all, (base::exp(ame_all) - 1) * 100))
cat(sprintf("  AME (Nam)      : %+.5f → %+.1f%%\n",
            ame_male, (base::exp(ame_male) - 1) * 100))
cat(sprintf("  AME (Nữ)       : %+.5f → %+.1f%%\n",
            ame_female, (base::exp(ame_female) - 1) * 100))

# MEM — Marginal Effect at the Mean
exp_mean_m <- mean(df_train$exp[df_train$female == 0])
exp_mean_f <- mean(df_train$exp[df_train$female == 1])

mem_male   <- b1 + b5 * exp_mean_m + b6 * 0 + b8 * exp_mean_m * 0
mem_female <- b1 + b5 * exp_mean_f + b6 * 1 + b8 * exp_mean_f * 1

cat(sprintf("\n  MEM (Nam, exp̄=%.1f yr)  : %+.5f → %+.1f%%\n",
            exp_mean_m, mem_male, (base::exp(mem_male) - 1) * 100))
cat(sprintf("  MEM (Nữ, exp̄=%.1f yr)   : %+.5f → %+.1f%%\n",
            exp_mean_f, mem_female, (base::exp(mem_female) - 1) * 100))

# ════════════════════════════════════════════════════════════
# C. XUẤT BẢNG SỐ TỔNG HỢP (CSV)
# ════════════════════════════════════════════════════════════
me_export <- me_table |>
  select(exp, group, ME, SE, CI_lo, CI_hi, ME_pct, CI_lo_pct, CI_hi_pct) |>
  mutate(across(where(is.numeric), \(x) round(x, 5)))

write.csv(me_export,
          paste0(OUT_TAB, "Table4_marginal_effects.csv"),
          row.names = FALSE)
cat("\n✓ Table4_marginal_effects.csv xuất xong\n")

# ════════════════════════════════════════════════════════════
# D. PLOT 1 — ME của Mismatch theo Exp, phân theo Giới tính
# ════════════════════════════════════════════════════════════
cat("\n--- Vẽ Plot 1: ME × Exp (Nam vs Nữ) ---\n")

# Exp range thực tế trong data (tránh extrapolation)
exp_q <- quantile(df_train$exp, c(0.05, 0.95))
cat(sprintf("  Exp range (5%%–95%%): %.0f – %.0f năm\n", exp_q[1], exp_q[2]))

p1 <- ggplot(me_table, aes(x = exp, y = ME_pct,
                           color = group, fill = group)) +
  geom_ribbon(aes(ymin = CI_lo_pct, ymax = CI_hi_pct),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  # Đánh dấu vùng exp thực tế
  annotate("rect",
           xmin = exp_q[1], xmax = exp_q[2],
           ymin = -Inf, ymax = Inf,
           alpha = 0.05, fill = "steelblue") +
  # AME reference lines
  geom_point(data = data.frame(
    exp    = c(exp_mean_m, exp_mean_f),
    ME_pct = c((base::exp(mem_male) - 1) * 100,
               (base::exp(mem_female) - 1) * 100),
    group  = c("Nam", "Nữ")
  ), shape = 21, size = 3, stroke = 1.2, fill = "white") +
  scale_color_manual(values = c("Nam" = "#2166ac", "Nữ" = "#d6604d"),
                     name = "Nhóm") +
  scale_fill_manual(values  = c("Nam" = "#2166ac", "Nữ" = "#d6604d"),
                    name = "Nhóm") +
  scale_x_continuous(breaks = seq(0, 30, 5),
                     limits = c(0, 30)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Marginal Effect của Horizontal Mismatch lên Tiền lương",
    subtitle = "Theo số năm kinh nghiệm · Phân theo giới tính · M3 Full (Robust SE HC1)",
    x        = "Kinh nghiệm (năm)",
    y        = "Wage penalty / premium (%)",
    caption  = paste0(
      "Vùng bóng: 95% CI (Delta method). ",
      "Điểm tròn: Marginal Effect at the Mean (MEM). ",
      "Vùng xanh nhạt: khoảng exp thực tế (5%–95%).\n",
      "Đường đứt: ME = 0 (không có hiệu ứng)."
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8,
                                 hjust = 0),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(paste0(OUT_FIG, "Fig2_marginal_effect_exp.png"),
       p1, width = 8, height = 5, dpi = 300)
cat("  ✓ Fig2_marginal_effect_exp.png xuất xong\n")

# ════════════════════════════════════════════════════════════
# E. PLOT 2 — Wage Penalty tại các mốc exp cụ thể (Bar chart)
# ════════════════════════════════════════════════════════════
cat("\n--- Vẽ Plot 2: Wage Penalty tại mốc exp cụ thể ---\n")

bar_data <- me_table |>
  filter(exp %in% c(0, 5, 10, 15, 20)) |>
  mutate(
    exp_label = paste0(exp, " năm"),
    exp_label = factor(exp_label,
                       levels = paste0(c(0, 5, 10, 15, 20), " năm"))
  )

p2 <- ggplot(bar_data,
             aes(x = exp_label, y = ME_pct,
                 fill = group, color = group)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.6, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = CI_lo_pct, ymax = CI_hi_pct),
    position = position_dodge(width = 0.7),
    width = 0.25, linewidth = 0.7
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  geom_text(
    aes(label = sprintf("%+.1f%%", ME_pct),
        vjust = ifelse(ME_pct >= 0, -0.5, 1.3)),
    position = position_dodge(width = 0.7),
    size = 3.2, fontface = "bold"
  ) +
  scale_fill_manual(values  = c("Nam" = "#2166ac", "Nữ" = "#d6604d"),
                    name = "Nhóm") +
  scale_color_manual(values = c("Nam" = "#2166ac", "Nữ" = "#d6604d"),
                     name = "Nhóm") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title    = "Wage Penalty của Horizontal Mismatch tại các Mốc Kinh nghiệm",
    subtitle = "M3 Full Model · Robust SE (HC1) · 95% CI",
    x        = "Kinh nghiệm",
    y        = "Wage penalty (%)",
    caption  = "Error bars: 95% CI (Delta method). Âm = penalty, Dương = premium."
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8,
                                    hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave(paste0(OUT_FIG, "Fig3_wage_penalty_bar.png"),
       p2, width = 8, height = 5, dpi = 300)
cat("  ✓ Fig3_wage_penalty_bar.png xuất xong\n")

# ════════════════════════════════════════════════════════════
# F. IN TÓM TẮT CUỐI
# ════════════════════════════════════════════════════════════
cat("\n====================================\n")
cat("   TÓM TẮT MARGINAL EFFECTS — M3    \n")
cat("====================================\n")

cat(sprintf("\nAME toàn mẫu  : %+.1f%% (mismatch → lương thấp hơn)\n",
            (base::exp(ame_all) - 1) * 100))
cat(sprintf("AME Nam        : %+.1f%%\n", (base::exp(ame_male) - 1) * 100))
cat(sprintf("AME Nữ         : %+.1f%%\n", (base::exp(ame_female) - 1) * 100))

cat(sprintf("\nMEM Nam (exp̄=%.1f): %+.1f%%\n",
            exp_mean_m, (base::exp(mem_male) - 1) * 100))
cat(sprintf("MEM Nữ  (exp̄=%.1f): %+.1f%%\n",
            exp_mean_f, (base::exp(mem_female) - 1) * 100))

# Kiểm tra xem penalty có giảm theo exp không (remedy?)
me_exp0_m  <- b1
me_exp10_m <- b1 + b5 * 10
me_exp0_f  <- b1 + b6
me_exp10_f <- b1 + b5 * 10 + b6 + b8 * 10

cat("\nKiểm tra REMEDY (penalty giảm khi exp tăng?):\n")
cat(sprintf("  Nam  exp=0  → exp=10 : %+.1f%% → %+.1f%% [%s]\n",
            (base::exp(me_exp0_m) - 1) * 100,
            (base::exp(me_exp10_m) - 1) * 100,
            ifelse(me_exp10_m > me_exp0_m, "REMEDY ↑", "WIDEN ↓")))
cat(sprintf("  Nữ   exp=0  → exp=10 : %+.1f%% → %+.1f%% [%s]\n",
            (base::exp(me_exp0_f) - 1) * 100,
            (base::exp(me_exp10_f) - 1) * 100,
            ifelse(me_exp10_f > me_exp0_f, "REMEDY ↑", "WIDEN ↓")))

cat("\n✓ 04_margins.R hoàn thành!\n")
cat("  Output:\n")
cat("    - Table4_marginal_effects.csv\n")
cat("    - Fig2_marginal_effect_exp.png\n")
cat("    - Fig3_wage_penalty_bar.png\n")
cat("  Tiếp theo chạy 05_robust.R\n")
