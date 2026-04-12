# ============================================================
# 02_eda.R — Exploratory Data Analysis & Descriptive Stats
# Project: Horizontal Mismatch & Wage Penalty · LFS 2018
# Input:  data/processed/data_processed.rds
# Output: output/tables/ + output/figures/
# ============================================================

source("scripts/00_setup.R")
library(modelsummary)

df           <- readRDS("data/processed/data_processed.rds")
df_reentrant <- readRDS("data/processed/df_reentrant.rds")
df <- df |>
  mutate(wage_monthly = exp(ln_wage) * 1000)  # đơn vị: VND nghìn đồng
# ════════════════════════════════════════════════════════════
# BẢNG 1: Summary Statistics — Toàn mẫu / Nam / Nữ
# Người làm: Ánh
# ════════════════════════════════════════════════════════════
vars_select <- df |>
  select(
    wage_monthly,   
    mismatch, exp, female,
    edu_level, urban, marital, industry_sub
  )

# Toàn mẫu
datasummary_skim(
  vars_select,
  output = paste0(OUT_TAB, "Table1a_fullsample.html"),
  title  = "Bảng 1a: Thống kê mô tả — Toàn mẫu (N = 5,310)"
)

# Tách Nam / Nữ
datasummary_balance(
  ~ female,
  data = vars_select |>
    mutate(female = factor(female, labels = c("Nam", "Nữ"))),
  output = paste0(OUT_TAB, "Table1b_by_gender.html"),
  title  = "Bảng 1b: Thống kê mô tả — Nam vs Nữ"
)

cat("✓ Bảng 1 xong\n")

# ════════════════════════════════════════════════════════════
# BẢNG 2: Wage theo mismatch × female + T-test
# Người làm: Linh
# ════════════════════════════════════════════════════════════
wage_group <- df |>
  group_by(mismatch, female) |>
  summarise(
    N            = n(),
    pct_of_total = round(n() / nrow(df) * 100, 1),
    mean_wage    = round(mean(wage_monthly), 0),   # ← đổi tên và đơn vị
    sd_wage      = round(sd(wage_monthly),   0),
    .groups      = "drop"
  ) |>
  mutate(
    mismatch = ifelse(mismatch == 1, "Mismatch", "Matched"),
    female   = ifelse(female   == 1, "Nữ",       "Nam")
  )

cat("\n--- Bảng 2: Wage theo mismatch × female ---\n")
print(wage_group)

# T-test: mismatch vs matched trong từng nhóm giới
t_all    <- t.test(ln_wage ~ mismatch, data = df)
t_male   <- t.test(ln_wage ~ mismatch, data = df |> filter(female == 0))
t_female <- t.test(ln_wage ~ mismatch, data = df |> filter(female == 1))

cat("\nT-test wage gap (matched vs mismatch):\n")
cat("  Toàn mẫu — p-value:", round(t_all$p.value,    4), "\n")
cat("  Nam      — p-value:", round(t_male$p.value,   4), "\n")
cat("  Nữ       — p-value:", round(t_female$p.value, 4), "\n")

write.csv(wage_group, paste0(OUT_TAB, "Table2_wage_by_group.csv"),
          row.names = FALSE)
cat("✓ Bảng 2 xong\n")

# ════════════════════════════════════════════════════════════
# BẢNG 3: Wage gap theo nhóm kinh nghiệm
# → Dẫn trực tiếp tới β₅ trong model
# Người làm: Linh
# ════════════════════════════════════════════════════════════
wage_gap_exp <- df |>
  mutate(exp_group = case_when(
    exp <  5  ~ "0–5 năm",
    exp < 10  ~ "5–10 năm",
    exp < 20  ~ "10–20 năm",
    TRUE      ~ "20+ năm"
  ),
  exp_group = factor(exp_group,
                     levels = c("0–5 năm", "5–10 năm", "10–20 năm", "20+ năm"))
  ) |>
  group_by(exp_group, mismatch) |>
  summarise(
    mean_wage = mean(wage_monthly, na.rm = TRUE),  # ← đổi
    n         = n(),
    .groups   = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from   = mismatch,
    values_from  = c(mean_wage, n),
    names_prefix = "m"
  ) |>
  rename(
    wage_matched  = mean_wagem0,
    wage_mismatch = mean_wagem1,
    n_matched     = nm0,
    n_mismatch    = nm1
  ) |>
  mutate(
    wage_gap  = round(wage_matched - wage_mismatch, 0),  # VND nghìn đồng
    gap_pct   = round(wage_gap / wage_matched * 100, 1),
    wage_matched  = round(wage_matched,  0),
    wage_mismatch = round(wage_mismatch, 0)
  )

cat("\n--- Bảng 3: Wage gap theo nhóm kinh nghiệm ---\n")
print(wage_gap_exp)
# Nếu gap thu hẹp theo exp → ủng hộ "remedy"
# Nếu gap mở rộng theo exp → ủng hộ "widen"

write.csv(wage_gap_exp, paste0(OUT_TAB, "Table3_wage_gap_exp.csv"),
          row.names = FALSE)
cat("✓ Bảng 3 xong\n")

# ════════════════════════════════════════════════════════════
# BẢNG 4: So sánh 3 nhóm — Male / Re-entrant / Female other
# → Justify subsample re-entrant
# Người làm: Ánh
# ════════════════════════════════════════════════════════════
group_compare <- df |>
  mutate(group = case_when(
    re_entrant == 1 ~ "Re-entrant",
    female == 0     ~ "Male",
    TRUE            ~ "Female (other)"
  )) |>
  group_by(group) |>
  summarise(
    N             = n(),
    pct_mismatch  = round(mean(mismatch) * 100, 1),
    mean_wage     = round(mean(wage_monthly), 0),  # ← đổi
    mean_exp      = round(mean(exp), 1),
    mean_age      = round(mean(C5), 1),
    pct_married   = round(mean(C9 == 2) * 100, 1),
    .groups       = "drop"
  )

cat("\n--- Bảng 4: So sánh 3 nhóm ---\n")
print(group_compare)

write.csv(group_compare, paste0(OUT_TAB, "Table4_group_compare.csv"),
          row.names = FALSE)
cat("✓ Bảng 4 xong\n")

# ════════════════════════════════════════════════════════════
# FIGURE 1: Density plot ln_wage — matched vs mismatch
# Người làm: Khánh
# ════════════════════════════════════════════════════════════
fig1 <- ggplot(
  df |> mutate(
    status = ifelse(mismatch == 1, "Mismatch", "Matched"),
    gender = ifelse(female   == 1, "Nữ",       "Nam")
  ),
  aes(x = ln_wage, fill = status, color = status)
) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  facet_wrap(~ gender) +
  scale_fill_manual(values  = c("Matched" = "#1a4a2e", "Mismatch" = "#b87a10")) +
  scale_color_manual(values = c("Matched" = "#1a4a2e", "Mismatch" = "#b87a10")) +
  labs(
    title = "Hình 1: Phân phối ln(wage) theo mismatch và giới tính",
    x     = "ln(Tiền lương tháng / 1,000 VND)",
    y     = "Mật độ",
    fill  = NULL, color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(paste0(OUT_FIG, "Fig1_wage_dist.png"),
       fig1, width = 8, height = 5, dpi = 300)
cat("✓ Figure 1 xong\n")

# ════════════════════════════════════════════════════════════
# FIGURE 2: Wage-Experience Profile — 4 đường
# Người làm: Ánh
# ════════════════════════════════════════════════════════════
fig2 <- df |>
  mutate(group = case_when(
    mismatch == 0 & female == 0 ~ "Matched — Nam",
    mismatch == 1 & female == 0 ~ "Mismatch — Nam",
    mismatch == 0 & female == 1 ~ "Matched — Nữ",
    mismatch == 1 & female == 1 ~ "Mismatch — Nữ"
  )) |>
  ggplot(aes(x = exp, y = ln_wage, color = group, linetype = group)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.12, linewidth = 1) +
  scale_color_manual(values = c(
    "Matched — Nam"  = "#1a4a2e",
    "Mismatch — Nam" = "#b87a10",
    "Matched — Nữ"   = "#2d7a4f",
    "Mismatch — Nữ"  = "#a02020"
  )) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(
    title   = "Hình 2: Wage-Experience Profile theo mismatch và giới tính",
    x       = "Kinh nghiệm (năm)",
    y       = "ln(Tiền lương tháng)",
    color   = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(paste0(OUT_FIG, "Fig2_wage_exp_profile.png"),
       fig2, width = 9, height = 5, dpi = 300)
cat("✓ Figure 2 xong\n")

# ════════════════════════════════════════════════════════════
# FIGURE 3: Wage gap (matched − mismatch) theo exp
# → Hình trực tiếp trả lời "remedy hay widen?"
# Người làm: Linh
# ════════════════════════════════════════════════════════════
# ── annotation data ──────────────────────────────────────────
ann <- tibble(
  exp_bin = c(16, 28),
  gap     = c(0.08, 0.55),
  gender  = c("Nam", "Nam"),
  label   = c("Gap thu hẹp\n(kinh nghiệm bù đắp)", "Penalty\nwiden")
)

# ── plot ─────────────────────────────────────────────────────
fig3 <- ggplot(df_gap, aes(x = exp_bin, y = gap,
                           color = gender, fill = gender)) +
  
  # vùng penalty / remedy
  annotate("rect", xmin = 22, xmax = 35,
           ymin = -Inf, ymax = Inf,
           fill = "#E24B4A", alpha = 0.05) +
  
  # đường zero
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray60", linewidth = 0.6) +
  
  # CI band mỏng hơn, alpha thấp hơn
  geom_smooth(method = "loess", se = TRUE,
              alpha = 0.08, linewidth = 1.2, span = 0.6) +
  
  # điểm nhỏ lại, không outline
  geom_point(size = 1.2, alpha = 0.55, shape = 16) +
  
  # annotation text
  geom_text(data = ann, aes(label = label),
            size = 3, lineheight = 0.9,
            color = "gray30", fontface = "italic",
            inherit.aes = FALSE) +
  
  # tách panel theo giới tính
  facet_wrap(~ gender, ncol = 2) +
  
  scale_color_manual(values = c("Nam" = "#185FA5", "Nữ" = "#993556")) +
  scale_fill_manual(values  = c("Nam" = "#185FA5", "Nữ" = "#993556")) +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  scale_y_continuous(breaks = seq(-0.2, 0.8, 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  
  labs(
    title    = "Wage gap (matched − mismatch) theo kinh nghiệm",
    subtitle = "Gap > 0: matched có lương cao hơn  |  Vùng đỏ: penalty widen sau 22 năm",
    x        = "Kinh nghiệm (năm)",
    y        = "Wage gap: ln(wage_matched) − ln(wage_mismatch)",
    caption  = "Đường cong: LOESS (span = 0.6) với 95% CI"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "none",           # facet đã nói rõ giới tính rồi
    strip.text        = element_text(size = 12, face = "bold"),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(linewidth = 0.3, color = "gray90"),
    plot.title        = element_text(size = 14, face = "bold"),
    plot.subtitle     = element_text(size = 10, color = "gray40"),
    plot.caption      = element_text(size = 9,  color = "gray50"),
    axis.title        = element_text(size = 10, color = "gray40"),
    plot.title.position = "plot"
  )

ggsave(paste0(OUT_FIG, "Fig3_wage_gap_exp.png"),
       fig3, width = 10, height = 5, dpi = 300)
cat("✓ Figure 3 xong\n")

cat("\n✓ 02_eda.R hoàn thành!\n")
cat("Output tables : ", OUT_TAB, "\n")
cat("Output figures: ", OUT_FIG, "\n")
cat("\nTiếp theo chạy 03_models.R\n")