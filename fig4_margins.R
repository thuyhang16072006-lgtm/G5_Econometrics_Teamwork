library(marginaleffects)
library(ggplot2)
library(dplyr)

df_train <- readRDS("data/processed/df_train.rds")

df_train <- df_train %>%
  mutate(
    mismatch_exp = mismatch * exp,
    mismatch_female = mismatch * female,
    exp_female = exp * female,
    triple = mismatch * female * exp
  )

f_full <- ln_wage ~ mismatch + exp + exp2 + female + 
  mismatch_exp + mismatch_female + exp_female + triple + 
  edu_level + urban + marital + 
  factor(sector) + factor(industry_sub)

m3 <- lm(f_full, data = df_train)

me_data <- avg_slopes(
  m3,
  variables = "mismatch",
  by = c("exp", "female"),
  newdata = datagrid(
    exp = c(0, 5, 10, 15, 20, 25),
    female = c(0, 1)
  )
)

fig4 <- ggplot(me_data, aes(x = exp, y = estimate, color = factor(female), fill = factor(female))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("0" = "#1b9e77", "1" = "#d95f02"), labels = c("Male", "Female")) +
  scale_fill_manual(values = c("0" = "#1b9e77", "1" = "#d95f02"), labels = c("Male", "Female")) +
  labs(
    title = "Figure 4: Wage Penalty of Mismatch over Experience",
    x = "Years of Experience",
    y = "Marginal Effect (Log Points)",
    color = "Gender",
    fill = "Gender"
  ) +
  theme_minimal()

ggsave("output/fig4_margins.png", plot = fig4, width = 8, height = 6)
saveRDS(m3, "models/m3_object.rds")