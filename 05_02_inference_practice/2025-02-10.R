library("tidyverse")

df_ggl <-
  read_csv("http://hdl.handle.net/10079/d3669799-4537-411e-b175-d9e837324c35") |>
  mutate(y = if_else(voted == "Yes", 1, 0))

# Calculate difference of means by hand
mean(df_ggl$y[df_ggl$treatment == "Neighbors"]) -
  mean(df_ggl$y[df_ggl$treatment == "Control"])

# Calculate difference of means using regression
df_ggl <- df_ggl |>
  mutate(neighbors = case_when(
    treatment == "Neighbors" ~ 1,
    treatment == "Control" ~ 0,
    TRUE ~ NA
  ))
fit <- lm(y ~ neighbors, data = df_ggl)

# Standard errors
summary(fit)

library("broom")
tidy(fit)

# 95% confidence interval
tidy(fit) |>
  mutate(lower = estimate - 2 * std.error,
         upper = estimate + 2 * std.error)


# Looking at all the treatments
fit_all <- lm(y ~ treatment, data = df_ggl)
tidy(fit_all)

# Changing the comparison
df_ggl <- df_ggl |>
  mutate(treatment = fct_relevel(treatment, "Control"))
fit_all_new <- lm(y ~ treatment, data = df_ggl)
tidy(fit_all_new)

# Clustered standard errors
library("fixest")
fit_all_clus <- feols(y ~ treatment,
                      vcov = ~ hh_id,
                      data = df_ggl)
tidy(fit_all_clus)






