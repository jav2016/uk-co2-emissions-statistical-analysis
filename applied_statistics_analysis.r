# =============================================================================
# Project: UK CO2, Renewable Energy and Energy Use Analysis (1990–Present)
# Module:  CMP7205 Applied Statistics (MSc Big Data Analytics)
# Purpose: Reproducible statistical analysis of associations between UK CO2
#          emissions per capita, renewable energy share, and energy use per capita.
#
# Data sources (downloaded automatically and cached locally):
#   - OWID CO2 dataset:   https://github.com/owid/co2-data
#   - OWID Energy dataset:https://github.com/owid/energy-data
#
# Outputs created (relative to the script directory):
#   /data    -> cached raw downloads (.csv)
#   /tables  -> analysis tables (.csv) + model summaries (.txt)
#   /figures -> Figure_01.png ... Figure_15.png
#
# Notes:
#   - The script is designed to run end-to-end without manual intervention.
#   - No hard-coded local file paths are used.
# =============================================================================

# -----------------------------------------------------------------------------
# 0) Libraries
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)   # ggplot2, dplyr, readr, tidyr
  library(car)         # vif()
  library(lmtest)      # dwtest()
})

# -----------------------------------------------------------------------------
# 1) Set working directory to the script location (portable)
# -----------------------------------------------------------------------------
set_script_wd <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) setwd(dirname(p))
  }
}
set_script_wd()

# -----------------------------------------------------------------------------
# 2) Create output folders
# -----------------------------------------------------------------------------
dir.create("data",    showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 3) Download data (cached)
# -----------------------------------------------------------------------------
URL_CO2   <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
URL_ENERG <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"

path_co2   <- file.path("data", "owid-co2-data.csv")
path_energ <- file.path("data", "owid-energy-data.csv")

download_if_needed <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading: ", basename(dest))
    utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  } else {
    message("Using cached file: ", basename(dest))
  }
}

download_if_needed(URL_CO2, path_co2)
download_if_needed(URL_ENERG, path_energ)

# -----------------------------------------------------------------------------
# 4) Load + prepare UK analytical dataset (1990 onward)
# -----------------------------------------------------------------------------
co2_data    <- read_csv(path_co2,   show_col_types = FALSE)
energy_data <- read_csv(path_energ, show_col_types = FALSE)

co2_uk <- co2_data %>%
  filter(country == "United Kingdom", year >= 1990) %>%
  select(year, co2_per_capita)

energy_uk <- energy_data %>%
  filter(country == "United Kingdom", year >= 1990) %>%
  select(year, renewables_share_energy, energy_per_capita)

merged <- inner_join(co2_uk, energy_uk, by = "year")

# Missing-value handling: drop incomplete years to keep a balanced analytic set
missing_by_col <- sapply(merged, function(x) sum(is.na(x)))
write_csv(
  tibble(Variable = names(missing_by_col), MissingCount = as.integer(missing_by_col)),
  file.path("tables", "Table_00_MissingValues_ByVariable.csv")
)

merged_clean <- merged %>% drop_na()

# Save final analytical dataset used throughout the script
write_csv(merged_clean, file.path("tables", "UK_Analytical_Dataset_Clean.csv"))

cat("\n========== FINAL ANALYTICAL DATASET ==========\n")
cat("Years:", min(merged_clean$year), "to", max(merged_clean$year), "\n")
cat("Observations:", nrow(merged_clean), "\n\n")

# -----------------------------------------------------------------------------
# 5) Helper functions (figure saving + consistent theme)
# -----------------------------------------------------------------------------
save_fig <- function(fig_no, plot_obj) {
  fn <- sprintf("Figure_%02d.png", fig_no)
  print(plot_obj)  # show in Plot pane
  ggsave(
    filename = file.path("figures", fn),
    plot     = plot_obj,
    width    = 9, height = 5, dpi = 300
  )
  invisible(fn)
}

theme_report <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# =============================================================================
# RQ1–RQ3: Descriptive statistics and time trends
# =============================================================================

# -----------------------------------------------------------------------------
# Table 1: Descriptive statistics
# -----------------------------------------------------------------------------
Table_1_Descriptives <- merged_clean %>%
  summarise(
    n_years = n(),
    co2_mean = mean(co2_per_capita),
    co2_median = median(co2_per_capita),
    co2_sd = sd(co2_per_capita),
    co2_min = min(co2_per_capita),
    co2_max = max(co2_per_capita),
    
    ren_mean = mean(renewables_share_energy),
    ren_median = median(renewables_share_energy),
    ren_sd = sd(renewables_share_energy),
    ren_min = min(renewables_share_energy),
    ren_max = max(renewables_share_energy),
    
    energy_mean = mean(energy_per_capita),
    energy_median = median(energy_per_capita),
    energy_sd = sd(energy_per_capita),
    energy_min = min(energy_per_capita),
    energy_max = max(energy_per_capita)
  ) %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value")

write_csv(Table_1_Descriptives, file.path("tables", "Table_01_Descriptive_Statistics.csv"))

# -----------------------------------------------------------------------------
# Figures 1–3: Time-series plots
# -----------------------------------------------------------------------------
p1 <- ggplot(merged_clean, aes(year, co2_per_capita)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "CO2 emissions per capita in the United Kingdom (1990-present)",
    x = "Year", y = "CO2 emissions per capita (tonnes per person)"
  ) +
  theme_report()
save_fig(1, p1)

p2 <- ggplot(merged_clean, aes(year, renewables_share_energy)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Renewable energy share in the United Kingdom (1990-present)",
    x = "Year", y = "Renewables share of primary energy (%)"
  ) +
  theme_report()
save_fig(2, p2)

p3 <- ggplot(merged_clean, aes(year, energy_per_capita)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Energy use per capita in the United Kingdom (1990-present)",
    x = "Year", y = "Energy use per capita (kWh per person)"
  ) +
  theme_report()
save_fig(3, p3)

# =============================================================================
# Assumption checks prior to parametric inference (supports RQ4–RQ8)
# =============================================================================

# -----------------------------------------------------------------------------
# Figures 4–6: Histograms
# -----------------------------------------------------------------------------
p4 <- ggplot(merged_clean, aes(x = co2_per_capita)) +
  geom_histogram(binwidth = 0.5, fill = "grey60", color = "white") +
  labs(
    title = "Distribution of CO2 emissions per capita in the UK (1990-present)",
    x = "CO2 emissions per capita (tonnes per person)",
    y = "Frequency (years)"
  ) +
  theme_report()
save_fig(4, p4)

p5 <- ggplot(merged_clean, aes(x = renewables_share_energy)) +
  geom_histogram(binwidth = 1, fill = "grey60", color = "white") +
  labs(
    title = "Distribution of renewable energy share in the UK (1990-present)",
    x = "Renewables share of primary energy (%)",
    y = "Frequency (years)"
  ) +
  theme_report()
save_fig(5, p5)

p6 <- ggplot(merged_clean, aes(x = energy_per_capita)) +
  geom_histogram(binwidth = 1000, fill = "grey60", color = "white") +
  labs(
    title = "Distribution of energy use per capita in the UK (1990-present)",
    x = "Energy use per capita (kWh per person)",
    y = "Frequency (years)"
  ) +
  theme_report()
save_fig(6, p6)

# -----------------------------------------------------------------------------
# Figures 7–9: Normal Q–Q plots (variables)
# -----------------------------------------------------------------------------
p7 <- ggplot(merged_clean, aes(sample = co2_per_capita)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q plot: CO2 emissions per capita") +
  theme_report()
save_fig(7, p7)

p8 <- ggplot(merged_clean, aes(sample = renewables_share_energy)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q plot: renewable energy share") +
  theme_report()
save_fig(8, p8)

p9 <- ggplot(merged_clean, aes(sample = energy_per_capita)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q plot: energy use per capita") +
  theme_report()
save_fig(9, p9)

# -----------------------------------------------------------------------------
# Table 2: Shapiro–Wilk normality tests (variables)
# -----------------------------------------------------------------------------
shapiro_row <- function(x) {
  st <- shapiro.test(x)
  tibble(W = as.numeric(st$statistic), p_value = st$p.value)
}

Table_2_Normality <- tibble(
  Variable = c("co2_per_capita", "renewables_share_energy", "energy_per_capita"),
  bind_rows(
    shapiro_row(merged_clean$co2_per_capita),
    shapiro_row(merged_clean$renewables_share_energy),
    shapiro_row(merged_clean$energy_per_capita)
  )
)

write_csv(Table_2_Normality, file.path("tables", "Table_02_ShapiroWilk_Normality.csv"))

# =============================================================================
# RQ4–RQ5: Correlation analysis and hypothesis testing
# =============================================================================

cor_ren <- cor.test(merged_clean$co2_per_capita,
                    merged_clean$renewables_share_energy,
                    method = "pearson")

cor_eng <- cor.test(merged_clean$co2_per_capita,
                    merged_clean$energy_per_capita,
                    method = "pearson")

cor_ren_eng <- cor.test(merged_clean$renewables_share_energy,
                        merged_clean$energy_per_capita,
                        method = "pearson")

# -----------------------------------------------------------------------------
# Table 3: Pearson correlations (r, p, n)
# -----------------------------------------------------------------------------
Table_3_Corr <- tibble(
  Pair = c("CO2_per_capita vs Renewables_share_energy",
           "CO2_per_capita vs Energy_per_capita",
           "Renewables_share_energy vs Energy_per_capita"),
  r = c(as.numeric(cor_ren$estimate),
        as.numeric(cor_eng$estimate),
        as.numeric(cor_ren_eng$estimate)),
  p_value = c(cor_ren$p.value, cor_eng$p.value, cor_ren_eng$p.value),
  n = nrow(merged_clean)
)

write_csv(Table_3_Corr, file.path("tables", "Table_03_Correlations_Pearson.csv"))

# -----------------------------------------------------------------------------
# Figures 10–11: Scatter plots with fitted line
# -----------------------------------------------------------------------------
p10 <- ggplot(merged_clean, aes(renewables_share_energy, co2_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "CO2 emissions per capita vs renewable energy share (UK)",
    x = "Renewables share of primary energy (%)",
    y = "CO2 emissions per capita (tonnes per person)"
  ) +
  theme_report()
save_fig(10, p10)

p11 <- ggplot(merged_clean, aes(energy_per_capita, co2_per_capita)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "CO2 emissions per capita vs energy use per capita (UK)",
    x = "Energy use per capita (kWh per person)",
    y = "CO2 emissions per capita (tonnes per person)"
  ) +
  theme_report()
save_fig(11, p11)

# =============================================================================
# RQ6–RQ7: Regression models (single-variable vs multivariate)
# =============================================================================

m1 <- lm(co2_per_capita ~ renewables_share_energy, data = merged_clean)
m2 <- lm(co2_per_capita ~ energy_per_capita, data = merged_clean)
m3 <- lm(co2_per_capita ~ renewables_share_energy + energy_per_capita, data = merged_clean)

# Save full model summaries
capture.output(summary(m1), file = file.path("tables", "Model_1_Summary.txt"))
capture.output(summary(m2), file = file.path("tables", "Model_2_Summary.txt"))
capture.output(summary(m3), file = file.path("tables", "Model_3_Summary.txt"))

# -----------------------------------------------------------------------------
# Table 4: Regression results (coefficients + R2 / Adj. R2)
# -----------------------------------------------------------------------------
coef_table <- function(model, model_name) {
  s <- summary(model)
  ct <- as.data.frame(coef(s))
  ct$Term <- rownames(ct)
  rownames(ct) <- NULL
  
  ct <- ct %>%
    relocate(Term) %>%
    rename(
      Std_Error = `Std. Error`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    mutate(
      Model = model_name,
      R2 = s$r.squared,
      Adj_R2 = s$adj.r.squared
    ) %>%
    relocate(Model, Term, Estimate, Std_Error, t_value, p_value, R2, Adj_R2)
  
  ct
}

Table_4_Regression <- bind_rows(
  coef_table(m1, "Model 1: CO2 ~ Renewables"),
  coef_table(m2, "Model 2: CO2 ~ Energy"),
  coef_table(m3, "Model 3: CO2 ~ Renewables + Energy")
)

write_csv(Table_4_Regression, file.path("tables", "Table_04_Regression_Results.csv"))

# =============================================================================
# RQ8: Regression diagnostics
# =============================================================================

# -----------------------------------------------------------------------------
# Figures 12–13: Model 1 diagnostics
# -----------------------------------------------------------------------------
df_m1 <- tibble(fitted = fitted(m1), resid = residuals(m1))

p12 <- ggplot(df_m1, aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted values (Model 1: CO2 ~ renewables)",
    x = "Fitted values", y = "Residuals"
  ) +
  theme_report()
save_fig(12, p12)

p13 <- ggplot(df_m1, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q plot of residuals (Model 1)") +
  theme_report()
save_fig(13, p13)

# -----------------------------------------------------------------------------
# Figures 14–15: Model 3 diagnostics
# -----------------------------------------------------------------------------
df_m3 <- tibble(fitted = fitted(m3), resid = residuals(m3))

p14 <- ggplot(df_m3, aes(fitted, resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted values (Model 3: joint model)",
    x = "Fitted values", y = "Residuals"
  ) +
  theme_report()
save_fig(14, p14)

p15 <- ggplot(df_m3, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q plot of residuals (Model 3)") +
  theme_report()
save_fig(15, p15)

# -----------------------------------------------------------------------------
# Table 5: Diagnostic statistics (Durbin–Watson, residual Shapiro–Wilk, VIF)
# -----------------------------------------------------------------------------
diag_row <- function(model, model_name, include_vif = FALSE) {
  dw <- dwtest(model)
  sw <- shapiro.test(residuals(model))
  
  out <- tibble(
    Model = model_name,
    DW_statistic = as.numeric(dw$statistic),
    DW_p_value   = dw$p.value,
    Residual_Shapiro_W = as.numeric(sw$statistic),
    Residual_Shapiro_p = sw$p.value
  )
  
  if (include_vif) {
    v <- vif(model)
    out <- out %>%
      mutate(
        VIF_Renewables = as.numeric(v["renewables_share_energy"]),
        VIF_Energy     = as.numeric(v["energy_per_capita"])
      )
  } else {
    out <- out %>%
      mutate(
        VIF_Renewables = NA_real_,
        VIF_Energy     = NA_real_
      )
  }
  
  out
}

Table_5_Diagnostics <- bind_rows(
  diag_row(m1, "Model 1: CO2 ~ Renewables", include_vif = FALSE),
  diag_row(m2, "Model 2: CO2 ~ Energy", include_vif = FALSE),
  diag_row(m3, "Model 3: CO2 ~ Renewables + Energy", include_vif = TRUE)
)

write_csv(Table_5_Diagnostics, file.path("tables", "Table_05_Model_Diagnostics.csv"))

# -----------------------------------------------------------------------------
# Completion message
# -----------------------------------------------------------------------------
cat("========== ANALYSIS COMPLETE ==========\n")
cat("Figures saved in:  /figures (Figure_01 ... Figure_15)\n")
cat("Tables saved in:   /tables  (Table_01 ... Table_05 + model summaries)\n")
cat("All figures were printed to the Plot pane during execution.\n")
