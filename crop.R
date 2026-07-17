## =====================================================================
## CROP GROWTH ANALYSIS: Rainwater vs Groundwater Irrigation
## Crops: Cucumber, Ewedu, Okra, Tete
## Data: crops growth.xlsx (4 sheets, 2 replicates per treatment, 13 weeks)
## =====================================================================

## SETUP --------------------------------------------------------

packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "stringr", "purrr",
              "tibble", "rstatix", "writexl", "lme4", "lmerTest", "scales")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)

invisible(lapply(packages, library, character.only = TRUE))

## FILE PATHS --------------------------------------------------
# Use the actual file name with a space
file_path <- "Final Year Shit FYS/crops growth.xlsx"

# Check if the file exists
if (!file.exists(file_path)) {
  stop("File not found at: ", file_path, 
       "\nPlease check the path and file name. Current working directory: ", getwd())
}

out_dir <- "Final Year Shit FYS/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

crops <- c("Cucumber", "Ewedu", "Okra", "Tete")

## Consistent plot theme with visible x & y axis lines
theme_growth <- function() {
  theme_bw(base_size = 12) +
    theme(
      axis.line        = element_line(color = "black", linewidth = 0.5),
      panel.border     = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      strip.background = element_rect(fill = "grey20"),
      strip.text       = element_text(color = "white", face = "bold"),
      plot.title       = element_text(face = "bold", size = 13),
      legend.position  = "bottom"
    )
}

treatment_colors <- c("Rainwater" = "#2166AC", "Groundwater" = "#B2182B")

## IMPORT & RESHAPE DATA -------------------------------------------

read_crop_sheet <- function(crop, path) {
  raw <- read_excel(path, sheet = crop, skip = 3, col_names = FALSE)
  colnames(raw) <- c("Week_label", "Stage",
                     "Height_R1", "Leaves_R1",
                     "Height_R2", "Leaves_R2",
                     "Height_G1", "Leaves_G1",
                     "Height_G2", "Leaves_G2")
  raw <- raw %>% filter(!is.na(Week_label))
  raw$Week <- as.integer(str_extract(raw$Week_label, "\\d+"))
  raw$Crop <- crop
  raw
}

raw_all <- map_dfr(crops, read_crop_sheet, path = file_path)

long_df <- raw_all %>%
  select(Crop, Week, Stage, Height_R1, Leaves_R1, Height_R2, Leaves_R2,
         Height_G1, Leaves_G1, Height_G2, Leaves_G2) %>%
  pivot_longer(
    cols = c(starts_with("Height_"), starts_with("Leaves_")),
    names_to = c("Variable", "Treatment_Rep"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  mutate(
    Treatment = ifelse(str_starts(Treatment_Rep, "R"), "Rainwater", "Groundwater"),
    Replicate = str_extract(Treatment_Rep, "\\d+"),
    Subject   = paste(Treatment, Replicate, sep = "_")
  ) %>%
  select(-Treatment_Rep) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(
    Crop      = factor(Crop, levels = crops),
    Treatment = factor(Treatment, levels = c("Rainwater", "Groundwater")),
    Week      = as.integer(Week)
  ) %>%
  arrange(Crop, Treatment, Replicate, Week)

write_xlsx(long_df, file.path(out_dir, "01_long_format_data.xlsx"))

cat("Data imported:", nrow(long_df), "rows (", length(crops), "crops x 13 weeks x 2 treatments x 2 replicates )\n")

## DESCRIPTIVE STATISTICS --------------------------------------

desc_weekly <- long_df %>%
  group_by(Crop, Treatment, Week) %>%
  summarise(
    n           = n(),
    Height_mean = mean(Height, na.rm = TRUE),
    Height_sd   = sd(Height, na.rm = TRUE),
    Height_se   = Height_sd / sqrt(n),
    Leaves_mean = mean(Leaves, na.rm = TRUE),
    Leaves_sd   = sd(Leaves, na.rm = TRUE),
    Leaves_se   = Leaves_sd / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(Crop, Treatment, Week)

desc_overall <- long_df %>%
  group_by(Crop, Treatment) %>%
  summarise(
    n           = n(),
    Height_mean = mean(Height), Height_sd = sd(Height),
    Height_min  = min(Height),  Height_max = max(Height),
    Leaves_mean = mean(Leaves), Leaves_sd = sd(Leaves),
    Leaves_min  = min(Leaves),  Leaves_max = max(Leaves),
    .groups = "drop"
  )

write_xlsx(list(Weekly = desc_weekly, Overall = desc_overall),
           file.path(out_dir, "02_descriptive_statistics.xlsx"))

print(desc_overall)

## COMPARATIVE GROWTH ANALYSIS ---------------------------------

growth_df <- desc_weekly %>%
  arrange(Crop, Treatment, Week) %>%
  group_by(Crop, Treatment) %>%
  mutate(
    Height_abs_growth = Height_mean - lag(Height_mean),
    Leaves_abs_growth = Leaves_mean - lag(Leaves_mean),
    Height_RGR = (log(Height_mean) - log(lag(Height_mean))) / (Week - lag(Week)),
    Total_Height_gain = last(Height_mean) - first(Height_mean),
    Pct_Height_gain   = 100 * Total_Height_gain / first(Height_mean)
  ) %>%
  ungroup()

write_xlsx(growth_df, file.path(out_dir, "03_growth_rate_analysis.xlsx"))

# Growth curves: Height
p_height_growth <- ggplot(desc_weekly, aes(Week, Height_mean, color = Treatment)) +
  geom_errorbar(aes(ymin = Height_mean - Height_se, ymax = Height_mean + Height_se),
                width = 0.25, linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~Crop, scales = "free_y") +
  scale_x_continuous(breaks = 1:13) +
  scale_color_manual(values = treatment_colors) +
  labs(title = "Plant Height Growth Over 13 Weeks", x = "Week", y = "Height (cm, mean \u00B1 SE)") +
  theme_growth()

ggsave(file.path(out_dir, "plot_01_height_growth_curves.png"), p_height_growth,
       width = 10, height = 7, dpi = 300)

# Growth curves: Leaves
p_leaves_growth <- ggplot(desc_weekly, aes(Week, Leaves_mean, color = Treatment)) +
  geom_errorbar(aes(ymin = Leaves_mean - Leaves_se, ymax = Leaves_mean + Leaves_se),
                width = 0.25, linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~Crop, scales = "free_y") +
  scale_x_continuous(breaks = 1:13) +
  scale_color_manual(values = treatment_colors) +
  labs(title = "Leaf Count Growth Over 13 Weeks", x = "Week", y = "Number of Leaves (mean \u00B1 SE)") +
  theme_growth()

ggsave(file.path(out_dir, "plot_02_leaves_growth_curves.png"), p_leaves_growth,
       width = 10, height = 7, dpi = 300)

# Relative growth rate plot
p_rgr <- ggplot(growth_df %>% filter(!is.na(Height_RGR)),
                aes(Week, Height_RGR, color = Treatment)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~Crop) +
  scale_x_continuous(breaks = 1:13) +
  scale_color_manual(values = treatment_colors) +
  labs(title = "Relative Growth Rate (Height) by Week",
       x = "Week", y = "RGR (cm/cm/week)") +
  theme_growth()

ggsave(file.path(out_dir, "plot_03_relative_growth_rate.png"), p_rgr,
       width = 10, height = 7, dpi = 300)

## INDEPENDENT SAMPLES T-TESTS (Rainwater vs Groundwater) ------

run_ttest <- function(df, var) {
  form <- as.formula(paste(var, "~ Treatment"))
  tt <- tryCatch(t.test(form, data = df), error = function(e) NULL)
  if (is.null(tt)) {
    return(tibble(t_statistic = NA_real_, df = NA_real_, p_value = NA_real_,
                  mean_diff = NA_real_))
  }
  tibble(
    t_statistic = unname(tt$statistic),
    df          = unname(tt$parameter),
    p_value     = tt$p.value,
    mean_diff   = unname(diff(rev(tt$estimate)))
  )
}

# 4a. Weekly independent t-tests per crop (Height & Leaves)
ttest_weekly <- long_df %>%
  group_by(Crop, Week) %>%
  group_modify(~ bind_rows(
    run_ttest(.x, "Height") %>% mutate(Variable = "Height"),
    run_ttest(.x, "Leaves") %>% mutate(Variable = "Leaves")
  )) %>%
  ungroup() %>%
  mutate(Significant = ifelse(!is.na(p_value) & p_value < 0.05, "Yes", "No")) %>%
  select(Crop, Week, Variable, t_statistic, df, p_value, mean_diff, Significant)

# 4b. Overall t-test per crop, pooling all weeks (n=26 per treatment)
# NOTE: pools repeated-measures data across weeks (pseudoreplication) -
# use for a broad-strokes summary only; the weekly tests (4a) and the
# repeated-measures ANOVA (section 6) are the statistically proper tests.
ttest_overall <- long_df %>%
  group_by(Crop) %>%
  group_modify(~ bind_rows(
    run_ttest(.x, "Height") %>% mutate(Variable = "Height"),
    run_ttest(.x, "Leaves") %>% mutate(Variable = "Leaves")
  )) %>%
  ungroup() %>%
  mutate(Significant = ifelse(!is.na(p_value) & p_value < 0.05, "Yes", "No"))

write_xlsx(list(Weekly = ttest_weekly, Overall_pooled = ttest_overall),
           file.path(out_dir, "04_t_test_results.xlsx"))

# Plot of weekly p-values (Height) to visualise where treatments diverge
p_pvals <- ggplot(ttest_weekly %>% filter(Variable == "Height"),
                  aes(Week, p_value)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_line(color = "grey40") +
  geom_point(aes(color = Significant), size = 2.2) +
  facet_wrap(~Crop) +
  scale_x_continuous(breaks = 1:13) +
  scale_color_manual(values = c(Yes = "#B2182B", No = "grey50")) +
  labs(title = "Weekly Independent t-test p-values: Height (Rainwater vs Groundwater)",
       x = "Week", y = "p-value") +
  theme_growth()

ggsave(file.path(out_dir, "plot_04_weekly_ttest_pvalues.png"), p_pvals,
       width = 10, height = 7, dpi = 300)

## REPEATED MEASURES ANOVA -------------------------------------
## Design: Treatment (between) x Week (within, 13 levels), Subject = replicate
## Only 2 subjects per treatment group -> classic rstatix ANOVA may fail on
## sphericity corrections with so few subjects; a linear mixed model (lmer)
## is included as a robust fallback/complement.

rm_anova_classic <- map_dfr(crops, function(cr) {
  df_c <- long_df %>% filter(Crop == cr)
  out <- list()
  for (v in c("Height", "Leaves")) {
    res <- tryCatch({
      a <- anova_test(data = df_c, dv = !!v, wid = Subject,
                      within = Week, between = Treatment)
      as_tibble(get_anova_table(a)) %>% mutate(Crop = cr, Variable = v)
    }, error = function(e) {
      tibble(Effect = c("Treatment", "Week", "Treatment:Week"),
             Note = paste("Classic RM-ANOVA failed:", conditionMessage(e)),
             Crop = cr, Variable = v)
    })
    out[[v]] <- res
  }
  bind_rows(out)
})

rm_anova_mixed <- map_dfr(crops, function(cr) {
  df_c <- long_df %>% filter(Crop == cr) %>%
    mutate(Week_f = factor(Week))
  out <- list()
  for (v in c("Height", "Leaves")) {
    form <- as.formula(paste(v, "~ Treatment * Week + (1 | Subject)"))
    m <- tryCatch(lmer(form, data = df_c), error = function(e) NULL)
    if (is.null(m)) next
    a <- as.data.frame(anova(m))
    a$Term <- rownames(a)
    a$Crop <- cr
    a$Variable <- v
    out[[v]] <- a
  }
  bind_rows(out)
})

write_xlsx(list(Classic_RM_ANOVA = rm_anova_classic,
                Mixed_Model_ANOVA = rm_anova_mixed),
           file.path(out_dir, "05_repeated_measures_anova.xlsx"))

print(rm_anova_mixed)

## FINAL HARVEST COMPARISON (WEEK 13) --------------------------

final_harvest <- long_df %>% filter(Week == 13)

final_summary <- final_harvest %>%
  group_by(Crop, Treatment) %>%
  summarise(
    n = n(),
    Height_mean = mean(Height), Height_se = sd(Height) / sqrt(n),
    Leaves_mean = mean(Leaves), Leaves_se = sd(Leaves) / sqrt(n),
    .groups = "drop"
  )

final_ttest <- final_harvest %>%
  group_by(Crop) %>%
  group_modify(~ bind_rows(
    run_ttest(.x, "Height") %>% mutate(Variable = "Height"),
    run_ttest(.x, "Leaves") %>% mutate(Variable = "Leaves")
  )) %>%
  ungroup() %>%
  mutate(Significant = ifelse(!is.na(p_value) & p_value < 0.05, "Yes", "No"))

write_xlsx(list(Week13_Summary = final_summary, Week13_TTest = final_ttest),
           file.path(out_dir, "06_final_harvest_week13.xlsx"))

print(final_ttest)

# Bar plots with error bars: Height & Leaves at Week 13
p_final_height <- ggplot(final_summary, aes(Crop, Height_mean, fill = Treatment)) +
  geom_col(position = position_dodge(0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Height_mean - Height_se, ymax = Height_mean + Height_se),
                position = position_dodge(0.7), width = 0.2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = treatment_colors) +
  labs(title = "Final Harvest (Week 13): Plant Height", x = "Crop", y = "Height (cm)") +
  theme_growth()

ggsave(file.path(out_dir, "plot_05_final_height_comparison.png"), p_final_height,
       width = 7, height = 5, dpi = 300)

p_final_leaves <- ggplot(final_summary, aes(Crop, Leaves_mean, fill = Treatment)) +
  geom_col(position = position_dodge(0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Leaves_mean - Leaves_se, ymax = Leaves_mean + Leaves_se),
                position = position_dodge(0.7), width = 0.2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = treatment_colors) +
  labs(title = "Final Harvest (Week 13): Leaf Count", x = "Crop", y = "Number of Leaves") +
  theme_growth()

ggsave(file.path(out_dir, "plot_06_final_leaves_comparison.png"), p_final_leaves,
       width = 7, height = 5, dpi = 300)

## CORRELATION ANALYSIS (Height vs Leaves) ---------------------

cor_results <- long_df %>%
  group_by(Crop, Treatment) %>%
  summarise(
    r       = cor(Height, Leaves, method = "pearson"),
    p_value = cor.test(Height, Leaves)$p.value,
    n       = n(),
    .groups = "drop"
  ) %>%
  mutate(Significant = ifelse(p_value < 0.05, "Yes", "No"))

write_xlsx(cor_results, file.path(out_dir, "07_correlation_analysis.xlsx"))
print(cor_results)

p_correlation <- ggplot(long_df, aes(Height, Leaves, color = Treatment)) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  facet_wrap(~Crop, scales = "free") +
  scale_color_manual(values = treatment_colors) +
  labs(title = "Correlation Between Height and Leaf Count",
       x = "Height (cm)", y = "Number of Leaves") +
  theme_growth()

ggsave(file.path(out_dir, "plot_07_height_leaves_correlation.png"), p_correlation,
       width = 10, height = 7, dpi = 300)

## SUMMARY TABLE ------------------------------------------------

summary_table <- final_summary %>%
  select(Crop, Treatment, Height_mean, Height_se, Leaves_mean, Leaves_se) %>%
  left_join(
    growth_df %>% distinct(Crop, Treatment, Total_Height_gain, Pct_Height_gain),
    by = c("Crop", "Treatment")
  ) %>%
  left_join(
    final_ttest %>% filter(Variable == "Height") %>%
      select(Crop, Height_p = p_value, Height_sig = Significant),
    by = "Crop"
  ) %>%
  left_join(
    final_ttest %>% filter(Variable == "Leaves") %>%
      select(Crop, Leaves_p = p_value, Leaves_sig = Significant),
    by = "Crop"
  ) %>%
  left_join(
    cor_results %>% select(Crop, Treatment, Height_Leaves_r = r, Cor_p = p_value),
    by = c("Crop", "Treatment")
  ) %>%
  arrange(Crop, Treatment)

write_xlsx(summary_table, file.path(out_dir, "08_summary_table.xlsx"))
print(summary_table, n = Inf)

cat("\n=====================================================================\n")
cat("ANALYSIS COMPLETE. Outputs written to:", normalizePath(out_dir), "\n")
cat("Tables (xlsx): 01-08 files\n")
cat("Plots  (png, 300dpi): plot_01 to plot_07\n")
cat("=====================================================================\n")

