########################################################################
# CMP7205 Applied Statistics Report
# Food Recipes Dataset — Full Analysis Script (Base R only)
#
# HOW TO USE:
# 1. Put food_recipes.csv in the same folder as this script.
# 2. Open this file in RStudio.
# 3. Set your working directory to that folder (Session > Set Working
#    Directory > To Source File Location).
# 4. Run section by section (highlight a section, press Ctrl+Enter / Cmd+Enter)
#    rather than the whole file at once, so you can read each result
#    before moving on.
# 5. All figures save automatically as PNG into an "outputs" folder.
#    Console output is captured into outputs/analysis_log.txt.
########################################################################

## ======================================================================
## SECTION 0: SETUP
## ======================================================================

# Set the working directory to wherever this script + food_recipes.csv
# actually live on your machine. Use forward slashes (/), not backslashes,
# even on Windows -- R treats backslash as an escape character.
# EDIT THIS PATH if you move the files to a different folder.
setwd("F:/uk/jyoshna UK/applied statistics")

# Sanity check: this should list food_recipes_analysis.R and
# food_recipes.csv. If food_recipes.csv is NOT in this list, the setwd()
# path above is wrong -- fix it before continuing.
cat("Working directory set to:", getwd(), "\n")
cat("Files found here:\n")
print(list.files())

# Create an outputs folder for figures, logs, and the cleaned dataset
if (!dir.exists("outputs")) dir.create("outputs")

# Start capturing all console output to a log file (for your appendix /
# for re-verification later). split=TRUE also shows it in the console.
log_con <- file("outputs/analysis_log.txt", open = "wt")
sink(log_con, split = TRUE)

set.seed(2026)  # for reproducibility of the train/test split later

cat("========================================\n")
cat("CMP7205 Applied Statistics — Analysis Log\n")
cat("Run date:", format(Sys.time()), "\n")
cat("========================================\n\n")


## ======================================================================
## SECTION 1: LOAD AND CLEAN DATA
## ======================================================================

df <- read.csv("food_recipes.csv", stringsAsFactors = FALSE)

cat("Raw dataset dimensions:\n")
print(dim(df))
cat("\nColumn names:\n")
print(names(df))

# --- 1.1 Missingness per variable (report BEFORE any cleaning) ---
missing_pct <- round(sapply(df, function(x) mean(is.na(x) | x == "") * 100), 2)
missing_table <- data.frame(Variable = names(missing_pct),
                             Missing_Percent = as.numeric(missing_pct))
missing_table <- missing_table[order(-missing_table$Missing_Percent), ]
cat("\n--- Missingness by variable (%) ---\n")
print(missing_table, row.names = FALSE)
write.csv(missing_table, "outputs/missingness_table.csv", row.names = FALSE)

# No variable exceeds ~2% missingness, so nothing needs exclusion on
# those grounds. record_health is retained in the raw file but will be
# dropped from analysis (see below) because it is constant, not because
# of missingness.

# --- 1.2 Check record_health has zero variance ---
cat("\n--- record_health value counts (checking for zero variance) ---\n")
print(table(df$record_health))
# If this prints only "good", the column carries no information and is
# excluded from all analysis. State this explicitly in your report.

# --- 1.3 Parse prep_time / cook_time from text ("15 M") to numeric minutes ---
df$prep_min <- as.numeric(gsub(" M", "", df$prep_time))
df$cook_min <- as.numeric(gsub(" M", "", df$cook_time))

cat("\n--- prep_min summary ---\n")
print(summary(df$prep_min))
cat("\n--- cook_min summary ---\n")
print(summary(df$cook_min))

# --- 1.4 Diet grouping for the t-test (RQ2) ---
# ASSUMPTION (state this in your Methodology / Pre-processing section):
# Eggetarian is classified as the "Vegetarian" branch because it excludes
# meat/fish (contains no flesh), consistent with how the dataset's own
# authors treat it as distinct from "Non Vegeterian". This is a judgement
# call — say so explicitly rather than presenting it as an obvious fact.
non_veg_labels <- c("Non Vegeterian", "High Protein Non Vegetarian")
df$diet_group <- ifelse(df$diet %in% non_veg_labels, "Non-Vegetarian",
                  ifelse(df$diet == "" | is.na(df$diet), NA, "Vegetarian"))

cat("\n--- diet_group counts ---\n")
print(table(df$diet_group, useNA = "ifany"))

# --- 1.5 Cuisine collapsing for ANOVA (RQ4) ---
# Collapse to the 5 largest categories + "Other". This avoids testing
# 20+ unbalanced groups, which would violate ANOVA's practical
# assumptions and be uninterpretable.
cat("\n--- Top cuisine categories before collapsing ---\n")
print(sort(table(df$cuisine), decreasing = TRUE)[1:10])

top5_cuisines <- c("Indian", "Continental", "North Indian Recipes",
                    "South Indian Recipes", "Italian Recipes")
df$cuisine_group <- ifelse(df$cuisine %in% top5_cuisines, df$cuisine, "Other")
df$cuisine_group <- factor(df$cuisine_group,
                            levels = c(top5_cuisines, "Other"))

cat("\n--- cuisine_group counts (after collapsing) ---\n")
print(table(df$cuisine_group))

# --- 1.6 Sanity check on prep_min / cook_min = 0 rows ---
# Per the brief's outlier-handling rules: do NOT blanket-delete these.
# Zero prep/cook time is domain-plausible for raw/no-cook dishes
# (salads, chutneys). Inspect a sample of titles before deciding.
cat("\n--- Sample of recipes with prep_min == 0 (inspect titles manually) ---\n")
print(head(df$recipe_title[df$prep_min == 0], 10))

cat("\n--- Sample of recipes with cook_min == 0 (inspect titles manually) ---\n")
print(head(df$recipe_title[df$cook_min == 0], 10))

# DECISION (made after inspecting the printed titles above):
#
# cook_min = 0 is KEPT as genuine. The titles are salsas, no-bake
# desserts, salads and dips -- dishes that legitimately require no
# cooking. This is a real subpopulation in the data, not an error.
#
# prep_min = 0 is treated as MISSING DATA, not a true value. The titles
# include a porridge, roasted potatoes, a cooked sauce, and cupcakes --
# none of these can plausibly have zero preparation (no chopping,
# mixing, or assembly at all). Zero is being used here as a default/
# placeholder rather than a real measurement, so it is recoded to NA
# and folded into the missingness reported for prep_min. State this
# reasoning explicitly in your Data Pre-processing section.
n_prep_zero <- sum(df$prep_min == 0, na.rm = TRUE)
df$prep_min[df$prep_min == 0] <- NA
cat("\n--- prep_min = 0 recoded to NA ---\n")
cat("Rows affected:", n_prep_zero, "\n")
cat("Updated prep_min missingness:",
    round(mean(is.na(df$prep_min)) * 100, 2), "%\n")
cat("(This replaces the original 0.37% NA figure reported for prep_time\n",
    "in the missingness table above -- report the UPDATED percentage in\n",
    "your final missingness table, not the original one.)\n")

cat("\n\n==== SECTION 1 COMPLETE — review missingness table and printed",
    "titles above before continuing ====\n\n")


## ======================================================================
## SECTION 2: EXPLORATORY DATA ANALYSIS (EDA)
## ======================================================================

# --- 2.1 Descriptive statistics table for core continuous variables ---
core_vars <- c("prep_min", "cook_min", "vote_count", "rating")
desc_stats <- t(sapply(df[core_vars], function(x) {
  x <- x[!is.na(x)]
  c(Mean = mean(x), Median = median(x), SD = sd(x),
    Min = min(x), Max = max(x), N = length(x))
}))
cat("--- Descriptive statistics: core continuous variables ---\n")
print(round(desc_stats, 2))
write.csv(round(desc_stats, 2), "outputs/descriptive_stats.csv")

# --- 2.2 Figure: missing data by variable ---
png("outputs/fig1_missing_data.png", width = 900, height = 600)
barplot(missing_table$Missing_Percent, names.arg = missing_table$Variable,
        las = 2, cex.names = 0.7, col = "steelblue",
        main = "Figure 1: Missing Data by Variable (%)",
        ylab = "Missing (%)")
dev.off()

# --- 2.3 Figure: correlation matrix / heatmap among continuous variables ---
cor_vars <- df[, c("prep_min", "cook_min", "vote_count", "rating")]
cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")
cat("\n--- Correlation matrix (core continuous variables) ---\n")
print(round(cor_matrix, 3))

png("outputs/fig2_correlation_heatmap.png", width = 700, height = 700)
image(1:ncol(cor_matrix), 1:ncol(cor_matrix), cor_matrix,
      axes = FALSE, xlab = "", ylab = "",
      main = "Figure 2: Correlation Heatmap")
axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2)
axis(2, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2)
for (i in 1:ncol(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    text(i, j, round(cor_matrix[i, j], 2))
  }
}
dev.off()

# --- 2.4 Figure: distribution histogram of main outcome (cook_min) ---
# The raw range (0-7200 min) is dominated by a small number of extreme
# outliers (e.g. slow-fermented/dried dishes traced in Section 7), which
# compress the entire real distribution into a single bar if plotted at
# full scale. The x-axis is capped at 200 minutes for READABILITY ONLY;
# no data is removed from the dataset or from any statistical test --
# this is a display choice, stated explicitly in the figure caption.
png("outputs/fig3_cook_min_distribution.png", width = 800, height = 600)
hist(df$cook_min[df$cook_min <= 200], breaks = 40, col = "darkorange",
     main = "Figure 3: Distribution of Cook Time (0-200 min range shown)",
     xlab = "Cook time (minutes)",
     sub = paste0("Note: ", sum(df$cook_min > 200, na.rm = TRUE),
                  " values above 200 min omitted from this plot for ",
                  "readability; all values retained in the analysis."))
dev.off()

cat("\n\n==== SECTION 2 COMPLETE — 3 figures saved to outputs/ ====\n\n")
cat("NOTE for your report: tie each figure back to a downstream decision,",
    "e.g. 'Figure 3 shows right skew in cook_min, which is why residual",
    "normality is checked (not assumed) in Sections 3-6 below.'\n\n")


## ======================================================================
## SECTION 3: RQ1 — CORRELATION + SIMPLE REGRESSION
## RQ: Is there a significant relationship between prep time and cook
## time, and can cook time be predicted from prep time?
## ======================================================================

cat("========================================\n")
cat("RQ1: Correlation + Simple Regression\n")
cat("H0: rho = 0 (no linear relationship between prep_min and cook_min)\n")
cat("H1: rho != 0\n")
cat("alpha = 0.05; reject H0 if p < alpha\n")
cat("========================================\n\n")

rq1_data <- df[!is.na(df$prep_min) & !is.na(df$cook_min), ]

# --- Assumption check: linearity (visual) ---
png("outputs/fig4_rq1_scatter.png", width = 800, height = 600)
plot(rq1_data$prep_min, rq1_data$cook_min,
     main = "Figure 4: Prep Time vs Cook Time",
     xlab = "Prep time (min)", ylab = "Cook time (min)", pch = 20,
     col = rgb(0, 0, 1, 0.2))
abline(lm(cook_min ~ prep_min, data = rq1_data), col = "red", lwd = 2)
dev.off()

# --- Pearson correlation with 95% CI ---
cor_test_result <- cor.test(rq1_data$prep_min, rq1_data$cook_min,
                             method = "pearson")
cat("--- Pearson correlation ---\n")
print(cor_test_result)

# --- Simple linear regression ---
rq1_model <- lm(cook_min ~ prep_min, data = rq1_data)
cat("\n--- Simple linear regression: cook_min ~ prep_min ---\n")
print(summary(rq1_model))
cat("\n95% CI for regression coefficients:\n")
print(confint(rq1_model))

# --- Assumption check: normality of residuals ---
# Shapiro-Wilk caps at n = 5000, so we take a random subsample if needed.
resid1 <- residuals(rq1_model)
sw_sample1 <- if (length(resid1) > 5000) sample(resid1, 5000) else resid1
cat("\n--- Shapiro-Wilk test on regression residuals (subsample if n>5000) ---\n")
print(shapiro.test(sw_sample1))
cat("NOTE: if this is significant (non-normal residuals), this does NOT\n",
    "invalidate the regression at this sample size. By the Central Limit\n",
    "Theorem, the sampling distribution of the coefficient estimates is\n",
    "approximately normal regardless of residual shape when n is large.\n",
    "State this explicitly in your report rather than ignoring the result.\n")

png("outputs/fig5_rq1_residuals.png", width = 800, height = 600)
par(mfrow = c(1, 2))
hist(resid1, breaks = 40, main = "Residual Histogram", col = "grey70")
qqnorm(resid1); qqline(resid1, col = "red")
dev.off()
par(mfrow = c(1, 1))

# --- Effect size ---
cat("\nEffect size: R-squared =", round(summary(rq1_model)$r.squared, 3),
    "-> proportion of variance in cook_min explained by prep_min.\n")

# --- Rejected alternative technique (write into Methodology) ---
cat("\nRejected alternative: Spearman's rank correlation was considered\n",
    "but rejected in favour of Pearson because both variables are\n",
    "continuous ratio-scale measures with an approximately linear\n",
    "relationship in Figure 4, and Pearson provides a directly\n",
    "interpretable regression coefficient (minutes per minute) which\n",
    "Spearman does not.\n\n")


## ======================================================================
## SECTION 4: RQ2 — TWO-GROUP COMPARISON (T-TEST)
## RQ: Does average cook time differ significantly between vegetarian
## and non-vegetarian recipes?
## ======================================================================

cat("========================================\n")
cat("RQ2: Two-group comparison (t-test)\n")
cat("H0: mu_veg = mu_nonveg (no difference in mean cook time)\n")
cat("H1: mu_veg != mu_nonveg\n")
cat("alpha = 0.05; reject H0 if p < alpha\n")
cat("========================================\n\n")

rq2_data <- df[!is.na(df$diet_group) & !is.na(df$cook_min), ]
cat("Group sizes:\n")
print(table(rq2_data$diet_group))

# --- Assumption check: equal variances (Bartlett's test) ---
bart_result <- bartlett.test(cook_min ~ diet_group, data = rq2_data)
cat("\n--- Bartlett's test for equal variances ---\n")
print(bart_result)
cat("If p < 0.05 here, variances are unequal -> use Welch's t-test\n",
    "(var.equal = FALSE), which is what this script runs below by\n",
    "default regardless, since Welch's is the safer default when group\n",
    "sizes are unbalanced (as they are here: Vegetarian n is much\n",
    "larger than Non-Vegetarian n).\n\n")

# --- Welch's t-test (robust to unequal variances/sizes) ---
t_result <- t.test(cook_min ~ diet_group, data = rq2_data, var.equal = FALSE)
cat("--- Welch's t-test ---\n")
print(t_result)

# --- Effect size: Cohen's d ---
veg <- rq2_data$cook_min[rq2_data$diet_group == "Vegetarian"]
nonveg <- rq2_data$cook_min[rq2_data$diet_group == "Non-Vegetarian"]
pooled_sd <- sqrt(((length(veg) - 1) * var(veg) +
                    (length(nonveg) - 1) * var(nonveg)) /
                   (length(veg) + length(nonveg) - 2))
cohens_d <- (mean(veg) - mean(nonveg)) / pooled_sd
cat("\nCohen's d =", round(cohens_d, 3),
    "(0.2 = small, 0.5 = medium, 0.8 = large effect)\n")

# --- Boxplot ---
png("outputs/fig6_rq2_boxplot.png", width = 700, height = 600)
boxplot(cook_min ~ diet_group, data = rq2_data,
        main = "Figure 6: Cook Time by Diet Group",
        ylab = "Cook time (min)", col = c("tomato", "seagreen"))
dev.off()

cat("\nRejected alternative: Mann-Whitney U test was considered as a\n",
    "non-parametric alternative but rejected in favour of Welch's\n",
    "t-test because the large sample size in each group (report the Ns\n",
    "printed above) means the Central Limit Theorem supports valid use\n",
    "of the t-test on group means even if raw cook_min is skewed.\n\n")


## ======================================================================
## SECTION 5: RQ3 — MULTIPLE REGRESSION
## RQ: Do prep time, cook time, and diet group together predict recipe
## popularity (vote_count), and which predictor matters most?
## ======================================================================

cat("========================================\n")
cat("RQ3: Multiple regression\n")
cat("H0: All regression coefficients (beta_1=beta_2=beta_3) = 0\n")
cat("H1: At least one coefficient != 0\n")
cat("alpha = 0.05; reject H0 if p < alpha\n")
cat("========================================\n\n")

rq3_data <- df[!is.na(df$prep_min) & !is.na(df$cook_min) &
               !is.na(df$diet_group) & !is.na(df$vote_count), ]

# vote_count is heavily right-skewed (see Section 2 descriptive stats),
# so it is log-transformed before modelling. This is stated explicitly
# in the report as a pre-processing step for this RQ specifically.
rq3_data$log_votes <- log(rq3_data$vote_count + 1)
rq3_data$diet_bin <- ifelse(rq3_data$diet_group == "Non-Vegetarian", 1, 0)

cat("--- Distribution check: raw vote_count vs log-transformed ---\n")
png("outputs/fig7_rq3_vote_transform.png", width = 900, height = 500)
par(mfrow = c(1, 2))
hist(rq3_data$vote_count, breaks = 50, main = "Raw vote_count", col = "grey60")
hist(rq3_data$log_votes, breaks = 50, main = "log(vote_count + 1)", col = "grey60")
dev.off()
par(mfrow = c(1, 1))

# --- Full model (raw units, for interpretability of raw coefficients) ---
rq3_model <- lm(log_votes ~ prep_min + cook_min + diet_bin, data = rq3_data)
cat("\n--- Multiple regression: log_votes ~ prep_min + cook_min + diet_bin ---\n")
print(summary(rq3_model))
cat("\n95% CI for coefficients:\n")
print(confint(rq3_model))

# --- Assumption check: multicollinearity via manual VIF ---
# VIF_j = 1 / (1 - R^2_j), where R^2_j comes from regressing predictor j
# on all other predictors. Written manually since the 'car' package may
# not be installable in a locked-down environment.
vif_manual <- function(data, predictors) {
  vifs <- numeric(length(predictors))
  names(vifs) <- predictors
  for (p in predictors) {
    others <- setdiff(predictors, p)
    formula_str <- paste(p, "~", paste(others, collapse = " + "))
    r2 <- summary(lm(as.formula(formula_str), data = data))$r.squared
    vifs[p] <- 1 / (1 - r2)
  }
  vifs
}
vif_values <- vif_manual(rq3_data, c("prep_min", "cook_min", "diet_bin"))
cat("\n--- Manual VIF values (VIF > 5-10 indicates a multicollinearity concern) ---\n")
print(round(vif_values, 3))

# --- Assumption check: normality of residuals ---
resid3 <- residuals(rq3_model)
sw_sample3 <- if (length(resid3) > 5000) sample(resid3, 5000) else resid3
cat("\n--- Shapiro-Wilk test on residuals (subsample if n>5000) ---\n")
print(shapiro.test(sw_sample3))

png("outputs/fig8_rq3_residuals.png", width = 800, height = 600)
par(mfrow = c(1, 2))
plot(fitted(rq3_model), resid3, main = "Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals", pch = 20,
     col = rgb(0, 0, 0, 0.2))
abline(h = 0, col = "red")
qqnorm(resid3); qqline(resid3, col = "red")
dev.off()
par(mfrow = c(1, 1))

# --- Standardized (beta) coefficients ---
# Fit the SAME model on z-scored variables so coefficients are
# comparable across predictors on different scales/units. Use THESE,
# not the raw coefficients, to claim which predictor matters most.
rq3_std_data <- as.data.frame(scale(rq3_data[, c("log_votes", "prep_min",
                                                   "cook_min", "diet_bin")]))
rq3_std_model <- lm(log_votes ~ prep_min + cook_min + diet_bin,
                     data = rq3_std_data)
cat("\n--- Standardized (beta) coefficients ---\n")
print(summary(rq3_std_model)$coefficients)
cat("Compare the ABSOLUTE VALUES of these standardized estimates to say\n",
    "which predictor matters most — do NOT compare the raw coefficients\n",
    "from the unstandardized model above, since they are in different\n",
    "units (minutes vs a 0/1 indicator).\n\n")

# --- Train/test validation (80/20 split) to check for overfitting ---
n <- nrow(rq3_data)
train_idx <- sample(1:n, size = round(0.8 * n))
train_data <- rq3_data[train_idx, ]
test_data <- rq3_data[-train_idx, ]

train_model <- lm(log_votes ~ prep_min + cook_min + diet_bin, data = train_data)
train_r2 <- summary(train_model)$r.squared

test_pred <- predict(train_model, newdata = test_data)
test_r2 <- cor(test_pred, test_data$log_votes)^2

cat("--- Train/test validation ---\n")
cat("Training R-squared:", round(train_r2, 3), "\n")
cat("Held-out test R-squared:", round(test_r2, 3), "\n")
cat("A large drop from training to test R-squared would indicate\n",
    "overfitting; report both numbers honestly even if R-squared is low\n",
    "overall — a weak model is still a valid, honestly-reported finding.\n\n")

cat("Rejected alternative: a regression on raw vote_count (untransformed)\n",
    "was considered but rejected because vote_count is heavily right-\n",
    "skewed (see Figure 7), which would violate the linear regression\n",
    "assumption of homoscedastic, roughly normal residuals more severely\n",
    "than the log-transformed version.\n\n")


## ======================================================================
## SECTION 6: RQ4 — ANOVA (3+ GROUP COMPARISON)
## RQ: Does average prep time differ across the 5 most common cuisines
## (vs an 'Other' category)?
## ======================================================================

cat("========================================\n")
cat("RQ4: One-way ANOVA\n")
cat("H0: mu_Indian = mu_Continental = mu_NorthIndian = mu_SouthIndian",
    "= mu_Italian = mu_Other\n")
cat("H1: At least one group mean differs\n")
cat("alpha = 0.05; reject H0 if p < alpha\n")
cat("========================================\n\n")

rq4_data <- df[!is.na(df$prep_min) & !is.na(df$cuisine_group), ]
cat("Group sizes:\n")
print(table(rq4_data$cuisine_group))

# --- Assumption check: equal variances (Bartlett's test) ---
bart4 <- bartlett.test(prep_min ~ cuisine_group, data = rq4_data)
cat("\n--- Bartlett's test for equal variances across cuisine groups ---\n")
print(bart4)

# --- If variances are unequal, use Welch's ANOVA (oneway.test) instead
#     of the standard aov(); this script reports BOTH so you can see
#     the difference and justify your choice in the report.
cat("\n--- Standard one-way ANOVA ---\n")
aov_model <- aov(prep_min ~ cuisine_group, data = rq4_data)
print(summary(aov_model))

cat("\n--- Welch's ANOVA (robust to unequal variances) ---\n")
welch_result <- oneway.test(prep_min ~ cuisine_group, data = rq4_data,
                             var.equal = FALSE)
print(welch_result)

# --- Effect size: eta-squared ---
aov_summary <- summary(aov_model)[[1]]
ss_between <- aov_summary["cuisine_group", "Sum Sq"]
ss_total <- sum(aov_summary[, "Sum Sq"])
eta_sq <- ss_between / ss_total
cat("\nEffect size: eta-squared =", round(eta_sq, 4),
    "(0.01 = small, 0.06 = medium, 0.14 = large)\n")

# --- Post-hoc: Tukey HSD with 95% CIs (only meaningful if ANOVA is significant) ---
cat("\n--- Tukey HSD post-hoc pairwise comparisons (95% CI) ---\n")
tukey_result <- TukeyHSD(aov_model)
print(tukey_result)

# --- Assumption check: normality of residuals ---
resid4 <- residuals(aov_model)
sw_sample4 <- if (length(resid4) > 5000) sample(resid4, 5000) else resid4
cat("\n--- Shapiro-Wilk test on ANOVA residuals (subsample if n>5000) ---\n")
print(shapiro.test(sw_sample4))

png("outputs/fig9_rq4_boxplot.png", width = 900, height = 600)
boxplot(prep_min ~ cuisine_group, data = rq4_data, las = 2,
        main = "Figure 9: Prep Time by Cuisine Group",
        ylab = "Prep time (min)", col = "lightblue")
dev.off()

cat("\nRejected alternative: Kruskal-Wallis (non-parametric ANOVA) was\n",
    "considered but rejected in favour of the parametric/Welch's ANOVA\n",
    "because group sizes are large (report Ns above), so the Central\n",
    "Limit Theorem supports valid use of ANOVA on group means despite\n",
    "any residual non-normality.\n\n")


## ======================================================================
## SECTION 7: OUTLIER TRACING (do this for RQ1 and RQ3 regression models)
## ======================================================================

cat("========================================\n")
cat("Outlier tracing: largest residuals from RQ1 and RQ3 models\n")
cat("========================================\n\n")

# --- RQ1 model largest residuals ---
rq1_data$resid <- residuals(rq1_model)
top_resid_rq1 <- rq1_data[order(-abs(rq1_data$resid)), ]
cat("--- Top 10 largest-residual rows from RQ1 (prep_min ~ cook_min) ---\n")
print(top_resid_rq1[1:10, c("recipe_title", "prep_min", "cook_min", "resid")])

# --- RQ3 model largest residuals ---
rq3_data$resid <- residuals(rq3_model)
top_resid_rq3 <- rq3_data[order(-abs(rq3_data$resid)), ]
cat("\n--- Top 10 largest-residual rows from RQ3 (log_votes model) ---\n")
print(top_resid_rq3[1:10, c("recipe_title", "prep_min", "cook_min",
                              "vote_count", "resid")])

cat("\nACTION REQUIRED FROM YOU:\n",
    "Look up the recipe_title values printed above (search them on\n",
    "archanaskitchen.com if needed). For each one, decide:\n",
    "  (a) Is this a genuine outlier (e.g. a legitimately slow-cooked\n",
    "      dish, or a very popular/unpopular recipe)? -> KEEP, explain why.\n",
    "  (b) Is this a data-entry / scraping error (e.g. an implausible\n",
    "      combination of prep/cook time for that dish)? -> REMOVE, and\n",
    "      re-run the affected model (copy the relevant lm()/aov() call\n",
    "      from above on the filtered data), then report both the\n",
    "      before/after coefficients and R-squared/eta-squared honestly.\n\n")


## ======================================================================
## SECTION 8: SAVE CLEANED DATASET AND SESSION INFO
## ======================================================================

write.csv(df, "outputs/cleaned_food_recipes.csv", row.names = FALSE)
cat("Cleaned dataset saved to outputs/cleaned_food_recipes.csv\n")

cat("\n--- Session info (for your Appendix, if the brief allows one) ---\n")
print(sessionInfo())

cat("\n\n==== ALL SECTIONS COMPLETE ====\n")
cat("Check the outputs/ folder for:\n")
cat(" - analysis_log.txt (full console output)\n")
cat(" - 9 PNG figures\n")
cat(" - missingness_table.csv, descriptive_stats.csv\n")
cat(" - cleaned_food_recipes.csv\n")

sink()  # stop logging to file
