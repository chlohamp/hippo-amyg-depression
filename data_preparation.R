# ============================================================================
# Data Preparation for PLSC Analysis
# Hippocampus & Amygdala Role in Socioenvironmental Factors and Adolescent Depression
# ============================================================================
#
# This script:
# 1. Loads raw CSVs from dset/ folder
# 2. Removes all rows with missing data (NAs)
# 3. Saves cleaned CSVs to derivatives/ folder with "clean-" prefix
# 4. Reports subject counts per timepoint before and after cleaning
#
# Output Files (in derivatives/):
#   - clean-demographics.csv
#   - clean-socioenvironment.csv
#   - clean-depression.csv
# ============================================================================

# Load required libraries
library(tidyverse)

# ============================================================================
# Define Paths
# ============================================================================

DATA_PATH <- "/Users/chloehampson/Desktop/hippo-amyg-depression/dset"
OUTPUT_DIR <- "/Users/chloehampson/Desktop/hippo-amyg-depression/derivatives"

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("DATA CLEANING SCRIPT\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Input directory:", DATA_PATH, "\n")
cat("Output directory:", OUTPUT_DIR, "\n\n")

# ============================================================================
# Load Data Files
# ============================================================================

cat("=== LOADING DATA FILES ===\n")

# Load demographics
demographics <- read_csv(file.path(DATA_PATH, "demographics.csv"), show_col_types = FALSE)
cat("✓ Loaded demographics:", nrow(demographics), "rows,", ncol(demographics), "columns\n")

# Load socioenvironment
socioenvironment <- read_csv(file.path(DATA_PATH, "socioenvironment.csv"), show_col_types = FALSE)
cat("✓ Loaded socioenvironment:", nrow(socioenvironment), "rows,", ncol(socioenvironment), "columns\n")

# Load depression
depression <- read_csv(file.path(DATA_PATH, "depression.csv"), show_col_types = FALSE)
cat("✓ Loaded depression:", nrow(depression), "rows,", ncol(depression), "columns\n")

# ============================================================================
# Count Subjects per Timepoint (BEFORE cleaning)
# ============================================================================

cat("\n=== SUBJECTS PER TIMEPOINT (BEFORE CLEANING) ===\n")
timepoint_counts_before <- demographics %>%
  group_by(session_id) %>%
  summarise(n_subjects = n_distinct(participant_id), .groups = 'drop') %>%
  arrange(session_id)
print(timepoint_counts_before)

# ============================================================================
# Remove Missing Data (NAs)
# ============================================================================

cat("\n=== CLEANING DATA: REMOVING NAs ===\n")

# Function to remove rows with any NA and report statistics
remove_na_rows <- function(df, df_name) {
  n_before <- nrow(df)
  
  # Count NAs per column
  na_counts <- colSums(is.na(df))
  cols_with_na <- names(na_counts[na_counts > 0])
  
  if (length(cols_with_na) > 0) {
    cat("\n", df_name, " - Columns with NAs:\n", sep = "")
    for (col in cols_with_na) {
      pct_na <- (na_counts[col] / n_before) * 100
      cat("  ", col, ": ", na_counts[col], " (", round(pct_na, 1), "%)\n", sep = "")
    }
  } else {
    cat("\n", df_name, " - No missing data found!\n", sep = "")
  }
  
  # Remove rows with any NA
  df_clean <- df[complete.cases(df), ]
  n_after <- nrow(df_clean)
  n_removed <- n_before - n_after
  
  cat("\n  Summary:\n")
  cat("    Rows before: ", n_before, "\n", sep = "")
  cat("    Rows after:  ", n_after, "\n", sep = "")
  cat("    Removed:     ", n_removed, " (", round((n_removed/n_before)*100, 1), "%)\n", sep = "")
  
  return(df_clean)
}

# Clean each dataframe
demographics_clean <- remove_na_rows(demographics, "Demographics")
socioenvironment_clean <- remove_na_rows(socioenvironment, "Socioenvironment")
depression_clean <- remove_na_rows(depression, "Depression")

# ============================================================================
# Count Subjects per Timepoint (AFTER cleaning)
# ============================================================================

cat("\n=== SUBJECTS PER TIMEPOINT (AFTER CLEANING) ===\n")

# Count for each cleaned dataset
cat("\nDemographics (cleaned):\n")
demo_counts_after <- demographics_clean %>%
  group_by(session_id) %>%
  summarise(n_subjects = n_distinct(participant_id), .groups = 'drop') %>%
  arrange(session_id)
print(demo_counts_after)

cat("\nSocioenvironment (cleaned):\n")
socio_counts_after <- socioenvironment_clean %>%
  group_by(session_id) %>%
  summarise(n_subjects = n_distinct(participant_id), .groups = 'drop') %>%
  arrange(session_id)
print(socio_counts_after)

cat("\nDepression (cleaned):\n")
dep_counts_after <- depression_clean %>%
  group_by(session_id) %>%
  summarise(n_subjects = n_distinct(participant_id), .groups = 'drop') %>%
  arrange(session_id)
print(dep_counts_after)

# ============================================================================
# Save Cleaned CSVs
# ============================================================================

cat("\n=== SAVING CLEANED CSVs ===\n")

# Save clean-demographics.csv
write_csv(demographics_clean, file.path(OUTPUT_DIR, "clean-demographics.csv"))
cat("✓ Saved: clean-demographics.csv (", nrow(demographics_clean), " rows)\n", sep = "")

# Save clean-socioenvironment.csv
write_csv(socioenvironment_clean, file.path(OUTPUT_DIR, "clean-socioenvironment.csv"))
cat("✓ Saved: clean-socioenvironment.csv (", nrow(socioenvironment_clean), " rows)\n", sep = "")

# Save clean-depression.csv
write_csv(depression_clean, file.path(OUTPUT_DIR, "clean-depression.csv"))
cat("✓ Saved: clean-depression.csv (", nrow(depression_clean), " rows)\n", sep = "")

# ============================================================================
# Summary Comparison
# ============================================================================

cat("\n=== DATA LOSS SUMMARY BY TIMEPOINT ===\n")
cat("(Comparing Demographics: Before vs After)\n\n")

comparison <- timepoint_counts_before %>%
  left_join(demo_counts_after, by = "session_id", suffix = c("_before", "_after")) %>%
  mutate(
    n_subjects_after = ifelse(is.na(n_subjects_after), 0, n_subjects_after),
    n_lost = n_subjects_before - n_subjects_after,
    pct_retained = round((n_subjects_after / n_subjects_before) * 100, 1)
  )

print(comparison)

# ============================================================================
# Final Summary
# ============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("CLEANING COMPLETE!\n")
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("\nCleaned files saved to:\n  ", OUTPUT_DIR, "\n\n", sep = "")
cat("Files created:\n")
cat("  - clean-demographics.csv\n")
cat("  - clean-socioenvironment.csv\n")
cat("  - clean-depression.csv\n\n")
cat("Total subjects retained (Demographics):\n")
cat("  Before: ", sum(timepoint_counts_before$n_subjects), "\n", sep = "")
cat("  After:  ", sum(demo_counts_after$n_subjects), "\n", sep = "")
cat("  Loss:   ", sum(timepoint_counts_before$n_subjects) - sum(demo_counts_after$n_subjects), 
    " (", round((1 - sum(demo_counts_after$n_subjects)/sum(timepoint_counts_before$n_subjects))*100, 1), "%)\n\n", sep = "")
