# ============================================================================
# Data Preparation for PLSC Analysis
# Hippocampus & Amygdala Role in Socioenvironmental Factors and Adolescent Depression
# ============================================================================
#
# Two-Stage Analysis:
#
# STAGE 1: PLSC - Socioenvironmental ↔ Depression
#   - Block 1 (X): Socioenvironmental factors
#   - Block 2 (Y): Depression symptoms
#
# STAGE 2: Brain's Role (4 separate ROI analyses)
#   - Left Hippocampus (2-back vs 0-back)
#   - Right Hippocampus (2-back vs 0-back)
#   - Left Amygdala (emotion vs neutral face)
#   - Right Amygdala (emotion vs neutral face)
#
# Output Files:
#   1. covariate.csv - Covariates for residualization
#   2. clean-socioenv.csv - Socioenvironmental factors (PLSC Block 1)
#   3. clean-depression.csv - Depression symptoms (PLSC Block 2)
#   4. clean-brain-hippo-amyg.csv - 4 Brain ROIs (Stage 2)
# ============================================================================

# Load required libraries
library(tidyverse)
library(data.table)
library(jsonlite)

# ============================================================================
# Define Paths - UPDATE THESE TO YOUR ABCD DATA LOCATION
# ============================================================================

ABCD_DATA_PATH <- "/path/to/abcd/data"  # UPDATE THIS PATH
OUTPUT_DIR <- "/Users/chloehampson/Desktop/hippo-amyg-depression/derivatives"

# Create output directory if it doesn't exist
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}
cat("Output directory:", OUTPUT_DIR, "\n")

# ============================================================================
# 1. Define All Variables (from ABCD Data Dictionary)
# ============================================================================

# Subject identifier
SUBJECT_ID <- "src_subject_id"
SESSION_ID <- "eventname"

# -----------------------------------------------------
# COVARIATE VARIABLES (for residualization)
# -----------------------------------------------------
COVARIATES <- list(
  age = "ab_p_demo_age",                        # Youth's age at data collection
  sex = "ab_g_stc__cohort_sex",                 # Participant's sex
  ethnicity = "ab_g_stc__cohort_ethnrace__meim", # Ethno-racial identity (15 levels)
  race = "ab_g_stc__cohort_race__nih",          # Race (NIH 7 levels)
  site = "ab_g_dyn__design_site",               # Assessment site
  family_id = "ab_g_stc__design_id__fam"        # Family ID (for nesting)
  # head_motion = "mr_y_qc__mot"                # TBD based on task
)

cat("\nCovariates:", length(COVARIATES), "\n")
for (k in names(COVARIATES)) {
  cat("  ", k, ":", COVARIATES[[k]], "\n")
}

# -----------------------------------------------------
# SOCIOENVIRONMENTAL VARIABLES (PLSC Block 1 / X)
# -----------------------------------------------------

# Family Environment (fc_p_fes)
FAMILY_ENV_VARS <- c(
  "fc_p_fes__cohes_mean",      # Family Environment Scale—Cohesion: Mean
  "fc_p_fes__confl_mean",      # Family Environment Scale—Conflict: Mean
  "fc_p_fes__expr_mean",       # Family Environment Scale—Expression: Mean
  "fc_p_fes__intelcult_mean",  # Family Environment Scale—Intellectual/cultural: Mean
  "fc_p_fes__org_mean",        # Family Environment Scale—Organization: Mean
  "fc_p_fes__rec_mean"         # Family Environment Scale—Activity/recreational: Mean
)

# Cultural & Social Environment
CULTURAL_SOCIAL_VARS <- c(
  "fc_y_meim_mean",                           # Multigroup Ethnic Identity Measure [Youth]: Mean
  "fc_p_meim_mean",                           # Multigroup Ethnic Identity Measure [Parent]: Mean
  "le_l_nbhsoc__addr1__factor3_score",        # Neighborhood ethnic/immigrant concentration
  "fc_y_srpf__env_mean"                       # School Risk & Protective Factors—School environment: Mean
)

# Socioeconomic Status & Demographics
SES_VARS <- c(
  "le_l_nbhsoc__addr1__factor1_score",        # Neighborhood disadvantage score
  "le_l_nbhsoc__addr1__aff_score",            # Neighborhood affluence score
  "le_l_coi__addr1__coi__total__metro_score", # Child Opportunity Index (metro-normed, 1-100)
  "ab_p_demo__income__hhold_001",             # Household income category
  "ab_g_dyn__cohort_edu__cgs"                 # Highest education across caregivers
)

# Combine all socioenvironmental
SOCIOENV_VARS <- c(FAMILY_ENV_VARS, CULTURAL_SOCIAL_VARS, SES_VARS)

cat("\nSOCIOENVIRONMENTAL VARIABLES:", length(SOCIOENV_VARS), "\n")
cat("\n  Family Environment (", length(FAMILY_ENV_VARS), "):\n", sep = "")
for (v in FAMILY_ENV_VARS) cat("    -", v, "\n")
cat("\n  Cultural/Social (", length(CULTURAL_SOCIAL_VARS), "):\n", sep = "")
for (v in CULTURAL_SOCIAL_VARS) cat("    -", v, "\n")
cat("\n  SES (", length(SES_VARS), "):\n", sep = "")
for (v in SES_VARS) cat("    -", v, "\n")

# -----------------------------------------------------
# DEPRESSION VARIABLES (PLSC Block 2 / Y)
# -----------------------------------------------------

DEPRESSION_VARS <- c(
  # Summary scores
  "mh_p_cbcl__dsm__dep_sum",      # CBCL [Parent] DSM-5 Depressive problems: Sum
  "mh_y_ysr__dsm__dep_sum",       # YSR [Youth] DSM-5 Depressive problems: Sum
  
  # Core symptoms - Parent report (KSADS)
  "mh_p_ksads__dep__mood__pres_sx",   # Depressed mood - Present [Parent]
  "mh_p_ksads__dep__anhed__pres_sx",  # Anhedonia - Present [Parent]
  "mh_p_ksads__dep__fatig__pres_sx",  # Fatigue - Present [Parent]
  
  # Core symptoms - Youth report (KSADS)
  "mh_y_ksads__dep__mood__pres_sx",   # Depressed mood - Present [Youth]
  "mh_y_ksads__dep__anhed__pres_sx",  # Anhedonia - Present [Youth]
  "mh_y_ksads__dep__fatig__pres_sx"   # Fatigue - Present [Youth]
)

cat("\nDEPRESSION VARIABLES:", length(DEPRESSION_VARS), "\n")
for (v in DEPRESSION_VARS) cat("  -", v, "\n")

# -----------------------------------------------------
# BRAIN VARIABLES - 4 ROIs (Stage 2 - Analyzed Separately)
# -----------------------------------------------------
# Emotional N-back task beta weights

# Hippocampus: 2-back vs 0-back contrast (working memory load)
HIPPOCAMPUS_VARS <- list(
  hippo_lh = "mr_y_tfmri__nback__2bv0b__aseg__hc__lh_beta",  # Left hippocampus
  hippo_rh = "mr_y_tfmri__nback__2bv0b__aseg__hc__rh_beta"   # Right hippocampus
)

# Amygdala: Emotion vs Neutral face contrast (emotional processing)
AMYGDALA_VARS <- list(
  amyg_lh = "mr_y_tfmri__nback__emovntf__aseg__ag__lh_beta",  # Left amygdala
  amyg_rh = "mr_y_tfmri__nback__emovntf__aseg__ag__rh_beta"   # Right amygdala
)

# Combined list for all brain ROIs
BRAIN_ROIS <- c(HIPPOCAMPUS_VARS, AMYGDALA_VARS)

# Vector of variable names for data loading
BRAIN_VARS <- unlist(BRAIN_ROIS)

# Labels for plotting/output
BRAIN_ROI_LABELS <- c(
  "mr_y_tfmri__nback__2bv0b__aseg__hc__lh_beta" = "Left Hippocampus (2b>0b)",
  "mr_y_tfmri__nback__2bv0b__aseg__hc__rh_beta" = "Right Hippocampus (2b>0b)",
  "mr_y_tfmri__nback__emovntf__aseg__ag__lh_beta" = "Left Amygdala (emo>neut)",
  "mr_y_tfmri__nback__emovntf__aseg__ag__rh_beta" = "Right Amygdala (emo>neut)"
)

cat("\nBRAIN ROIs:", length(BRAIN_VARS), "(analyzed separately in Stage 2)\n")
cat("\n  Hippocampus (2-back vs 0-back - working memory):\n")
for (k in names(HIPPOCAMPUS_VARS)) {
  cat("    -", k, ":", HIPPOCAMPUS_VARS[[k]], "\n")
}
cat("\n  Amygdala (emotion vs neutral face - emotional processing):\n")
for (k in names(AMYGDALA_VARS)) {
  cat("    -", k, ":", AMYGDALA_VARS[[k]], "\n")
}

# ============================================================================
# 2. Load ABCD Data Tables
# ============================================================================

load_abcd_table <- function(table_name, data_path = ABCD_DATA_PATH) {

  # Try different file extensions
  for (ext in c(".csv", ".txt", ".tsv")) {
    filepath <- file.path(data_path, paste0(table_name, ext))
    if (file.exists(filepath)) {
      sep <- ifelse(ext == ".tsv", "\t", ",")
      df <- fread(filepath, sep = sep, data.table = FALSE)
      return(df)
    }
  }
  warning(paste("Could not find table", table_name))
  return(NULL)
}

cat("\nTable loading function defined.\n")

# Tables to load - UPDATE table names based on your ABCD data release
tables_to_load <- list(
  # Demographics
  ab_p_demo = "ab_p_demo",       # Parent demographics (age, income)
  ab_g_stc = "ab_g_stc",         # Static cohort info (sex, race, family ID)
  ab_g_dyn = "ab_g_dyn",         # Dynamic design info (site, education)
  
  # Family Environment Scale
  fc_p_fes = "fc_p_fes",         # Family Environment Scale [Parent]
  
  # Cultural & Social
  fc_y_meim = "fc_y_meim",       # MEIM [Youth]
  fc_p_meim = "fc_p_meim",       # MEIM [Parent]
  le_l_nbhsoc = "le_l_nbhsoc",   # Neighborhood social
  fc_y_srpf = "fc_y_srpf",       # School Risk & Protective Factors
  
  # SES
  le_l_coi = "le_l_coi",         # Child Opportunity Index
  
  # Depression
  mh_p_cbcl = "mh_p_cbcl",       # CBCL [Parent]
  mh_y_ysr = "mh_y_ysr",         # YSR [Youth]
  mh_p_ksads__dep = "mh_p_ksads__dep",  # KSADS Depression [Parent]
  mh_y_ksads__dep = "mh_y_ksads__dep",  # KSADS Depression [Youth]
  
  # MRI
  mr_y_qc = "mr_y_qc",           # MRI Quality Control
  mr_y_tfmri__nback__2bv0b__aseg = "mr_y_tfmri__nback__2bv0b__aseg",   # N-back 2b>0b (hippocampus)
  mr_y_tfmri__nback__emovntf__aseg = "mr_y_tfmri__nback__emovntf__aseg" # N-back emo>neut (amygdala)
)

# Load all tables
loaded_tables <- list()
for (name in names(tables_to_load)) {
  loaded_tables[[name]] <- load_abcd_table(tables_to_load[[name]])
}

# Check what loaded
cat("\nTable loading status:\n")
for (name in names(loaded_tables)) {
  if (!is.null(loaded_tables[[name]])) {
    cat("  ✓", name, ":", nrow(loaded_tables[[name]]), "rows,", 
        ncol(loaded_tables[[name]]), "columns\n")
  } else {
    cat("  ✗", name, ": NOT FOUND\n")
  }
}

# ============================================================================
# 3. Merge Data Tables
# ============================================================================

merge_abcd_tables <- function(tables_list, on_cols = c("src_subject_id", "eventname"), 
                               how = "inner") {
  # Filter to non-NULL tables
  valid_tables <- Filter(Negate(is.null), tables_list)
  
  if (length(valid_tables) == 0) {
    stop("No valid tables to merge")
  }
  
  merged_df <- NULL
  
  for (name in names(valid_tables)) {
    df <- valid_tables[[name]]
    
    available_cols <- intersect(on_cols, colnames(df))
    if (length(available_cols) == 0) {
      cat("Skipping", name, "- No merge columns found\n")
      next
    }
    
    if (is.null(merged_df)) {
      merged_df <- df
      cat("Initialized with", name, ":", nrow(merged_df), "rows\n")
    } else {
      merged_df <- merge(merged_df, df, by = available_cols, all = (how == "outer"))
      cat("After merging", name, ":", nrow(merged_df), "rows\n")
    }
  }
  
  return(merged_df)
}

# Filter to tables that loaded successfully
valid_tables <- Filter(Negate(is.null), loaded_tables)
cat("\nTables available for merging:", length(valid_tables), "\n")

# Uncomment when data is loaded:
# merged_df <- merge_abcd_tables(valid_tables)
# cat("\nFinal merged dataset:", nrow(merged_df), "rows,", ncol(merged_df), "columns\n")

cat("NOTE: Uncomment the merge function once data is loaded\n")

# ============================================================================
# 4. Select Timepoint & Apply QC Filters
# ============================================================================

# Select timepoint - many variables available at ses-02A (2-year follow-up)
TIMEPOINT <- "ses-02A"  # UPDATE AS NEEDED

# Alternative timepoints:
# "ses-00A" = Baseline
# "ses-02A" = 2-year follow-up
# "ses-04A" = 4-year follow-up

cat("\nSelected timepoint:", TIMEPOINT, "\n")

apply_qc_filters <- function(df, motion_threshold = 0.5, motion_var = NULL, qc_var = NULL) {
  n_initial <- nrow(df)
  
  if (!is.null(qc_var) && qc_var %in% colnames(df)) {
    df <- df[df[[qc_var]] == 1, ]
    cat("After QC inclusion:", nrow(df), "(", n_initial - nrow(df), "removed)\n")
  }
  
  if (!is.null(motion_var) && motion_var %in% colnames(df)) {
    n_before <- nrow(df)
    df <- df[df[[motion_var]] <= motion_threshold, ]
    cat("After motion filter (≤", motion_threshold, "):", nrow(df), 
        "(", n_before - nrow(df), "removed)\n")
  }
  
  return(df)
}

# Uncomment when data is loaded:
# # Filter to timepoint
# analysis_df <- merged_df[merged_df$eventname == TIMEPOINT, ]
# cat("Subjects at", TIMEPOINT, ":", nrow(analysis_df), "\n")
# 
# # Apply QC filters
# analysis_df <- apply_qc_filters(analysis_df)

cat("QC filter function defined.\n")

# ============================================================================
# 5. Create PLSC Dataframes
# ============================================================================

create_plsc_dataframes <- function(df, socioenv_vars, depression_vars, brain_vars,
                                    covariate_vars, subject_id = "src_subject_id") {
  # Create FOUR separate dataframes for the two-stage PLSC analysis.
  #
  # Stage 1 PLSC:
  #   1. Socioenvironmental (Block 1 / X)
  #   2. Depression (Block 2 / Y)
  #
  # Stage 2 (Brain - analyzed separately):
  #   3. Brain ROIs (4 variables)
  #
  # Plus:
  #   4. Covariates (for residualization)
  
  # Filter to existing variables
  existing_socioenv <- intersect(socioenv_vars, colnames(df))
  existing_depression <- intersect(depression_vars, colnames(df))
  existing_brain <- intersect(brain_vars, colnames(df))
  existing_covars <- covariate_vars[unlist(covariate_vars) %in% colnames(df)]
  
  cat("Variables found:\n")
  cat("  Socioenvironmental:", length(existing_socioenv), "/", length(socioenv_vars), "\n")
  cat("  Depression:", length(existing_depression), "/", length(depression_vars), "\n")
  cat("  Brain ROIs:", length(existing_brain), "/", length(brain_vars), "\n")
  cat("  Covariates:", length(existing_covars), "/", length(covariate_vars), "\n")
  
  # Report missing variables
  missing_socioenv <- setdiff(socioenv_vars, colnames(df))
  missing_depression <- setdiff(depression_vars, colnames(df))
  missing_brain <- setdiff(brain_vars, colnames(df))
  
  if (length(missing_socioenv) > 0) {
    cat("\n  Missing socioenvironmental:", paste(missing_socioenv, collapse = ", "), "\n")
  }
  if (length(missing_depression) > 0) {
    cat("  Missing depression:", paste(missing_depression, collapse = ", "), "\n")
  }
  if (length(missing_brain) > 0) {
    cat("  Missing brain:", paste(missing_brain, collapse = ", "), "\n")
  }
  
  # Create covariate dataframe
  covar_cols <- c(subject_id, unlist(existing_covars))
  covar_df <- df[, covar_cols, drop = FALSE]
  rownames(covar_df) <- covar_df[[subject_id]]
  
  # Create socioenvironmental dataframe (PLSC Block 1 / X)
  socioenv_df <- df[, c(subject_id, existing_socioenv), drop = FALSE]
  rownames(socioenv_df) <- socioenv_df[[subject_id]]
  
  # Create depression dataframe (PLSC Block 2 / Y)
  depression_df <- df[, c(subject_id, existing_depression), drop = FALSE]
  rownames(depression_df) <- depression_df[[subject_id]]
  
  # Create brain dataframe (Stage 2 - 4 ROIs)
  brain_df <- df[, c(subject_id, existing_brain), drop = FALSE]
  rownames(brain_df) <- brain_df[[subject_id]]
  
  return(list(
    covariates = covar_df,
    socioenv = socioenv_df,
    depression = depression_df,
    brain = brain_df,
    variable_lists = list(
      socioenv = existing_socioenv,
      depression = existing_depression,
      brain = existing_brain,
      covariates = existing_covars,
      brain_labels = BRAIN_ROI_LABELS
    )
  ))
}

cat("PLSC dataframe creation function defined.\n")

# Uncomment when analysis_df is available:
# plsc_data <- create_plsc_dataframes(
#   analysis_df,
#   socioenv_vars = SOCIOENV_VARS,
#   depression_vars = DEPRESSION_VARS,
#   brain_vars = BRAIN_VARS,
#   covariate_vars = COVARIATES
# )
#
# cat("\nDataframe shapes:\n")
# cat("  Covariates:", dim(plsc_data$covariates), "\n")
# cat("  Socioenvironmental (PLSC Block 1):", dim(plsc_data$socioenv), "\n")
# cat("  Depression (PLSC Block 2):", dim(plsc_data$depression), "\n")
# cat("  Brain - 4 ROIs (Stage 2):", dim(plsc_data$brain), "\n")

# ============================================================================
# 6. Handle Missing Data
# ============================================================================

assess_and_handle_missing <- function(plsc_data, threshold = 0.2) {
  cat("Missing data assessment:\n")
  
  for (name in c("covariates", "socioenv", "depression", "brain")) {
    df <- plsc_data[[name]]
    cat("\n", toupper(name), ":\n", sep = "")
    
    for (col in colnames(df)) {
      if (col == "src_subject_id") next
      pct_missing <- mean(is.na(df[[col]])) * 100
      if (pct_missing > 0) {
        flag <- ifelse(pct_missing > threshold * 100, " ⚠️ HIGH", "")
        cat("  ", col, ": ", round(pct_missing, 1), "% missing", flag, "\n", sep = "")
      }
    }
  }
  
  # Get common subjects across all dataframes (complete cases)
  common_subjects <- rownames(plsc_data$covariates)
  
  for (name in c("covariates", "socioenv", "depression", "brain")) {
    df <- plsc_data[[name]]
    complete_rows <- complete.cases(df[, -which(colnames(df) == "src_subject_id")])
    complete_subjects <- rownames(df)[complete_rows]
    common_subjects <- intersect(common_subjects, complete_subjects)
  }
  
  cat("\nSubjects with complete data:", length(common_subjects), "\n")
  
  # Filter all dataframes to common subjects
  for (name in c("covariates", "socioenv", "depression", "brain")) {
    plsc_data[[name]] <- plsc_data[[name]][common_subjects, ]
  }
  
  return(plsc_data)
}

# Uncomment when plsc_data is available:
# plsc_data <- assess_and_handle_missing(plsc_data)

cat("Missing data function defined.\n")

# ============================================================================
# 7. Save Dataframes for R Analysis
# ============================================================================

save_plsc_dataframes <- function(plsc_data, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save covariates
  write.csv(plsc_data$covariates, file.path(output_dir, "covariate.csv"), row.names = TRUE)
  cat("Saved: covariate.csv", dim(plsc_data$covariates), "\n")
  
  # Save socioenvironmental (PLSC Block 1)
  write.csv(plsc_data$socioenv, file.path(output_dir, "clean-socioenv.csv"), row.names = TRUE)
  cat("Saved: clean-socioenv.csv", dim(plsc_data$socioenv), "\n")
  
  # Save depression (PLSC Block 2)
  write.csv(plsc_data$depression, file.path(output_dir, "clean-depression.csv"), row.names = TRUE)
  cat("Saved: clean-depression.csv", dim(plsc_data$depression), "\n")
  
  # Save brain data (Stage 2 - 4 ROIs)
  write.csv(plsc_data$brain, file.path(output_dir, "clean-brain-hippo-amyg.csv"), row.names = TRUE)
  cat("Saved: clean-brain-hippo-amyg.csv", dim(plsc_data$brain), "\n")
  
  # Save variable lists as JSON
  write_json(plsc_data$variable_lists, file.path(output_dir, "variable_lists.json"), 
             pretty = TRUE, auto_unbox = TRUE)
  cat("Saved: variable_lists.json\n")
  
  cat("\nAll files saved to:", output_dir, "\n")
}

# Uncomment when plsc_data is available:
# save_plsc_dataframes(plsc_data, OUTPUT_DIR)

cat("Save function defined.\n")

# ============================================================================
# Summary
# ============================================================================
#
# Files to be Created:
# | File                       | Contents                   | Purpose              |
# |----------------------------|----------------------------|----------------------|
# | covariate.csv              | Age, sex, race, site, etc. | Residualization      |
# | clean-socioenv.csv         | 15 socioenvironmental vars | PLSC Block 1 (X)     |
# | clean-depression.csv       | 8 depression vars          | PLSC Block 2 (Y)     |
# | clean-brain-hippo-amyg.csv | 4 brain ROIs               | Stage 2 (separate)   |
#
# Analysis Flow:
#
# STAGE 1: PLSC
#    Socioenvironmental (15 vars) ↔ Depression (8 vars)
#    → Identifies latent dimensions
#    → Extract LV scores
#
# STAGE 2: Brain (4 separate analyses)
#    LV scores ↔ Left Hippocampus (2b>0b)
#    LV scores ↔ Right Hippocampus (2b>0b)
#    LV scores ↔ Left Amygdala (emo>neut)
#    LV scores ↔ Right Amygdala (emo>neut)
#
# ============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("DATA PREPARATION SCRIPT READY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("\n1. Update ABCD_DATA_PATH at the top of this script\n")
cat("2. Run script to load and merge data\n")
cat("3. Output will be saved to:", OUTPUT_DIR, "\n")
cat("4. Then run hippo_amyg_depression_plsc.Rmd in R\n")
