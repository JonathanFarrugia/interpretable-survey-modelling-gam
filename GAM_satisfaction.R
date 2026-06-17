# GAM-Based Key Driver Analysis using mgcv

# =================
# 0. Load libraries
# =================

library(mgcv)       # GAM modeling
library(dplyr)      # Data manipulation
library(tidyr)      # Data tidying
library(ggplot2)    # Diagnostic plots
library(readxl)     # Excel import
library(forcats)    # Factor manipulation
library(rmarkdown)  # Report rendering
library(car)        # GVIF for multi-level categorical variables
library(gam.hp)     # Hierarchical partitioning of R² for GAMs
library(pagedown)   # HTML to PDF conversion
library(gratia)     # GAM plotting
library(here)      # File path management

# ================
# 1. Configuration
# ================

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

data_path <- here::here("data", "synthetic_training_data.xlsx")

target <- "satisfaction_level"

categorical_features <- c(
  "session_delivery",
  "session_framework",
  "session_organiser",
  "session_language",
  "modules_topic",
  "level",
  "year"
)

numerical_features <- c(
  "enrolments",
  "trainers",
  "duration_days"
)

k_map <- c(
  enrolments = 10,
  trainers   = 4,
  duration_days  = 8
)

# Construct list of num x num interaction pairs if required
numeric_interactions <- list()

# Construct list of cat × cat interaction pairs if required
cat_cat_interactions <- list()

factor_smooth <- FALSE  # Set TRUE to use factor smooths for num x cat interactions

# Construct list of num x cat interaction pairs if required
interaction_pairs <- list()

var_labels <- c(
  enrolments = "Enrolments",
  trainers   = "Trainers",
  duration_days  = "Duration (Days)"
)

plot_dir <- here::here("plots")
dir.create(plot_dir, showWarnings = FALSE)

# ============
# 2. Load Data
# ============
df <- readxl::read_excel(data_path)

pooled_var <- sum((df$respondents - 1) * df$satisfaction_variance) / sum(df$respondents - 1)

df <- df %>% dplyr::mutate(
  n_resp = df$respondents,
  inv_sd = 1 / sqrt(df$satisfaction_variance),  # inverse of standard deviation
  inv_sd = inv_sd / mean(inv_sd)  # rescale to average weight of 1
)

# ===================================
# 3. Preprocess Categorical Variables
# ===================================
gvif_tbl <- NULL # GVIF computation only when collapsing is applied to avoid aliased coefficients errors
collapse_applied <- TRUE # set TRUE to compute GVIFs after collapsing

# Apply collapsing and releveling dynamically
for (f in categorical_features) {  

  # Custom merges for specific variables
  if (f == "session_organiser") {
    df[[f]] <- forcats::fct_collapse(df[[f]],
                "Institution B or C" = c("Institution B", "Institution C"))
  }

  # Ensure factor type
  df[[f]] <- factor(df[[f]])

  # Set most frequent level as reference
  most_freq_level <- names(which.max(table(df[[f]])))
  df[[f]] <- forcats::fct_relevel(df[[f]], most_freq_level)
}

# Get reference level for each
ref_levels <- sapply(categorical_features, function(f) {
  levels(df[[f]])[1]  # first level is reference after relevel()
})

# Convert to data frame for reporting
ref_levels_df <- data.frame(
  Feature = names(ref_levels),
  Reference = as.character(ref_levels),
  row.names = NULL
)

# =============================
# 4. GAM Specification Function
# =============================
fit_gam <- function(data, target, num_features, cat_features, weight_var = NULL,
                    scale_value = NULL, k_map) {

  # Construct GAM formula
  smooth_terms <- sapply(num_features, function(num_var) {
    paste0("s(", num_var, ", k=", k_map[[num_var]], ", bs='cs')")
  })

  # Add numeric × numeric interactions using ti()
  interaction_terms_numeric <- NULL
  if (!is.null(numeric_interactions)) {
    interaction_terms_numeric <- sapply(numeric_interactions, function(pair) {
      num1 <- pair[1]
      num2 <- pair[2]
      paste0("ti(", num1, ", ", num2, ", k=c(", k_map[[num1]], ",", k_map[[num2]], "), bs='cs')")
    })
  }
  
  # Add numeric × categorical interactions
  numcat_smooth_terms <- NULL
  
  if (!is.null(interaction_pairs) && factor_smooth) {
    # Factor smooths (bs='fs')
    numcat_smooth_terms <- sapply(interaction_pairs, function(pair) {
      num_var <- pair[1]
      fac_var <- pair[2]
      paste0("s(", num_var, ", ", fac_var,
             ", bs='fs', k=", min(10, k_map[[num_var]]), ")")
    })
    
  } else if (!is.null(interaction_pairs)) {
    # By-variable smooths (bs='cs')
    numcat_smooth_terms <- sapply(interaction_pairs, function(pair) {
      num_var <- pair[1]
      fac_var <- pair[2]
      paste0("s(", num_var, ", by=", fac_var,
             ", k=", k_map[[num_var]], ", bs='cs')")
    })
  }

  # Add cat × cat interactions as random effects
  re_interaction_terms <- NULL
  if (!is.null(cat_cat_interactions)) {
    re_interaction_terms <- sapply(cat_cat_interactions, function(pair) {
      paste0("s(", pair[1], ", ", pair[2], ", bs='re')")
    })
  }

  all_smooths <- c(smooth_terms, numcat_smooth_terms, interaction_terms_numeric, re_interaction_terms)
  smooth_terms_str <- paste(all_smooths, collapse = " + ")

  factor_terms <- paste(cat_features, collapse = " + ")

  formula_str <- paste(target, "~", smooth_terms_str, "+", factor_terms)

  cat("GAM formula:\n", formula_str, "\n")
  gam_formula <- as.formula(formula_str)

  # Fit the model
  if (is.null(weight_var)) {

    gam_model <- mgcv::gam(
      gam_formula,
      data = data,
      family = gaussian(),
      method = "REML",
      select = TRUE  # enable automatic smoothness selection
    )

  } else {

    if (is.null(scale_value)) {

      # Case 1: weighted model, scale estimated
      gam_model <- mgcv::gam(
        gam_formula,
        data = data,
        weights = data[[weight_var]],
        family = gaussian(),
        method = "REML",
        select = TRUE
      )
    
    } else {

      # Case 2: weighted model with fixed pooled variance
      gam_model <- mgcv::gam(
        gam_formula,
        data = data,
        weights = data[[weight_var]],
        family = gaussian(),
        method = "REML",
        select = TRUE,
        scale = scale_value
      )
    }
  }  

  attr(gam_model, "formula_str") <- formula_str

  return(gam_model)  # nolint
}

cv_gam <- function(data, target, K = 5, seed = 42, weight_var = NULL,  # nolint: object_name_linter
                   scale_value = NULL) { 

  set.seed(seed)

  fold_id <- sample(rep(1:K, length.out = nrow(data)))

  results <- lapply(seq_len(K), function(k) {
    train <- data[fold_id != k, ]
    test  <- data[fold_id == k, ]

    model <- fit_gam(train, target, numerical_features, categorical_features,
                     weight_var = weight_var, scale_value = scale_value, k_map = k_map)

    preds <- predict(model, newdata = test, type = "response")
    obs   <- test[[target]]

    data.frame(
      fold = k,
      rmse = sqrt(mean((obs - preds)^2)),
      mae  = mean(abs(obs - preds))
    )
  })

  do.call(rbind, results)
}

compute_gvif <- function(data, target, parametric_features) {

  # Build a purely parametric linear model
  formula_str <- paste(target, "~", paste(parametric_features, collapse = " + "))
  param_formula <- as.formula(formula_str)

  param_lm <- lm(param_formula, data = data)

  # Hard stop if aliased coefficients exist
  if (any(is.na(coef(param_lm)))) {
    stop("Aliased coefficients detected in parametric model; GVIF undefined.")
  }

  gvif_raw <- car::vif(param_lm)

  # Always return adjusted GVIF
  gvif_tbl <- data.frame(
    Term = rownames(gvif_raw),
    GVIF = gvif_raw[, "GVIF"],
    Df   = gvif_raw[, "Df"],
    GVIF_adj = gvif_raw[, "GVIF"]^(1 / (2 * gvif_raw[, "Df"])),
    row.names = NULL
  )

  gvif_tbl
}

# =====================
# 5. Plotting functions
# =====================
save_smooth_plots <- function(gam_model, var_labels, out_dir) {

  plot_list <- list()
  num_smooths <- length(gam_model$smooth)

  for (i in seq_len(num_smooths)) {

    p <- draw(gam_model, select = i, rug = TRUE)

    for (v in names(var_labels)) {
      p <- p + labs(
        x = gsub(v, var_labels[[v]], p$labels$x),
        title = gsub(v, var_labels[[v]], p$labels$title)
      )
    }

    # Save
    ggsave(
      file.path(out_dir, paste0("smooth_", i, ".png")),
      p, width = 7, height = 5, dpi = 400
    )

    plot_list[[i]] <- p
  }

  return(plot_list)  # nolint
}

plot_model_diagnostics <- function(gam_model, df, target, out_dir) {
  predicted <- observed <- residuals <- NULL

  obs <- df[[target]]  # Observed values
  preds <- predict(gam_model, newdata = df, type = "response")  # Predicted values
  rmse_val <- sqrt(mean((obs - preds)^2))
  
  df_plot <- data.frame(
    observed = obs,
    predicted = preds,
    residuals = obs - preds
  )

  # Residuals vs predicted
  p_resid <- ggplot(df_plot, aes(x = predicted, y = residuals)) +
    geom_point(alpha = 0.5, color = "#2c7bb6") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Predicted", y = "Residuals",
         title = paste("Residuals vs Predicted:", target)) +
    theme_minimal()

  # Observed vs predicted
  p_obs_pred <- ggplot(df_plot, aes(x = predicted, y = observed)) +
    geom_point(alpha = 0.5, color = "#2c7bb6") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    labs(x = "Predicted", y = "Observed",
         title = paste("Observed vs Predicted:", target)) +
    theme_minimal()

  # SAVE HIGH-RES
  ggsave(file.path(out_dir, paste0("residuals_", target, ".png")),
         p_resid, width = 7, height = 5, dpi = 400)

  ggsave(file.path(out_dir, paste0("obs_vs_pred_", target, ".png")),
         p_obs_pred, width = 7, height = 5, dpi = 400)
  
  # Return both plots in a list
  list(residuals = p_resid, obs_vs_pred = p_obs_pred, rmse = rmse_val)
}

generate_report <- function(target, gam_model, residuals_plot, obs_vs_pred_plot, smooth_plots,
                            appraise_plot, rmse, ref_levels_df, gvif_tbl, cv_results, cv_summary,
                            categorical_features, importance_tbl) {
  rmarkdown::render(
    here::here("R", "GAM_satisfaction_Report.Rmd"),
    params = list(
      target = target,
      gam_model = gam_model,
      residuals_plot = residuals_plot,
      obs_vs_pred_plot = obs_vs_pred_plot,
      smooth_plots = smooth_plots, 
      appraise_plot = appraise_plot,
      rmse = rmse,
      ref_levels_df = ref_levels_df,
      gvif_tbl = gvif_tbl,
      cv_results = cv_results,
      cv_summary = cv_summary,
      categorical_features = categorical_features,
      importance_tbl = importance_tbl
    ),
    output_file = paste0("GAM_Report_", target, "_", timestamp, ".html")
  )
}

# ==========================
# 6.Fit GAM and Generate Report
# ==========================
message("\n=== Processing target: ", target, " ===")

needed_cols <- c(numerical_features, categorical_features, target)

df_target <- df %>% tidyr::drop_na(dplyr::all_of(needed_cols))

# --------------------------
# Fit GAM on full data (use df_target)
# --------------------------
gam_model <- fit_gam(df_target, target, numerical_features, categorical_features,
                     weight_var = "inv_sd", scale_value = NULL, k_map = k_map)
print(summary(gam_model))

smooth_plots <- save_smooth_plots(gam_model, var_labels, plot_dir)

appraise_plot <- appraise(
  gam_model,
  seed = 42,
  point_col = "steelblue",
  type = "pearson",
  method = "simulate"
)

ggsave(
  filename = file.path(plot_dir, paste0("gam_appraise_", target, ".png")),
  plot = appraise_plot,
  width = 10,
  height = 8,
  dpi = 400
)

# Cross-validation for GAM performance assessment
cv_results <- cv_gam(df_target, target, K = 5, seed = 42,
                     weight_var = "n_resp", scale_value = pooled_var )

cv_summary <- cv_results %>%
  summarise(
    RMSE_mean = mean(rmse),
    RMSE_sd   = sd(rmse),
    MAE_mean  = mean(mae),
    MAE_sd    = sd(mae)
  )

diag_plots <- plot_model_diagnostics(gam_model, df_target, target, plot_dir)

# Compute GVIFs for parametric features (categoricals) only
if (collapse_applied) {
  gvif_tbl <- compute_gvif(
    data = df_target,
    target = target,
    parametric_features = categorical_features
  )
}

# --------------------------
# Variable importance using gam.hp and term variance (unweighted model required for gam.hp)
# --------------------------
gam_model_unweighted <- fit_gam(df_target, target, numerical_features, categorical_features,
                                weight_var = NULL, scale_value = NULL, k_map = k_map)

hp_res <- gam.hp::gam.hp(gam_model_unweighted, type = "dev")

# Full gam.hp output
importance_hp <- as.data.frame(hp_res$hierarchical.partitioning) %>%
  tibble::rownames_to_column("covariate") %>%
  dplyr::mutate(
    covariate = gsub("^s\\(([^,]*).*\\)$", "s(\\1)", covariate)
  )

# Combine with term variance
term_mat_unweighted <- predict(gam_model_unweighted, type = "terms") 

term_mat <- predict(gam_model, type = "terms")

importance_var_unweighted <- apply(term_mat_unweighted, 2, var) %>%
  tibble::enframe(name = "covariate", value = "TermVar") %>%
  dplyr::mutate(
    TermSD_unweighted     = sqrt(TermVar),
    TermSD_unweighted_pct = 100 * TermSD_unweighted / sum(TermSD_unweighted)
  )

importance_var <- apply(term_mat, 2, var) %>%
  tibble::enframe(name = "covariate", value = "TermVar") %>%
  dplyr::mutate(
    TermSD      = sqrt(TermVar),
    TermSD_pct = 100 * TermSD / sum(TermSD)
  )

# Full importance table passed to markdown
importance_tbl <- importance_hp %>%
  dplyr::left_join(importance_var, by = "covariate") %>%
  dplyr::left_join(importance_var_unweighted, by = "covariate") %>%
  dplyr::arrange(desc(`I.perc(%)`))  # sort by % contribution

# --------------------------
# Generate PDF report with all results and diagnostics
# --------------------------
generate_report(
  target,
  gam_model,
  residuals_plot = diag_plots$residuals,
  obs_vs_pred_plot = diag_plots$obs_vs_pred,
  smooth_plots = smooth_plots,
  appraise_plot = appraise_plot,
  rmse = diag_plots$rmse,
  ref_levels_df = ref_levels_df,
  gvif_tbl = gvif_tbl,
  cv_results = cv_results,
  cv_summary = cv_summary,
  categorical_features = categorical_features,
  importance_tbl = importance_tbl
)

# Construct full HTML path
html_file <- file.path(
  getwd(),
  paste0("GAM_Report_", target, "_", timestamp, ".html")
)