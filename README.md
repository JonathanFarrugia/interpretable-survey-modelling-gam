# GAM-Based Key Driver Analysis using mgcv

A Generalized Additive Model (GAM) framework for key driver analysis of survey satisfaction outcomes using the **mgcv** package in R.

This project demonstrates a complete modelling workflow including:

- Data preprocessing
- Weighted GAM estimation
- Cross-validation
- Diagnostic evaluation
- Variable importance analysis
- Automated reporting

The repository uses a synthetic training dataset and is intended to showcase methodology rather than produce substantive policy conclusions.

---

## Project Overview

Generalized Additive Models (GAMs) extend traditional regression by allowing non-linear relationships between predictors and outcomes while retaining interpretability.

This implementation combines:

- Penalised regression splines for continuous variables
- Categorical main effects
- Optional interaction structures
- Observation weighting
- Automated diagnostics and reporting

The workflow was developed as part of a broader analytical portfolio demonstrating interpretable statistical modelling for survey-based key driver analysis.

---

## Repository Structure

```text
.
├── GAM_satisfaction.R
├── GAM_satisfaction_Report.Rmd
├── install_packages.R
├── styles.css
├── data/
│   └── synthetic_training_data.xlsx
├── results/
│   ├── GAM_Report_satisfaction_level_sample_report.html
│   ├── gam_appraise_satisfaction_level.png
│   ├── obs_vs_pred_satisfaction_level.png
│   ├── residuals_satisfaction_level.png
│   ├── smooth_1.png
│   ├── smooth_2.png
│   └── smooth_3.png
└── README.md
```

---

## Model Specification

The final model uses a Gaussian GAM fitted via:

```r
mgcv::gam(
  family = gaussian(),
  method = "REML",
  select = TRUE
)
```

### Continuous Predictors

Continuous variables are modelled using penalised cubic regression splines:

```r
s(variable, bs = "cs")
```

Example variables:

- Enrolments
- Number of trainers
- Duration (days)

### Categorical Predictors

Categorical variables enter as standard parametric effects:

- Session delivery
- Session framework
- Session organiser
- Session language
- Module topic
- Level
- Year

### Optional Interaction Support

The framework includes configurable support for:

#### Numeric × Numeric

```r
ti(x1, x2)
```

#### Numeric × Categorical

```r
s(x, by = factor)
```

or

```r
bs = "fs"
```

factor smooths.

#### Categorical × Categorical

```r
bs = "re"
```

random-effect interaction smooths.

No interactions are enabled in the final model configuration to prioritise interpretability and model stability.

---

## Weighting Strategy

Observations are weighted according to outcome variability:

```r
inv_sd = 1 / sqrt(variance)
```

Weights are subsequently normalised:

```r
inv_sd = inv_sd / mean(inv_sd)
```

This approach gives greater influence to observations with lower variance while maintaining numerical stability.

A pooled-variance alternative is implemented but not used in the final model.

---

## Data Processing

The pipeline performs:

### Factor Handling

- Conversion to factors
- Sparse-level collapsing where required
- Automatic selection of modal reference categories

### Missing Data Handling

Rows with missing values in modelling variables are removed prior to fitting.

### Feature Configuration

Numerical and categorical variables are configured independently, allowing rapid experimentation with alternative specifications.

---

## Cross-Validation

Model performance is evaluated using K-fold cross-validation.

Reported metrics include:

- RMSE
- MAE

The procedure repeatedly trains and evaluates the GAM across held-out folds to estimate out-of-sample performance.

---

## Model Diagnostics

The pipeline automatically generates:

### Residual Diagnostics

- QQ plots
- Residual histograms
- Residuals vs fitted values
- Residuals vs linear predictor

### Predictive Diagnostics

- Observed vs predicted plots

### Smooth Effect Visualisation

Smooth functions are visualised using:

```r
gratia::draw()
```

including uncertainty bands.

Example outputs are included in the `results/` directory.

---

## Variable Importance

Two complementary approaches are implemented.

### Hierarchical Partitioning

Using:

```r
gam.hp
```

to estimate each predictor's contribution to explained deviance.

### Variance Decomposition

Based on variability of predictor contributions to the linear predictor.

Together these provide:

- Structural importance
- Relative effect magnitude

---

## Automated Reporting

The project includes an R Markdown reporting workflow.

Generated reports contain:

- Model specification
- Coefficient summaries
- Smooth term summaries
- Cross-validation results
- Diagnostic plots
- Variable importance rankings

A sample report is included:

```text
results/GAM_Report_satisfaction_level_sample_report.html
```

---

## Installation

Install required packages:

```r
source("requirements.R")
```

or manually install packages:

```r
install.packages(c(
  "mgcv",
  "dplyr",
  "tidyr",
  "forcats",
  "readxl",
  "ggplot2",
  "gratia",
  "car",
  "gam.hp",
  "rmarkdown",
  "here"
))
```

---

## Running the Analysis

Place the dataset inside:

```text
data/
```

Update the filename if necessary:

```r
data_path <- here::here(
  "data",
  "synthetic_training_data.xlsx"
)
```

Run:

```r
source("GAM_satisfaction.R")
```

Outputs will be written automatically to the configured results directory.

---

## Example Findings

Example smooth effects included in this repository demonstrate:

- Non-linear enrolment effects
- Positive trainer effects
- Positive duration effects

These findings are illustrative only and arise from synthetic data.

---

## Notes on Synthetic Data

The included dataset is synthetic and intended solely for methodological demonstration.

Accordingly:

- Results should not be interpreted as real-world findings.
- Effect sizes should not be interpreted causally.
- The focus of the repository is modelling methodology and workflow design.

---

## Potential Extensions

Future enhancements could include:

- Interaction-rich GAM specifications
- Alternative weighting schemes
- Distributional GAMs
- Classification GAMs
- Automated hyperparameter tuning
- Comparative benchmarking against machine learning models

---

## Technologies Used

- R
- mgcv
- gratia
- gam.hp
- dplyr
- ggplot2
- R Markdown

---