
# INXVALID

**INXVALID** is an R package for validating composite indices. It
provides a structured workflow for assessing redundancy, dimensionality,
theoretical structure, and contribution balance among variables.

------------------------------------------------------------------------

## 🚀 Key Features

- **Bivariate screening** for redundancy (`bi_var`)
- **Dimensionality assessment** using PCA (`factor_var`)
- **Confirmatory factor analysis** for theoretical validation
  (`cfa_var`)
- **Contribution balance diagnostics** (`contrib_balance`)

------------------------------------------------------------------------

## 📦 Installation

``` r
# install.packages("devtools")
devtools::install_github("S-E-SCROGGINS/INXVALID")
```

------------------------------------------------------------------------

## 🔄 Example Workflow

``` r
library(INXVALID)

# Example dataset
set.seed(123)
df <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100),
  var4 = rnorm(100)
)

# 1. Check for redundancy
bi_var(df, threshold = 0.7)

# 2. Assess dimensionality
factor_var(df, var_target = 0.9)

# 3. Confirm theoretical structure
model <- "
  f1 =~ var1 + var2
  f2 =~ var3 + var4
"

cfa_var(df, model = model)

# 4. Evaluate contribution balance
contrib_balance(df)
```

------------------------------------------------------------------------

## 🧠 Conceptual Framework

INXVALID follows a four-step validation pipeline:

1.  **Redundancy Detection**  
    Identify highly correlated variables that may distort the index.

2.  **Dimensionality Assessment**  
    Determine the number of latent components required to explain
    variance.

3.  **Theoretical Validation**  
    Confirm whether variables load according to expected conceptual
    groupings.

4.  **Contribution Balance**  
    Evaluate whether certain variables dominate the index.

------------------------------------------------------------------------

## 📊 Output Philosophy

All functions in INXVALID:

- Print concise, interpretable summaries
- Return structured objects for further analysis
- Handle missing data appropriately for each method

------------------------------------------------------------------------

## ⚠️ Notes

- Input data should consist of numeric variables
- Missing data handling varies by function:
  - `factor_var()` and `cfa_var()` use complete cases
  - `contrib_balance()` uses row-wise weight normalization

------------------------------------------------------------------------

## 📌 Future Development

Planned extensions include:

- Sensitivity analysis (leave-one-out)
- Rank stability diagnostics
- Outcome validation tools

------------------------------------------------------------------------

## 👤 Author

Stephen Scroggins, PhD


