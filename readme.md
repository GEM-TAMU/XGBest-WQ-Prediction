Replication Code for: Enhancing Prediction and Inference of Daily In-stream Nutrient and Sediment Concentrations using an Extreme Gradient Boosting based Water Quality Estimation Tool - XGBest
================
Shubham Jain

    ## [1] "Time of last build: 2024-07-07 09:15:24.713642"

**This project contains the R code for:** Jain, Shubham; Bawa, Arun;
Mendoza, Katie; Srinivasan, Raghavan; Parmar, Rajbir; Smith, Deron;
Wolfe, Kurt; Johnston, John M., 2024, Enhancing Prediction and Inference of Daily In-stream Nutrient and Sediment Concentrations using an Extreme Gradient Boosting based Water Quality Estimation Tool - XGBest. ***Manuscript Under Review***.

## Required data

Data required to reproduce the results is available at: <https://doi.org/10.18738/T8/8X7QMA>

## Replication files Descriptions

- `run_file.R`: Step-by-step code to replicate the analysis.
- `load_libraries.R`: Load all required packages.
- `prep_data.R`: Process raw data files and datasets.
- `run_loadest_cv.R`: Run LOADEST Cross validation.
- `run_WRTDS_cv.R`: Run WRTDS Cross validation.
- `run_xgb_cv.R`: Run XGB cross validation.
- `run_xgb_complete_model.R`: Run XGB model with complete data and SHAP
  analysis.
- `evals_cv.R`: Evaluate the performance statistics from cv runs for the
  three methods (LOADEST, WRTDS, XGB).

## Instructions

To run the code, follow these steps:

1.  **Clone Repository**: Clone this GitHub repository to your local
    machine.

    ``` bash
    git clone https://github.com/GEM-TAMU/XGB-WQ-Prediction.git
    ```

2.  **Download data**: Download required data from:
    <https://doi.org/10.18738/T8/8X7QMA> and save it within the current
    working directory in folder name “data”.

3.  **Run R code**: Run R scripts in the order as listed in
    `run_file.R`.
