# Software Code and Reproducibility Guide

This folder contains the R scripts utilized for data preprocessing and bibliometric analysis in the study: **"AI-Driven Precision Sports Nutrition: From General Guidelines to Algorithmic Decision-Making"**.

## 1. Environment Requirements
- **R Version**: 4.4.3
- **Integrated Development Environment (IDE)**: RStudio (recommended)
- **Primary Packages**: 
  - `bibliometrix` (and its dependencies)

To install the necessary package, run:
```R
install.packages("bibliometrix")

```

## 2. File Descriptions

* `01_Data_Deduplication.R`: This script imports raw data exported from the Web of Science Core Collection (WoSCC), performs automated deduplication, and exports the refined dataset.
* `02_Bibliometric_Analysis.R`: This script performs comprehensive bibliometric mapping, including temporal growth modeling, geospatial analysis, network topology construction (co-citation and co-occurrence), and strategic diagram generation.

## 3. Execution Workflow

To ensure the scripts run correctly, please follow these steps:

### Step 1: Preprocessing and Deduplication

1. Open `01_Data_Deduplication.R`.
2. **Set Working Directory**: Use `setwd()` to point to the folder containing your raw WoSCC files (Plain Text format).
3. Execute the script.
4. **Output**: The script will automatically create a subfolder named `cleaned_data` within your current directory and save the deduplicated dataset there.

### Step 2: Bibliometric Analysis

1. Open `02_Bibliometric_Analysis.R`.
2. **Update Working Directory**: Change your working directory to the `cleaned_data` folder generated in Step 1.
3. Execute the script to generate all statistical outputs, network visualizations, and evolutionary plots presented in the manuscript.

## 4. Note on Data Source

The scripts are optimized for **Full Record and Cited References** exported from the Web of Science Core Collection in **Plain Text** format. Ensure the file names in the scripts match your local file names.

