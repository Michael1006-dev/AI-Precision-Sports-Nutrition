# Data Repository Structure

This directory contains the bibliographic datasets retrieved from the Web of Science Core Collection (WoSCC) for the study **"AI-Driven Precision Sports Nutrition: From General Guidelines to Algorithmic Decision-Making"**.

The data is organized into two sequential stages of processing to ensure reproducibility and transparency.

## 1. Directory Overview

### Folder: `01_Filtered_Dataset_Pre-Deduplication_n587`
- **Content**: The initial raw dataset obtained immediately after applying the search strategy and exclusion criteria (e.g., excluding veterinary sciences, gray literature).
- **Record Count**: 587 documents.
- **Purpose**: This dataset serves as the baseline for the deduplication process. It represents the raw search results prior to data cleaning.

### Folder: `02_Final_Cleaned_Dataset_n583`
- **Content**: The final, refined dataset used for all bibliometric analyses, network mapping, and statistical modeling in the manuscript.
- **Record Count**: 583 documents.
- **Processing**: 4 duplicate records were removed from the initial dataset using the `bibliometrix` R package automated deduplication function.
- **Usage**: All results reported in the "Results" section are derived exclusively from this dataset.

## 2. Data Specifications
- **Source**: Web of Science Core Collection (WoSCC).
- **Date of Retrieval**: December 14, 2025.
- **File Format**: Plain Text File (.txt) / Tab-delimited File (Win/Mac).
- **Content Type**: Full Record and Cited References.

## 3. Replication Note
To replicate the deduplication process (transforming the dataset from *n=587* to *n=583*), please refer to the script `01_Data_Deduplication.R` located in the `Software_Code_R` folder.