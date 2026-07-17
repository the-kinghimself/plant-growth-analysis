# Plant Growth Analysis: Rainwater vs Groundwater Irrigation

## Overview

This project analyzes and compares the growth performance of four crops under two irrigation methods: rainwater and groundwater. The analysis covers 13 weeks of growth measurements with 2 replicates per treatment.

## Crops Analyzed

- **Cucumber** (*Cucumis sativus*)
- **Ewedu** (*Talinum triangulare*)
- **Okra** (*Abelmoschus esculentus*)
- **Tete** (*Amaranthus viridis*)

## Project Structure

```
plant-growth-analysis/
├── crop.R                    # Main analysis script
├── crops growth.xlsx         # Raw data file (4 sheets, one per crop)
├── output/                   # Generated outputs directory
│   ├── 01_long_format_data.xlsx
│   ├── 02_descriptive_statistics.xlsx
│   ├── 03_growth_rate_analysis.xlsx
│   ├── 04_statistical_tests.xlsx
│   ├── plot_01_height_growth_curves.png
│   ├── plot_02_leaves_growth_curves.png
│   └── plot_03_relative_growth_rate.png
└── README.md
```

## Data Description

The raw data (`crops growth.xlsx`) contains:
- **4 sheets**: One for each crop
- **2 replicates** per treatment (Rainwater/Groundwater)
- **13 weeks** of measurements
- **Variables measured**:
  - Plant height (cm)
  - Leaf count (number of leaves)

### Data Structure

Each sheet contains weekly measurements organized as:
- Treatment replicates (R1, R2 for rainwater; G1, G2 for groundwater)
- Growth stage information
- Height and leaf count measurements

## Analysis Workflow

### 1. **Data Processing** (`crop.R`)
- Imports data from Excel sheets
- Reshapes from wide to long format
- Structures data for statistical analysis
- Exports processed data for reference

### 2. **Descriptive Statistics**
- Weekly mean, SD, and SE for height and leaf count
- Overall treatment comparisons
- Summary statistics by crop and irrigation method

### 3. **Growth Analysis**
- Absolute weekly growth rates
- Relative growth rates (RGR)
- Total growth gains (absolute and percentage)
- Week-by-week growth trajectories

### 4. **Statistical Comparisons**
- Independent samples t-tests (Rainwater vs Groundwater)
- Significance testing for height and leaf count
- P-value and effect size calculations
- Linear mixed-effects modeling (account for repeated measures)

### 5. **Visualizations**
- **Height growth curves**: Mean ± SE over 13 weeks by treatment
- **Leaf count curves**: Leaf development over time
- **Relative growth rate plots**: Weekly RGR trends

## Output Files

### Excel Files
- `01_long_format_data.xlsx` - Processed data in long format
- `02_descriptive_statistics.xlsx` - Weekly and overall summary statistics
- `03_growth_rate_analysis.xlsx` - Growth rates and gains
- `04_statistical_tests.xlsx` - T-test and mixed model results

### Plots (PNG, 300 dpi)
- `plot_01_height_growth_curves.png` - Height trajectories
- `plot_02_leaves_growth_curves.png` - Leaf count trajectories
- `plot_03_relative_growth_rate.png` - Relative growth rates

## Key Features

- **Reproducible analysis**: Complete R workflow from raw data to visualizations
- **Statistical rigor**: Multiple testing approaches (t-tests, mixed models)
- **Publication-ready plots**: High-resolution graphics with error bars and faceting
- **Comprehensive outputs**: Multiple data formats for further analysis

## Requirements

### R Packages
- `readxl` - Excel import
- `dplyr`, `tidyr` - Data manipulation
- `ggplot2` - Visualization
- `stringr`, `purrr` - String/functional programming
- `rstatix` - Statistical testing
- `lme4`, `lmerTest` - Mixed-effects modeling
- `writexl` - Excel export
- `scales` - Plot scaling

### Data Requirements
- Input file: `crops growth.xlsx` (in the project root)
- Properly formatted sheets with measurement data

## How to Run

1. Ensure all required R packages are installed
2. Place `crops growth.xlsx` in the project root directory
3. Run the analysis:
   ```r
   source("crop.R")
   ```
4. Check the `output/` directory for all generated files

## Output Interpretation

### Growth Curves
- Lines show mean values; error bars represent ±1 SE
- Separate panels for each crop due to different growth scales
- Color coding: Blue = Rainwater, Red = Groundwater

### Statistical Tests
- P-values < 0.05 indicate significant differences between treatments
- Effect sizes show magnitude of differences
- Mixed models account for correlations within replicates over time

### Growth Rates
- Positive RGR indicates active growth phase
- RGR near zero indicates plateau or senescence
- Comparison between treatments identifies optimal irrigation

## Contact & Notes

This analysis provides evidence-based comparison of irrigation methods for improved agricultural productivity.

---
*Generated from R analysis framework | Last updated: 2026-07-17*
