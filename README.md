# R Data Analysis Framework
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
![R](https://img.shields.io/badge/R-Data_Analysis-blue)
[![Experimental](https://img.shields.io/badge/Status-Experimental-orange.svg)](https://shields.io/)


A modular R framework for data analysis, data manipulation, and visualization. This library provides functions for handling data, with emphasis on reproducibility and clean code practices.

## Overview

This framework implements data analysis workflows focusing on:
- Geometric calculations and transformations
- Descriptive statistics and data summarization  
- Statistical hypothesis testing
- Data preprocessing and cleaning
- Correlation analysis

## Installation

```r
# Clone repository
git clone https://github.com/username/r-statistical-analysis.git

# Install required R packages
install.packages(c("stats", "utils", "base"))
```

## Project Structure

```
.
├── assignment1/
│   ├── assignment1.R      # Core statistical functions
│   └── chol.txt           # Sample cholesterol dataset
├── assignment2/
│   ├── assignment2.R      # Data preprocessing utilities  
│   └── Assignment2.RData  # Multi-dataset RData file
└── README.md
```

## Core Features

### Geometric Analysis Module
- Circle and sphere calculations (area, volume, circumference)
- Multi-dimensional geometric transformations
- Configurable precision and rounding

### Statistical Analysis Tools
- Descriptive statistics generation
- Two-sample t-test automation
- Missing value handling
- Outlier detection and processing

### Data Processing Utilities
- Data normalization
- Correlation analysis for metrics
- Categorical data imputation
- Flexible data frame operations

## Usage Examples

### Basic Circle Calculations
```r
# Calculate circle area
calCircle('ac', 4)

# Multiple calculations
calCircle2(c('AC', 'VS'), seq(5,25,5))
```

### Statistical Analysis
```r
# Generate descriptive statistics
table1(patient_num, c("GLUC", "TGL", "HDL", "LDL"))

# Perform t-tests
myTtest(dat=chol, classVar="sex", numVar=c("age", "chol", "tg"))
```

### Data Preprocessing
```r
# Handle missing values
impute(dat=patient, varlist=c("LDL", "HRT", "MAMM"))

# Calculate correlations
myCorTest(chol, "bmi", c("sbp", "dbp", "vldl", "hdl", "ldl"))
```

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/enhancement`)
3. Commit changes (`git commit -am 'Add enhancement'`)
4. Push to branch (`git push origin feature/enhancement`)
5. Create Pull Request

## License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

## Citation

If you use this framework in your research, please cite:
```bibtex
@software{r_data_analysis,
  author = {Shadman, Nabil},
  title = {R Data Analysis Framework},
  year = {2020},
  url = {https://github.com/nabilshadman/r-data-analysis}
}
```
