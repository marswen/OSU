# **O**bservational **S**tudy **U**nderstanding

This tool is used to help those who have clinical data but are not familiar with programming or statistics, quickly explore the data, and even conduct clinical study analysis. Based on the collected data, a complete clinical study analysis can be finished interactively through the three parts of study definition, data collation, and data analysis.

## Study definition

Define dataset, inclusion variables, exposure variable, analysis type, outcome variable for association analysis, and survival time and outcome event for survival analysis.

## Data collation

#### Data diagnosis

Preliminary description of missing data, data type, value range and options, and data distribution, so that researchers can take further processing according to the data situation.

#### Data preprocessing

* Variable filtering: further selection of inclusion variables.
* Missing value imputation: drop missing data, simple statistics imputation, multiple interpolation.
* Variable type adjustment: confirm categorical variables, continuous variables, and other types of variables.
* Variable binning: transform continuous variables into categorical variables.
* Option mapping: map string values to canonical codes.
* Dummy variable: convert a multiple-choice categorical variable into multiple binary categorical variables.

## Data analysis

#### Baseline

Describe basic demographic information, and test differences between groups, to control confounding factors.

#### Propensity score matching

If there is an imbalance in the basic information between groups, the data can be resampled through PSM.

#### Risk factor analysis

If there is an imbalance in the basic information between groups, the data is re-matched through PSM.

#### Effect estimation

Estimate the effect of the exposure on the outcome, supporting confounding factors adjustment and subgroup analysis.

## Demo

[Observational Study Understanding (shinyapps.io)](https://marswen.shinyapps.io/ObservationalStudyUnderstanding/)