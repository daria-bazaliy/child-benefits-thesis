# child-benefits-thesis
Bachelor's thesis: econometric analysis of child benefits and poverty in Russia (panel data, RLMS-HSE, 2014–2024)
# Child Benefits and Poverty of Families with Children in Modern Russia

Bachelor's thesis, HSE University, 2025–2026

## Overview

This project examines whether child benefit reforms in Russia (2014–2024) 
reduced the risk of poverty among families with children, and which design 
features of the reforms (coverage, payment thresholds, targeting) mattered most.

## Data

- Source: RLMS-HSE (Russian Longitudinal Monitoring Survey), household + 
  individual panel data
- Regional subsistence minimum (living wage) data, used to construct 
  instrumental variables
- Sample: households with at least one child aged 0–16, 2014–2024 
  (23,259 observations after cleaning)
- Four poverty measures constructed from the data: absolute poverty, 
  consumption-based poverty, and two relative poverty measures 
  (60% and 50% of median income)

## Method

- Descriptive statistics and visualizations of benefit levels and poverty 
  dynamics over time, by household type and settlement type
- Cross-sectional probit models for 2014 and 2024, built up stepwise 
  (household structure → location/employment → parental characteristics)
- Panel random-effects probit (via GLMMadaptive) as the main specification, 
  with average marginal effects (AME)
- Panel fixed-effects probit (via bife) as a robustness check, plus a 
  Hausman test comparing RE and FE
- Instrumental variables / control function approach, using regional 
  subsistence minimum as an instrument for benefit amount, to address 
  endogeneity of benefit receipt
- Robustness checks across all four poverty measures, reform-period dummies, 
  and single-parent/large-family subgroups

## Key Finding

Expanding coverage and raising the payment threshold reduced poverty risk 
more effectively than earlier, narrower reform stages. Hving many children was not a significant driver of poverty 
once family composition and other controls were accounted for.

## Files

- `child-benefit-code.R` — full analysis: data cleaning, 
  descriptive statistics, cross-sectional and panel probit models, IV 
  estimation, robustness checks
  Full work(https://www.hse.ru/en/edu/vkr/1158311521)

## Tools

R (dplyr, ggplot2, GLMMadaptive, bife, plm, AER/ivreg, modelsummary)

## Note on data

Raw data not included due to RLMS-HSE access restrictions; available via 
official registration at hse.ru/rlms
