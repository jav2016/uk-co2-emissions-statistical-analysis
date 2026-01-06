# UK CO₂ Emissions and Energy Analysis

## Project Overview
This project looks at long-term changes in CO₂ emissions per capita in the United Kingdom and
examines how these changes are related to renewable energy use and overall energy consumption.

The analysis uses annual UK data from 1990 onwards and focuses on understanding patterns and
relationships in the data using applied statistical methods. The aim is to see whether lower
emissions are more closely linked to changes in the energy mix, changes in energy use, or both.

This project is presented as a data analysis portfolio and is not a coursework submission.

---

## Data Used
The project uses publicly available UK data from **Our World in Data**, including:
- CO₂ emissions per capita
- Share of renewable energy
- Energy use per capita

All datasets are filtered for the United Kingdom and aligned by year.

---

## Analysis Approach
The analysis is carried out in **R** and follows a clear, step-by-step process:
- Summary statistics and trend analysis
- Visual exploration of data distributions
- Correlation analysis
- Linear regression modelling
- Diagnostic checks to assess model reliability

The focus is on understanding statistical relationships rather than making predictions or
causal claims.

---

## Key Results
- CO₂ emissions per capita in the UK show a clear decline over time.
- Higher renewable energy share is associated with lower emissions.
- Higher energy use per capita is associated with higher emissions.
- Models that include both renewable energy and energy use explain the data better than
single-variable models.
- Diagnostic checks suggest that the regression results are reasonably reliable given the
data limitations.

---

## Tools
- R (tidyverse, ggplot2, lmtest, car)
- Reproducible statistical scripting

---

## Repository Structure
