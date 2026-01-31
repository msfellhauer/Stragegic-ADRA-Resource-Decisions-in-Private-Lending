# Stragegic-ADRA-Resource-Decisions-in-Private-Lending
This is the code base for private lending research. The results were part of my Doctoral Dissertation entitled


STRATEGIC A.D.R.A. RESOURCE DECISIONS IN PRIVATE LENDING:
THE ROLE OF ADAPTIVE LEARNING AS
MEDIATED BY RISK AND ENTREPRENEURIAL ATTITUDES

The foundational, theoritical, elements were presented at the Academy of Business Research (2015), and the 
Midwest Academy of Management (2018). The final Dissertation was defended in 2025. This research is the first
quantiative test of ADRA logic, and one of the first academic studies in hard money lending. The data was gathered via a survey in available in 
qualtrics, a population defined in prolific, with the final data set coming through as a .csv file. The data set was cleaned 
in excel, then imported into R. The rationale behind cleaning in excel was due to the small data sets, that did not require extra coding in 
R. Further, having developed the survey, I was able to extract the data in a more efficient manner that did not require extensive cleaning
(e.g. non-responses were removed from the overall dataset). 


# Statistical Analysis in R

This repository contains R scripts used to perform descriptive statistics, hypothesis testing,
factor suitability tests, and structural equation modeling (SEM) with mediation analysis.

The analysis includes:
- Descriptive statistics
- T-tests and ANOVA
- Kaiser-Meyer-Olkin (KMO) and Bartlett’s tests
- Parallel mediation analysis using `lavaan`
- Automated export of results to Word documents

## Repository Structure

- `scripts/` — Modular R scripts for each analysis stage
- `data/` — Data documentation only (no raw data included)
- `outputs/` — Generated Word documents and tables

## Key Packages Used

- `psych`
- `ggpubr`
- `factoextra`
- `lavaan`
- `officer`
- `flextable`
- `tidyverse`

## Notes

This project demonstrates reproducible statistical analysis, model specification,
and automated reporting suitable for academic or applied analytics workflows.

