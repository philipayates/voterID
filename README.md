# voterID

This repository contains files for the data and models used in "Poll voting and the ompact of voter ID on Party Vote Share" by Elizabeth Bergman (Department of Political Science, Cal State East Bay), Dari Sylvester Tran (Department of Political Science, University of the Pacific), and Philip Yates (Department of Mathematical Sciences, DePaul University).

The following represents a codebook for the congress2020.csv file. It should be noted that data for Alaska are not included. All other CSV files, except for state_info.csv, are generated by the R code. To be as transparent as possible, we have included all of the R-generated CSV files in this repository.

## Variables include:

### Dist
- **Description**: congressional district

----------------

### GOPVote
- **Description**: total votes in the 2020 congressional race for the Republican candidate

----------------

### DemVote
- **Description**: total votes in the 2020 congressional race for the Democrat candidate

----------------

### OtherVote
- **Description**: total votes in the 2020 congressional race for all third party candidates

----------------

### stateID
- **Description**: numeric code for the state

----------------

### VID
- **Description**: `1` if voter ID law is present in the state, `0` otherwise

----------------

### PVI\_GOP\_state
- **Description**: The Cook Political Report's Partisan Voter Index for the state (positive means more Republican-leaning, negative means Democrat-leaning)
- **Source**: https://cookpolitical.com/state-pvis

----------------

### PVI\_GOP
- **Description**: The Cook Political Report's Partisan Voter Index for the district (positive means more Republican-leaning, negative means Democrat-leaning)
- **Source**: https://cookpolitical.com/pvi-map-and-district-list

----------------

All other demographic information were obtained from https://www.census.gov/mycd/

From https://www.census.gov/mycd/: "My Congressional District gives you quick and easy access to selected statistics collected by the U.S. Census Bureau through the American Community Survey (ACS) and County Business Patterns (CBP). The ACS provides detailed demographic, social, economic, and housing statistics every year for the nation's communities. CBP provides annual statistics for businesses with paid employees at a detailed geography and industry level. My Congressional District is powered by ACS and CBP data through the Census Application Programming Interface (API)."

Sources: 2019 American Community Survey 1-Year Estimates and 2018 County Business Patterns

ACS: https://www.census.gov/programs-surveys/acs/about.html

CBP: https://www.census.gov/programs-surveys/cbp.html
