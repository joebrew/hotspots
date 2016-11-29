# hotspots
Hotspots of malaria and diarrhoea in Manhiça, Mozambique

## Collaborators

Joe Brew
Quique Bassat
Beatriz Galatas

## Instructions

- Clone the repository
- Create a data folder (ignored by git) and place into that folder a `.csv` of the raw OPD data named "opd_YYYY-mm-dd.csv" (for example, "opd_2016-05-26.csv").
- Run the "clean_opd.R" script, based loosely on Bea Galatas' "misc/opd_cleaning_joe.do", in order to clean the .csv and produce a dataframe ready for analysis.

## Datasets

- basetmr is the dataset which provides person-time at risk
-- expand = the expanded area (use only expand == 0)

## Misc
- There needs to be a 28 days following onset to not be considered at risk
- Take out people once above 15

## Other ideas
- We missed so many episodes in the < 28 days period
  - Mira (already passed CCI) protocols: look at risk factors of having multiple cases within 60 days

- To do: Bea needs to send mira (multiple infection risk analysis) protocl to joe

## Outcomes
-Incidence 
-Prevalence from cross-sectionals
-Parasitimia analysis and risk factors
-Spatial-temporal stuff (satscan)