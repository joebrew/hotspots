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
- Run the "create_time_at_risk.R" script, written by Joe Brew, in order to clean the `ohds_basetmr.dta` data, and convert into "long" person-time-at-risk format.

## Datasets

- opd: outpatient diagnoses
- basetmr is the dataset which provides person-time at risk
- cross-sectional = includes moqutio control prevalence

## Misc
- There needs to be a 28 days following onset to not be considered at risk
- Take out people once above 15
- does incidence in children match up with PCR prevalence


## Other ideas
- We missed so many episodes in the < 28 days period
  - Mira (already passed CCI) protocols: look at risk factors of having multiple cases within 60 days

- To do:
- Create our own BASETMR from census - ask maxi re: which correct table to use, and how to ensure we get full migration/death history for everyone, regardless of current migration status
- Finish OPD cleaning
- Create BaseTMR dataset
- Replicate Ilha Josina files to get incidence
- Install ServolabR from gitlab
- Download all the NIDAS (numero de identificaco da mostra) from servolab, and
 - get the parasite density and paxo volumne using the ServolabR (use RProjects folder locally)
- Dig into ICD issues (Joe) and see if anything is salvageable (ask Quique about this) - prepare report for him about what we found, with guiding questions regarding problems
- Discuss sensitivity / specificity analysis

# Big research questions

## TRENDS AND PATHOGENSIS
(Ilha Josina but expanded)
- What are trends in malaria in Manhica over last 20 years?
- What is the profile of clinical malaria cases (replicate Ilha Josina), with more detail into symptoms, outcomes, etc.
- What are the breakdowns of malaria species over time / place?
- What is the attributible fraction of parasitemia to fever and to anemia? And how has this changed over time?

## SPATIAL TEMPORAL
- Are there hotspots?
- Have hotspots changed over time? 
  - Can we assume that when there is a clinical index case will it always drive you back to an area of community prevalence?
    - Do incidence and prevalence travel together (spatial-temporal analysis)?
- Should a program manager rely on prevalence data, or real-time incidence data?

## HEALTH SEEKING BEHAVIOR
- Health-seeking behavior: what factors determine one's likelihood of seeking care, at equality of illness?

## VECTOR CONTROL'S EFFECT
  - To what extent does vector control protect?
    - Look at MALDEMO quesionnaire in census.
    - Differential effect of vector control by class xmal


## Outcomes
-Incidence 
-Prevalence from cross-sectionals
-Parasitimia analysis and risk factors
-Spatial-temporal stuff (satscan)