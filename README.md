# Consolidating cause-of-death evidence for children and adolescents

Data, data collection forms, and code for:

Wahi A, Perin J, Eilerts-Spinelli H, Prieto-Merino D, Villavicencio F, Black RE, Liu L. Consolidating cause-of-death evidence for children and adolescents through an updated systematic review and known investigator-tracing. Journal of Global Health. 2026.

## Repository structure

| Folder | Contents |
|---|---|
| `data/` | Cause-of-death data extracted from studies identified through the systematic review, and two lookup files |
| `forms/` | Data collection forms used for extraction |
| `code/` | R code reproducing Table 1 (systematic review columns), Figures 2–3, and Figures S1–S3 |

## Data

The `data/` folder contains one cause-of-death file per age group (neonates, 1–59 months, 5–9 years, 10–14 years, and 15–19 years by sex), together with a country-to-region lookup file and a file of high-mortality countries used for Figure S3.

Each row is one study data point: a reporting unit with a distinct cause-of-death distribution by geography, study period, age group, or sex. Cause columns give the number of deaths by harmonized cause category; the mapping from study-reported causes to these categories is given in Table S3 of the paper's supplement, and study characteristics for all data points are listed in Table S5. Blank cause cells indicate causes not reported separately by the study; deaths with an undetermined cause are recorded in a separate column and are not included in the total.

Investigator-traced datasets are not included because of data-sharing agreements with the original data owners; access may be requested from those owners. The public files therefore reproduce the portion of the published studies' results.

## Forms

The `forms/` folder contains the DistillerSR forms used for data extraction (study characteristics, deaths by cause, and study covariates for children under 5 and for ages 5–19 years), reproduced as used during extraction. The reference shown at the top of each form is DistillerSR's placeholder preview reference and is unrelated to this review.

## Reproducing the results

Set the working directory to the repository's main folder and run `code/figures_and_tables.R`. Outputs are saved to an `output/` folder created by the script.

## License and contact

Data and forms are shared under a Creative Commons Attribution 4.0 International (CC BY 4.0) license. Please cite the paper above when using these materials.

Contact: Astha Wahi (awahi1@jhmi.edu)
