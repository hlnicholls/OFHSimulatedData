# OFHSimulatedData

Synthetic cohort data generator for health researchers.

This repository creates test datasets that mimic the structure of multiple linked health data sources (participant, questionnaire, clinic measures, HES-like events, primary care medicines, deaths, and geography) for the Our Future Health cohort. It is designed so researchers can develop and test workflows safely before running them on real data.

## What It Generates

The generator produces CSV files for a synthetic cohort, including:

- participant data
- questionnaire data
- clinic measurements
- outpatient, inpatient, emergency, and deaths HES data
- primary care medication data
- country/region data

All datasets are linked by `pid`.

The sample size, diagnostic codes, and BNF drug codes are determined by the user. The codes then randomly distributed throughout the inpatient, outpatient, A&E, and medicines dispensed in primary care datasets.

## Requirements

Minimum:

- R 4.2+

For vignette building/preview:

- `rmarkdown`
- `knitr`

Install vignette dependencies (if needed):

```r
install.packages(c("rmarkdown", "knitr"))
```

## Quick Start

```
git clone https://github.com/hlnicholls/OFHSimulatedData

cd ./OFHSimulatedData
```

```r
install.packages(".", repos = NULL, type = "source")
library(OFHSimulatedData)

sim <- OFHCohortSimulator$new(project_root = ".", seed = 123)

out <- sim$run_all(n = 1000)
```

## Custom Code Lists (ICD-10, OPCS4, BNF)

You control which codes appear in outputs.

```r
library(OFHSimulatedData)

out <- generate_ofh_cohort(
	n = 1000,
	seed = 123,
	icd10 = c(
		I210 = "STEMI of anterolateral wall",
		I500 = "Congestive heart failure"
	),
	opcs4 = c(
		K401 = "Percutaneous transluminal balloon angioplasty of coronary artery"
	),
	bnf_codes = c("0212000B0", "0601023A0")
)

# or load code lists from files (one code per line in .txt or .csv)
out <- generate_ofh_cohort(
	n = 1000,
	seed = 123,
	icd10_file = "icd10_codes.txt",
	opcs4_file = "opcs4_codes.csv",
	bnf_codes_file = "bnf_codes.txt"
)
```
