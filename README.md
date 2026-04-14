# OFHSimulatedData

This repository creates synthetic datasets that aim to mimic the structure of multiple linked health data sources (participant, questionnaire, clinic measures, HES-like events, primary care medicines, deaths, and geography) available in the Our Future Health cohort. It is designed so researchers can develop and test workflows before running them on the much larger real data.

The output data is a general approximation of the schema intended for code development, not for statistical inference.

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

# return objects only in the R environment (no CSV files written)
out_objects_only <- generate_ofh_cohort(
	n = 1000,
	seed = 123,
	save_csv = FALSE,
	return_objects = TRUE
)

# write CSV files only (no returned R objects)
generate_ofh_cohort(
	n = 1000,
	seed = 123,
	save_csv = TRUE,
	return_objects = FALSE,
	output_dir = "example"
)
```
