# OFH Synthetic Data Generator

This repository introduces the `ofhsyn` R library that creates synthetic datasets, aiming to mimic the structure of multiple linked health data sources (participant, questionnaire, clinic measures, HES-like events, primary care medicines, deaths, and geography) available in the Our Future Health cohort. It is designed so researchers can develop and test workflows before running them on the much larger real data.

This project is designed to generate synthetic data from rule-based and stochastic generators, schema dictionaries, and user-supplied code lists, rather than from participant-level OFH source records.

The output data is a general approximation of the schema intended for code development, not for statistical inference. It applies rule-based data generation based on public schemas and does not take any kind of real partcipant data as input.

_This is an unofficial tool and is not affiliated with Our Future Health._

## What It Generates

The generator produces CSV files for a synthetic cohort, including:

- participant data
- questionnaire data
- clinic measurements
- outpatient, inpatient, emergency, and deaths HES data
- primary care medication data
- country/region data

All datasets are linked by `pid`.

The sample size, diagnostic codes, and BNF drug codes are determined by the user. These codes are then sampled and distributed using configurable probabilities (for cohort coverage, record counts, and field-level missingness) across the inpatient, outpatient, A&E, and primary care medicines datasets.

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
git clone https://github.com/hlnicholls/ofhsyn

cd ./ofhsyn
```

```r
install.packages(".", repos = NULL, type = "source")
library(ofhsyn)

syn <- OFHCohortSynthesizer$new(project_root = ".", seed = 123)

out <- syn$run_all(n = 1000)
```

## Custom Code Lists (ICD-10, OPCS4, BNF)

You control which codes appear in outputs, and how frequently records are generated.

- Set code pools with `icd10`, `opcs4`, and `bnf_codes` (or `*_file` inputs).
- Set dataset-level coverage and record volume with `proportions` and `record_multipliers`.
	- E.g., setting `proportions = list(nhse_outpat = 0.25)` and `record_multipliers = list(nhse_outpat = 1.2)` for `n = 1000` gives about 250 unique outpatient participants and about 1,200 outpatient rows (including repeat visits).
- Set within-dataset code proportionality (relative code frequencies) with `code_config` weights, for example `icd10_weights`, `opcs4_weights`, `primary_icd10_weights`, `underlying_icd10_weights`, `ae_specific_weights`, and `read_weights`.
	- E.g., setting `code_config = list(nhse_outpat_data = list(icd10_weights = c(I210 = 5, I500 = 1)))` gives roughly a 5:1 ratio of `I210` to `I500` among sampled outpatient ICD-10 codes (subject to normal random variation).

### Code Lists File Formats
ICD10 code files should use this structure (CSV columns, or tab-separated TXT with same columns):
| code | description |
| --- | --- |
| I210 | STEMI of anterolateral wall |
| I500 | Congestive heart failure |
| N189 | Chronic kidney disease unspecified |

OPCS4 code files should use this structure (CSV columns, or tab-separated TXT with same columns):
| code | description |
| --- | --- |
| K401 | Percutaneous transluminal balloon angioplasty of coronary artery |
| K451 | Insertion of drug-eluting stent into coronary artery |
| K561 | Repair of heart valve |

BNF codes should use this structure:
| BNFCode | BNFName | Formulation | Strength |
| --- | --- | --- | --- |
| ZZZ0001AA | Custom Test Drug A | tablets | 10 mg |
| ZZZ0002BB | Custom Test Drug B | inhaler | 100 mcg |
| ZZZ0003CC | Custom Test Drug C | capsules | 50 mg |


## Example Data Generation with set codes

```r
library(ofhsyn)

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
	bnf_codes = data.frame(
		BNFCode = c("0212000B0", "0601023A0"),
		BNFName = c("Atorvastatin 20 mg tablets", "Metformin 500 mg tablets"),
		Formulation = c("tablets", "tablets"),
		Strength = c("20 mg", "500 mg"),
		stringsAsFactors = FALSE
	)
)

# or load code lists from files
# - ICD10/OPCS4: include both code and description
#   - TXT: tab-separated columns (code\tdescription)
#   - CSV: 'code,description' columns
# - BNF: use a CSV with columns BNFCode, BNFName, Formulation (optional Strength)
out <- generate_ofh_cohort(
	n = 1000,
	seed = 123,
	icd10_file = "icd10_codes.txt",
	opcs4_file = "opcs4_codes.csv",
	bnf_codes_file = "bnf_medications.csv"
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

# customize generation probabilities
out_custom_probs <- generate_ofh_cohort(
	n = 1000,
	seed = 123,
	proportions = list(
		nhse_outpat = 0.25,
		nhse_inpat = 0.20,
		nhse_ed = 0.30,
		nhse_primcare_meds = 0.75
	),
	record_multipliers = list(
		nhse_outpat = 1.2,
		nhse_inpat = 1.1,
		nhse_ed = 1.3
	),
	code_config = list(
		nhse_outpat_data = list(diag_4_02_missing_prob = 0.70),
		nhse_inpat_data = list(single_diag_prob = 0.85)
	),
	save_csv = FALSE,
	return_objects = TRUE
)
```
