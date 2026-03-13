# Roche Coding Assessment

## Repository Structure

This repository contains the work completed for the Roche ADS Programmer coding assessment.
It is organized into three folders, one per question.

---

## Question 1

**Focus:** SDTM Programming — Creation of the DS (Disposition) domain from raw clinical data
using the `{sdtm.oak}` and `{pharmaverse}` ecosystem.

### Folder Structure
```
Question 1/
├── 00_raw_data/              # Controlled terminology (sdtm_ct.csv)
├── 01_r_script/              # R script used to create the DS domain
├── 02_output/                # Output files (ds.xpt)
├── 03_supportive_documents/  # Supportive documents (eCRF)
├── 04_logs/                  # Log files as evidence of error-free execution
└── Question_1.Rproj          # R Project file
```

---

## Question 2

**Focus:** ADaM Programming — Creation of the ADSL dataset from SDTM domains
using the `{admiral}` package.

### Folder Structure
```
Question 2/
├── 00_raw_data/              # Empty — SDTM data sourced from {pharmaversesdtm}
├── 01_r_script/              # R script used to create the ADSL dataset
├── 02_output/                # Output files (adsl.xpt)
├── 03_logs/                  # Log files as evidence of error-free execution
└── Question_2.Rproj          # R Project file
```

---

## Question 3

**Focus:** Data Visualization — Creation of a summarizing table and plot summarizing Adverse Events
from the ADAE dataset using `{ggplot2}` and `{gtsummary}`.

### Folder Structure
```
Question 3/
├── 00_raw_data/              # Empty — data sourced from {pharmaverseadam}
├── 01_r_script/              # R scripts used to create the visualization
├── 02_output/                # Output files (table as html and plots as png)
├── 03_logs/                  # Log files as evidence of error-free execution
└── Question_3.Rproj          # R Project file
```

---

## Packages Used

| Package | Purpose |
|---------|---------|
| `{sdtm.oak}` | SDTM domain creation |
| `{admiral}` | ADaM dataset creation |
| `{lubridate}` | Date and datetime manipulation |
| `{pharmaverseraw}` | Raw clinical data |
| `{pharmaversesdtm}` | SDTM reference data |
| `{pharmaversesadam}` | ADaM reference data |
| `{ggplot2}` | Data visualization |
| `{dplyr}` | Data manipulation |
| `{stringr}` | String manipulation |
| `{haven}` | Export to XPT format |
| `{gt}` | Table formatting |
| `{gtsummary}` | Summary tables |
| `{testthat}` | Unit testing |

---

## Author
Julien Chabbey
