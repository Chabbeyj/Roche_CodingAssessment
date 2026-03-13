#Question 1: ADaM ADSL Dataset Creation

#Objective: Create an ADSL (Subject Level) dataset using SDTM source data, the {admiral} family of packages, and tidyverse tools.

#Start log file
log_file <- paste0("03_logs/adsl_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)

cat("Script started at:", format(Sys.time()), "\n")
cat("================================\n")

#Packages
cat("Loading necessary packages...\n")
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(haven)
library(testthat)
cat("Packages loaded successfully...\n")

#Variable to derive
#AGEGR9 & AGEGR9N
#     Age grouping into the following categories: “<18”, “18 - 50”, “>50”

#TRTSDTM & TRTSTMF
#     Treatment start date-time (using the first exposure record for each
#     participant and imputing missing hours and minutes but not seconds)

#ITTFL
#     “Y”/”N” flag identifying patients who have been randomized, that is,
#     where ARM is populated in pharmaversesdtm::dm domain.

#LSTAVLDT
#     Last known alive date using any vital signs visit date, any adverse event
#     start date, any disposition record and any exposure record.


#Load the data
cat("Loading SDTM domain...\n")
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)
cat("SDTM domain loaded successfully...\n")

#DM as basis for ADSL
adsl <- dm %>%
  select(-DOMAIN) #Remove DOMAIN as it is not accurate anymore

#Age grouping
cat("Derive AGEGR9 and AGEGR9N...\n")
#Define age groups
agegr1_lookup <- exprs(
  ~condition,             ~AGEGR9,   ~AGEGR9N,
  AGE < 18,               "<18",     1,
  between(AGE, 18, 50), "18-50",     2,
  AGE > 50,               ">50",     3,
  is.na(AGE),         "Missing",     NA
)

#Adding AGEGR9 and AGEGR9N based on AGE from the dm domain
adsl <- adsl %>%
  derive_vars_cat(
    definition = agegr1_lookup
  ) 

test_that("AGEGR9 and AGEGR9N correctly derived", {
  expect_true(all(c("AGEGR9","AGEGR9N") %in% names(adsl)))
  # AGEGR9 should only contain expected values
  expect_true(all(adsl$AGEGR9 %in% c("<18", "18-50", ">50", "Missing", NA)))
  
  # AGEGR9N should only contain expected numeric values
  expect_true(all(adsl$AGEGR9N %in% c(1, 2, 3, NA)))
})

#treatment start day time
cat("Derive treatment start and end day (TRTSDTM and TRTSTML)...\n")
#Derive EXSTDTM and EXENTMF from EXSTDTC and EXENDTC by converting to datetime and imputing the time
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "00:00:00",
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  )  %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "23:59:59",
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  )

test_that("EXSTDTM and EXENDTM correctly derived and check format", {
  expect_true(all(c("EXSTDTM","EXENDTM") %in% names(ex_ext)))
  expect_true(inherits(ex_ext$EXSTDTM, "POSIXct") && inherits(ex_ext$EXENDTM, "POSIXct"))
  
})

#Derived TRTSDTM and TRTSTML from EXSTDTM and EXSTTML respectively
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    #Condition for valid dose
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO")))& !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )  %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    #Condition for valid dose
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

test_that("TRTSDTM and TRTSTML correctly derived and check format", {
  expect_true(all(c("TRTSDTM","TRTEDTM") %in% names(adsl)))
  expect_true(inherits(adsl$TRTSDTM, "POSIXct") && inherits(adsl$TRTEDTM, "POSIXct"))
  
})

cat("Successfully derived treatment start and end day (TRTSDTM and TRTSTML)...\n")

#ITTFL
cat("Derive ITTFL based on ARM...\n")

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N")
  )

test_that("ITTFL correctly derived", {
  expect_true("ITTFL" %in% names(adsl))
  expect_true(all(adsl$ITTFL %in% c("Y", "N", NA)))
  
})

cat("Derive ITTFL successfully...\n")

#Last known alive date as numeric date
cat("Derive Last known alive date (LSTALVDT)...\n")

adsl <- adsl %>%
  #VS.VSSTRESN and VS.VSSTRESC not both missing as well as VS.VSDTC not missing
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        order = exprs(VSDTC,VSSEQ),
        mode = "last",
        condition = !(is.na(VSSTRESN) | is.na(VSSTRESC)) & !is.na(VSDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "M"),
          seq = VSSEQ
        ),
      ),
      #last complete onset date of AE AE.AESTDTC
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC,AESEQ),
        mode = "last",
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        ),
      ),
      #last complete disposition date DS.DSSTDTC
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        ),
      ),
      #last date of treatment administration ADSL.TRTEDTM
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDTM),
        set_values_to = exprs(LSTALVDT = TRTEDTM, seq = 0),
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT)
  )

test_that("LSTALVDT correctly derived", {
  expect_true("LSTALVDT" %in% names(adsl))
  expect_true(inherits(adsl$LSTALVDT, "POSIXct"))
  
})

#Export data to xpt
cat("Exporting data...\n")
write_xpt(adsl, path = "02_output/adsl.xpt")

#End of log file
cat("================================\n")
cat("Script completed at:", format(Sys.time()), "\n")

sink()


