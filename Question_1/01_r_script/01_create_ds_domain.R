#Question 1: SDTM DS Domain Creation using {sdtm.oak}

#Objective: Create an SDTM Disposition (DS) domain dataset from raw clinical trial data using the {sdtm.oak}.

#Start log file
log_file <- paste0("04_logs/adsl_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)

cat("Script started at:", format(Sys.time()), "\n")
cat("================================\n")

#Load necessary packages
cat("Loading necessary packages...\n")
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)
library(haven)
library(testthat)
cat("Packages loaded successfully...\n")

#Loading the raw data
cat("Loading raw data...\n")
ds_raw <- pharmaverseraw::ds_raw
cat("Raw data loaded successfully...\n")

#Loading the study controlled terminology
cat("Loading study CT...\n")
study_ct <- read.csv("00_raw_data/sdtm_ct.csv")
cat("Study CT loaded successfully...\n")

#Create oak ID variables
cat("Creating oak ID variables...\n")
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

test_that("oak_id_vars are correctly generated", {
  expect_equal(nrow(ds_raw), nrow(pharmaverseraw::ds_raw))  # no rows lost
  expect_false(any(is.na(ds_raw$oak_id)))                   # no missing IDs
})
cat("oak ID variables created successfully...\n")


#Date subject competed or discontinued from study period
cat("Mapping DSSTDTC from DSSTDAT in iso8601 format...\n")
ds <- assign_datetime(
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  tgt_var = "DSSTDTC",
  id_vars = oak_id_vars(),  
  raw_fmt = "m-d-y"
)

test_that("DSSTDTC is correctly created and has the right format", {
  expect_true("DSSTDTC" %in% names(ds), #DSSTDTC column should be present in the ds dataframe
              label = "DSSTDTC column exists in ds")
  expect_true(class(ds$DSSTDTC) == "iso8601", #format of DSSTDTC should be iso8601
              label = "DSSTDTC is in iso8601 format")
})
cat("DSSTDTC mapped successfully from DSSTDAT in iso8601 format...\n")


#Creating Standardized Disposition Term (DSDECOD)
cat("Mapping DSDECOD from IT.DSDECOD...\n")
ds <- ds %>%
  assign_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)), #When OTHERSP is null
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
    )

test_that("DSDECOD is correctly created", {
  # DSDECOD column should be present in the ds dataframe
  expect_true("DSDECOD" %in% names(ds))
})
  

#Creating DSCAT based on whether IT.DSDECOD == "Randomized" or not
cat("Mapping DSCAT from IT.DSDECOD...\n")
ds <- ds %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD != "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    id_vars = oak_id_vars()
  )

test_that("DSCAT is correctly created", {
  # DSCAT column should be present in the ds dataframe
  expect_true("DSCAT" %in% names(ds))
  expect_true(all(ds$DSCAT %in% c("PROTOCOL MILESTONE", "DISPOSITION EVENT", NA)))
})


#If OTHERSP is not null map OTHERSP to both DSDECOD and DSTERM
cat("Mapping OTHERSP to DSDECOD and DSTERM if OTHERSP not null...\n")
ds <- ds %>%
  assign_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)), #OTHERSP should not be null
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)), #OTHERSP should not be null
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

test_that("DSSTERM is correctly created", {
  # DSTERM column should be present in the ds dataframe
  expect_true("DSTERM" %in% names(ds))
  
})


#If OTHERSP is not null map DSCAT == "OTHER EVENT"
cat("Mapping OTHER EVENT TO DSCAT IF OTHERSP IS NOT NULL...\n")
ds <- ds %>%
 hardcode_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)), #OTHERSP should not be null
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    id_vars = oak_id_vars()
  )

test_that("DSCAT has additional imput", {
  expect_true(all(ds$DSCAT %in% c("PROTOCOL MILESTONE", "DISPOSITION EVENT", "OTHER EVENT")))
})

#Reported term for Disposition Event (DSTERM)
ds <- ds %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)), #OTHERSP should be null
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

#Date and time of collection
ds <- ds %>%
  assign_datetime(
  raw_dat = ds_raw,
  raw_var = c("DSDTCOL", "DSTMCOL"),
  tgt_var = "DSDTC",
  id_vars = oak_id_vars(),  
  raw_fmt = c("m-d-y", "H:M")
)

test_that("DSDTC is correctly created and has the right format", {
  expect_true("DSDTC" %in% names(ds)) #DSSTDTC column should be present in the ds dataframe
  expect_true(class(ds$DSSTDTC) == "iso8601") #format of DSSTDTC should be iso8601
})

#Create SDTM derived variables
cat("Deriving remaining variables...\n")

#Manually creating VISITNUM, as I didn't find how to do it the right way
visit_lookup <- tibble::tribble(
  ~INSTANCE,            ~VISITNUM,
  "Screening 1",        1,
  "Baseline",           2,
  "Week 2",             3,
  "Week 4",             4,
  "Week 6",             5,
  "Week 8",             6,
  "Week 12",            7,
  "Week 16",            8,
  "Week 20",            9,
  "Week 24",            10,
  "Week 26",            11,
  "Retrieval",          12,
  "Ambul Ecg Removal",  13,
  "Unscheduled 1.1",    101, #Unscheduled visits have a higher number
  "Unscheduled 4.1",    102,
  "Unscheduled 5.1",    103,
  "Unscheduled 6.1",    104,
  "Unscheduled 8.2",    105,
  "Unscheduled 13.1",   106
)

#Adding the VISITNUM to the raw data
ds_raw <- ds_raw %>%
  dplyr::left_join(visit_lookup, by = c("INSTANCE" = "INSTANCE"))

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY, #STUDYID is simply derived from the STUDY column of the raw data
    DOMAIN = "DS", #The domain is manually assigned to DS
    USUBJID = paste0(STUDYID, "-",ds_raw$PATNUM), #USUBJID is derived by combining the STUDYID and the PATNUM
    VISIT = ds_raw$INSTANCE, #VISIT is derived fromt the INSTANCE column
    VISITNUM = ds_raw$VISITNUM
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", 
    "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )

test_that("DS domain contain the expected variables", {
  expect_true(all(c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD",
                    "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY") %in% names(ds)))
})

cat("Remaining variables derived successfully...\n")
cat("DS domain complete...\n")

#Export the SDTM formatted as xpt
write_xpt(ds, path = "02_output/ds.xpt")


#End of log file
cat("================================\n")
cat("Script completed at:", format(Sys.time()), "\n")

sink()

