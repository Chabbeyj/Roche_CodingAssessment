#Question 1 of the Roche coding assessment

#Load necessary packages
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)
library(haven)

#Loading the data
study_ct <- read.csv("00_raw_data/sdtm_ct.csv")
ds_raw <- pharmaverseraw::ds_raw


#Create oak ID variables
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )


#Date subject competed or discontinued from study period
ds <- assign_datetime(
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  tgt_var = "DSSTDTC",
  id_vars = oak_id_vars(),  
  raw_fmt = "m-d-y"
)


#Reason for completion/discontinuation
ds <- ds %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
    )
  


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



#Reason for completion/discontinuation is something else
ds <- ds %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )



ds <- ds %>%
 hardcode_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    id_vars = oak_id_vars()
  )

#Reported term for Disposition Event
ds <- ds %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
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


#Create SDTM derived variables

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
  "Unscheduled 1.1",    101,
  "Unscheduled 4.1",    102,
  "Unscheduled 5.1",    103,
  "Unscheduled 6.1",    104,
  "Unscheduled 8.2",    105,
  "Unscheduled 13.1",   106
)

ds_raw <- ds_raw %>%
  dplyr::left_join(visit_lookup, by = c("INSTANCE" = "INSTANCE"))

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    VISIT = ds_raw$INSTANCE,
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


#Export the SDTM formatted 

write_xpt(ds, path = "02_output/ds.xpt")

