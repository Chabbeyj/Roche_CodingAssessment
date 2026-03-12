#Packages

library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(haven)


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

#DM as basis for ADSL

#Load the data
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



adsl <- dm %>%
  select(-DOMAIN)


#Age grouping
agegr1_lookup <- exprs(
  ~condition,             ~AGEGR9,   ~AGEGR9N,
  AGE < 18,               "<18",     1,
  between(AGE, 18, 50), "18-50",     2,
  AGE > 50,               ">50",     3,
  is.na(AGE),         "Missing",     NA
)

adsl <- adsl %>%
  derive_vars_cat(
    definition = agegr1_lookup
  ) 

#treatment start day time

#Convert EXSTDTC to datetime and impute missing time
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
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  ) #Issue with the flag imputation ?


adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
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
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

#ITTFL
adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N")
  )





#Last known alive date as numeric date

#1: VS.VSSTRESN and VS.VSSTRESC not both missing as well as VS.VSDTC not missing
#2: last complete onset date of AE AE.AESTDTC
#3. last complete disposition date DS.DSSTDTC
#4: last date of treatment administration ADSL.TRTEDTM



adsl <- adsl %>%
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
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC,AESEQ),
        mode = "last",
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "M"),
          seq = AESEQ
        ),
      ),
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "M"),
          seq = DSSEQ
        ),
      ),
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


#Export data
write_xpt(adsl, path = "02_output/adsl.xpt")




