#Packages

library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)


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

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
lb <- pharmaversesdtm::lb

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)


#Age grouping
agegr1_lookup <- exprs(
  ~condition,             ~AGEGR9,   ~AGEGR9N,
  AGE < 18,               "<18",     1,
  between(AGE, 18, 50), "18-50",     2,
  AGE > 50,               ">50",     3,
  is.na(AGE),         "Missing",     NA
)

dm2 <- dm %>%
  derive_vars_cat(
    definition = agegr1_lookup
  ) 

#treatment start day time

ex2 <- ex %>%
  filter(
    EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))
  ) %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "s",
    time_imputation = "00:00:00",
    flag_imputation = "time",
    min_dates = NULL
  )




