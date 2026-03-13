#Question 1: TLG - Adverse Events Reporting

#Objective: Create outputs for adverse events summary using the ADAE dataset and {gtsummary}.

#Start log file
log_file <- paste0("03_logs/TLG_table_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)

cat("Script started at:", format(Sys.time()), "\n")
cat("================================\n")

#Load necessary packages
cat("Loading necessary packages...\n")
library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)
cat("Packages loaded successfully...\n")

#Loading imput data
cat("Loading adam data...\n")
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl
cat("Adam data loaded successfully...\n")


#Treatment-emergent AE records will have TRTEMFL == "Y" in pharmaverseadam::adae
adae <- adae |>
  filter(
    TRTEMFL == "Y"
  )

#Create table
tbl <- adae |>
  tbl_hierarchical(
    variables = c(AETERM, AESOC), #Rows: AETERM or AESOC
    by = ACTARM,#Columns: Treatment groups (ACTARM)
    id = USUBJID,
    denominator = adsl,
  ) |>
  #Include total column with all subjects
  add_overall() 

#Sort by descending frequency
tbl <- sort_hierarchical(tbl)
cat("Table created successfully...\n")


#Save the data
cat("Saving Table...\n")
tbl %>%
  as_gt() %>%
  gt::gtsave("02_output/TEAEs_table.html")

#End of log file
cat("================================\n")
cat("Script completed at:", format(Sys.time()), "\n")

sink()
