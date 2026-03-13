#Packages
library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)

#Imput data
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl


#Create a summary table of treatment-emergent adverse events (TEAEs).
#- Treatment-emergent AE records will have TRTEMFL == "Y" in pharmaverseadam::adae
#- Rows: AETERM or AESOC
#- Columns: Treatment groups (ACTARM)
#- Cell values: Count (n) and percentage (%)
#- Include total column with all subjects
#-                                Sort by descending frequency

#Preprocessing
adae <- adae |>
  filter(
    # Treatment Emergent Analysis Flag
    TRTEMFL == "Y"
    
  )

#Table
tbl <- adae |>
  tbl_hierarchical(
    variables = c(AETERM, AESOC),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    #overall_row = TRUE,
    #label = "..ard_hierarchical_overall.." ~ "Any SAE"
  ) |>
  add_overall() 

#Sort by descending frequency
tbl <- sort_hierarchical(tbl)

tbl

#Save the data
tbl %>%
  as_gt() %>%
  gt::gtsave("02_output/TEAEs_table.html")

