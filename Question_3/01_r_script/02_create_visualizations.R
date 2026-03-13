#Question 1: TLG - Adverse Events Reporting

#Objective: Create outputs for adverse events summary using the ADAE dataset and {gtsummary}.

#Start log file
log_file <- paste0("03_logs/TLG_visualization_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)

cat("Script started at:", format(Sys.time()), "\n")
cat("================================\n")

#Load necessary packages
cat("Loading necessary packages...\n")
library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)
library(ggplot2)
cat("Packages loaded successfully...\n")

#Loading imput data
cat("Loading adam data...\n")
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl
cat("Adam data loaded successfully...\n")

#AE severity (captured by AESEV) by treatment (ACTARM)
#Barplot
cat("Plotting AE severity barplots...\n")
AE_severity_plot <- ggplot(adae, aes(ACTARM)) +
  geom_bar(aes(fill = AESEV)) +
  ylab("Count of AEs") +
  xlab("Treatment Arm")
AE_severity_plot

#Top 10 most frequent AEs (AETERM)
#Deriving n_subjects from ADSL
n_subjects <- adsl %>% 
  filter(SAFFL == "Y") %>%
  nrow()

#Calculate frequencies and CIs:
ae_freq <- adae %>%
  distinct(USUBJID, AETERM) %>%
  group_by(AETERM) %>%
  summarise(n = n()) %>%
  mutate(
    pct     = n / n_subjects,
    ci_low  = qbeta(0.025, n, n_subjects - n + 1),
    ci_high = qbeta(0.975, n + 1, n_subjects - n)
  ) %>%
  arrange(desc(pct)) %>%
  slice_head(n = 10)

#Plot
cat("Plotting CI AE plot...\n")
CI_AE_plot <-  ggplot(ae_freq %>% mutate(AETERM = reorder(AETERM, pct)), aes(x = pct, y = AETERM)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high),
    width = 0.3
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) + theme_bw()

CI_AE_plot

#Save the output
cat("Saving plots...\n")
ggsave("02_output/AE_severity_plot.png", AE_severity_plot)
ggsave("02_output/CI_AE_plot.png", CI_AE_plot)

#End of log file
cat("================================\n")
cat("Script completed at:", format(Sys.time()), "\n")

sink()
