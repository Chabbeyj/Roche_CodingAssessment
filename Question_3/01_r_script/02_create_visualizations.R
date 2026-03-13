#Packages
library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)
library(ggplot2)

#Imput data
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

#AE severity (captured by AESEV) by treatment (ACTARM)

#Barplot
AE_severity_plot <- ggplot(adae, aes(ACTARM)) +
  geom_bar(aes(fill = AESEV)) +
  ylab("Count of AEs") +
  xlab("Treatment Arm")
AE_severity_plot

#Heatmap


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
g <-  ggplot(ae_freq %>% mutate(AETERM = reorder(AETERM, pct)), aes(x = pct, y = AETERM)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high),
    height = 0.3
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) + theme_bw()

g



