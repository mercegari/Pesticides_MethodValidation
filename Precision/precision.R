#!/usr/bin/Rscript

# Mercè Garí
# http://mercegari.net

#------------------------------------------------------------------------------------------
# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)


#------------------------------------------------------------------------------------------
# Analysis on Reproducibility
# REPRODUCIBILITY: QCs injected 5 times in a row
reproducibility.table <- tibble(rio::import("Reproducibility.xlsx")) %>% 
  group_by(Type, Compound) %>%
  summarize(mean = mean(Concentration, na.rm=TRUE),
            sd = sd(Concentration, na.rm=TRUE),
            CV = sd/mean*100) %>% # Calculate Coefficient of Variation in %
  ungroup() %>%
  dplyr::select(-mean, -sd) %>%
  spread(Compound, CV)
reproducibility.table

write.csv(reproducibility.table, file="reproducibility-table.csv")

# Plot on reproducibility
reproducibility.table %>%
  gather(Compound, CV, -Type) %>%
  ggplot(aes(y=reorder(Compound, CV, min), x=CV, color=Type)) +
  geom_point() +
  facet_wrap(~Type, scales="free") +
  theme_bw() +
  xlab("CV (%)") + ylab("") +
  xlim(0, 25)
ggsave("Reproducibility.png", height=4, width=6)

#------------------------------------------------------------------------------------------
# Analysis on Repeatability
# REPEATABILITY: QCs injected once in different days
repeatability.table <- tibble(rio::import("Repeatability.xlsx")) %>%
  group_by(Type, Compound) %>%
  summarize(mean = mean(Concentration, na.rm=TRUE),
            sd = sd(Concentration, na.rm=TRUE),
            CV = sd/mean*100) %>% # Calculate Coefficient of Variation in %
  ungroup() %>%
  dplyr::select(-mean, -sd) %>%
  spread(Compound, CV)
repeatability.table

write.csv(repeatability.table, file="repeatability-table.csv")

# Plot on repeatability
repeatability.table  %>%
  gather(Compound, CV, -Type) %>%
  ggplot(aes(y=reorder(Compound, CV, min), x=CV, color=Type)) +
  geom_point() +
  facet_wrap(~Type, scales="free") +
  theme_bw() +
  xlab("CV (%)") + ylab("") +
  xlim(0, 27)
ggsave("Repeatability.png", height=4, width=6)

