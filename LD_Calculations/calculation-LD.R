#!/usr/bin/Rscript

# Mercè Garí
# http://mercegari.net

#------------------------------------------------------------------------------------------
# Load packages
library(readxl)
library(stringr)
library(tidyverse)
library(chemCal)

#------------------------------------------------------------------------------------------
# Import data from LDLQ for all compounds in 7 different batches
# LDs from the 7 batches were calculated automatically using chemCal package
ldlq.7batch <- tibble(rio::import("method.ldlq.7batches.csv"))

# Calculate the arithmetic mean of the LDs from these 7 batches
ld.7batch <- ldlq.7batch %>%
  dplyr::select(-LQ) %>%
  group_by(Compound) %>%
  summarize(LD = mean(LD, na.rm=TRUE))
ld.7batch

#------------------------------------------------------------------------------------------
# Import data from calibration curves for neonicotinoids
# These compounds were not included in the previous 7 batches calculations
curves.neo <- tibble(rio::import("method.ldlq.neonicotinoids.xlsx"))

# Loop for LD calculation from calibration curves, using chemCal package
Compounds <- unique(curves.neo$Compound)
M <- NULL
for(c in 1:length(unique(curves.neo$Compound))){
  d.now <- curves.neo %>%
    filter(Compound %in% Compounds[c]) %>%
    mutate(A = (Area/AreaIS)/10, # Divide by 10 (=injection volume)
           C = (Concentration/ConcentrationIS)/10) # Divide by 10 (=injection volume)
  curve <- lm(A ~ C, data=d.now) # Apply linear model
  lod <- lod(curve)$C * (120/1000) # Apply lod calculation from chemCal package
  M <- bind_rows(M, tibble(Compound = Compounds[c], 
                           LD = lod))
}

M
ld.neo <- M

#------------------------------------------------------------------------------------------
# Complete list of LDs
limits.of.detection <- bind_rows(ld.7batch, ld.neo)

limits.of.detection
write.csv(limits.of.detection, "LD-method-validation.csv")
