# Pakete laden
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Daten einlesen
ghg_emissions_sector <- read_tsv("env_air_gge.tsv.gz")

# Daten vorbereiten
summary(ghg_emissions_sector)
str(ghg_emissions_sector)

ghg_emissions_sector_tidy <- ghg_emissions_sector %>% 
  separate(col = `unit,airpol,src_crf,geo\\time`, 
           into = c("unit", "airpol", "src_crf", "geo"), 
           sep = ",") %>% 
  mutate(unit = as.factor(unit),
         airpol = as.factor(airpol),
         src_crf = as.factor(src_crf),
         geo = as.factor(geo)) %>% 
  gather(-c("unit", "airpol", "src_crf", "geo"), key = "year", value = "ghg_emissions") %>% 
  mutate(year = as.numeric(year),
         ghg_emissions = as.numeric(ghg_emissions))
  
# Daten visualisieren
summary(ghg_emissions_sector_tidy)

ghg_emissions_sector_tidy %>% 
  filter(unit == "THS_T", 
#         year == 2019,
         airpol == "GHG",
         geo %in% c("DE", "ES", "FI", "SE"),
         src_crf == "CRF1A2D") %>% 
  ggplot(aes(x = year, y = ghg_emissions)) +
    geom_line() +
    facet_wrap(~geo, ncol = 1, scales = "free_y") +
    labs(title = "Treibhausgasemissionen",
         subtitle = "Durch Verbrennung von Brennstoffen in der Zellstoff- und Papierindustrie")
