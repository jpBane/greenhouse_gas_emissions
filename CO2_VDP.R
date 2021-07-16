# Pakete laden
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Daten einlesen
co2_spez <- read_csv2("CO2_VDP.txt")

# Daten aufbereiten
co2_spez_tidy <- co2_spez %>% 
  gather(-Jahr, key = "Parameter", value = "Wert")

co2_total <- co2_spez %>% 
  mutate(t_co2 = prod_1000t*1000*t_co2_t,
         mio_t_co2 = t_co2/10^6)

# Daten visualisieren
co2_spez %>% 
  ggplot(aes(x = Jahr, y = t_co2_t)) +
    geom_point() +
    geom_line() +
    labs(title = expression("CO"[2]*" Emissionen der deutschen Papierindustrie 1995-2020"),
         subtitle = expression("in Tonnen CO"[2]*" / Tonne Papier"),
         x = NULL,
         y = NULL,
         caption = "Quelle: VDP Leistungsbericht Papier 2017 & 2021")

co2_spez_tidy %>% 
  ggplot(aes(x = Jahr, y = Wert)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Parameter, ncol = 1, scales = "free_y") +
    labs(title = "CO2 Emissionen der deutschen Papierindustrie")

co2_total %>% 
  ggplot(aes(x = Jahr, y = mio_t_co2)) +
    geom_point() +
    geom_line() +
    labs(title = "CO2 Emissionen der deutschen Papierindustrie",
         subtitle = "in absoluten Mengen",
         y = "Mio. t CO2")

# Jetzt hier noch die Zielwerte irgendwie einbauen
co2_total[2,5]*20/100 # da müssen wir hin (absolute CO2 Menge, wenn 1995 Bezugswert wäre)
co2_total[19,5]*100/co2_total[2,5] # da stehen wir 2020. Knappe 84%
