# Pakete laden
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

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
    geom_point(color = "#0078bb") +
    geom_line(color = "#0078bb") +
    geom_area(fill = "#0078bb", alpha = 0.5) +
    theme_classic() +
    labs(title = expression("CO"[2]*" Emissionen der deutschen Papierindustrie 1995-2020"),
         subtitle = expression("in Tonnen CO"[2]*" / Tonne Papier"),
         x = NULL,
         y = NULL,
         caption = "Quelle: VDP Leistungsbericht Papier 2017 & 2021")

co2_spez_tidy %>% 
  mutate(Parameter = ifelse(Parameter == "prod_1000t", "Papierproduktion [1000 t]", "Spez. CO2 Emission [t / t Papier]")) %>% 
  ggplot(aes(x = Jahr, y = Wert)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Parameter, ncol = 1, scales = "free_y") +
    labs(title = expression("CO"[2]*" Emissionen der deutschen Papierindustrie 1995-2020"),
         subtitle = expression("Spezifische CO"[2]*" Emission im Vergleich zur produzierten Papiermenge"),
         x = NULL,
         y = NULL,
         caption = "Quelle: VDP Leistungsbericht Papier 2017 & 2021")

co2_total %>% 
  ggplot(aes(x = Jahr, y = mio_t_co2)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "loess") +
    geom_hline(yintercept = 14.14641*0.2, lty = 2) +
    geom_hline(yintercept = 14.14641, lty = 2) +
    labs(title = expression("CO"[2]*" Emissionen der deutschen Papierindustrie 1995-2020"),
         subtitle = expression("in Mio. t CO"[2]),
         x = NULL,
         y = NULL)

# Jetzt hier noch die Zielwerte irgendwie einbauen
co2_total[2,5]*20/100 # da müssen wir hin (absolute CO2 Menge, wenn 1995 Bezugswert wäre)
co2_total[19,5]*100/co2_total[2,5] # da stehen wir 2020. Knappe 84%

# -------------
# Vorhersage
# -------------


