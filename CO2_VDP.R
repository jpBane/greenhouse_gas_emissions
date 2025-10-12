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
max_jahr = max(co2_spez$Jahr)

co2_spez_tidy %>% 
  mutate(Parameter = recode(Parameter, 
                            "t_co2_t" = "Spez. CO2 Emission [t / t Papier]",
                            "prod_1000t" = "Papierproduktion [1000 t]",
                            "n_werke" = "Anzahl Werke / Standorte",
                            "n_pm" = "Anzahl Papiermaschinen")) %>% 
  ggplot(aes(x = Jahr, y = Wert)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Parameter, ncol = 1, scales = "free_y") +
    labs(title = bquote("Spezifische CO"[2]*" Emissionen der deutschen Papierindustrie 1995-"*.(max_jahr)),
         subtitle = expression("Im Kontext verschiedener Rahmenbedingungen"),
         x = NULL,
         y = NULL,
         caption = "Quelle: VDP Leistungsbericht Papier 2017, 2021-2025")

reference_1995 = co2_total$mio_t_co2[2]

co2_total %>% 
  ggplot(aes(x = Jahr, y = mio_t_co2)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "loess") +
    geom_hline(yintercept = reference_1995, lty = 2) +
    geom_text(aes(x = max(Jahr), y = reference_1995, label = "100%", vjust = -1)) +
    geom_hline(yintercept = reference_1995*0.2, lty = 2) +
    geom_text(aes(x = max(Jahr), y = reference_1995*0.2, label = "-80%", vjust = -1)) +
    labs(title = bquote("CO"[2]*" Emissionen der deutschen Papierindustrie 1995-"*.(max_jahr)),
         subtitle = expression("in Mio. t CO"[2]),
         x = NULL,
         y = NULL)

# Jetzt hier noch die Zielwerte irgendwie einbauen
co2_total[2,5]*20/100 # da müssen wir hin (absolute CO2 Menge, wenn 1995 Bezugswert wäre)
co2_total[19,5]*100/co2_total[2,5] # da stehen wir 2020. Knappe 84%

# -------------
# Vorhersage
# -------------

co2_total %>% 
  filter(Jahr > 2010) %>% 
  add_row(Jahr = 2050) %>% 
  ggplot(aes(x = Jahr, y = mio_t_co2)) +
    geom_point() +
    stat_smooth(method = "lm", fullrange = T) +
    geom_hline(yintercept = reference_1995, lty = 2) +
    geom_text(aes(x = max(Jahr), y = reference_1995, label = "100%", vjust = -1)) +
    geom_hline(yintercept = reference_1995*0.2, lty = 2) +
    geom_text(aes(x = max(Jahr), y = reference_1995*0.2, label = "-80%", vjust = -1)) +
    labs(title = expression("Prognose zukünftiger CO"[2]*" Emissionen"),
         subtitle = "Lineares Modell basierend auf Daten seit 2011")
