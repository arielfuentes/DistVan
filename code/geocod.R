library(tidygeocoder)
library(readxl)
library(dplyr)

Acciona <- read_xlsx("data/Georreferencia.xlsx", sheet = "Acciona", skip = 2) %>%
  mutate(`Dirección` = paste(`Dirección`, " Chile", sep = ",")) 
Acciona_geo2 <- Acciona %>%
  geocode(`Dirección`, method = "osm", lat = latitude, long = longitude)


LATAM <- read_xlsx("data/Georreferencia.xlsx", sheet = "LATAM", skip = 2) %>%
  mutate(DIRECCION = paste(DIRECCION, COMUNA," Chile", sep = ",")) 
LATAM_geo <- LATAM %>%
  geocode(DIRECCION, method = "osm", lat = latitude, long = longitude)

Codelco <- read_xlsx("data/Georreferencia.xlsx", sheet = "Codelco", skip = 2) %>%
  mutate(`Dirección Destino` = paste(`Dirección Destino`, `Comuna Destino`," Chile", sep = ",")) 
LATAM_geo <- LATAM %>%
  geocode(DIRECCION, method = "osm", lat = latitude, long = longitude)