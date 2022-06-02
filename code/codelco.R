library(readxl)
library(dplyr)
library(tidygeocoder)
codelco <- read_excel("data/CODELCO.xlsx", sheet = 1) %>%
  select(`Dirección Origen`, 
         `Dirección Destino`, 
         `Comuna Origen`, 
         `Comuna Destino`)
codelco <- bind_rows(select(codelco, Address = `Dirección Origen`, City = `Comuna Origen`),
                     select(codelco, Address =`Dirección Destino`, City = `Comuna Destino`)) %>%
  distinct() %>%
  mutate(Address = paste0(Address, ",", City), ID = row_number()) %>%
  select(-City)

codelco_geo <- codelco %>%
  geocode(Address, method = "arcgis", lat = latitude, long = longitude)

codelco_geo <- read_excel("data/codelco_geo 1.xlsx", sheet = "Sheet 1") %>%
  select(-Address)
aeropuerto <- tibble(ID = c(1:237), 
                     latitude = rep(-33.39859965587092, 237), 
                     longitude = rep(-70.79438030778914, 237))
codelco_geo <- bind_rows(codelco_geo, aeropuerto) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32719) %>%
  group_split(ID)
