library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)

viajes <- read_xlsx("data/Tabla de viaje enero y febrero.xlsx") %>%
  select(ID, Georreferencia, Itinerario) %>%
  filter(!is.na(.) | Georreferencia != "AGREGADO") %>%
  separate(Georreferencia, into = c("y", "x"), sep = ",", convert = T) %>%
  mutate(y = as.numeric(y), Itinerario = str_to_upper(Itinerario)) %>%
  na.omit()
Aeropuerto <- tibble(ID = "Aeropuerto", 
                     y = -33.39859965587092, 
                     x = -70.79438030778914, 
                     Itinerario = "Aeropuerto")

viajes_lst <- group_split(viajes, ID)

dt_lst <- lapply(viajes_lst, function(x) if (unique(x$Itinerario)  == "RECOGIDA") {
  bind_rows(x, Aeropuerto) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    st_transform(32719)
} else {
  bind_rows(Aeropuerto, x) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    st_transform(32719)
}) 

pt_dt <- lapply(1:length(viajes_lst), 
                 function(x) if (unique(viajes_lst[[x]]$Itinerario) == "RECOGIDA") {
                   bind_rows(viajes_lst[[x]], Aeropuerto) %>%
                     st_as_sf(coords = c("x", "y"), crs = 4326) %>%
                     st_transform(32719)
                 } else {
                   bind_rows(Aeropuerto, viajes_lst[[x]]) %>%
                     st_as_sf(coords = c("x", "y"), crs = 4326) %>%
                     st_transform(32719)
                 }  )

ID_vjs <- viajes_lst %>% bind_rows() %>% distinct(ID)
