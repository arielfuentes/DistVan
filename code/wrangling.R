library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(sfnetworks)
library(tidygraph)
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

 
pt_dt <- if (unique(viajes_lst[[20]]$Itinerario) == "RECOGIDA") {
  bind_rows(viajes_lst[[20]], Aeropuerto) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    st_transform(32719)
} else {
  bind_rows(Aeropuerto, viajes_lst[[20]]) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    st_transform(32719)
}  

#network
vial <- st_read("data/redvial/Capas_Base.shp") %>%
  st_transform(32719) %>%
  select(-c("NOMBRE_VIA", "CLASE_COMU", "SHAPE_Leng")) %>%
  filter(!CLASE_URBA %in% c("PRIVADO") & REGION == 13)

vial2 <- st_filter(vial, 
                   st_convex_hull(x = st_union(pt_dt)), 
                   .predicate = st_intersects)

vial2 <- vial2 %>%
as_sfnetwork(directed = T) %>% 
  activate("edges") %>%
  mutate(weight = edge_length())

vial2 %>%
  convert(to_spatial_shortest_paths,
          from = pt_dt[1,], to = pt_dt[2,])
st_network_paths(vial2, from = pt_dt[1,], to = pt_dt[2,])
