library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(sfnetworks)
library(tidygraph)
library(tmap)
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
pt_dt <- if (unique(viajes_lst[[1]]$Itinerario) == "RECOGIDA") {
  bind_rows(viajes_lst[[1]], Aeropuerto) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    st_transform(32719)
} else {
  bind_rows(Aeropuerto, viajes_lst[[1]]) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    st_transform(32719)
}  
pt_dt2 <- lapply(1:length(viajes_lst), 
                 function(x) if (unique(viajes_lst[[x]]$Itinerario) == "RECOGIDA") {
                   bind_rows(viajes_lst[[x]], Aeropuerto) %>%
                     st_as_sf(coords = c("x", "y"), crs = 4326) %>%
                     st_transform(32719)
                 } else {
                   bind_rows(Aeropuerto, viajes_lst[[x]]) %>%
                     st_as_sf(coords = c("x", "y"), crs = 4326) %>%
                     st_transform(32719)
                 }  )
#network
vial <- st_read("data/redvial/Capas_Base.shp") %>%
  st_transform(32719) %>%
  select(-c("NOMBRE_VIA", "CLASE_COMU", "SHAPE_Leng")) %>%
  filter(!CLASE_URBA %in% c("PRIVADO") & REGION == 13)

vial_filter <- st_filter(vial, 
                   st_buffer(st_convex_hull(x = st_union(pt_dt)), 5000), 
                   .predicate = st_intersects)

vialnet <- vial_filter %>%
as_sfnetwork(directed = F) %>% 
  activate("edges") %>%
  mutate(weight = edge_length())

vial_short1 <- vialnet %>%
  convert(to_spatial_shortest_paths,
          from = pt_dt[1,], to = pt_dt[2,]) %>%
  st_as_sf()
vial_short2 <- vialnet %>%
  convert(to_spatial_shortest_paths,
          from = pt_dt[2,], to = pt_dt[3,]) %>%
  st_as_sf()
vial_short3 <- vialnet %>%
  convert(to_spatial_shortest_paths,
          from = pt_dt[3,], to = pt_dt[4,]) %>%
  st_as_sf()
vial_short4 <- vialnet %>%
  convert(to_spatial_shortest_paths,
          from = pt_dt[4,], to = pt_dt[5,]) %>%
  st_as_sf()
vial_short5 <- vialnet %>%
  convert(to_spatial_shortest_paths,
          from = pt_dt[5,], to = pt_dt[6,]) %>%
  st_as_sf()
vial_short <- bind_rows(vial_short1,
                        vial_short2,
                        vial_short3,
                        vial_short4,
                        vial_short5)
  
    tm_shape(pt_dt) +
    tm_dots(col = "red", size = 2) +
      tm_shape(vial_short) +
      tm_lines()
    
lapply(1:5, function(x) vialnet %>%
         convert(to_spatial_shortest_paths,
                 from = pt_dt[x,], to = pt_dt[x+1,]) %>%
         st_as_sf()) %>%
  bind_rows()
lapply(pt_dt2, function(z) lapply(1:length(z) - 1, function(y) vialnet %>%
                                    convert(to_spatial_shortest_paths,
                                            from = pt_dt[y,], to = pt_dt[y+1,]) %>%
                                    st_as_sf())) %>%
         bind_rows()
x <- st_network_paths(vialnet, from = pt_dt[1:5,], to = pt_dt[6,])
x %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

plot_path = function(node_path) {
  vialnet %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

x %>%
  pull(node_paths) %>%
  walk(plot_path)
###################################
roads_clean = vialnet %>% 
  convert(to_spatial_subdivision, .clean = TRUE) %>% 
  convert(to_spatial_smooth, .clean = TRUE) 

paths = mapply(
  st_network_paths,
  from = pt_dt,
  to = pt_dt,
  MoreArgs = list(x = roads_clean)
)["node_paths", ] %>%
  unlist(recursive = FALSE)

vialnet %>%
  activate("nodes") %>%
  slice(x %>%
          pull(node_paths))

#################################################
x2 <- x %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

x3 <- vialnet %>%
  activate("nodes") %>%
  slice(x2) %>%
  activate("edges") %>%
  st_as_sf()

tm_shape(x3) +
  tm_lines(col = "black", lwd = 2) +
  tm_shape(pt_dt) +
  tm_dots(col = "red", size = 1.5) +
  tm_shape(vial_filter) +
  tm_lines(col = "grey")
########################################################
