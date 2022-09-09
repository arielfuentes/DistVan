library(readxl)
library(dplyr)
library(tidygeocoder)
library(readr)
sodex <- read_excel("data/Consolidados de viajes Sodexo.xlsx", sheet = 1) %>%
  select(c(Origen, Destino, `Comuna Origen`, `Comuna Destino`))
sodex <- bind_rows(select(sodex, Address = Origen, City = `Comuna Origen`),
                   select(sodex, Address = Origen, City = `Comuna Origen`)) %>%
  distinct() %>%
  mutate(Address = paste0(Address, ",", City), ID = row_number()) %>%
  select(-City)
sodex_geo <- sodex %>%
  geocode(Address, method = "arcgis", lat = latitude, long = longitude) %>%
  na.omit()
sodex_geo2 <- filter(sodex_geo, latitude <= -33 & longitude <= -70)

write_excel_csv(sodex_geo, "data/sodex_coor.csv")

sodex_reset <- read_excel("data/Consolidados de viajes Sodexo.xlsx", sheet = 1) %>%
  select(c(Origen, Destino, `Comuna Origen`, `Comuna Destino`)) %>%
  mutate(Origen = paste0(Origen, ",", `Comuna Origen`), 
         Destino = paste0(Destino, ",", `Comuna Destino`)) %>%
  select(Origen, Destino) %>%
  left_join(sodex_geo2, by = c("Origen" = "Address")) %>%
  select(Origen, Destino, lat_Or = latitude, long_Or = longitude) %>%
  left_join(sodex_geo2, by = c("Destino" = "Address")) %>%
  select(Origen, Destino, lat_Or, long_Or, lat_Des = latitude, long_Des = longitude) %>%
  distinct() %>%
  na.omit() %>%
  mutate(ID = row_number())

sodex_reset_lst <- group_split(sodex_reset, ID)
sodex_reset_sf <- lapply(sodex_reset_lst, 
                         function(x) st_as_sf(bind_rows(select(x, Dir = Origen, lat = lat_Or, long = long_Or),
                                                        select(x, Dir = Destino, lat = lat_Des, long = long_Des)), 
                                              coords = c("long", "lat"), 
                                              crs = 4326))
