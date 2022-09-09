library(mapboxapi)
library(readr)
library(sf)
mb_access_token(read_lines("data/mapbox_token.txt"), 
                install = TRUE, 
                overwrite = T)
readRenviron("~/.Renviron")
Sys.getenv("MAPBOX_PUBLIC_TOKEN")

rt <- lapply(1:length(sodex_reset_sf), 
              function(x) st_drop_geometry(mb_optimized_route(select(sodex_reset_sf[[x]], -Dir), 
                                                              profile = "driving-traffic")$route))

rt_bind <- bind_rows(rt) %>%
  bind_cols(select(bind_rows(sodex_reset_lst), Origen, Destino)) %>%
  as_tibble()
