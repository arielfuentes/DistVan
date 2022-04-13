library(mapboxapi)
library(readr)
mb_access_token(read_lines("data/mapbox_token.txt"), 
                install = TRUE, 
                overwrite = T)
readRenviron("~/.Renviron")
Sys.getenv("MAPBOX_PUBLIC_TOKEN")

rt <- lapply(1:length(viajes_lst), 
             function(x) st_drop_geometry(mb_optimized_route(select(pt_dt[[x]], 
                                                                    -c(ID, Itinerario)), roundtrip = F, 
                                           profile = "driving-traffic")$route))
rt %>%
  bind_rows() %>%
  bind_cols(ID_vjs) %>%
  write_excel_csv2("output/rutas.csv")
  

