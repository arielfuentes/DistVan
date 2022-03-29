library(hereR)
set_key("kJ-6fes-iX0tCRAF8fHUVAvubtneFwOmZUPwLjBKa30")
?hereR::route_matrix()

to <- poi[rep(seq_len(nrow(poi)), nrow(poi)), ]
from <- poi[rep(seq_len(nrow(poi)), each = nrow(poi)), ]
idx <- apply(to != from, any, MARGIN = 1)
to <- to[idx, ]
from <- from[idx, ]

# Routing
routes <- route(
  origin = from, destination = to, results = 3,
  transport_mode = "car", url_only = F
)

rt_mtx <- route_matrix(origin = from, 
             destination = to,
             routing_mode = "fast",
             transport_mode = "car", 
             traffic = F)
###############################
to <- pt_dt[5,]
from <- pt_dt[6,]
# idx <- apply(to != from, any, MARGIN = 1)
# to <- to[idx, ]
# from <- from[idx, ]
rt <- route(origin = from, 
      destination = to, 
      results = 1, 
      routing_mode = "fast",
      transport_mode = "car", 
      traffic = F)

tm_shape(rt) + tm_lines()

route_matrix(origin = pt_dt, 
             # destination = pt_dt[6, ], 
             routing_mode = "short",
             transport_mode = "car", 
             traffic = F) %>%
  select(- c("request_id", "departure", "arrival", "error_code")) %>%
  mutate(diagnl = if_else(orig_id == dest_id - 1, 1, 0)) %>%
  filter(diagnl == 1) %>%
  summarise(d = sum(distance)/1000, t = sum(duration)/60)
################################################################
rts_len <- lapply(1:length(dt_lst), function(x) route_matrix(origin = dt_lst[[x]], 
                                                   routing_mode = "short",
                                                   transport_mode = "car", 
                                                   traffic = F) %>%
                    select(- c("request_id", "departure", "arrival", "error_code")) %>%
                    mutate(diagnl = if_else(orig_id == dest_id - 1, 1, 0)) %>%
                    filter(diagnl == 1) %>%
                    summarise(d = sum(distance)/1000, t = sum(duration)/60))
#######################################################################################
usu <- read_xlsx("data/Tabla de viaje enero y febrero.xlsx") %>%
  select(Nombre, Georreferencia) %>%
  filter(!is.na(.) | Georreferencia != "AGREGADO") %>%
  separate(Georreferencia, into = c("y", "x"), sep = ",", convert = T) %>%
  mutate(y = as.numeric(y)) %>%
  na.omit() %>%
  bind_rows(tibble(Nombre = "Aeropuerto", 
            y = -33.39859965587092, 
            x = -70.79438030778914)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(32719)

rt_mtx <- route_matrix(origin = usu, 
                       destination = usu,
                       routing_mode = "fast",
                       transport_mode = "car", 
                       traffic = F)
