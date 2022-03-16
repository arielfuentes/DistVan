library(hereR)
set_key("kJ-6fes-iX0tCRAF8fHUVAvubtneFwOmZUPwLjBKa30")
?hereR::route()

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
