# Minnesota county choropleth with sf + leaflet
library(sf)
library(leaflet)
library(dplyr)
library(tidyverse)
library(ggspatial)
library(janitor)
library(readxl)

# 1. read in ovio raw data and prep for merging with the spatial map data:
sampling_data_raw<-read_xlsx("data/Ovi o_Sup1_081126.xlsx") %>% 
  mutate(across(everything(),  trimws, which = "both"))

#clean data
clean_data<- sampling_data_raw %>% 
  row_to_names(3) %>%
  clean_names() %>% 
  filter(grepl(",",county_state)) %>% #necessary to drop unknown and NA
  separate_wider_delim(cols="county_state",delim=",",names = c("county","state")) %>%
  filter(state==" MN")


#create a county vector that is county counts
county_vals<-clean_data %>% 
  group_by(county) %>% 
  summarize(n=n()) #%>%
  #mutate(county=str_to_lower(county))

# county_vals <- c(
#   kanabec = 9,
#   ramsey  = 11,
#   stearns = 0
  # ... add the rest; "st louis", "St. Louis", "saint louis" all match
#)

# ---- 2. County outlines ----
# Census cartographic boundary file (all US counties, 1:500k), filtered to MN (STATEFP 27).
# Swap in your own shapefile path here if you prefer a different source.
shp_path <- "data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"

counties <- st_read(shp_path, quiet = TRUE) %>% 
  filter(STATEFP==27) #filter to mn counties only


# ---- 3. Join values to polygons ----
norm_name <- function(x) {
  x <- tolower(x)
  x <- sub("^saint", "st", x)
  gsub("[^a-z]", "", x)
}

#transform ovio surveillance data to df and drop the one borderline:
vals_df <- data.frame(key   = norm_name(county_vals$county), #normalized names from county colum
                      value = county_vals$n) %>% #values from n column 
  filter(key!="goodhuewabasha")


#join the data:
counties <- counties |>
  mutate(key = norm_name(NAME)) |>
  left_join(vals_df, by = "key") |>
  st_transform(26915)   %>%            # changing this 
  replace_na(list("value"=0)) #this fixes the color scaling later

# Flag typos / names that didn't match a county
unmatched <- setdiff(vals_df$key, counties$key)
if (length(unmatched) > 0) {
  warning("No county match for: ", paste(unmatched, collapse = ", "))
}

# ---- 4. Leaflet map ----


# m <- leaflet(counties) |>
#   #addProviderTiles(providers$CartoDB.Positron) |>
#   addPolygons(
#     fillColor   = ~pal(value),
#     fillOpacity = 0.8,
#     color       = "black",
#     weight      = 1,
#     label       = ~sprintf("%s: %s", NAME,
#                            ifelse(is.na(value), "no data", value)),
#     highlightOptions = highlightOptions(weight = 2, color = "#333333",
#                                         bringToFront = TRUE)
#   ) |>
#   addLegend(pal = pal, values = ~value, title = "Value",
#             position = "bottomright", na.label = "No data")
# 
# m

#simplest ggplot way: 
# map<-ggplot(counties) +
#   geom_sf(aes(fill = value), color = "black") +
#   scale_fill_distiller(palette = "pal", direction = 1, na.value = "white") +
#   theme_void()
# map

#ggspatial way:

map <- ggplot(counties) +
  geom_sf(aes(fill = value), color = "black") +
  #scale_fill_viridis_c()+
  scale_fill_distiller(direction = 1, na.value = "white") +
  annotation_scale(location = "bl", 
                   pad_x = unit (0.5, "in"), 
                   pad_y=unit(-0.02,"in"), 
                   width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(-0.15, "in"),
                         pad_y = unit (.15,"in"),
                         style = north_arrow_orienteering()) +
  coord_sf(clip = "off") +
  theme_void()+
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.4, l = 0.1, unit = "in"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        legend.position="inside",
        legend.position.inside = c(0.9,0.4)) +
  labs(fill="Number of \nsamples")
map


#save out as a static image: 
ggsave(filename=paste0("outputs/MN_map_figure",Sys.Date(),".pdf"),width=20,height=20,units="cm",
       device = cairo_pdf)
