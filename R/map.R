library(tidyverse)
library(leaflet)

df <- map(c("Magyar", "Nemzetközi", "Vegyes Kamarák"),
          function(sheet) {
            df <- readxl::read_excel("c:/rprojects/chamber_map/data/Kereskedelmi szervezetek, kamarák adatbázis.xlsx", sheet = sheet)
            df$type <- rep(sheet, nrow(df))
            c("type", "Név", "Központi / Regionális iroda címe", "Cím (Központi Iroda)", "Cím") %>% 
              keep(~ . %in% names(df)) %>% 
              {select(df, .)} %>% 
              set_names("type", "name", "place")
          }
) %>% 
  reduce(rbind) %>% 
  mutate(
    popup = str_c(
      "<b>", name, "</b>", "<br/>",
      place
      ),
    place = snakecase::to_snake_case(place) # avoid error from spaces
    ) %>% 
  left_join(readr::read_csv("data/geocodes.csv") %>% 
              mutate(place = snakecase::to_snake_case(place)))
  
  

add_icon <- function(x) {
  icon_color <- case_when(
    x == "Magyar" ~ "red",
    x == "Nemzetközi" ~ "blue",
    TRUE ~ "green"
  )
  awesomeIcons(
    iconColor = 'black',
    library = 'ion',
    markerColor = icon_color
  )
}

map <- leaflet(df) %>% 
  addTiles() %>% 
  addAwesomeMarkers(~Longitude, ~Latitude, popup = ~popup, icon = ~add_icon(type))

htmlwidgets::saveWidget(map, "map.html")
