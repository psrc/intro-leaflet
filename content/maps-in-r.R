
# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr)
library(leaflet)


# General Inputs ----------------------------------------------------------

geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")

spn <- 2285
wgs84 <- 4326

current.city <- "Poulsbo"

# Create a function for spatial layers ------------------------------------
intersect.layers <- function(a.layer, a.columns, overlay, intersect.crs=spn, final.crs=wgs84) {
  
  data <- st_read(a.layer) %>%
    select(all_of(a.columns)) %>%
    st_transform(intersect.crs)
  
  trimmed <- st_intersection(data, overlay) %>%
    st_transform(final.crs)
  
  return(trimmed)
}

# Create layers for mapping -----------------------------------------------
my.city <- st_read("cities.shp") %>% 
  filter(city_name == current.city) %>% 
  select(city_name) %>%
  st_transform(spn)

city.signals <- intersect.layers("its_signals.shp", c("owner", "majorst_1", "ped_signal", "tsp"), my.city)
city.transit <- intersect.layers("transit_lines.shp", c("trans_line", "mode"), my.city)
my.city <- my.city %>% st_transform(wgs84)


# Creating a Map ----------------------------------------------------------

signal.pal <- colorFactor(
  palette = c("#AD5CAB", "#F4835E"),
  domain = city.signals$tsp
)

transit.pal <- colorFactor(
  palette = c("#BCBEC0", "#E3C9E3", "#BFE9E7", "#E2F1CF", "#FBD6C9"),
  domain = city.transit$mode
)

my.map <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addLayersControl(baseGroups = c("Base Map"),
                   overlayGroups = c("City Boundary","Signals","Transit Routes"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  
  addEasyButton(easyButton(
    icon="fa-globe", title="Region",
    onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%

  addPolygons(data=my.city,
              fillColor = "76787A",
              fillOpacity = 0.0,
              opacity = 1.0,
              weight = 4,
              color = "#91268F",
              dashArray = "4",
              group = "City Boundary") %>%
  
  addPolylines(data = city.transit,
               color = ~transit.pal(mode),
               weight = 3,
               fillColor = ~transit.pal(mode),
               group = "Transit Routes") %>%
  
  addLegend(position = "bottomright",
            values = city.transit$mode,
            pal = transit.pal,
            title = "Transit Modes",
            group = "Transit Routes") %>%
  
  addCircles(data=city.signals,
             weight = 4, 
             radius = 48,
             fill = TRUE,
             opacity = 1,
             popup = ~tsp,
             color = ~signal.pal(tsp),
             group = ("Signals")) %>%
  
  addLegend(pal = signal.pal,
            values = city.signals$tsp,
            group = "Signals",
            position = "bottomright",
            title = "Transit Signal Priority")
  

# Spatial Layers from GeoElmer----------------------------------------------------------

# Read full shapefile and use dplyr to filter
my.city <- st_read(gdb.nm, "dbo.cities", crs = spn) %>% 
  filter(city_name == current.city) %>% 
  select(city_name)

# Query the database before reading
my.city <- st_read(dsn = gdb.nm,
                   layer = "dbo.cities",
                   query = paste0("SELECT city_name, Shape FROM cities WHERE city_name = " , "'",current.city,"'"),
                   crs = spn)

# Spatial Layers from file -------------------------------------------------
my.city <- st_read("cities.shp") %>% 
  filter(city_name == current.city) %>% 
  select(city_name) %>%
  st_transform(spn)

# Spatial Layers from ArcOnline -------------------------------------------------

city.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/cities/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"

my.city <- st_read(city.url) %>% 
  filter(CITYNAME == current.city) %>% 
  select(CITYNAME) %>%
  st_transform(spn) %>%
  rename(city_name = CITYNAME) %>%
  st_transform(spn)
