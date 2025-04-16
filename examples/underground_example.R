library(ggraph)
library(igraph)
library(readr)
library(ggplot2)
library(biotubemapR)
library(dplyr)
library(wesanderson)
library(tidyr)

setwd("/Users/charliebarker/Desktop/packages/biotubemapR")

# Read stations data
stations <- read_csv("./datasets/connections/london.stations.csv")
# Read connections data
connections <- read_csv("./datasets/connections/london.connections.csv")
# Read lines data
lines <- read_csv("./datasets/connections/london.lines.csv")


# Convert stations to a named list
station_names <- setNames(stations$name, stations$id)
# Create graph edges (connections between station IDs)
tube_graph <- graph_from_data_frame(connections, directed = FALSE, vertices = stations)
# Assign station names to vertex labels
V(tube_graph)$label <- station_names[V(tube_graph)$name]
# Assign line colors
edge_colors <- setNames(lines$colour, lines$line)
E(tube_graph)$color <- edge_colors[as.character(E(tube_graph)$line)]
# Assign zones
edge_colors <- setNames(lines$colour, lines$line)
E(tube_graph)$color <- edge_colors[as.character(E(tube_graph)$line)]

pLayout <- data.frame(x = stations$longitude, y = stations$latitude)
E(tube_graph)$color <- paste0("#", E(tube_graph)$color)
ggraph(tube_graph, layout = pLayout) +
  geom_edge_link(aes(colour = I(color)), show.legend = FALSE) +
  geom_node_point() +
  theme_graph()


#adding footfall

#https://tfl.gov.uk/corporate/publications-and-reports/network-demand-data
footfall<-read_csv("datasets/footfall/StationFootfall_2023_2024 .csv")

# Step 1: Summarize footfall data (use total, mean, or any summary function)
footfall_summary <- footfall %>%
  group_by(Station) %>%
  summarise(
    EntryTapCount = sum(EntryTapCount, na.rm = TRUE),
    ExitTapCount = sum(ExitTapCount, na.rm = TRUE)
  )

# Step 2: Convert to dataframe for joining
footfall_df <- as.data.frame(footfall_summary)

# Step 3: Join into graph vertex attributes
V(tube_graph)$EntryTapCount <- footfall_df$EntryTapCount[match(V(tube_graph)$name, footfall_df$Station)]
V(tube_graph)$ExitTapCount  <- footfall_df$ExitTapCount[match(V(tube_graph)$name, footfall_df$Station)]


# Build a data frame from graph vertices
node_df <- igraph::as_data_frame(tube_graph, what = "vertices")
pal <- wes_palette("Zissou1", 100, type = "continuous")

x_range <- range(pLayout[, 1])
y_range <- range(pLayout[, 2])

ggraph(tube_graph, layout = pLayout) +
  geom_edge_link(aes(colour = I(color)), show.legend = FALSE, width = 2) +
  biotubemapR::theme_tfl(node.size = .3, stroke = .2, nudge_label_y = 0.1, show.label = FALSE) +
  add_levels(
    levels = c("EntryTapCount", "ExitTapCount"),
    size = 4,
    size_increment = 1,
    alpha = 0.8
  ) +
  scale_colour_gradientn(
    colours = pal,
    name = "Tap Count"
  ) +
  scale_x_continuous(limits = x_range, expand = c(0, 0)) +
  scale_y_continuous(limits = y_range, expand = c(0, 0)) +
  coord_fixed()


# https://crowding.data.tfl.gov.uk/
# hourly crowding

ByQhrEntryExit_2019 <- read_csv("./datasets/footfall/ByQhrEntryExit_2019 - ByQhr.csv")
ByQhrEntryExit_2019_long <- ByQhrEntryExit_2019 %>%
  select(Station, day, `0500-0515`:`0445-0500`) %>%
  pivot_longer(
    cols = `0500-0515`:`0445-0500`,
    names_to = "time",
    values_to = "value"
  ) %>%
  mutate(
    hour = as.numeric(substr(time, 1, 2)),
    minute = as.numeric(substr(time, 3, 4)),
    time_decimal = hour + minute / 60
  )

merged_df <- ByQhrEntryExit_2019_long %>%
  left_join(node_df, by = c("Station" = "name")) %>%
  group_by(Station) %>%
  mutate(value_scaled = value / max(value, na.rm = TRUE)) %>%
  ungroup()

merged_df <- merged_df %>%
  mutate(day = factor(day, levels = c("MTT", "FRI", "SAT", "SUN"))) %>%
  mutate(time_label = format(strptime(substr(time, 1, 4), "%H%M"), "%H:%M")) %>%
  mutate(day_time_label = paste("Day:", day, "| Time:", time_label)) %>%
  arrange(day, time_label) %>%
  mutate(day_time_label = factor(day_time_label, levels = unique(day_time_label)))



unmatching_stations<-anti_join(ByQhrEntryExit_2019_long, node_df, by = c("Station" = "name")) %>%
  distinct(Station)

x_range <- range(merged_df[, 'longitude'], na.rm = T)
y_range <- range(merged_df[, 'latitude'], na.rm = T)

library(gganimate)
library(ggmap)

# Get bounding box
lon_min <- min(merged_df$longitude, na.rm = TRUE)
lon_max <- max(merged_df$longitude, na.rm = TRUE)
lat_min <- min(merged_df$latitude, na.rm = TRUE)
lat_max <- max(merged_df$latitude, na.rm = TRUE)

bbox <- c(left = lon_min, bottom = lat_min, right = lon_max, top = lat_max)

# Get a basemap (you can change "toner-lite" to "terrain", etc.)
# basemap <- get_stadiamap(bbox, zoom = 12, maptype = "stamen_terrain")

library(ggraph)
library(gganimate)

# Combined plot with network and animated tap points
ggraph(tube_graph, layout = pLayout) +
  geom_edge_link(aes(colour = I(color)), show.legend = FALSE, width = .2) +

  # Tap count color scale
  scale_colour_gradientn(
    colours = pal,
    limits = c(0, 1),
    guide = "none"
  ) +

  # TfL-style theme
  biotubemapR::theme_tfl(
    node.size = 0.3, stroke = 0.2,
    nudge_label_y = 0.1, show.label = FALSE
  ) +

  # Add tap data layer
  geom_point(
    data = merged_df,
    aes(x = longitude, y = latitude, colour = value_scaled),
    inherit.aes = FALSE,
    size = .6, alpha = 0.85
  ) +

  # Coordinate system & transitions
  coord_fixed() +
  transition_states(day_time_label, transition_length = 0.5, state_length = 0.5) +
  labs(title = '{closest_state}', x = '', y = '') +
  theme_void() +
  theme(legend.position = "none") +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')+
  scale_x_continuous(limits = x_range, expand = c(0, 0)) +
  scale_y_continuous(limits = y_range, expand = c(0, 0)) +
  coord_fixed()

animate(last_plot(), width = 1600, height = 1000, res = 200)


#in shiny form

# app.R
library(shiny)
library(leaflet)
library(dplyr)
library(scales)

# ---- Sample mock data structure ----
# Replace this with your actual `merged_df` and prep

# Assume: merged_df has columns: Station, longitude, latitude, value_scaled, day, time

# Set day order
day_levels <- c("MTT", "FRI", "SAT", "SUN")

merged_df <- merged_df %>%
  mutate(
    day = factor(day, levels = day_levels),
    day_index = as.numeric(day),
    time_index = as.numeric(factor(time, levels = unique(time))),
    time_step = (day_index - 1) * length(unique(time)) + time_index,
    label = paste(day, time)  # Optional: pretty label
  )

# Define color palette
pal <- colorRampPalette(c("lightblue", "blue", "darkblue"))(100)
pal_func <- scales::col_numeric(palette = pal, domain = c(0, 1))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Animated Tube Tap Count Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("time", "Day & Time",
                  min = min(merged_df$time_step),
                  max = max(merged_df$time_step),
                  value = min(merged_df$time_step),
                  step = 1,
                  animate = animationOptions(interval = 300, loop = TRUE)),
      textOutput("current_time")
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  filtered_data <- reactive({
    merged_df %>% filter(time_step == input$time)
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Change this to addProviderTiles() for different styles
      setView(lng = -0.1, lat = 51.5, zoom = 11)
  })

  observe({
    data <- filtered_data()
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~value_scaled * 10,
        color = ~pal_func(value_scaled),
        stroke = FALSE, fillOpacity = 0.8
      )
  })

  output$current_time <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      paste("Showing:", data$day[1], "|", data$time[1])
    } else {
      "No data for selected time."
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

