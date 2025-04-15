library(ggraph)
library(igraph)
library(readr)
library(ggplot2)
library(biotubemapR)
library(dplyr)
library(wesanderson)

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
ggraph(tube_graph, layout = pLayout) +
  geom_edge_link(aes(colour = I(color)), show.legend = FALSE) +
  add_levels(
    levels = c("EntryTapCount", "ExitTapCount"),
    size = 5,
    size_increment = 2,
    alpha = 0.8
  ) +
  scale_colour_gradientn(
    colours = pal,
    name = "Tap Count"
  ) +
  theme_graph()

