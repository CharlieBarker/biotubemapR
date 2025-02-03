
library(igraph)
library(readr)
library(ggplot2)
library(biotubemapR)

# Read stations data
stations <- read_csv("./datasets/london.stations.csv")
# Read connections data
connections <- read_csv("./datasets/london.connections.csv")
# Read lines data
lines <- read_csv("./datasets/london.lines.csv")


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

londonUnderground_plot(tube_graph, path_layout = pLayout, color = "blue")+
  theme(
    legend.position = "none",  # Remove legend
    plot.margin = margin(0, 0, 0, 0)  # Adjust the plot margins (top, right, bottom, left)
  ) +
  coord_fixed() +  # Use fixed aspect ratio
  scale_x_continuous(expand = expansion(c(0.1, 0.1))) +  # Adjust X axis to ensure nodes are not too close to the edges
  scale_y_continuous(expand = expansion(c(0.1, 0.1)))  # Adjust Y axis similarly
