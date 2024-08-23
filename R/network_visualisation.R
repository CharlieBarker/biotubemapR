#' Classify Nodes into Receptors, Transcription Factors, and Others
#'
#' @param nodes A vector of node names to classify.
#' @return A data frame with node names and their corresponding types ("receptor", "transcription_factor", or "other").
#' @importFrom dplyr pull
#' @importFrom OmnipathR import_omnipath_intercell import_omnipath_annotations
#' @export
classify_nodes <- function(nodes) {
  # Get list of receptors
  receptors <- OmnipathR::import_omnipath_intercell(
    parent = 'receptor',
    topology = 'pmtm',
    consensus_percentile = 50,
    loc_consensus_percentile = 30,
    entity_types = 'protein'
  ) %>%
    pull(genesymbol) %>% unique()

  # Get list of transcription factors
  transcriptionFactors <- import_omnipath_annotations(
    resources = 'TFcensus',
    entity_types = 'protein'
  ) %>%
    pull(genesymbol) %>%
    unique()

  # Create a data frame with nodes and their types
  node_types <- data.frame(
    name = nodes,
    type = ifelse(nodes %in% receptors, "receptor",
                  ifelse(nodes %in% transcriptionFactors, "transcription_factor", "other"))
  )

  return(node_types)
}

#' Create a Custom Layout for Nodes Based on Their Type
#'
#' @param graph An igraph object.
#' @param type_labels A vector of node types to include in the layout.
#' @param y_position Numeric value specifying the vertical position for the nodes of this type.
#' @return A data frame with the x and y coordinates of the nodes.
#' @importFrom igraph induced_subgraph layout_with_sugiyama
#' @export
create_custom_layout <- function(graph, type_labels, y_position) {
  # Get the nodes of the specified type
  type_nodes <- V(graph)$name[V(graph)$type %in% type_labels]

  # Create a subgraph with only these nodes
  subgraph_type <- induced_subgraph(graph, type_nodes)

  # Use the Sugiyama layout for this subgraph
  layout_type <- layout_with_sugiyama(subgraph_type)$layout

  # Convert the layout to a data frame and adjust y positions
  layout_type <- as.data.frame(layout_type)
  colnames(layout_type) <- c("x", "y")
  layout_type$y <- layout_type$y + y_position
  rownames(layout_type) <- type_nodes

  return(layout_type)
}

#' Create a Custom Layout for the Entire Graph
#'
#' @param graph An igraph object with vertex types classified.
#' @param receptor_y_position Numeric value specifying the vertical position for receptor nodes.
#' @param transcription_factor_y_position Numeric value specifying the vertical position for transcription factor nodes.
#' @param other_y_position Numeric value specifying the vertical position for other nodes.
#' @param name_var Character value specifying the variable in the igraph object containing the gene names.
#' @return A matrix representing the layout of the entire graph.
#' @importFrom igraph V
#' @export
pathwayLayout <- function(graph,
                          receptor_y_position = 1,
                          transcription_factor_y_position = -1,
                          other_y_position = 0,
                          name_var = "name") {
  # Extract node names
  nodes <- V(graph)[[name_var]]

  # Classify nodes
  node_types <- classify_nodes(nodes)

  # Add 'type' attribute to vertices
  V(graph)$type <- node_types$type[match(V(graph)$name, node_types$name)]

  layout_others <- create_custom_layout(graph, "other", y_position = other_y_position)

  # Create layouts for different types of nodes
  layout_receptors <- create_custom_layout(graph, "receptor", y_position = max(layout_others$y) + receptor_y_position)
  layout_tfs <- create_custom_layout(graph, "transcription_factor", y_position = min(layout_others$y) + transcription_factor_y_position)

  # Combine all layouts into one data frame
  all_layouts <- rbind(layout_receptors, layout_tfs, layout_others)

  # Reorder the layout to match the original graph's node order
  all_layouts <- all_layouts[V(graph)$name,]

  # Convert the data frame to a matrix for igraph layout
  layout_matrix <- as.matrix(all_layouts)

  #wrap up the output
  out<-list()
  out$layout_matrix <- layout_matrix
  out$node_types <- node_types

  return(out)
}

#' Plot a Biological Network Using a Custom Layout
#'
#' @param graph An igraph object representing the biological network.
#' @param path_layout A matrix or data frame representing the layout of the nodes.
#' @param color A character string specifying the color for edges.
#' @return A ggplot2 object of the plotted network.
#' @import ggraph ggplot2
#' @export
londonUnderground_plot <- function(graph, path_layout, color) {
  ggraph(graph, layout = path_layout) +
    geom_edge_link(aes(color = "line"), width = 2) + # Customize the edge color and thickness
    geom_node_point(color = "black", size = 8, shape = 21, fill = "white", stroke = 2) + # Customize the node color, outline, and size
    geom_node_text(aes(label = name), nudge_y = 0.5, size = 4, fontface = "bold", color = "black") + # Avoid label overlap
    scale_edge_color_manual(values = color) + # Set edge color based on argument
    theme_void() +
    theme(legend.position = "none") + # Remove legend
    coord_fixed() +
    scale_x_continuous(expand = expansion(c(1, 1))) +
    scale_y_continuous(expand = expansion(c(1, 1)))
}
