#' Classify Nodes into Receptors, Transcription Factors, and Others
#'
#' @param nodes A vector of node names to classify.
#' @return A data frame with node names and their corresponding types ("receptor", "transcription_factor", or "other").
#' @importFrom dplyr pull
#' @importFrom OmnipathR import_omnipath_intercell import_transcriptional_interactions
#' @export
classify_nodes <- function(nodes) {

  # Get list of transcription factors
  transcriptionFactors <- OmnipathR::import_transcriptional_interactions(
      # confidence levels;
      # we use only the 3 levels with highest confidence
      dorothea_levels = c('A', 'B', 'C'),
      entity_types = 'protein'
    ) %>%
    pull(source_genesymbol) %>%
    unique

  ligands <-
    OmnipathR::import_omnipath_intercell(
      parent = 'ligand',
      topology = 'sec',
      consensus_percentile = 50,
      loc_consensus_percentile = 30,
      entity_types = 'protein'
    ) %>%
    pull(genesymbol) %>%
    unique

  receptors <-
    OmnipathR::import_omnipath_intercell(
      parent = 'receptor',
      topology = 'pmtm',
      consensus_percentile = 50,
      loc_consensus_percentile = 30,
      entity_types = 'protein'
    ) %>%
    pull(genesymbol) %>%
    unique

  # Correct nested ifelse
  node_types <- data.frame(
    name = nodes,
    type = ifelse(nodes %in% receptors, "receptor",
                  ifelse(nodes %in% ligands, "ligand",
                         ifelse(nodes %in% transcriptionFactors, "transcription_factor", "other")))
  )

  return(node_types)
}

#' Create a Custom Layout for Nodes Based on Their Type
#'
#' @param graph An igraph object.
#' @param type_labels A vector of node types to include in the layout.
#' @param y_position Numeric value specifying the vertical position for the nodes of this type.
#' @param name_var Character value specifying the variable in the igraph object containing the gene names.
#' @return A data frame with the x and y coordinates of the nodes.
#' @importFrom igraph induced_subgraph layout_with_sugiyama
#' @export
create_custom_layout <- function(graph, type_labels, y_position,
                                 name_var = "name") {
  # Get the nodes of the specified type
  type_nodes <- igraph::vertex_attr(graph, "name")[V(graph)$type %in% type_labels]

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
#' @param type_df (OPTIONAL) Dataframe that specifies the different types of nodes, and therefore what layers they belong to. Columns names are "name" and "type". "type" can be one of "receptor", "ligand", "transcription_factor", "phenotype" and "other".

#' @return A matrix representing the layout of the entire graph.
#' @importFrom igraph V
#' @export
pathwayLayout <- function(graph,
                          ligand_y_position = 2,
                          receptor_y_position = 1,
                          transcription_factor_y_position = -1,
                          other_y_position = 0,
                          name_var = "name",
                          type_df = NULL) {
  # Extract node names
  nodes <- igraph::vertex_attr(graph, name_var)
  if (is.null(type_df)) {
    # Classify nodes
    node_types <- classify_nodes(nodes)
  }else{
    node_types <- type_df
  }

  # Add 'type' attribute to vertices
  V(graph)$type <- node_types$type[match(igraph::vertex_attr(graph, name_var), node_types$name)]

  layout_others <- create_custom_layout(graph, "other", y_position = other_y_position)

  # Create layouts for different types of nodes
  layout_receptors <- create_custom_layout(graph, "receptor", y_position = max(layout_others$y) + receptor_y_position)
  layout_ligands <- create_custom_layout(graph, "ligand", y_position = max(layout_receptors$y) + ligand_y_position)
  layout_tfs <- create_custom_layout(graph, "transcription_factor", y_position = min(layout_others$y) + transcription_factor_y_position)
  layout_phenotypes <- create_custom_layout(graph, "phenotype", y_position = min(layout_tfs$y) + transcription_factor_y_position - 1)

  # Combine all layouts into one data frame
  all_layouts <- rbind(layout_ligands, layout_receptors, layout_others, layout_tfs, layout_phenotypes)

  # Reorder the layout to match the original graph's node order
  all_layouts <- all_layouts[igraph::vertex_attr(graph, "name"),]

  # Convert the data frame to a matrix for igraph layout
  layout_matrix <- as.matrix(all_layouts)

  #wrap up the output
  out<-list()
  out$layout_matrix <- layout_matrix
  out$node_types <- node_types

  return(out)
}

#' TFL-style ggplot theme and geoms for biological network plots
#'
#' @param color Optional: A character string or named vector for edge colors.
#' @param nudge_label_y Vertical adjustment for node labels.
#' @param node.size Size of the node points.
#' @param stroke Stroke width of the nodes.
#' @param text.size Font size for node labels.
#' @param show.label Logical; whether to display node labels.
#' @return A list of ggplot2 layers styled in a London Underground aesthetic.
#' @export
theme_tfl <- function(nudge_label_y = 0.5,
                      node.size = 8,
                      stroke = 2,
                      text.size = 4,
                      show.label = TRUE) {

  layers <- list(
    geom_node_point(color = "black", size = node.size,
                    shape = 21, fill = "white", stroke = stroke),
    coord_fixed(),
    scale_x_continuous(expand = expansion(c(1, 1))),
    scale_y_continuous(expand = expansion(c(1, 1))),
    theme_void(),
    theme(legend.position = "none")
  )

  if (show.label) {
    layers <- append(layers, list(
      geom_node_text(
        aes(label = name),
        nudge_y = nudge_label_y,
        size = text.size,
        fontface = "bold",
        color = "black"
      )
    ))
  }
  return(layers)
}

#' Plot a Biological Network Using a TFL-style Layout
#'
#' @param graph An igraph object representing the biological network.
#' @param path_layout A layout matrix or data frame.
#' @param color A character string or named vector for edge colors.
#' @param nudge_label_y Vertical adjustment for node labels.
#' @param size Font size for node labels.
#' @param stroke Node stroke width.
#' @return A ggplot2 object of the plotted network.
#' @export
londonUnderground_plot <- function(graph, path_layout,
                                   nudge_label_y = 0.5,
                                   size = 4,
                                   stroke = 2) {
  ggraph(graph, layout = path_layout) +
    theme_tfl(nudge_label_y = nudge_label_y,
              text.size = size,
              stroke = stroke)
}
