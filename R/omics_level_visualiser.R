#' Add Multiple `geom_point` Layers for Different Levels of 'omics data
#'
#' This function creates a list of `geom_point` layers for different 'omics levels
#' with customizable sizes, alpha transparency, and stroke width. The size of the
#' points decreases incrementally for each subsequent level.
#'
#' @param df Data frame containing the 'omics data for the plot.
#' @param x Name of the column in `df` to be used for the x-axis.
#' @param y Name of the column in `df` to be used for the y-axis.
#' @param levels A character vector containing the names of 'omics levels.
#' @param size Initial size of the points for the first level. Default is 6.
#' @param alpha Alpha transparency for the points. Default is 1 (opaque).
#' @param stroke Stroke width for the points. Default is 1.
#' @param size_increment Amount to decrease the size for each subsequent level. Default is 3.
#' @param ... Additional arguments to be passed to `geom_point`.
#' @return A list of `geom_point` layers.
#' @export
#' @examples
#' # Create a list of layers with different levels
#' layers <- add_levels(df = my_data, x = "x", y = "y", levels = c("level1", "level2"))


# Define a function that returns a list of geom_point layers for different levels

add_levels <- function(df, x, y, size = 6, alpha = 1, stroke = 1, size_increment = 3, ...) {
  # Initialize a size multiplier
  current_size <- size

  # Create a list to store the layers
  layers <- list()

  # Loop through each level and create a geom_point layer
  for (level in levels) {
    layer <- geom_point(
      data = df,
      aes_string(x = x, y = y, colour = level),
      alpha = alpha,
      size = current_size,
      stroke = stroke,
      ... # Pass additional arguments to geom_point
    )

    # Add the layer to the list
    layers <- append(layers, list(layer))

    # Increase the size for the next level
    current_size <- current_size - size_increment
  }

  return(layers)
}


#' Create a Custom Legend with Geom Points for Different 'omics Levels
#'
#' This function creates a custom legend as a single node graph with different
#' 'omics levels represented by varying point sizes. A title and a square border
#' are added around the node.
#'
#' @param df Data frame containing the data for the legend.
#' @param levels A character vector containing the names of factor levels.
#' @param size Initial size of the points for the first level. Default is 6.
#' @param size_increment Amount to decrease the size for each subsequent level. Default is 3.
#' @param alpha Alpha transparency for the points. Default is 1 (opaque).
#' @param stroke Stroke width for the points. Default is 1.
#' @param title Title for the legend plot. Default is "Custom Legend".
#' @param ... Additional arguments to be passed to `geom_point`.
#' @return A `ggplot` object representing the custom legend.
#' @export
#' @examples
#' # Create a custom legend with different levels
#' custom_legend <- create_custom_legend(df = my_data, levels = c("level1", "level2"))
#'

create_custom_legend <- function(df, levels,
                                 size = 6, size_increment = 3, alpha = 1, stroke = 1, title = "Custom Legend", ...) {
  # Create a simple graph with a single node
  graph <- make_empty_graph(1)  # Create a graph with one node

  # Create a layout for the single node (centered)
  layout <- matrix(c(0, 0), ncol = 2)

  # Create a data frame for the single node with an empty placeholder
  node_df <- data.frame(x = 0, y = 0)

  # Add columns to node_df for each level, using the first non-NA value from df for each level
  for (level in levels) {
    # Get the first non-NA value from df for the corresponding level
    node_df[[level]] <- df[[level]][!is.na(df[[level]])][1]
  }

  # Initialize the ggraph object with the single node
  g <- ggraph(graph, layout = layout) +
    theme_void() +  # Remove all background, gridlines, etc.
    coord_fixed()   # Ensure equal aspect ratio

  # Initialize a size multiplier
  current_size <- size

  # Loop through each level to add corresponding geom_point layers
  for (level in levels) {
    g <- g +
      geom_point(
        data = node_df,
        aes_string(x = "x", y = "y", colour = level),
        size = current_size,
        stroke = stroke,
        alpha = alpha,
        ...
      )
    # Decrease the size for the next level (or adjust as needed)
    current_size <- current_size - size_increment
  }

  # Add a title and a square around the node
  g <- g +
    ggtitle(title) +  # Add the title above the legend
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") +  # Center the title
    geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1),
              fill = NA, colour = "black", size = 0.5)  # Draw a square around the node

  return(g)
}

