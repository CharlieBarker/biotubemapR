#' Add Multiple `geom_node_point` Layers for Different Levels of 'Omics Data
#'
#' This function creates a list of `geom_node_point` layers (for use with `ggraph`) to visualize different 'omics-style levels as overlapping node aesthetics.
#' Each level is rendered with a customizable point size, stroke width, and alpha transparency. Point size decreases for each subsequent level to show multi-layered detail.
#'
#' @param levels A character vector specifying the names of vertex attributes (e.g., 'omics levels') to visualize.
#' @param size Initial point size for the topmost level. Default is 6.
#' @param alpha Alpha transparency of the points. Default is 1 (fully opaque).
#' @param stroke Stroke width of the points. Default is 1.
#' @param size_increment Amount to decrease the point size for each successive level. Default is 3.
#' @param ... Additional aesthetic arguments passed to `geom_node_point()`.
#'
#' @return A list of `geom_node_point` layers that can be added to a `ggraph` plot.
#'
#' @examples
#' # Assuming tube_graph is an igraph object with vertex attributes:
#' # EntryTapCount and ExitTapCount
#' ggraph(tube_graph, layout = layout_df) +
#'   geom_edge_link(aes(colour = I(color)), show.legend = FALSE) +
#'   add_levels(levels = c("EntryTapCount", "ExitTapCount"), size = 5, size_increment = 2, alpha = 0.8) +
#'   theme_graph()
#'
#' @export

add_levels <- function(levels, size = 6, alpha = 1, stroke = 1, size_increment = 3, ...) {
  current_size <- size
  layers <- list()

  for (level in levels) {
    layer <- geom_node_point(
      aes(colour = .data[[level]]),  # <- this replaces aes_string()
      size = current_size,
      stroke = stroke,
      alpha = alpha,
      ...
    )
    layers <- append(layers, list(layer))
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


#' Create a Combined Legend Grid
#'
#' This function generates a combined grid of legends for multiple omics levels of a given dataset.
#'  Each legend displays a gradient scale corresponding to a specific level.
#'  The resulting legends are combined into a grid layout.
#'
#' @param L_df A data frame containing the data to be plotted. It should include columns for each 'omics level specified in the `levels` argument.
#' @param levels A character vector of column names of levels in `L_df` for which legends should be created. Each level should have a corresponding gradient scale.
#' @param colours A vector of colours to be used for the gradient scale. This should include at least two colours for a gradient.
#' @param nrow Number of rows in the combined legend grid. Default is 1.
#' @param ncol Number of columns in the combined legend grid. Default is 2.
#'
#' @return A `ggplot` object containing the combined grid of legends.
#' @importFrom ggplot2 ggplot geom_point aes_string scale_fill_gradientn
#' @importFrom patchwork plot_spacer
#' @importFrom cowplot get_legend plot_grid
#' @examples
#' # Example data frame
#' L_df <- data.frame(
#'   mRNA_Factor1 = runif(100, 0, 1),
#'   protein_Factor1 = runif(100, 0, 1)
#' )
#'
#' # Create a combined legend grid
#' create_combined_legend(
#'   L_df = L_df,
#'   levels = c("mRNA_Factor1", "protein_Factor1"),
#'   colours = c("#000000", "#FFFFFF", "#BA0000"),
#'   nrow = 1,
#'   ncol = 2
#' )
#'

# Function to create a combined legend grid
create_combined_legend <- function(L_df, levels, colours, nrow = 1, ncol = 2) {
  # Initialize an empty list to store legends
  legend_list <- list()

  # Loop through each level to create individual plots and extract legends
  for (i in seq_along(levels)) {
    level <- levels[i]

    # Filter out rows with missing values in the current level
    level_data <- L_df[!is.na(L_df[[level]]), ]

    # Create the ggplot for the current level
    p <- ggplot(data = level_data) +
      geom_point(aes_string(x = level, y = level, fill = level), shape = 21) +
      scale_fill_gradientn(
        name = paste("Legend", i),
        limits = c(0, max(level_data[[level]], na.rm = TRUE)),
        colours = colours,
        values = c(0, 0.5, 1)
      )

    # Extract the legend from the plot (handling multiple components)
    legend_list[[i]] <- cowplot::get_legend(p)
  }
  # Create a blank plot to align legends
  blank_p <- plot_spacer() + theme_void()

  # Combine all legends into a grid
  combined_legends <- plot_grid(
    plotlist = legend_list,
    blank_p,
    nrow = nrow,
    ncol = ncol
  )

  # Return the combined legends
  return(combined_legends)
}
