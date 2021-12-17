#' Calculates equivalent passability based on graph directionality and directionality type
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param dir_fragmentation_type how directionality in c_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph)
#' @param pass_confluence a value in the range [0,1] that defines the passability of confluences (default is 1).
#' @param pass_u the 'graph' edge attribute to be used as upstream passability. Default is "pass_u".
#' @param pass_d the 'graph' edge attribute to be used as downstream passability. Default is "pass_d".
#'
#' @return an object of class 'igraph' with the equivalent passability stored in the edge attribute 'pass_eq'
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#'
#' @keywords internal
#'
set_c_directionality <- function(graph, dir_fragmentation_type = "symmetric", pass_confluence = 1, pass_u = "pass_u", pass_d = "pass_d"){

  # error messages
  if( !(dir_fragmentation_type %in% c("symmetric", "asymmetric")) ) stop(
    "'dir_fragmentation_type' must me either 'symmetric' or 'asymmetric'")
  if( !(pass_u %in% igraph::edge_attr_names(graph) )) stop(
    "edge attributes in river_graph must include the argument defined in 'pass_u' (upstream passability)")
  if( !(pass_d %in% igraph::edge_attr_names(graph)) ) stop(
    "edge attributes in river_graph must include the argument defined in'pass_d' (downstream passability)")
  if( pass_confluence < 0 | pass_confluence > 1 ) stop(
    "'pass_confluence' must be between 0 and 1")
  if( TRUE %in% (stats::na.omit(igraph::get.edge.attribute(graph, pass_u) ) < 0) |
      TRUE %in%  (stats::na.omit(igraph::get.edge.attribute(graph, pass_u) ) > 1) ) stop(
        "'pass_u' must be between 0 and 1")
  if( TRUE %in% (stats::na.omit(igraph::get.edge.attribute(graph, pass_d) ) < 0) |
      TRUE %in% (stats::na.omit(igraph::get.edge.attribute(graph, pass_d) ) > 1) ) stop(
        "'pass_d' must be between 0 and 1")


  # Rename the passabilities based on user names
  # Set the names of the vertices. By default keep the name argument.
  igraph::E(graph)$pass_u <- igraph::get.edge.attribute(graph, pass_u)
  igraph::E(graph)$pass_d <- igraph::get.edge.attribute(graph, pass_d)

  # Fill up the passabilities of the confluences -before it's NANs-
  igraph::E(graph)$pass_u <- ifelse(is.na(igraph::E(graph)$pass_u), pass_confluence, igraph::E(graph)$pass_u)
  igraph::E(graph)$pass_d <- ifelse(is.na(igraph::E(graph)$pass_d), pass_confluence, igraph::E(graph)$pass_d)

  # If directionality is "asymmetric" the graph is made undirectional
  if(dir_fragmentation_type == "symmetric"){

    # Calculate the passability as product of upstream and downstream
    igraph::E(graph)$pass_eq <- igraph::E(graph)$pass_u * igraph::E(graph)$pass_d

    # Make the graph undirected
    graph_output <- igraph::as.undirected(graph, mode = "each" )

  }

  # If directionality is "asymmetric" the graph is made undirectional
  if(dir_fragmentation_type == "asymmetric"){

    graph_df_bidir<- rbind(
      # "downstream" graph: graph with directions going downstream, uses pass_d as attribute
      igraph::as_data_frame(graph, what = "edges") %>%
        dplyr::select(-pass_u) %>%
        dplyr::rename(pass_eq = .data$pass_d),
      # "upstream" graph: graph with directions going upstream, uses pass_u as attribute
      igraph::as_data_frame(graph, what = "edges") %>%
        dplyr::rename(from = .data$to, to = .data$from) %>%
        dplyr::select(-.data$pass_d) %>%
        dplyr::rename(pass_eq = .data$pass_u)
    )

    # Vertices dataframe
    graph_df_bidir_v <- igraph::as_data_frame(graph, what = "vertices") %>%
      dplyr::relocate(.data$name)

    # Create the bidirectional fraph
    graph_output <- igraph::graph_from_data_frame(
      d = graph_df_bidir,
      vertices = graph_df_bidir_v )
  }


  return(graph_output)
}
