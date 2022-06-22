#' Calculates c_ij: the coincidence probability due to the presence of barriers
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param dir_fragmentation_type how directionality in c_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details.
#' @param pass_confluence a value in the range [0,1] that defines the passability of confluences (default is 1).
#' @param pass_u the 'graph' edge attribute to be used as upstream passability. Default is "pass_u".
#' @param pass_d the 'graph' edge attribute to be used as downstream passability. Default is "pass_d".
#'
#' @details
#' \code{dir_fragmentation_type = "symmetric"} is to be used when the directionality of the river network is not relevant.
#' The equivalent passability for each barrier is calculated as the product of upstream and downstream passabilities.
#' \code{dir_fragmentation_type = "asymmetric"} is to be used when the directionality is relevant.
#' The equivalent passability of each barrier is calculated as a function of the path connecting each couple of reaches
#' and depends on the direction of the path. Check the package vignette for more details.
#'
#' @return a matrix in data.frame format whose columns are 'from', 'to', and 'c_ij'.
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#'
#' @keywords internal
#'
c_ij_fun_old <- function(graph, dir_fragmentation_type = "symmetric", pass_confluence = 1, pass_u = "pass_u", pass_d = "pass_d"){

  # Error messages

  # Set graph directionality
  graph <- set_c_directionality(graph,
                                dir_fragmentation_type = dir_fragmentation_type,
                                pass_confluence = pass_confluence,
                                pass_u = pass_u, pass_d = pass_d)

  # Extract the vertices names
  vertices_id <- names(igraph::V(graph))

  # Create data frame with all the combinations
  cij_mat <- tidyr::expand_grid(from = vertices_id, to = vertices_id)

  # Function that extracts subgraph and calculate product of the 'pass_eq' field
  prod_prob <- function(i,j, graph){
    subgraph <- igraph::subgraph.edges(
      graph, igraph::shortest_paths(graph, from = i, to = j, mode = "out", output = "both")$epath[[1]])
    return(igraph::E(subgraph)$pass_eq %>% prod)
  }

  # Calculate product probabilities
  cij_mat$c_ij <-  mapply(
    FUN = prod_prob,
    cij_mat$from, cij_mat$to, list(graph))

  return(cij_mat)

}
