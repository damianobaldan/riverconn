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
#' @return a square matrix of size length(V(graph)) containing c_ij values.
#' The matrix is organized with "from" nodes on the columns and "to" nodes on the rows
#' @export
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#' @importFrom reshape2 melt
#' @importFrom dodgr dodgr_dists
#'
#' @examples
#' library(igraph)
#' g <- igraph::graph_from_literal(1-+2, 2-+5, 3-+4, 4-+5, 6-+7, 7-+10,
#' 8-+9, 9-+10, 5-+11, 11-+12, 10-+13, 13-+12, 12-+14, 14-+15, 15-+16)
#' E(g)$id_dam <- c("1", NA, "2", "3", NA, "4", NA, "5", "6", NA,  NA, NA, NA, "7", NA)
#' E(g)$type <- ifelse(is.na(E(g)$id_dam), "joint", "dam")
#' V(g)$length <- c(1, 1, 2, 3, 4, 1, 5, 1, 7, 7, 3, 2, 4, 5, 6, 9)
#' V(g)$HSI <- c(0.2, 0.1, 0.3, 0.4, 0.5, 0.5, 0.5, 0.6, 0.7, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
#' V(g)$Id <- V(g)$name
#' E(g)$pass_u <- E(g)$pass_d <- ifelse(!is.na(E(g)$id_dam),0.1,NA)
#' dist_mat <- c_ij_fun(g)
#'
c_ij_fun <- function(graph,
                     dir_fragmentation_type = "symmetric",
                     pass_confluence = 1,
                     pass_u = "pass_u",
                     pass_d = "pass_d"){

  # Error messages - none here for the moment
  if( !("name" %in% igraph::vertex_attr_names(graph)) ) stop(
    "'nodes_id' argument must be a valid vertex attribute in 'graph'")

  # Set graph directionality
  graph <- set_c_directionality(graph,
                                dir_fragmentation_type = dir_fragmentation_type,
                                pass_confluence = pass_confluence,
                                pass_u = pass_u, pass_d = pass_d)

  # Extract the vertices names
  vertices_id <- names(igraph::V(graph))

  # Create dodgr graph
  graph_dodgr <- igraph::as_data_frame(graph, what = "edges") %>%
    select(.data$from, .data$to, .data$pass_eq) %>%
    mutate(dist = log10(.data$pass_eq))

  # Calculate all shortest paths
  cij_mat <- dodgr::dodgr_dists(graph_dodgr, from = vertices_id, to = vertices_id)

  # Postprocess matrix
  cij_mat <- 10^cij_mat

  return(cij_mat)

}
