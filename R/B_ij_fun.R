#' Calculates B_ij: the coincidence probability due to dispersal
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param field_B the 'graph' edge attribute to be used to calculate the distance. Default is \code{"length"}.
#' @param dir_distance_type how directionality in B_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details.
#' @param disp_type the formula used to calculate the probabilities in the B_ij matrix.
#' Use \code{"exponential"} for exponential decay, or \code{"threshold"} for setting a distance threshold.
#' @param param_u the upstream dispersal parameter. Must be a numeric value. Only used if \code{dir_distance_type = "asymmetric"}. See details.
#' @param param_d the downstream dispersal parameter. Must be a numeric value. Only used if \code{dir_distance_type = "asymmetric"}. See details.
#' @param param the dispersal parameter. Must be a numeric value. Only used if \code{dir_distance_type = "symmetric"}. See details.
#'
#' @return a square matrix of size length(V(graph)) containing B_ij values.
#' The matrix is organized with "from" nodes on the columns and "to" nodes on the rows
#' @export
#'
#' @details
#' \code{dir_distance_type = "symmetric"} is to be used when the directionality of the river network is not relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches.
#' \code{dir_distance_type = "asymmetric"} is to be used when the directionality is relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches and splitted
#' between 'upstream travelled' distance and 'downstream travelled' distance
#'
#' The 'param_u', 'param_d', and 'param' values are interpreted differently based on the formula used to relate distance and probability.
#' When \code{disp_type ="exponential"}, those values are used as the base of the exponential dispersal kernel: B_ij = param^{d_ij}.
#' When \code{disp_type ="threshold"}, those values are used to define the maximum dispersal length: B_ij = ifelse(d_ij < param, 1, 0).
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @importFrom dodgr dodgr_dists
#'
#' @examples
#' library(igraph)
#' g <- igraph::graph_from_literal(1-+2, 2-+5, 3-+4, 4-+5, 6-+7, 7-+10, 8-+9, 9-+10,
#' 5-+11, 11-+12, 10-+13, 13-+12, 12-+14, 14-+15, 15-+16)
#' E(g)$id_dam <- c("1", NA, "2", "3", NA, "4", NA, "5", "6", NA,  NA, NA, NA, "7", NA)
#' E(g)$type <- ifelse(is.na(E(g)$id_dam), "joint", "dam")
#' V(g)$length <- c(1, 1, 2, 3, 4, 1, 5, 1, 7, 7, 3, 2, 4, 5, 6, 9)
#' V(g)$HSI <- c(0.2, 0.1, 0.3, 0.4, 0.5, 0.5, 0.5, 0.6, 0.7, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
#' V(g)$Id <- V(g)$name
#' E(g)$pass_u <- E(g)$pass_d <- ifelse(!is.na(E(g)$id_dam),0.1,NA)
#' dist_mat <- B_ij_fun(g, param = 0.9)
#'
B_ij_fun <- function(graph,
                     field_B = "length",
                     dir_distance_type = "symmetric",
                     disp_type = "exponential",
                     param_u ,
                     param_d ,
                     param ) {

  # Error messages
  if( !(field_B %in% igraph::vertex_attr_names(graph)) ) stop(
    "'field_B' argument must be a valid vertex attribute in 'graph'")
  if( !(disp_type %in% c("exponential", "threshold")) ) stop(
    "'disp_type' must be either 'exponential' or 'threshold'")

  # Errors for asymmetric distance
  if( dir_distance_type == "asymmetric"){
    if( missing(param_u) | is.na(param_u) ) stop(
      "'param_u' must be defined when dir_distance_type = 'asymmetric' (if you do not want to include
      dispersal limitation, set 'B_ij_flag = FALSE' in the function's arguments)")
    if( missing(param_d) | is.na(param_d) ) stop(
      "'param_d' must be defined when dir_distance_type = 'asymmetric' (if you do not want to include
      dispersal limitation, set 'B_ij_flag = FALSE' in the function's arguments)")
    if( param_u < 0 | param_d < 0 ) stop(
      "'param_u', 'param_d', and 'param' must be > 0")
    if(  !is.numeric(param_u)) stop(
      "'param_u' must be numeric")
    if( !is.numeric(param_d)) stop(
      "'param_d' must be numeric")
    if(disp_type == "exponential") {
      if( param_u > 1 | param_d > 1 ) stop(
        "'param_u' and 'param_d' must be < 1 for disp_type = 'exponential'")
    }}

  # Errors for symmetric distance
  if( dir_distance_type == "symmetric" ) {
    if( missing(param) | is.na(param) )  stop(
      "'param' must be specified when dir_distance_type = 'symmetric' (if you do not want to include
      dispersal limitation, set 'B_ij_flag = FALSE' in the function's arguments)")
    if( param < 0  ) stop(
      "'param_u', 'param_d', and 'param' must be > 0")
    if(disp_type == "exponential") {
      if( param > 1 ) stop(
        "'param' must be < 1 for disp_type = 'exponential'")
    } }


  # Set the directionality for Bij calculations
  graph <- set_B_directionality(graph,
                                dir_distance_type = dir_distance_type,
                                field_B = field_B)

  # Extract the vertices names
  vertices_id <- names(igraph::V(graph))

  #
  # symmetric dispersal: I use only the sum of the distances
  #
  if(dir_distance_type == "symmetric"){

    # Create dodgr graph
    graph_dodgr <- igraph::as_data_frame(graph, what = "edges") %>%
      rename(dist = .data$d_att) %>%
      select(.data$from, .data$to, .data$dist)

    # Calculate all shortest paths
    Bij_mat <- reshape2::melt(
      dodgr::dodgr_dists(graph_dodgr, from = vertices_id, to = vertices_id) ) %>%
      dplyr::mutate(from = as.character(.data$Var1), to = as.character(.data$Var2), n = .data$value) %>%
      dplyr::select(.data$from, .data$to, .data$n)

    # if exponential decay
    if(disp_type == "exponential"){
      Bij_mat$B_ij = param^Bij_mat$n }

    # if threshold decay
    if(disp_type == "threshold"){
      Bij_mat$B_ij = ifelse(Bij_mat$n < param, 1, 0) }
  }


  #
  # asymmetric dispersal: I use both distances
  #
  if(dir_distance_type == "asymmetric"){

    # Create dodgr graph for upstream movement
    graph_dodgr_u <- igraph::as_data_frame(graph, what = "edges") %>%
      filter(.data$flag_dir == "u") %>%
      rename(dist = .data$d_att) %>%
      select(.data$from, .data$to, .data$dist)

    graph_dodgr_u <- rbind(graph_dodgr_u,
                           graph_dodgr_u %>%
                             rename(from = .data$to, to = .data$from) %>%
                             mutate(dist = 0))

    # Calculate all shortest paths for upstream movement
    Bij_mat_u <- dodgr::dodgr_dists(graph_dodgr_u, from = vertices_id, to = vertices_id)

    # Create dodgr graph for downstream movement
    graph_dodgr_d <- igraph::as_data_frame(graph, what = "edges") %>%
      filter(.data$flag_dir == "d") %>%
      rename(dist = .data$d_att) %>%
      select(.data$from, .data$to, .data$dist)

    graph_dodgr_d <- rbind(graph_dodgr_d,
                           graph_dodgr_d %>%
                             rename(from = .data$to, to = .data$from) %>%
                             mutate(dist = 0))

    # Calculate all shortest paths for downstream movement
    Bij_mat_d <- dodgr::dodgr_dists(graph_dodgr_d, from = vertices_id, to = vertices_id)

    # post process matrix if exponential decay
    if(disp_type == "exponential"){
      Bij_mat = (param_u^Bij_mat_u) * (param_d^Bij_mat_d )}

    # post process matrix
    if(disp_type == "threshold"){
      Bij_mat$B_ij = ifelse(Bij_mat_u <= param_u, 1, 0) * ifelse(Bij_mat_d <= param_d, 1, 0)  }
  }

  return(Bij_mat)

}
