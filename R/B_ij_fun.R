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
#' @return a matrix in data.frame format whose columns are 'from', 'to', and 'B_ij'.
#' For diagnosing purposes, also the distances are reported in the columns
#' 'n' (undirected idstance), 'u'(upstream distance), and 'd'(downstream distance).
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
#'
B_ij_fun <- function(graph, field_B = "length", dir_distance_type = "symmetric", disp_type = "exponential", param_u , param_d , param ) {

  # Error messages
  if( !(field_B %in% igraph::vertex_attr_names(graph)) ) stop(
    "'field_B' argument must be a valid vertex attribute in 'graph'")
  if( !(disp_type %in% c("exponential", "threshold")) ) stop(
    "'disp_type' must be either 'exponential' or 'threshold'")

  # Errors for asymmetric distance
  if( dir_distance_type == "asymmetric"){
    if( missing(param_u) ) stop(
      "'param_u' must be defined when dir_distance_type = 'asymmetric'")
    if( missing(param_d) ) stop(
      "'param_d' must be defined when dir_distance_type = 'asymmetric'")
    if( param_u < 0 | param_d < 0 ) stop(
      "'param_u', 'param_d', and 'param' must be > 0")
    if(  !is.numeric(param_u)) stop(
      "'param_u' must be numeric")
    if( !is.numeric(param_d)) stop(
      "'param_d' must be numeric")
    if(disp_type == "exponential") {
      if( param_u > 1 | param_d > 1 ) stop(
        "'param_u' and 'param_d' must be < 1 for disp_type == 'exponential'")
    }}

  # Errors for symmetric distance
  if( dir_distance_type == "symmetric" ) {
    if( missing(param) )  stop(
      "'param' must be specified when dir_distance_type = 'asymmetric'")
    if( param < 0  ) stop(
      "'param_u', 'param_d', and 'param' must be > 0")
    if(disp_type == "exponential") {
      if( param > 1 ) stop(
        "'param' must be < 1 for disp_type == 'exponential'")
    } }


  # Set the directionality for Bij calculations
  graph <- set_B_directionality(graph,
                                dir_distance_type = dir_distance_type,
                                field_B = field_B)

  # Extract the vertices names
  vertices_id <- names(V(graph))

  # Create data frame with all the combinations
  Bij_mat <- tidyr::expand_grid(from = vertices_id, to = vertices_id)

  # Function that extracts subgraph and calculate upstream and downstream distances
  dist_calc <- function(i,j, graph){

    subgraph <- igraph::subgraph.edges(
      graph, igraph::shortest_paths(graph, from = i, to = j, mode = "out", output = "both")$epath[[1]])

    d_att = igraph::get.edge.attribute(subgraph, "d_att")
    flag_dir = igraph::get.edge.attribute(subgraph, "flag_dir")

    output <- data.frame("from" = i, "to" = j,
                         "u" = sum(d_att[flag_dir == "u"]),
                         "d" = sum(d_att[flag_dir == "d"]),
                         "n" = sum(d_att[flag_dir == "n"]) )

    return(list(output))
  }

  # Calculate u/d/n distances for each couple of nodes
  Bij_mat <-  do.call(rbind,
                      mapply(
                        FUN = dist_calc,
                        Bij_mat$from, Bij_mat$to, list(graph))
                      )

  # symmetric dispersal: I use only the sum of the distances
  if(dir_distance_type == "symmetric"){

    Bij_mat <- Bij_mat %>% dplyr::select(.data$from, .data$to, .data$n)

    # if exponential decay
    if(disp_type == "exponential"){
      Bij_mat$B_ij = param^Bij_mat$n }

    # if threshold decay
    if(disp_type == "threshold"){
      Bij_mat$B_ij = ifelse(Bij_mat$n < param, 1, 0) }
  }

  # asymmetric dispersal: I use both distances
  if(dir_distance_type == "asymmetric"){

    Bij_mat <- Bij_mat %>% dplyr::select(.data$from, .data$to, .data$u, .data$d)

    # if exponential decay
    if(disp_type == "exponential"){
      Bij_mat$B_ij = (param_u^Bij_mat$u) * (param_d^Bij_mat$d )}

    # if threshold decay
    if(disp_type == "threshold"){
      Bij_mat$B_ij = ifelse(Bij_mat$u <= param_u, 1, 0) * ifelse(Bij_mat$d <= param_d, 1, 0)  }
  }

  return(Bij_mat)

}
