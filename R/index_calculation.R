#' Reach- and Catchment-scale indices of connectivity
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param weight graph vertex attribute used to assign weights to the reaches (nodes/vertices). Should not be also an edge attribute.
#' Default is \code{"length"}.
#' @param nodes_id graph vertex attribute used to univoquely label reaches (nodes/vertices). Should not be also an edge attribute.
#' Default is \code{"name"}. The graph attribute must be a charachter vector.
#' Used to label the results when \code{index_type = "reach"}
#' @param index_type indicates if the index should be calculated for the whole catchment (\code{index_type = "full"}),
#' or for each reach (\code{index_type = "reach"})
#' @param index_mode indicates if reach index should be calculated based on inbound links ("to") or outbound links ("from").
#' Only active when \code{index_type = "reach"}.
#' @param c_ij_flag include in the calculation the barriers.
#' @param B_ij_flag include in the calculation the dispersal/movement among reaches.
#' @param dir_fragmentation_type how directionality in c_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details below.
#' @param pass_confluence a value in the range [0,1] that defines the passability of confluences (default is 1).
#' @param pass_u the 'graph' edge attribute to be used as upstream passability. Default is "pass_u".
#' @param pass_d the 'graph' edge attribute to be used as downstream passability. Default is "pass_d".
#' @param field_B the 'graph' vertex attribute to be used to calculate the distance. Should not be also an edge attribute.
#' Default is \code{"length"}.
#' @param dir_distance_type how directionality in B_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details below.
#' @param disp_type the formula used to calculate the probabilities in the B_ij matrix.
#' Use \code{"exponential"} for exponential decay, or \code{"threshold"} for setting a distance threshold.
#' @param param_u  upstream dispersal parameter. Must be a numeric value.
#' Only used if \code{dir_distance_type = "asymmetric"}. See details below.
#' @param param_d  downstream dispersal parameter.
#' Must be a numeric value. Only used if \code{dir_distance_type = "asymmetric"}. See below for details.
#' @param param  dispersal parameter. Must be a numeric value.
#' Only used if \code{dir_distance_type = "symmetric"}. See details below.
#'
#' @return If \code{index_type = "full"}, returns a numeric value with the index value (column 'index').
#' if \code{index_type = "reach"}, returns a data frame with the index value (column 'index') for each reach
#' (the field specified in 'nodes_id' is used for reach identification in the data frame).
#' In both cases, both numerator and denominator used in the index calculations are reported in the columns 'num' and 'den'.
#' @export
#'
#' @details
#' Setting \code{c_ij_flag = FALSE} removes from the calculations the effect of barriers, i.e. the c_{ij} contribution
#' is not used in the calculation of the index.
#' Setting \code{B_ij_flag = FALSE} removes from the calculations the effect of movement/dispersal,
#' i.e. the B_{ij} contribution is not used in the calculation of the index.
#' Note that it is not possible to set both \code{c_ij_flag = FALSE} and \code{B_ij_flag = FALSE}.
#'
#' The setting \code{dir_distance_type = "symmetric"} is to be used when the directionality of the river network is not relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches.
#' The setting \code{dir_distance_type = "asymmetric"} is to be used when the directionality is relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches and splitted
#' between 'upstream travelled' distance and 'downstream travelled' distance
#'
#' The 'param_u', 'param_d', and 'param' values are interpreted differently based on the formula used to relate distance and probability.
#' When \code{disp_type ="exponential"}, those values are used as the base of the exponential dispersal kernel: B_ij = param^{d_ij}.
#' When \code{disp_type ="threshold"}, those values are used to define the maximum dispersal length: B_ij = ifelse(d_ij < param, 1, 0).
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#'
index_calculation <- function(graph,
                              weight = "length",
                              nodes_id = "name",
                              index_type = "full",
                              index_mode = "to",
                              c_ij_flag = TRUE,
                              B_ij_flag = TRUE,
                              dir_fragmentation_type = "symmetric",
                              pass_confluence = 1, pass_u = "pass_u", pass_d = "pass_d",
                              field_B = "length",
                              dir_distance_type = "symmetric",
                              disp_type = "exponential",
                              param_u, param_d, param) {

  # Error messages
  if( !(class(graph) ==  "igraph")) stop(
    "'graph' must be an 'igraph' object")
  if( !(index_type %in% c("full", "reach")) ) stop(
    "'index_type' must me either 'full' or 'reach'")
  if( index_type == "reach" & !(index_mode %in% c("from", "to")) ) stop(
    "'index_mode' must me either 'from' or 'to'")
  if( index_type == "reach" & missing(index_mode) ) stop(
    "'index_mode' must me defined when index_type = 'reach'")
  if( !(weight %in% igraph::vertex_attr_names(graph)) ) stop(
    "'weight' argument must be a valid vertex attribute in 'graph'")
  if( !(nodes_id %in% igraph::vertex_attr_names(graph)) ) stop(
    "'nodes_id' argument must be a valid vertex attribute in 'graph'")
  if( !(c_ij_flag | B_ij_flag) ) stop(
    "at least one among c_if and B_ij should be selected for calculations")
  if( length(igraph::vertex_attr(graph, nodes_id)) < igraph::gorder(graph)  ) stop(
    "'nodes_id' must be unique for each vertex")
  if( !igraph::is_connected(graph)  ) stop(
    "'graph' must be connected (check if some nodes are disconnected with igraph::components() )")
  if( (dir_fragmentation_type == "asymmetric" |  dir_distance_type == "asymmetric") & igraph::is_directed(graph) == FALSE ) stop(
    "'graph' must be directed when 'dir_fragmentation_type' or 'dir_distance_type' are set to 'asymmetric'")
  if( weight %in% igraph::edge_attr_names(graph) ) stop(
    "'weight' argument must be a edge attribute in 'graph'")
  if( field_B %in% igraph::edge_attr_names(graph) ) stop(
    "'field_B' argument must be a edge attribute in 'graph'")
  if( class(igraph::get.vertex.attribute(graph, nodes_id)) != "character") stop(
    "'nodes_id' attribute of 'graph' must be of type 'charachter'")

  if(c_ij_flag == TRUE){
    if( !( pass_u %in% igraph::edge_attr_names(graph)) ) stop(
      "'pass_u' argument must be a edge attribute in 'graph'")
    if( !(pass_d %in% igraph::edge_attr_names(graph)) ) stop(
      "'pass_d' argument must be a edge attribute in 'graph'")  }


  # What happens if B_ij_flag is false? suppress further warnings
  if(B_ij_flag == FALSE) {param_u = param_d = param <- NA}
  if(dir_distance_type == "symmetric") {param_u = param_d <- NA}
  if(dir_distance_type == "asymmetric") {param  <- NA}

  # Set the names of the vertices. By default keep the name argument.
  igraph::V(graph)$name <- igraph::vertex_attr(graph, nodes_id)

  # 1. Calculate c_ij (get arguments from index_calculation function)
  if (c_ij_flag == TRUE) {
    c_ij_mat <- c_ij_fun(graph,
                         dir_fragmentation_type = dir_fragmentation_type,
                         pass_confluence = pass_confluence,
                         pass_u = pass_u,
                         pass_d = pass_d)
  }


  # 2. Calculate B_ij  (get arguments from index_calculation function)
  if (B_ij_flag == TRUE) {
    B_ij_mat <- B_ij_fun(graph,
                         field_B = field_B,
                         dir_distance_type = dir_distance_type,
                         disp_type = disp_type,
                         param_u = param_u,
                         param_d = param_d,
                         param = param)
  }

  # 3. Aggregate results
  if(c_ij_flag == TRUE & B_ij_flag == TRUE) {
    agg_mat <- c_ij_mat %>%
      dplyr::left_join(B_ij_mat, by = c("from", "to")) %>%
      dplyr::mutate(P = .data$c_ij * .data$B_ij) }

  if(c_ij_flag == TRUE & B_ij_flag == FALSE){
    agg_mat <- c_ij_mat %>%
      dplyr::mutate(P = .data$c_ij ) }

  if(c_ij_flag == FALSE & B_ij_flag == TRUE) {
    agg_mat <- B_ij_mat %>%
      dplyr::mutate(P = .data$B_ij) }

  # 4. Get the weight information

  # Get vertices list from graph
  g_v_df <- dplyr::rename_with(
    igraph::as_data_frame(graph, what = "vertices"),
    ~"weight_node", contains(weight))

  # Join probaility matrix with weight information
  agg_mat <- agg_mat %>%
    dplyr::left_join( g_v_df %>% dplyr::select(.data$name, .data$weight_node) %>%
                 dplyr::rename(from = .data$name, weight_from = .data$weight_node), by = "from"  ) %>%
    dplyr::left_join( g_v_df %>% dplyr::select(.data$name, .data$weight_node) %>%
                 dplyr::rename(to = .data$name, weight_to = .data$weight_node), by = "to" )

  # 5. Calculate indices

  # If the full index is to be calculated
  if (index_type == "full") {

    agg_mat <- agg_mat %>%
      dplyr::mutate(prod = .data$P * .data$weight_from * .data$weight_to)
    index_num <- sum( agg_mat$prod )
    index_den <- ( sum( g_v_df$weight_node ) )^2
    index = index_num / index_den

    index <- data.frame("num" = index_num, "den" = index_den, "index" = index)
  }

  # If the reach index is to be calculated
  if (index_type == "reach") {

    index <- agg_mat %>%
      dplyr::rename_with(~"weight", contains("weight_node") & !contains(index_mode)) %>%
      dplyr::select(-matches(paste0("weight_",index_mode))) %>%
      dplyr::rename_with(~"name", contains(index_mode) ) %>%
      dplyr::rename_with(~ "weight_node", contains("weight_") ) %>%
      dplyr::mutate(prod = .data$weight_node * .data$P) %>%
      dplyr::group_by(.data$name) %>%
      dplyr::summarize(num = sum(.data$prod)) %>%
      dplyr::mutate(den = sum( g_v_df$weight_node ), index = .data$num/.data$den ) %>%
      dplyr::rename_with(~nodes_id, contains("name") )
  }

  # Return value or df with output
  return(index)


}


