#' Reach- and Catchment-scale indices of connectivity
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param weight graph vertex attribute used to assign weights to the reaches (nodes/vertices). Should not be also an edge attribute.
#' Default is \code{"length"}.
#' @param nodes_id graph vertex attribute used to univoquely label reaches (nodes/vertices). Should not be also an edge attribute.
#' Default is \code{"name"}. The graph attribute must be a character vector.
#' Used to label the results when \code{index_type = "reach"}
#' @param index_type indicates if the index should be calculated for the whole catchment (\code{index_type = "full"}),
#'  for each reach (\code{index_type = "reach"}), or for each barrier (\code{index_type = "sum"})
#' @param index_mode indicates if reach index should be calculated based on inbound links ("to") or outbound links ("from").
#' Only active when \code{index_type = "reach"}.
#' @param c_ij_flag include the presence of barriers in the calculations (c_ij term).
#' @param B_ij_flag include dispersal/movement among reaches in the calculations (B_ij term).
#' @param dir_fragmentation_type how directionality in c_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details below.
#' @param pass_confluence a value in the range [0,1] that defines the passability of confluences (default is 1).
#' @param pass_u the 'graph' edge attribute to be used as upstream passability. Default is "pass_u".
#' @param pass_d the 'graph' edge attribute to be used as downstream passability. Default is "pass_d".
#' @param field_B the 'graph' vertex attribute to be used to calculate the distance. Should not be also an edge attribute.
#' Default is \code{"length"}.
#' @param dir_distance_type how directionality in B_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details.
#' @param disp_type the formula used to calculate the probabilities in the B_ij matrix.
#' Use \code{"exponential"} for exponential decay, \code{"threshold"} for setting a distance threshold,
#' or \code{"leptokurtic"} for leptokurtic dispersal.
#' @param param_u  upstream dispersal parameter. Must be a numeric value.
#' Only used if \code{dir_distance_type = "asymmetric"}. See details below.
#' @param param_d  downstream dispersal parameter.
#' Must be a numeric value. Only used if \code{dir_distance_type = "asymmetric"}. See below for details.
#' @param param  dispersal parameter. Must be a numeric value.
#' Only used if \code{dir_distance_type = "symmetric"}. See details below.
#' @param param_l the parameters for the leptokurtic dispersal mode. Must be a numeric vector of the
#' type \code{c(sigma_stat, sigma_mob, p)}. See details below.
#'
#' @return If \code{index_type = "full"}, returns a numeric value with the index value (column 'index').
#' if \code{index_type = c("reach", "sum")}, returns a data frame with the index value (column 'index') for each reach
#' (the field specified in 'nodes_id' is used for reach identification in the data frame).
#' In both cases, both numerator and denominator used in the index calculations are reported in the columns 'num' and 'den'.
#' @export
#'
#' @details
#' Setting \code{c_ij_flag = FALSE} removes from the calculations the effect of barriers, i.e. the c_ij contribution
#' is not used in the calculation of the index.
#' Setting \code{B_ij_flag = FALSE} removes from the calculations the effect of movement/dispersal,
#' i.e. the B_ij contribution is not used in the calculation of the index.
#' Note that it is not possible to set both \code{c_ij_flag = FALSE} and \code{B_ij_flag = FALSE}.
#'
#' The setting \code{dir_distance_type = "symmetric"} is to be used when the directionality of the river network is not relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches.
#' The setting \code{dir_distance_type = "asymmetric"} is to be used when the directionality is relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches and splitted
#' between 'upstream travelled' distance and 'downstream travelled' distance.
#' When \code{disp_type ="leptokurtic"} is selected, symmetric dispersal is assumed.
#'
#' The 'param_u', 'param_d', and 'param' values are interpreted differently based on the formula used to relate distance (d_ij) and probability (B_ij).
#' When \code{disp_type ="exponential"}, those values are used as the base of the exponential dispersal kernel: B_ij = param^d_ij.
#' When \code{disp_type ="threshold"}, those values are used to define the maximum dispersal length: B_ij = ifelse(d_ij < param, 1, 0).
#'
#' When \code{disp_type ="leptokurtic"} is selected, a leptokurtic dispersal kernel is used to calculate B_ij.
#' A leptokurtic dispersal kernel is a mixture of two zero-centered gaussian distributions with standard deviations
#' \code{sigma_stat} (static part of the population), and \code{sigma_mob} (mobile part of the population).
#' The probability of dispersal is calculated as: B_ij = p F(0, sigma_stat, d_ij) + (1-p) F(0, sigma_mob, d_ij)
#' where F is the upper tail of the gaussian cumulative density function.
#'
#'
#' @references
#' Baldan, D., Cunillera-Montcusí, D., Funk, A., & Hein, T. (2022). Introducing ‘riverconn’: an R package to assess river connectivity indices. Environmental Modelling & Software, 156, 105470.
#'
#' Jumani, S., Deitch, M. J., Kaplan, D., Anderson, E. P., Krishnaswamy, J., Lecours, V., & Whiles, M. R. (2020). River fragmentation and flow alteration metrics: a review of methods and directions for future research. Environmental Research Letters, 15(12), 123009.
#'
#' Radinger, J., & Wolter, C. (2014). Patterns and predictors of fish dispersal in rivers. Fish and fisheries, 15(3), 456-473.
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#'
#' @examples
#' library(igraph)
#' g <- igraph::graph_from_literal(1-+2, 2-+5, 3-+4, 4-+5, 6-+7,
#' 7-+10, 8-+9, 9-+10, 5-+11, 11-+12, 10-+13, 13-+12, 12-+14, 14-+15, 15-+16)
#' E(g)$id_dam <- c("1", NA, "2", "3", NA, "4", NA, "5", "6", NA,  NA, NA, NA, "7", NA)
#' E(g)$type <- ifelse(is.na(E(g)$id_dam), "joint", "dam")
#' V(g)$length <- c(1, 1, 2, 3, 4, 1, 5, 1, 7, 7, 3, 2, 4, 5, 6, 9)
#' V(g)$HSI <- c(0.2, 0.1, 0.3, 0.4, 0.5, 0.5, 0.5, 0.6, 0.7, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
#' V(g)$Id <- V(g)$name
#' E(g)$pass_u <- E(g)$pass_d <- ifelse(!is.na(E(g)$id_dam),0.1,NA)
#' index <- index_calculation(g, param = 0.9)
#'
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
                              param_u, param_d, param, param_l) {

  # Error messages
  if( !igraph::is_igraph(graph)) stop(
    "'graph' must be an 'igraph' object")
  if( !(index_type %in% c("full", "reach", "sum")) ) stop(
    "'index_type' must me either 'full', 'reach', or 'sum'")
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
  if( !is.character(igraph::get.vertex.attribute(graph, nodes_id))) stop(
    "'nodes_id' attribute of 'graph' must be of type 'charachter'")

  if(c_ij_flag == TRUE){
    if( !( pass_u %in% igraph::edge_attr_names(graph)) ) stop(
      "'pass_u' argument must be a edge attribute in 'graph'")
    if( !(pass_d %in% igraph::edge_attr_names(graph)) ) stop(
      "'pass_d' argument must be a edge attribute in 'graph'")  }


  # What happens if B_ij_flag is false? suppress further warnings
  if(B_ij_flag == FALSE) {param_u = param_d = param = param_l <- NA}
  if(dir_distance_type == "symmetric") {param_u = param_d <- NA}
  if(dir_distance_type == "asymmetric") {param  <- NA}
  if(disp_type == "leptokurtic") {param_u = param_d = param  <- NA}

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
                         param = param,
                         param_l = param_l)
  }

  # 3. Aggregate c_ij and B_ij
  if(c_ij_flag == TRUE & B_ij_flag == TRUE) {
    agg_mat <- c_ij_mat * B_ij_mat }

  if(c_ij_flag == TRUE & B_ij_flag == FALSE){
    agg_mat <- c_ij_mat  }

  if(c_ij_flag == FALSE & B_ij_flag == TRUE) {
    agg_mat <- B_ij_mat  }

  # 4. Get the weight information

  # Get vertices list from graph
  g_v_df <- dplyr::rename_with(
    igraph::as_data_frame(graph, what = "vertices"),
    ~"weight_node", contains(weight))

  # Create veights vector
  v_weights <- g_v_df$weight_node

  # 5. Calculate indices

  # If the full index is to be calculated
  if (index_type == "full") {

    index_num = t(v_weights) %*% agg_mat %*% v_weights
    index_den = sum(v_weights)^2
    index = index_num / index_den

    index <- data.frame("num" = index_num,
                        "den" = index_den,
                        "index" = index)
  }

  # If the reach index is to be calculated
  if (index_type == "reach") {

    if (index_mode == "to"){
      index_num =  agg_mat %*% v_weights }

    if (index_mode == "from"){
      index_num = t(t(v_weights) %*% agg_mat)
    }

    index_den = sum(v_weights)
    index = index_num / index_den

    index = data.frame("name" = igraph::V(graph)$name,
                       "num" = index_num,
                       "den" = index_den,
                       "index" = index) %>%
      dplyr::rename_with(~nodes_id, contains("name") )

  }

  # If the summation index is to be calculated
  if (index_type == "sum") {

    # Error message when dams are not clearly identified
    if( !("type" %in% igraph::edge_attr_names(graph)) ) stop(
      "the graph's edges must contain the 'type' attribute with labels 'dam' or
      'link' depending on the role of the edge (barrier or confluence).
      Essential to calculate CAFI properly.")

    # Rename the passabilities based on user names
    # Set the names of the vertices. By default keep the name argument.
    igraph::E(graph)$pass_u <- igraph::get.edge.attribute(graph, pass_u)
    igraph::E(graph)$pass_d <- igraph::get.edge.attribute(graph, pass_d)

    # Fill up the passabilities of the confluences -before it's NANs-
    igraph::E(graph)$pass_u <- ifelse(is.na(igraph::E(graph)$pass_u), pass_confluence, igraph::E(graph)$pass_u)
    igraph::E(graph)$pass_d <- ifelse(is.na(igraph::E(graph)$pass_d), pass_confluence, igraph::E(graph)$pass_d)

    # Calculate passability based on symmetric or asymmetric
    if(dir_fragmentation_type == "symmetric") {
      igraph::E(graph)$pass <- igraph::E(graph)$pass_d * igraph::E(graph)$pass_u  }
    if(dir_fragmentation_type == "asymmetric" & index_mode == "to") {
      igraph::E(graph)$pass <- igraph::E(graph)$pass_d }
    if(dir_fragmentation_type == "asymmetric" & index_mode == "from") {
      igraph::E(graph)$pass <- igraph::E(graph)$pass_u }

    # Calculate index
    g_v_df <- dplyr::rename_with(
      igraph::as_data_frame(graph, what = "vertices"),
      ~"weight_node", contains(weight))

    g_e_df <- igraph::as_data_frame(graph, what = "edges") %>%
      dplyr::filter(.data$type == "dam") %>%
      dplyr::mutate(pass = 1 - .data$pass ) %>%
      dplyr::select(.data$from, .data$pass) %>%
      dplyr::rename(name = .data$from) %>%
      dplyr::left_join(g_v_df)

    index = sum(g_e_df$pass*g_e_df$weight_node / max(g_v_df$weight_node) )

  }

  # Return value or df with output
  return(index)


}


