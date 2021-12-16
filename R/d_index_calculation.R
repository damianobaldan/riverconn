#' Calculate index improvement for scenarios of barriers removal.
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param id_dam graph edges numeric attribute used to label dams. Default is \code{"id_dam"}.
#' @param dams_metadata data.frame that must contain a column having the same name as the 'id_dam' attribute of the graph,
#' and two columns with the corresponding upstream and downstream improved passabilities (see pass_u_updated and pass_d_updated).
#' @param pass_u_updated field in dam_metadata where updated value for upstream passability is stored
#' (recommended values higher than the original passability).
#' @param pass_d_updated field in dam_metadata where updated value for downstream passability is stored
#' (recommended values higher than the original passability).
#' @param mode currentlym only \code{"leave_one_out"} is implemented.
#' @param parallel logical value to flag if parallel option is to be used.
#' @param ncores define how many cores are used in parallel processing. Active only when \code{parallel = TRUE}
#' @param index_type indicates if the index should be calculated for the whole catchment (\code{index_type = "full"}),
#' or for each reach (\code{index_type = "reach"})
#' @param index_mode indicates if reach index should be calculated based on inbound links ("to") or outbound links ("from").
#' Only active when \code{index_type = "reach"}.
#' @param param_u  upstream dispersal parameter. Must be a numeric value. Only used if \code{dir_distance_type = "asymmetric"}. See details.
#' @param param_d  downstream dispersal parameter. Must be a numeric value. Only used if \code{dir_distance_type = "asymmetric"}. See details.
#' @param param  dispersal parameter. Must be a numeric value. Only used if \code{dir_distance_type = "symmetric"}. See details.
#' @param ... other arguments passed to the function index_calculation
#'
#' @return returns a data.frame containing the percent improvement of the index for
#' each barrier present in the 'dams_metadata' variable.
#' If \code{index_type = "full"}, the data.frame is organized by 'id_dam'.
#' If \code{index_type = "reach"}, the data.frame is organized by 'id_dam' and 'name'.
#' In both cases, both numerator and denominator used in the index calculations are reported in the columns 'num' and 'den'.
#' The column 'd_index' contains the relative index improvement when each barrier is removed.
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
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom rlang .data
#'
d_index_calculation <- function(graph,
                                dams_metadata,
                                id_dam = "id_dam",
                                pass_u_updated = "pass_u_updated",
                                pass_d_updated = "pass_d_updated",
                                mode = "leave_one_out", # "add_one", # sequence
                                parallel = TRUE, ncores,
                                index_type = "full",
                                index_mode = "from",
                                ..., param_u, param_d, param){

  # Error messages if something wrong happens
  if( !(id_dam %in% colnames(dams_metadata)) ) stop("'id_dam' must be present among the column names of the input data frame 'dams_metadata'")
  if( !(pass_u_updated %in% colnames(dams_metadata)) ) stop("'pass_u_updated' must be present among the column names of the input data frame 'dams_metadata'")
  if( !(pass_d_updated %in% colnames(dams_metadata)) ) stop("'pass_d_updated' must be present among the column names of the input data frame 'dams_metadata'")
  if( !(id_dam %in% igraph::edge_attr_names(graph)) ) stop("'id_dam' argument must be a valid vertex attribute in the input graph")
  if( missing(dams_metadata) ) stop("'dams_metadata' dataframe must be specified")

  # Rename graph vertices and dams metadata based on id_dam
  igraph::E(graph)$id_dam <- igraph::get.edge.attribute(graph, id_dam)
  dams_metadata <- dams_metadata %>% rename_with( ~"id_dam", contains(id_dam))
  dams_metadata <- dams_metadata %>% rename_with( ~"pass_u_updated", contains(pass_u_updated))
  dams_metadata <- dams_metadata %>% rename_with( ~"pass_d_updated", contains(pass_d_updated))

  # More error messages
  if( c(dams_metadata$pass_u_updated < 0, dams_metadata$pass_u_updated > 1) %>% sum >0 ) stop("'pass_u_updated' must be between 0 and 1")
  if( c(dams_metadata$pass_d_updated < 0, dams_metadata$pass_d_updated > 1) %>% sum >0 ) stop("'pass_d_updated' must be between 0 and 1")
  if( parallel == TRUE & missing(ncores) ) stop("'ncores' must be specified when 'parallel = TRUE' ")

  # 1. Function to Calculate d_index when one barrier is removed
  one_barrier_removal_index <- function(dam_to_remove, river_graph, pass_u_updated , pass_d_updated, ...,
                                        pass_u = "pass_u", pass_d = "pass_d",
                                        param_u = param_u, param_d = param_d, param = param,
                                        index_type = index_type, index_mode = index_mode){

    # Change passability information
    igraph::E(river_graph)$pass_u <- igraph::get.edge.attribute(river_graph, pass_u)
    igraph::E(river_graph)$pass_d <- igraph::get.edge.attribute(river_graph, pass_d)

    # Adjust id_dam
    igraph::E(river_graph)$id_dam <- ifelse(is.na(igraph::E(river_graph)$id_dam), -999, igraph::E(river_graph)$id_dam)

    # Initialize the graph for the loop
    river_graph_att_loop <- river_graph

    # Change the passability of the corresponding edge to 1
    igraph::E(river_graph_att_loop)$pass_u[stats::na.omit(igraph::E(river_graph_att_loop)$id_dam == dam_to_remove)] <-
      dams_metadata$pass_u_updated[dams_metadata$id_dam == dam_to_remove]

    igraph::E(river_graph_att_loop)$pass_d[stats::na.omit(igraph::E(river_graph_att_loop)$id_dam == dam_to_remove)] <-
      dams_metadata$pass_d_updated[dams_metadata$id_dam == dam_to_remove]

    # Calculate index
    index <- index_calculation(river_graph_att_loop, index_type = index_type, index_mode = index_mode,
                               ...,
                               param_u = param_u, param_d = param_d, param = param)

    # Output is different for catchment and reach indices
    if (index_type == "full") {
      index <- data.frame("id_dam" = dam_to_remove) %>%
        mutate(num = index$num, den = index$den, index = index$index) }

    if (index_type == "reach") {
      index <- index %>% mutate(id_dam = dam_to_remove)
    }

    return(index)
  }


  # If parallel calculations are on
  if (parallel == TRUE) {

    # Set parallel calculation options
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)

    # Packages to import
    pcks <- c("igraph", "tidyverse")

    # Start parallel loop
    iii <- NULL
    out_index <- foreach(iii = 1:length(dams_metadata$id_dam),
                         .packages=pcks,
                         .export = c("index_calculation")) %dopar% {

      #source("dci_package_funs_v3.R") # remove in package and add my package to functions import in foreach

      # Calculate and return index
      out_index <-  one_barrier_removal_index(dams_metadata$id_dam[iii], graph, pass_u_updated , pass_d_updated, ...,
                                              pass_u = "pass_u", pass_d = "pass_d",
                                              param_u = param_u, param_d = param_d, param = param,
                                              index_type = index_type, index_mode = index_mode)

    }

    # Close the parllel processes
    parallel::stopCluster(cl)

    out_index <- do.call(rbind,out_index)

  }


  # if parallel calculations are off
  if (parallel == FALSE){

    # Apply the function
    out_index <-  do.call(rbind,
                          lapply(dams_metadata$id_dam, FUN = one_barrier_removal_index,
                                 river_graph = graph,
                                 pass_u_updated = pass_u_updated , pass_d_updated = pass_d_updated,
                                 index_type = index_type,
                                 index_mode = index_mode,
                                 ...,
                                 param_u = param_u, param_d = param_d, param = param)  )
  }


  # 3. Calculate DCI for baseline, add it to the table and  calculate relative change
  out_index_bl <- index_calculation(graph,
                                    index_type = index_type,
                                    index_mode = index_mode,
                                    ...,
                                    param_u = param_u, param_d = param_d, param = param) %>%
    rename(num_bl = .data$num, den_bl = .data$den, index_bl = .data$index)

  # Different outputs for reach and full
  if (index_type == "reach") {
    out_index <- out_index %>%
      left_join(out_index_bl, by = "name") %>%
      mutate(d_index = (.data$index - .data$index_bl) / .data$index_bl * 100) }

  if (index_type == "full") {
    out_index <- out_index  %>%
      mutate("index_bl" = out_index_bl$index_bl) %>%
      mutate(d_index = (.data$index - .data$index_bl) / .data$index_bl * 100) }

  return(out_index)

}


