#' Calculate index improvement for scenarios of barriers removal - function not for export
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
#' @param weight check index_calculation function
#' @param nodes_id check index_calculation function
#' @param index_type check index_calculation function
#' @param index_mode check index_calculation function
#' @param c_ij_flag check index_calculation function
#' @param B_ij_flag check index_calculation function
#' @param dir_fragmentation_type check index_calculation function
#' @param pass_confluence check index_calculation function
#' @param pass_u check index_calculation function
#' @param pass_d check index_calculation function
#' @param field_B check index_calculation function
#' @param dir_distance_type check index_calculation function
#' @param disp_type check index_calculation function
#' @param param_u check index_calculation function
#' @param param_d check index_calculation function
#' @param param check index_calculation function
#' @param ncores define how many cores are used in parallel processing. Active only when \code{parallel = TRUE}
#'
#' @return returns a data.frame containing the percent improvement of the index for
#' each barrier present in the 'dams_metadata' variable.
#' If \code{index_type = "full"}, the data.frame is organized by 'id_dam'.
#' If \code{index_type = "reach"}, the data.frame is organized by 'id_dam' and 'name'.
#' In both cases, both numerator and denominator used in the index calculations are reported in the columns 'num' and 'den'.
#' The column 'd_index' contains the relative index improvement when each barrier is removed.
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
#' @keywords internal
#'
inner_d_index_calculation <- function(graph,
                                      dams_metadata,
                                      id_dam ,
                                      pass_u_updated ,
                                      pass_d_updated ,
                                      mode = "leave_one_out", # "add_one", # sequence
                                      parallel ,
                                      ncores,
                                      weight ,
                                      nodes_id ,
                                      index_type ,
                                      index_mode ,
                                      c_ij_flag ,
                                      B_ij_flag ,
                                      dir_fragmentation_type ,
                                      pass_confluence ,
                                      pass_u ,
                                      pass_d ,
                                      field_B ,
                                      dir_distance_type ,
                                      disp_type ,
                                      param_u ,
                                      param_d,
                                      param ){

  # Error messages if something wrong happens
  if( !(class(dams_metadata) ==  "data.frame")) stop(
    "'dams_metadata' must be a data.frame")
  if( !(id_dam %in% colnames(dams_metadata)) ) stop(
    "'id_dam' must be present among the column names of the input data frame 'dams_metadata'")
  if( !(pass_u_updated %in% colnames(dams_metadata)) ) stop(
    "'pass_u_updated' must be present among the column names of the input data frame 'dams_metadata'")
  if( !(pass_d_updated %in% colnames(dams_metadata)) ) stop(
    "'pass_d_updated' must be present among the column names of the input data frame 'dams_metadata'")
  if( !(id_dam %in% igraph::edge_attr_names(graph)) ) stop(
    "'id_dam' argument must be a valid vertex attribute in the input graph")
  if( missing(dams_metadata) ) stop(
    "'dams_metadata' dataframe must be specified")
  if(id_dam == nodes_id ) stop(
    "please specify two different attributes for 'id_dam'and 'nodes_id' in the graph")
  if( !( pass_u %in% igraph::edge_attr_names(graph)) ) stop(
    "'pass_u' argument must be a edge attribute in 'graph'")
  if( !(pass_d %in% igraph::edge_attr_names(graph)) ) stop(
    "'pass_d' argument must be a edge attribute in 'graph'")

  # Rename graph vertices and dams metadata based on id_dam
  igraph::E(graph)$id_dam <- igraph::get.edge.attribute(graph, id_dam) %>% as.character()
  dams_metadata <- dams_metadata %>% dplyr::rename_with( ~"id_dam", contains(id_dam))
  dams_metadata <- dams_metadata %>% dplyr::rename_with( ~"pass_u_updated", contains(pass_u_updated))
  dams_metadata <- dams_metadata %>% dplyr::rename_with( ~"pass_d_updated", contains(pass_d_updated))

  # Change passability information
  igraph::E(graph)$pass_u <- igraph::get.edge.attribute(graph, pass_u)
  igraph::E(graph)$pass_d <- igraph::get.edge.attribute(graph, pass_d)

  # More error messages
  if( length(unique(dams_metadata$id_dam)) <  length(dams_metadata$id_dam) ) stop(
    "'id_dam' in 'dams_metadata' must be unique")
  if( c(dams_metadata$pass_d_updated < 0, dams_metadata$pass_d_updated > 1) %>% sum >0 ) stop(
    "'pass_d_updated' must be between 0 and 1")
  if( parallel == TRUE & missing(ncores) ) stop(
    "'ncores' must be specified when 'parallel = TRUE' ")

  # What if param, param_u, or param_d are missing
  if(missing(param_u)) {param_u  <- NA}
  if(missing(param_d)) {param_d  <- NA}
  if(missing(param)) {param  <- NA}

  ##### function to calculate improvements #####
  one_barrier_removal_index <- function(dam_to_remove,
                                        dams_metadata,
                                        pass_u_updated = "pass_u_updated",
                                        pass_d_updated = "pass_d_updated",
                                        river_graph,
                                        weight = "length", nodes_id = "name",
                                        index_type = "full", index_mode = "to",
                                        c_ij_flag = TRUE, B_ij_flag = TRUE,
                                        dir_fragmentation_type = "symmetric", pass_confluence = 1,
                                        pass_u = "pass_u", pass_d = "pass_d",
                                        field_B = "length", dir_distance_type = "symmetric",
                                        disp_type = "exponential",
                                        param_u = NA, param_d = NA, param = NA){

    # Error messages
    if(!(dam_to_remove %in% dams_metadata$id_dam)) stop(
      "you are trying to remove a dam that is not listed in 'dams_metadata'")

    # Change the passability of the corresponding edge
    # to the value defined in pass_u_updated and pass_d_updated
    igraph::E(river_graph)$pass_u[igraph::E(river_graph)$id_dam == dam_to_remove] <-
      dams_metadata$pass_u_updated[dams_metadata$id_dam == dam_to_remove]

    igraph::E(river_graph)$pass_d[igraph::E(river_graph)$id_dam == dam_to_remove] <-
      dams_metadata$pass_d_updated[dams_metadata$id_dam == dam_to_remove]

    # Calculate index
    index <- index_calculation(river_graph,
                               weight = weight,
                               nodes_id = nodes_id,
                               index_type = index_type,
                               index_mode = index_mode,
                               c_ij_flag = c_ij_flag,
                               B_ij_flag = B_ij_flag,
                               dir_fragmentation_type = dir_fragmentation_type,
                               pass_confluence = pass_confluence,
                               pass_u = pass_u,
                               pass_d = pass_d,
                               field_B = field_B,
                               dir_distance_type = dir_distance_type,
                               disp_type = disp_type,
                               param_u = param_u,
                               param_d = param_d,
                               param = param)

    # Output is different for catchment and reach indices
    if (index_type == "full") {
      index <- data.frame("id_dam" = dam_to_remove) %>%
        dplyr::mutate(num = index$num, den = index$den, index = index$index) }

    if (index_type == "reach") {
      index <- index %>%
        dplyr::mutate(id_dam = dam_to_remove)
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
                         .export = c("one_barrier_removal_index")) %dopar% {
                           # Calculate and return index
                           out_foreach <- one_barrier_removal_index(dam_to_remove = dams_metadata$id_dam[iii],
                                                                    dams_metadata = dams_metadata,
                                                                    pass_u_updated = pass_u_updated,
                                                                    pass_d_updated = pass_d_updated,
                                                                    river_graph = graph,
                                                                    weight = weight,
                                                                    nodes_id = nodes_id,
                                                                    index_type = index_type,
                                                                    index_mode = index_mode,
                                                                    c_ij_flag = c_ij_flag,
                                                                    B_ij_flag = B_ij_flag,
                                                                    dir_fragmentation_type = dir_fragmentation_type,
                                                                    pass_confluence = pass_confluence,
                                                                    pass_u = "pass_u",
                                                                    pass_d = "pass_d",
                                                                    field_B = field_B,
                                                                    dir_distance_type = dir_distance_type,
                                                                    disp_type = disp_type,
                                                                    param_u = param_u,
                                                                    param_d = param_d,
                                                                    param = param)
                           return(out_foreach)
                         }

    # Close the parllel processes
    parallel::stopCluster(cl)

    out_index <- do.call(rbind, out_index)

  }


  # if parallel calculations are off
  if (parallel == FALSE){

    # Apply the function
    out_index <-  do.call(rbind,
                          lapply(dams_metadata$id_dam,
                                 FUN =  one_barrier_removal_index,
                                 dams_metadata = dams_metadata,
                                 pass_u_updated = pass_u_updated,
                                 pass_d_updated = pass_d_updated,
                                 river_graph = graph,
                                 weight = weight,
                                 nodes_id = nodes_id,
                                 index_type = index_type,
                                 index_mode = index_mode,
                                 c_ij_flag = c_ij_flag,
                                 B_ij_flag = B_ij_flag,
                                 dir_fragmentation_type = dir_fragmentation_type,
                                 pass_confluence = pass_confluence,
                                 pass_u = "pass_u",
                                 pass_d = "pass_d",
                                 field_B = field_B,
                                 dir_distance_type = dir_distance_type,
                                 disp_type = disp_type,
                                 param_u = param_u,
                                 param_d = param_d,
                                 param = param)  )
  }


  # 3. Calculate DCI for baseline, add it to the table and  calculate relative change
  out_index_bl <-  riverconn::index_calculation(graph,
                                                weight = weight,
                                                nodes_id = nodes_id,
                                                index_type = index_type,
                                                index_mode = index_mode,
                                                c_ij_flag = c_ij_flag,
                                                B_ij_flag = B_ij_flag,
                                                dir_fragmentation_type = dir_fragmentation_type,
                                                pass_confluence = pass_confluence,
                                                pass_u = "pass_u",
                                                pass_d = "pass_d",
                                                field_B = field_B,
                                                dir_distance_type = dir_distance_type,
                                                disp_type = disp_type,
                                                param_u = param_u,
                                                param_d = param_d,
                                                param = param) %>%
    rename(num_bl = .data$num, den_bl = .data$den, index_bl = .data$index)

  # Different outputs for reach and full
  if (index_type == "reach") {
    out_index <- out_index %>%
      dplyr::left_join(out_index_bl, by = nodes_id) %>%
      dplyr::mutate(d_index = (.data$index - .data$index_bl) / .data$index_bl * 100) %>%
      dplyr::rename_with(~id_dam, contains("id_dam"))    }

  if (index_type == "full") {
    out_index <- out_index  %>%
      dplyr::mutate("index_bl" = out_index_bl$index_bl) %>%
      dplyr::mutate(d_index = (.data$index - .data$index_bl) / .data$index_bl * 100) %>%
      dplyr::rename_with(~id_dam, contains("id_dam"))     }

  return(out_index)

}


