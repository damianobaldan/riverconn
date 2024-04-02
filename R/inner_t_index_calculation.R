#' Calculates time-dependent index when nodes weights or barriers passability are changing - not for exporting
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param barriers_metadata data.frame that must contain a column having the same name as the 'id_barrier' attribute of the graph,
#'  and two columns with the corresponding upstream and downstream improved passabilities (see pass_u and pass_d), and a column with the year
#'  passability was changed. This data frame can be obtained from easily-formatted data with the function \code{t_passability_sequencer}.
#' @param id_barrier graph edges numeric attribute used to label barriers. Default is \code{"id_barrier"}. It should be present in the 'barriers metadata' input as well.
#' @param year field of the 'barriers metadata' where temporal information on the changes in passabiity is stored.
#' @param pass_u field of the 'barriers metadata' where temporal-dependent upstream passabiity is stored.
#' @param pass_d field of the 'barriers metadata' where temporal-dependent downstream passabiity is stored.
#' @param weights_metadata data.frame that must contain a column having the same name as the 'nodes_id' attribute of the graph,
#'  a column with he corresponding weight information (see 'weight' parameter), and a column with the year
#'  weight was changed. This data frame can be obtained from easily-formatted data with the function \code{t_weight_sequencer}.
#' @param weight param weight graph vertex attribute used to assign weights to the reaches (nodes). Default is \code{"length"}.
#' @param nodes_id graph vertex attribute used to univoquely label reaches (nodes). Default is \code{"name"}.
#' @param parallel logical value to flag if parallel option is to be used.
#' @param ncores define how many cores are used in parallel processing. Active only when \code{parallel = TRUE}
#' @param index_type check index_calculation function.
#' @param index_mode check index_calculation function.
#' @param c_ij_flag check index_calculation function.
#' @param B_ij_flag check index_calculation function.
#' @param dir_fragmentation_type check index_calculation function.
#' @param pass_confluence check index_calculation function.
#' @param pass_u check index_calculation function.
#' @param pass_d check index_calculation function.
#' @param field_B check index_calculation function.
#' @param dir_distance_type check index_calculation function.
#' @param disp_type check index_calculation function.
#' @param param_u check index_calculation function.
#' @param param_d check index_calculation function.
#' @param param check index_calculation function.
#'
#' @return a data.frame with a 'year' field and related connectity index.
#' If \code{index_type = "reach"}, the data.frame is organized by 'year' and 'name'.
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
inner_t_index_calculation <- function(graph,
                                      barriers_metadata,
                                      id_barrier ,
                                      year ,
                                      pass_u ,
                                      pass_d ,
                                      weights_metadata,
                                      weight ,
                                      nodes_id ,
                                      parallel ,
                                      ncores,
                                      index_type ,
                                      index_mode ,
                                      c_ij_flag ,
                                      B_ij_flag ,
                                      dir_fragmentation_type ,
                                      pass_confluence ,
                                      field_B ,
                                      dir_distance_type ,
                                      disp_type ,
                                      param_u ,
                                      param_d ,
                                      param,
                                      param_l ){

  # Error messages if something wrong happens
  if(missing(graph)) stop(
    "'graph' must be defined")
  if(missing(barriers_metadata) | missing(weights_metadata)) stop(
    "either 'barriers_metadata' or 'weights_metadata' must be defined")
  if( !(id_barrier %in% colnames(barriers_metadata)) ) stop(
    "'id_barrier' must be present among the column names of the input data frame 'barriers_metadata'")
  if( !(year %in% colnames(barriers_metadata)) ) stop(
    "'id_barrier' must be present among the column names of the input data frame 'barriers_metadata'")
  if( !(pass_u %in% colnames(barriers_metadata)) ) stop(
    "'pass_u_updated' must be present among the column names of the input data frame 'barriers_metadata'")
  if( !(pass_d %in% colnames(barriers_metadata)) ) stop(
    "'pass_d_updated' must be present among the column names of the input data frame 'barriers_metadata'")
  if( !(weight %in% colnames(weights_metadata)) ) stop(
    "'weight' must be present among the column names of the input data frame 'weights_metadata'")
  if( !(nodes_id %in% colnames(weights_metadata)) ) stop(
    "'nodes_id' must be present among the column names of the input data frame 'weights_metadata'")

  # Rename graph vertices, barriers metadata, and weight metadata
  igraph::E(graph)$id_barrier <- igraph::get.edge.attribute(graph, id_barrier)
  barriers_metadata <- barriers_metadata %>% dplyr::rename_with( ~"id_barrier", contains(id_barrier))
  barriers_metadata <- barriers_metadata %>% dplyr::rename_with( ~"year", contains(year))
  barriers_metadata <- barriers_metadata %>% dplyr::rename_with( ~"pass_u", contains(pass_u))
  barriers_metadata <- barriers_metadata %>% dplyr::rename_with( ~"pass_d", contains(pass_d))
  weights_metadata <- weights_metadata %>% dplyr::rename_with( ~"weight", contains(weight))
  weights_metadata <- weights_metadata %>% dplyr::rename_with( ~"name", contains(nodes_id))

  # Set the names of the vertices. By default keep the name argument.
  igraph::V(graph)$name <- igraph::vertex_attr(graph, nodes_id)
  igraph::V(graph)$weight <- igraph::vertex_attr(graph, weight)
  igraph::V(graph)$field_B <- igraph::vertex_attr(graph, field_B)

  # More error messages
  if( !(id_barrier %in% igraph::edge_attr_names(graph)) ) stop(
    "'id_barrier' argument must be a valid vertex attribute in the input graph")
  if( missing(barriers_metadata) ) stop(
    "'barriers_metadata' dataframe must be specified")
  if( c(barriers_metadata$pass_u < 0, barriers_metadata$pass_u > 1) %>% sum >0 ) stop(
    "'pass_u' must be between 0 and 1")
  if( c(barriers_metadata$pass_d < 0, barriers_metadata$pass_d > 1) %>% sum >0 ) stop(
    "'pass_d' must be between 0 and 1")
  if( parallel == TRUE & missing(ncores) ) stop(
    "'ncores' must be specified when 'parallel = TRUE' ")

  ##### Create new barriers/weight sequences to account for both information #####

  # new time sequence
  if(!missing(barriers_metadata)) {time_seq_long = barriers_metadata$year
  } else if(!missing(weights_metadata)) {time_seq_long = weights_metadata$year
  } else {time_seq_long = c(barriers_metadata$year, weights_metadata$year)}

  years_seq <- data.frame("years" = time_seq_long %>% unique() %>% as.numeric() ) %>%
    dplyr::mutate(years_plus = .data$years + 1, years_minus = .data$years - 1) %>%
    tidyr::pivot_longer(cols = everything(), values_to = "years") %>%
    dplyr::select(.data$years) %>%
    dplyr::distinct() %>%
    stats::na.omit() %>%
    dplyr::arrange(.data$years) %>%
    #dplyr::filter(!(row_number() == 1) ) %>%
    dplyr::pull(.data$years)

  # function to "interpolate" barriers passability
  barriers_interpolate <- function(barriers_metadata, year_selection, id_barrier_selection){

    barriers_metadata <- rbind(barriers_metadata,
                           barriers_metadata %>%
                             dplyr::filter(year == min(year)) %>%
                             dplyr::mutate(year = 0))

    last_line <- barriers_metadata %>%
      dplyr::filter(id_barrier == id_barrier_selection) %>%
      dplyr::filter(year <= year_selection) %>%
      dplyr::arrange(year) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n())

    out <- data.frame(
      "id_barrier" = id_barrier_selection,
      "year" = year_selection,
      "pass_u" = last_line %>% dplyr::select(contains("_u")) %>% dplyr::pull(),
      "pass_d" = last_line %>% dplyr::select(contains("_d")) %>% dplyr::pull()
    ) %>% list()

    return(out) }

  # function to "interpolate" habitat suitability index
  weights_interpolate <- function(weights_metadata, year_selection, name_selection){

    weights_metadata <- rbind(weights_metadata,
                              weights_metadata %>%
                                dplyr::filter(year == min(year)) %>%
                                dplyr::mutate(year = 0))

    last_line <- weights_metadata %>%
      dplyr::filter(.data$name == name_selection) %>%
      dplyr::filter(year <= year_selection) %>%
      dplyr::arrange(year) %>%
      dplyr::filter(dplyr::row_number() == dplyr::n())

    out <- data.frame(
      "name" = name_selection,
      "year" = year_selection,
      "weight" = last_line$weight
    ) %>% list()

    return(out) }

  # Interpolate barriers metadata
  if(!missing(barriers_metadata)) {
    time_metadata <- tidyr::expand_grid(time_steps = years_seq, id_barrier = unique(barriers_metadata$id_barrier))
    barriers_metadata_interpolate <- do.call(rbind,
                                         mapply(FUN = barriers_interpolate, list(barriers_metadata),
                                                year_selection = time_metadata$time_steps,
                                                id_barrier_selection = time_metadata$id_barrier) )    }

  # Interpolate weights metadata
  if(!missing(weights_metadata)){
    name_metadata <- tidyr::expand_grid(time_steps = years_seq, name = unique(weights_metadata$name))
    weights_metadata_interpolate <- do.call(rbind,
                                            mapply(FUN = weights_interpolate, list(weights_metadata),
                                                   year_selection = name_metadata$time_steps,
                                                   name_selection = name_metadata$name) )    }

  ##### 1. Function to slice the df and calculate DCI -> for the loop #####
  t_slice_index <- function(river_graph,
                            barriers_metadata,
                            weights_metadata,
                            barriers_metadata_interpolate,
                            weights_metadata_interpolate,
                            t,
                            id_barrier,
                            pass_u ,
                            pass_d ,
                            weight ,
                            nodes_id ,
                            index_type ,
                            index_mode ,
                            c_ij_flag ,
                            B_ij_flag ,
                            dir_fragmentation_type ,
                            pass_confluence ,
                            field_B ,
                            dir_distance_type ,
                            disp_type ,
                            param_u ,
                            param_d ,
                            param,
                            param_l) {

    # Create and update graph edges and vertices
    if(!missing(barriers_metadata)){

      barriers_metadata_sliced <- barriers_metadata_interpolate %>% dplyr::filter(year == t)

      river_graph_df_e <- igraph::as_data_frame(river_graph, what = "edges") %>%
        dplyr::select(.data$from, .data$to, .data$id_barrier, .data$type) %>%
        dplyr::left_join(barriers_metadata_sliced, by = "id_barrier") %>%
        dplyr::mutate(pass_u = ifelse(is.na(pass_u), 1, pass_u),
                      pass_d = ifelse(is.na(pass_d), 1, pass_d))

    } else {river_graph_df_e <- igraph::as_data_frame(river_graph, what = "edges")}

    # Create and update graph edges and vertices
    if(!missing(weights_metadata)){

      weights_metadata_sliced <- weights_metadata_interpolate %>%
        dplyr::filter(year == t)

      river_graph_df_v <- igraph::as_data_frame(river_graph, what = "vertices") %>%
        dplyr::select( .data$name, .data$field_B ) %>%
        dplyr::left_join(weights_metadata_sliced, by = "name")

    } else {river_graph_df_v <- igraph::as_data_frame(river_graph, what = "vertices")}

    # Create back an igraph object that has the right passability information
    river_graph_att <- igraph::graph_from_data_frame(
      d = river_graph_df_e,
      vertices = river_graph_df_v )

    # Calculate DCI index
    index <- index_calculation(river_graph_att,
                               weight = "weight",
                               nodes_id = nodes_id,
                               index_type = index_type,
                               index_mode = index_mode,
                               c_ij_flag = c_ij_flag,
                               B_ij_flag = B_ij_flag,
                               dir_fragmentation_type = dir_fragmentation_type,
                               pass_confluence = pass_confluence,
                               pass_u = pass_u,
                               pass_d = pass_d,
                               field_B =  "field_B",
                               dir_distance_type = dir_distance_type,
                               disp_type = disp_type,
                               param_u = param_u,
                               param_d = param_d,
                               param = param,
                               param_l = param_l)

    # Output is different for catchment and reach indices (change the catchment output to )
    if (index_type == "full") {
      index <- data.frame("year" = t) %>%
        dplyr::mutate(num = index$num, den = index$den, index = index$index) }

    if (index_type == "reach") {
      index <- index %>% dplyr::mutate(year = t)
    }

    return(index)
  }


  ##### If parallel calculations are on   #####
  if (parallel == TRUE) {

    # Set parallel calculation options
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)

    # Packages to import
    pcks <- c("igraph", "tidyverse")

    # Start parallel loop
    iii <- NULL
    out_index <- foreach(iii = 1:length(unique(barriers_metadata$year)),
                         .packages=pcks,
                         .export = c("index_calculation", "t_slice_index")) %dopar% {

                           # Calculate and return index
                           out_index <- t_slice_index(river_graph = graph,
                                                      barriers_metadata = barriers_metadata,
                                                      weights_metadata = weights_metadata,
                                                      barriers_metadata_interpolate = barriers_metadata_interpolate,
                                                      weights_metadata_interpolate = weights_metadata_interpolate,
                                                      t = years_seq[iii],
                                                      id_barrier = id_barrier,
                                                      pass_u = pass_u,
                                                      pass_d = pass_d,
                                                      weight = "weight",
                                                      nodes_id = nodes_id,
                                                      index_type = index_type,
                                                      index_mode = index_mode,
                                                      c_ij_flag = c_ij_flag,
                                                      B_ij_flag = B_ij_flag,
                                                      dir_fragmentation_type = dir_fragmentation_type,
                                                      pass_confluence = pass_confluence,
                                                      field_B = "field_B",
                                                      dir_distance_type = dir_distance_type,
                                                      disp_type = disp_type,
                                                      param_u = param_u,
                                                      param_d = param_d,
                                                      param = param,
                                                      param_l = param_l)

                         }

    # Close the parllel processes
    parallel::stopCluster(cl)

    out_index <- do.call(rbind,out_index)

  }


  #####  if parallel calculations are off   #####
  if (parallel == FALSE){

    # Apply the function
    out_index <-  do.call(rbind,
                          lapply(years_seq,
                                 FUN = t_slice_index,
                                 river_graph = graph,
                                 barriers_metadata = barriers_metadata,
                                 weights_metadata = weights_metadata,
                                 barriers_metadata_interpolate = barriers_metadata_interpolate,
                                 weights_metadata_interpolate = weights_metadata_interpolate,
                                 id_barrier = id_barrier,
                                 pass_u = pass_u,
                                 pass_d = pass_d,
                                 weight = "weight",
                                 nodes_id = nodes_id,
                                 index_type = index_type,
                                 index_mode = index_mode,
                                 c_ij_flag = c_ij_flag,
                                 B_ij_flag = B_ij_flag,
                                 dir_fragmentation_type = dir_fragmentation_type,
                                 pass_confluence = pass_confluence,
                                 field_B = "length",
                                 dir_distance_type = dir_distance_type,
                                 disp_type = disp_type,
                                 param_u = param_u,
                                 param_d = param_d,
                                 param = param,
                                 param_l = param_l) )
  }

  return(out_index)

}
