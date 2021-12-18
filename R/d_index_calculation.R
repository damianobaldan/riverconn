#' Calculate index improvement for scenarios of barriers removal
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
#' @param ... other arguments passed to the function index_calculation
#'
#' @return returns a data.frame containing the percent improvement of the index for
#' each barrier present in the 'dams_metadata' variable.
#' If \code{index_type = "full"} (see index_calculation arguments), the data.frame is organized by 'id_dam'.
#' If \code{index_type = "reach"} (see index_calculation arguments), the data.frame is organized by 'id_dam' and 'name'.
#' In both cases, both numerator and denominator used in the index calculations are reported in the columns 'num' and 'den'.
#' The column 'd_index' contains the relative index improvement when each barrier is removed.
#' @export
#'
#' @details
#' Setting \code{c_ij_flag = FALSE} (see index_calculation arguments) removes from the calculations the effect of barriers, i.e. the c_{ij} contribution
#' is not used in the calculation of the index.
#' Setting \code{B_ij_flag = FALSE} (see index_calculation arguments) removes from the calculations the effect of movement/dispersal,
#' i.e. the B_{ij} contribution is not used in the calculation of the index.
#' Note that it is not possible to set both \code{c_ij_flag = FALSE} and \code{B_ij_flag = FALSE}.
#'
#' The setting \code{dir_distance_type = "symmetric"} (see index_calculation arguments) is to be used when the directionality of the river network is not relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches.
#' The setting \code{dir_distance_type = "asymmetric"} (see index_calculation arguments) is to be used when the directionality is relevant.
#' The distance between reaches midpoints is calculated for each couple of reaches and splitted
#' between 'upstream travelled' distance and 'downstream travelled' distance
#'
#' The 'param_u', 'param_d', and 'param' values are interpreted differently based on the formula used to relate distance and probability.
#' When \code{disp_type ="exponential"} (see index_calculation arguments), those values are used as the base of the exponential dispersal kernel: B_ij = param^{d_ij}.
#' When \code{disp_type ="threshold"} (see index_calculation arguments), those values are used to define the maximum dispersal length: B_ij = ifelse(d_ij < param, 1, 0).
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
                                parallel = TRUE,
                                ncores,
                                ...){

  # Call the function that calculates the index with the default values
  inner_d_index_calculation(graph = graph,
                            dams_metadata = dams_metadata,
                            id_dam = id_dam,
                            pass_u_updated = pass_u_updated,
                            pass_d_updated = pass_d_updated,
                            mode = mode, # "add_one", # sequence
                            parallel = parallel,
                            ncores = ncores,
                            ...)

}


