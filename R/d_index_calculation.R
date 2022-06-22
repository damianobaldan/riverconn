#' Calculate index improvement for scenarios of barriers removal
#'
#' @param graph an object of class 'igraph.' Can be both directed or undirected.
#' @param ... other arguments passed to the function index_calculation
#' @param id_barrier graph edges attribute used to label barriers. Default is \code{"id_barrier"}. Must be of type charachter.
#' @param barriers_metadata data.frame that must contain a column having the same name as the 'id_barrier' attribute of the graph,
#' and two columns with the corresponding upstream and downstream improved passabilities
#' (see 'pass_u_updated' and 'pass_d_updated' parameters).
#' @param pass_u_updated field in barrier_metadata where updated value for upstream passability is stored
#' (recommended values higher than the original passability).
#' @param pass_d_updated field in barrier_metadata where updated value for downstream passability is stored
#' (recommended values higher than the original passability).
#' @param mode currentlym only \code{"leave_one_out"} is implemented.
#' @param parallel logical value to flag if parallel option is to be used.
#' @param ncores define how many cores are used in parallel processing. Active only when \code{parallel = TRUE}
#'
#' @return returns a data.frame containing the percent improvement of the index for
#' each barrier present in the 'barriers_metadata' variable.
#' If \code{index_type = "full"} (see index_calculation arguments), the data.frame is organized by 'id_barrier'.
#' If \code{index_type = "reach"} (see index_calculation arguments), the data.frame is organized by 'id_barrier' and 'name'.
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
#' dams_metadata <- data.frame("id_dam" =  c("1", "2", "3", "4", "5", "6", "7"),
#' "pass_u_updated" = c(1, 1, 1, 1, 1, 1, 1), "pass_d_updated" = c(1, 1, 1, 1, 1, 1, 1))
#' d_index <- d_index_calculation(g, barriers_metadata = dams_metadata,
#' id_barrier = "id_dam", parallel = FALSE, param = 0.6)
#'
#'
d_index_calculation <- function(graph,
                                ...,
                                barriers_metadata,
                                id_barrier = "id_barrier",
                                pass_u_updated = "pass_u_updated",
                                pass_d_updated = "pass_d_updated",
                                mode = "leave_one_out", # "add_one", # sequence
                                parallel = TRUE,
                                ncores){


  # Get elements from the ... list and if missing assign default values
  weight <- if (is.null(list(...)$weight))
    "length" else list(...)$weight
  nodes_id <- if (is.null(list(...)$nodes_id))
    "name" else list(...)$nodes_id

  index_type <- if (is.null(list(...)$index_type))
    "full" else list(...)$index_type
  index_mode <- if (is.null(list(...)$index_mode))
    "to" else list(...)$index_mode

  c_ij_flag <- if (is.null(list(...)$c_ij_flag))
    TRUE else list(...)$c_ij_flag
  B_ij_flag <- if (is.null(list(...)$B_ij_flag))
    TRUE else list(...)$B_ij_flag
  dir_fragmentation_type <- if (is.null(list(...)$dir_fragmentation_type))
    "symmetric" else list(...)$dir_fragmentation_type

  pass_confluence <- if (is.null(list(...)$pass_confluence))
    1 else list(...)$pass_confluence

  pass_u <- if (is.null(list(...)$pass_u))
    "pass_u" else list(...)$pass_u
  pass_d <- if (is.null(list(...)$pass_d))
    "pass_d" else list(...)$pass_d

  field_B <- if (is.null(list(...)$field_B))
    "length" else list(...)$field_B
  dir_distance_type <- if (is.null(list(...)$dir_distance_type))
    "symmetric" else list(...)$dir_distance_type
  disp_type <- if (is.null(list(...)$disp_type))
    "exponential" else list(...)$disp_type

  param_u <- if (is.null(list(...)$param_u))
    NA else list(...)$param_u
  param_d <- if (is.null(list(...)$param_d))
    NA else list(...)$param_d
  param <- if (is.null(list(...)$param))
    NA else list(...)$param

  # Call the function that calculates the index with the default values
  inner_d_index_calculation(graph = graph,
                            barriers_metadata = barriers_metadata,
                            id_barrier = id_barrier,
                            pass_u_updated = pass_u_updated,
                            pass_d_updated = pass_d_updated,
                            mode = mode, # "add_one", # sequence
                            parallel = parallel,
                            ncores = ncores,
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

}


