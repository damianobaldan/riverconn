#' Calculates time-dependent index when nodes weights or barriers passability are changing
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param ... other arguments passed to the function index_calculation
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param barriers_metadata data.frame that must contain a column having the same name as the 'id_barrier' attribute of the graph,
#' and two columns with the corresponding upstream and downstream improved passabilities (see pass_u and pass_d), and a column with the year
#' passability was changed. This data frame can be obtained from easily-formatted data with the function \code{t_passability_sequencer}.
#' @param id_barrier graph edges attribute used to label barriers. Default is \code{"id_barrier"}. It should be present in the 'barriers metadata' input as well.
#' @param year field of the 'barriers metadata' where temporal information on the changes in passability is stored.
#' @param pass_u field of the 'barriers metadata' where temporal-dependent upstream passability is stored.
#' @param pass_d field of the 'barriers metadata' where temporal-dependent downstream passability is stored.
#' @param weights_metadata data.frame that must contain a column having the same name as the 'nodes_id' attribute of the graph,
#' a column with he corresponding weight information (see 'weight' parameter), and a column with the year
#' weight was changed. This data frame can be obtained from easily-formatted data with the function \code{t_weight_sequencer}.
#' @param weight param weight graph vertex attribute used to assign weights to the reaches (nodes). Default is \code{"length"}.
#' @param nodes_id graph vertex attribute used to uniquely label reaches (nodes). Default is \code{"name"}.
#' @param parallel logical value to flag if parallel option is to be used.
#' @param ncores define how many cores are used in parallel processing. Active only when \code{parallel = TRUE}
#'
#' @return a data.frame with a 'year' field and related connectivity index.
#' If \code{index_type = "reach"}, the data.frame is organized by 'year' and 'name'.
#'
#' @references
#' Baldan, D., Cunillera-Montcusí, D., Funk, A., & Hein, T. (2022). Introducing ‘riverconn’: an R package to assess river connectivity indices. Environmental Modelling & Software, 156, 105470.
#'
#' @export
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
#' g <- igraph::graph_from_literal(1-+2, 2-+4, 3-+2, 4-+6, 6-+5)
#' E(g)$id_barrier <- c(NA, NA, "1", NA, NA)
#' E(g)$type <- ifelse(is.na(E(g)$id_barrier), "joint", "dam")
#' V(g)$length <- c(1, 1, 2, 3, 4,5)
#' V(g)$Id <- V(g)$name
#' E(g)$pass_u <- E(g)$pass_d <- ifelse(!is.na(E(g)$id_barrier),0.1,NA)
#' barriers_data <- data.frame("id_barrier" = c("1"),
#'    "year_c" = 2000, "pass_c_u" = 0.1, "pass_c_d" = 0.4)
#' seq_ops <- c("c")
#' barriers_metadata <- t_passability_sequencer(barriers_data, seq_ops)
#' weights_dataframe <- data.frame("name" = seq(1,6) %>% as.character,
#'     "length_1999" = c(1, 1, 2, 3, 4,5))
#' weights_metadata <- t_weights_sequencer(weights_dataframe, weight = "length")
#' t_index <- t_index_calculation(g, barriers_metadata = barriers_metadata,
#'  weights_metadata = weights_metadata, weight = "length", parallel = FALSE, B_ij_flag = FALSE)
#'
#'
#'
#'
t_index_calculation <- function(graph = graph,
                                ...,
                                barriers_metadata,
                                id_barrier = "id_barrier",
                                year = "year",
                                pass_u = "pass_u",
                                pass_d = "pass_d",
                                weights_metadata,
                                weight = "length",
                                nodes_id = "name",
                                parallel = TRUE,
                                ncores){


  # Get elements from the ... list and if missing assign default values
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
  param_l <- if (is.null(list(...)$param_l))
    NA else list(...)$param_l

  # Call the function that calculates the index with the default values
  inner_t_index_calculation(graph = graph,
                            barriers_metadata = barriers_metadata,
                            id_barrier = id_barrier,
                            year = year,
                            pass_u = pass_u,
                            pass_d = pass_d,
                            weights_metadata = weights_metadata,
                            weight = weight,
                            nodes_id = nodes_id,
                            parallel = parallel,
                            ncores = ncores,
                            index_type = index_type,
                            index_mode = index_mode,
                            c_ij_flag = c_ij_flag,
                            B_ij_flag = B_ij_flag,
                            dir_fragmentation_type = dir_fragmentation_type,
                            pass_confluence = pass_confluence,
                            field_B = field_B,
                            dir_distance_type = dir_distance_type,
                            disp_type = disp_type,
                            param_u = param_u,
                            param_d = param_d,
                            param = param,
                            param_l = param_l)

}
