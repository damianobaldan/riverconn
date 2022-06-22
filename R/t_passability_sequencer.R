#' Create the time-dependent metadata for barriers
#'
#' @param passability_information a data frame in wide format. Must contain an 'id_barrier' column. Each change
#' in passability is listed in a group of 3 columns: 'year_op', 'pass_op_u', and 'pass_op_d', listing the year the operation (op) took place, and
#' the related upstream and downstream passabilities. In case the passability did not change, a NA value should be used. See details.
#' @param seq_ops A charachter vector with the temporal sequence of operations. It should contain all the operation strings
#' in the 'passability_information' data frame.
#'
#' @return a dataframe in a long format that can be used as input to the tDCI function.
#' @export
#'
#' @details This function is meant to help processing data the way they can be obtained from a database, or the way they are stored in a spreadsheet.
#' The substring 'op' in the fields 'year_op', 'pass_op_u', and 'pass_op_d' is used to identify each operation and to relate it to the
#' relative passability parameters. For example, c can be used for construction, and fp for the implementation of a fish pass.
#' In this case, \code{passability_information} will have the fields 'year_c', 'pass_c_u', and 'pass_c_d', 'year_fp', 'pass_fp_u', and 'pass_fp_d'.
#' Then, the input \code{seq_ops = c("c", "fp")}, meaning that first the operation named 'c' occurred, and then the operation named 'fp' occurred.
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom tidyselect starts_with everything
#' @importFrom igraph E V
#'
#' @examples
#' barriers_data <- data.frame("id_barrier" = c("1", "2"),
#' "year_c" = c(1950, 1990), "pass_c_u" = c(0.1, 0.1), "pass_c_d" = c(0.4, 0.4),
#' "year_fp" = c(2000, 2010), "pass_fp_u" = c(0.5, 0.5), "pass_fp_d" = c(0.8, 0.8))
#' seq_ops <- c("c", "fp")
#' t_metadata <- t_passability_sequencer(barriers_data, seq_ops)
#'
t_passability_sequencer <- function(passability_information, seq_ops) {

  # Error messages
  check_string <- passability_information %>%
    dplyr::select(-.data$id_barrier) %>%
    colnames()  %>%
    sub(pattern = 'year_', replacement = "") %>%
    sub(pattern ='pass_', replacement =  "") %>%
    sub(pattern ='_d',  replacement = "") %>%
    sub(pattern ='_u',  replacement = "")

  if(sum(!(seq_ops %in% check_string)) > 1 ) stop(
    "'seq_ops' must be mirrored in the 'passability_information' fields")
  if(missing(passability_information)) stop(
    "'passability_information' must be defined")
  if(missing(seq_ops)) stop(
    "'seq_ops' must be defined")


  # Create vector with time steps for metadata creation
  time_steps <- passability_information %>%
    dplyr::select(starts_with("year")) %>%
    tidyr::pivot_longer(cols = everything(), values_to = "years") %>%
    dplyr::mutate(years_plus = .data$years + 1, years_minus = .data$years - 1) %>%
    dplyr::select(-.data$name) %>%
    tidyr::pivot_longer(cols = everything(), values_to = "years") %>%
    dplyr::select(.data$years) %>%
    dplyr::distinct() %>%
    stats::na.omit() %>%
    dplyr::arrange(.data$years) %>%
    dplyr::pull(.data$years)

  # Create vector with barriers id for metadata creation
  id_barrier <- passability_information$id_barrier

  # Create metadata
  time_metadata <- tidyr::expand_grid(time_steps = time_steps, id_barrier = id_barrier)

  # Function that selects the right passability based on the year
  temporal_table <- function(passability_information, time_metadata, seq_ops, year, barrier) {

    # Add the pre-barrier passability (set to 1 both pass_u and pass_d)
    seq_ops_loop <- c("no", seq_ops)

    # get the index (string_match) of the closest year
    string_match <- passability_information %>%
      dplyr::mutate(year_no = 0, pass_no_u = 1, pass_no_d = 1) %>%
      dplyr::filter(id_barrier == barrier) %>%
      dplyr::select(contains("year")) %>%
      tidyr::pivot_longer(cols = contains(seq_ops_loop)) %>%
      dplyr::filter(.data$value <= year) %>%
      dplyr::pull(.data$name) %>%
      dplyr::last()
    string_match <- sub('year_', "", x = string_match)

    # extract the passability information for the year
    time_barrier_passability <- passability_information %>%
      dplyr::mutate(year_no = 0, pass_no_u = 1, pass_no_d = 1) %>%
      dplyr::filter(id_barrier == barrier) %>%
      dplyr::select(contains(string_match))

    # return the data frame
    out <- data.frame(
      "id_barrier" = barrier,
      "year" = year,
      "pass_u" = time_barrier_passability %>% dplyr::select(contains("_u")) %>% dplyr::pull(),
      "pass_d" = time_barrier_passability %>% dplyr::select(contains("_d")) %>% dplyr::pull()
    ) %>% list()

    return(out)

  }

  # mapply function
  out <- mapply(FUN = temporal_table, list(passability_information), list(time_metadata), list(seq_ops),
                year = time_metadata$time_steps, barrier = time_metadata$id_barrier)

  return(do.call(rbind,out))

}

