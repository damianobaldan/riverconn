#' Create the time-dependent weights data
#'
#' @param weights_information a data.frame that must contain a 'nodes_id' column and several 'weight' columns.
#' Weight columns are named with the string contained in the 'weight' input and the relative year (4 digits format),
#' separated by an underscore (e.g. when \code{weight = "length"}, the names of the 'weight' columns will be:
#' 'weight_1990', 'weight_2000', 'weight_2020', etc.).
#' @param weight a character object containing the label of the columns whose weight change with time
#' @param nodes_id a character object containing the label of the columns that uniquely identify reaches.
#'
#' @return a data frame with columns 'name', 'year', and 'weight' to be used in the function \code{t_index_calculation}
#' @export
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom tidyselect starts_with everything
#'
#' @examples
#' weights_dataframe <- data.frame("id" = c("1", "2", "3", "4", "5"),
#' "weight_1900" = c(10, 15, 100, 50, 40),
#' "weight_1950"= c(11, 16, 90, 55, 45),
#' "weight_2000"= c(13, 19, 80, 49, 44))
#' weights_metadata <- t_weights_sequencer(weights_dataframe, weight = "weight", nodes_id = "id")
#'
t_weights_sequencer <- function(weights_information, weight = "length", nodes_id = "name"){

  # Error messages
  if(missing(weights_information)) stop(
    "'weights_information' must be defined")
  if(!(nodes_id %in% colnames(weights_information))) stop(
    "the value of 'nodes_id' must be present as a column in 'weights_information'")

  # rename data frame
  weights_information <- weights_information %>% dplyr::rename_with( ~"name", contains(nodes_id))

  # Get column names
  col_names <- weights_information %>%
    dplyr::select(-.data$name) %>%
    colnames()

  # get years
  years <- substr(col_names, start = nchar(col_names)-3, stop = (nchar(col_names)))

  # rename columns
  weights_information <- weights_information %>%
    dplyr::rename_with( ~paste0("weight_",years), starts_with(weight))

  # Make pivot longer
  weights_information_long <- weights_information %>%
    tidyr::pivot_longer(cols = starts_with("weight"), values_to = "weight", names_to = "year") %>%
    dplyr::mutate(year = substr(.data$year, start = 8, stop = 11))

  # Change back names
  weights_information_long <- weights_information_long %>%
    dplyr::rename_with( ~weight, contains("weight")) %>%
    dplyr::rename_with( ~nodes_id, contains("name"))

  return(weights_information_long)

}
