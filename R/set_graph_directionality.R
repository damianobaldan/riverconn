#' Create directed river graph based on outlet flag
#'
#' @param graph an 'igraph' object representing a river structure where reaches are nodes and confluences (or fragmentation items) are links.
#' @param field_name a character value that flags the vertices attribute used to designate the outlet.
#' Each vertex must have an unique value for this field.
#' @param outlet_name a character value corresponding to the 'field_name' attribute
#'
#' @return an object of class 'igraph' containing a directed graph.
#' @export
#'
#' @description The input graph can be either directed or undirected. If directed, then it is made undirected before directionality is assigned.
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#'
#' @examples
#' library(igraph)
#' g <- igraph::graph_from_literal(1-2, 2-4, 3-2, 4-6, 6-7, 5-6, 7-8, 9-5, 10-5 )
#' g1 <- set_graph_directionality(g, field_name = "name", "8")
#'
set_graph_directionality <- function(graph, field_name = "name", outlet_name) {

  # Error messages
  if( !(field_name %in% igraph::vertex_attr_names(graph)) ) stop(
    "'field_name' argument must be a valid vertex attribute in 'graph'")
  if( !(outlet_name %in% igraph::get.vertex.attribute(graph, field_name)) ) stop(
    "'outlet_name' must be present in 'field_name' attribute")
  if( length(igraph::get.vertex.attribute(graph, field_name)) < igraph::gorder(graph)  ) stop(
    "'field_name' must be unique for each vertex")
  if( !igraph::is_connected(graph)  ) stop(
    "'graph' must be connected (check if some node is disconnected with igraph::components() )")

  # Set the names of the vertices. By default keep the name argument.
  if (field_name != "name"){
  igraph::V(graph)$name <- igraph::get.vertex.attribute(graph, field_name)
  graph <-  igraph::delete_vertex_attr(graph, field_name)}

  # Force graph to be undirectional
  graph_temp <- igraph::as.undirected(graph, mode = "each")

  # Get edges list to iterate through
  graph_temp_df <- igraph::as_data_frame(graph_temp, what = "edges")
  graph_v_df <- igraph::as_data_frame(graph_temp, what = "vertices")

  # loop over edges list: assign directionality after checking which
  #   vertex for each pair is closest to the outlet
  out  <- list()

  for (iii in 1:nrow(graph_temp_df)) {

    df_iter <- graph_temp_df[iii, ]

    delta <- igraph::distances(graph_temp, v = df_iter$from, to = outlet_name) -
      igraph::distances(graph_temp, v = df_iter$to, to = outlet_name)

    if (delta > 0) {from = df_iter$from; to = df_iter$to}
    if (delta < 0) {from = df_iter$to; to = df_iter$from}

    out[[iii]] <- data.frame(
      "from" = from, "to" = to,
      df_iter %>% dplyr::select(-.data$from, -.data$to)
    )

  }

  out_graph_df <- do.call(rbind, out)

  # Get vertices information
  graph_v_df <- igraph::as_data_frame(graph_temp, what = "vertices") %>%
    dplyr::relocate(.data$name) %>%
    dplyr::rename_with(~field_name, contains("name") )

  # Recreate new graph with directions assigned
  river_graph <- igraph::graph_from_data_frame(
    d = out_graph_df,
    directed = TRUE,
    vertices = graph_v_df )

  return(river_graph)

}
