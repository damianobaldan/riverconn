#' Calculates average upstream/downstream or combined distance between each couple of reaches
#'
#' @param graph an object of class igraph. Can be both directed or undirected.
#' @param dir_distance_type how directionality in B_ij calculations is dealt with:
#' \code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph). See details.
#' @param field_B the 'graph' edge attribute to be used to calculate the distance. Default is \code{"length"}.
#'
#' @return an object of class 'igraph' with the upstream/downstream/total distances stored as edges attributes.
#'
#' @importFrom dplyr select filter summarize left_join rename mutate rename_with contains matches group_by
#' @importFrom igraph E V
#'
#' @keywords internal
#'
set_B_directionality <- function(graph, dir_distance_type = "symmetric", field_B = "length"){

  if( !(dir_distance_type %in% c("symmetric", "asymmetric")) ) stop("'dir_distance_type' must me either 'symmetric' or 'asymmetric'")
  if( !(field_B %in% igraph::vertex_attr_names(graph)) ) stop("'field_B' argument must be a valid vertex attribute in 'graph'")

  # get the vertices information
  graph_v_df <- igraph::as_data_frame(graph, what = "vertices") %>%
    dplyr::select(.data$name, matches(field_B))

  # add the length attribute information to the edges information
  graph_e_df <- igraph::as_data_frame(graph, what = "edges") %>%
    dplyr::left_join(graph_v_df %>% rename(from = .data$name), by = "from") %>%
    dplyr::mutate(d_att_from =  .[,field_B] ) %>%
    dplyr::left_join(graph_v_df %>%
                       rename(to = .data$name) %>%
                       select(to, contains(field_B, vars = field_B)), by = "to") %>%
    dplyr::mutate(d_att_to =  .[,field_B] ) %>%
    dplyr::mutate(d_att = (.data$d_att_from + .data$d_att_to) / 2,
                  flag_dir = "n")

  # create intermediate graph that has the distances
  graph_intermediate <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)


  # If directionality is "asymmetric" the graph is made undirectional
  if(dir_distance_type == "symmetric"){

    # Bidirectional edge list
    graph_df_bidir<- rbind(
      # "downstream" graph: graph with directions going downstream, uses pass_d as attribute
      igraph::as_data_frame(graph_intermediate, what = "edges") %>%
        dplyr::mutate(flag_dir = "n"),
      # "upstream" graph: graph with directions going upstream, uses pass_u as attribute
      igraph::as_data_frame(graph_intermediate, what = "edges") %>%
        dplyr::rename(from = .data$to, to = .data$from) %>%
        mutate(flag_dir = "n")
    )

    # Vertices dataframe
    graph_df_bidir_v <- igraph::as_data_frame(graph, what = "vertices") %>%
      dplyr::relocate(.data$name)

    # Create the bidirectional fraph
    graph_output <- igraph::graph_from_data_frame(
      d = graph_df_bidir,
      vertices = graph_df_bidir_v )

  }

  # If directionality is "asymmetric" the graph is made undirectional
  if(dir_distance_type == "asymmetric"){

    # Bidirectional edge list
    graph_df_bidir<- rbind(
      # "downstream" graph: graph with directions going downstream, uses pass_d as attribute
      igraph::as_data_frame(graph_intermediate, what = "edges") %>%
        dplyr::mutate(flag_dir = "d"),
      # "upstream" graph: graph with directions going upstream, uses pass_u as attribute
      igraph::as_data_frame(graph_intermediate, what = "edges") %>%
        dplyr::rename(from = .data$to, to = .data$from) %>%
        mutate(flag_dir = "u")
    )

    # Vertices dataframe
    graph_df_bidir_v <- igraph::as_data_frame(graph, what = "vertices") %>%
      dplyr::relocate(.data$name)

    # Create the bidirectional fraph
    graph_output <- igraph::graph_from_data_frame(
      d = graph_df_bidir,
      vertices = graph_df_bidir_v )
  }


  return(graph_output)
}
