% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_c_directionality.R
\name{set_c_directionality}
\alias{set_c_directionality}
\title{Calculates equivalent passability based on graph directionality and directionality type}
\usage{
set_c_directionality(
  graph,
  dir_fragmentation_type = "symmetric",
  pass_confluence = 1,
  pass_u = "pass_u",
  pass_d = "pass_d"
)
}
\arguments{
\item{graph}{an object of class igraph. Can be both directed or undirected.}

\item{dir_fragmentation_type}{how directionality in c_ij calculations is dealt with:
\code{"symmetric"} (i.e. undirected graph) or \code{"asymmetric"} (i.e. directed graph)}

\item{pass_confluence}{a value in the range [0,1] that defines the passability of confluences (default is 1).}

\item{pass_u}{the 'graph' edge attribute to be used as upstream passability. Default is "pass_u".}

\item{pass_d}{the 'graph' edge attribute to be used as downstream passability. Default is "pass_d".}
}
\value{
an object of class 'igraph' with the equivalent passability stored in the edge attribute 'pass_eq'
}
\description{
Calculates equivalent passability based on graph directionality and directionality type
}
\keyword{internal}
