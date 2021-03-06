% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repository-impl.R
\name{deprecated_history_to_deltas}
\alias{deprecated_history_to_deltas}
\alias{is_deltas}
\title{Transform a graph of commits into a graph of deltas.}
\usage{
deprecated_history_to_deltas(hist)

is_deltas(x)
}
\arguments{
\item{hist}{List of commits.}

\item{x}{Object to be tested.}
}
\value{
Object of S3 class \code{deltas}.
}
\description{
\code{history_to_deltas} is the main function which orchestrates
the transformation.

\code{is_deltas} verifies if the given object is a valid
\code{deltas} structure.
}
\details{
A \emph{delta} is an introduction of a new artifact (object, plot, printout)
in the R session. Graph of deltas is easier to read for a person than
a graph of commits becase only the relevant (new) information is shown
in each node of the graph. Thus, translating from commits to deltas is
the first step to present the history of changes in R session recorded
in commits.
}
