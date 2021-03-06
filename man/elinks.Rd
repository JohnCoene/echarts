% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/force.R, R/force_.R
\name{elinks}
\alias{elinks}
\alias{elinks_}
\title{Add edges}
\usage{
elinks(p, links, source, target, weight = 1)

elinks_(p, links, source, target, weight = 1)
}
\arguments{
\item{p}{an echart object.}

\item{links}{edges data.frame.}

\item{source}{source column.}

\item{target}{target column.}

\item{weight}{edge weight.}
}
\description{
Add edges for \code{\link{eforce}}.

Add edges for \code{\link{eforce}}.
}
\examples{
let <- LETTERS[1:20]

edges <- data.frame(source = sample(let, 20), target = sample(let, 20),
  weight = runif(20, 5, 20))

nodes <- data.frame(name = let, value = runif(20, 5, 25), group = rep(LETTERS[1:4], 5))

echart() \%>\%
  eforce(itemStyle = list(normal = list(label = list(show = TRUE)))) \%>\% # show labels
  enodes(nodes, name, value = value, category = group) \%>\%
  elinks(edges, source, target)

echart() \%>\%
  eforce(itemStyle = list(normal = list(label = list(show = TRUE)))) \%>\% # show labels
  enodes(nodes, name, value = value, category = group) \%>\%
  elinks(edges, source, target, weight = 1)

let <- LETTERS[1:20]

edges <- data.frame(source = sample(let, 20), target = sample(let, 20),
  weight = runif(20, 5, 20))

nodes <- data.frame(name = let, value = runif(20, 5, 25), group = rep(LETTERS[1:4], 5))

echart() \%>\%
  eforce_(itemStyle = list(normal = list(label = list(show = TRUE)))) \%>\% # show labels
  enodes_(nodes, "name", value = "value", category = "group") \%>\%
  elinks_(edges, "source", "target")

}
\seealso{
\code{\link{enodes}} \code{\link{eforce}}
}
