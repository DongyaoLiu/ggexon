\name{store_projected_domains}
\alias{store_projected_domains}
\title{Store a projected protein-domain table on a SynIndividual}
\usage{
store_projected_domains(x, projected, name = "last_projection")
}
\arguments{
\item{x}{A \code{SynIndividual} object.}

\item{projected}{A data frame returned by \code{project_domains_to_genome()}.}

\item{name}{Name used to store the projected table.}
}
\value{
An updated \code{SynIndividual} object.
}
\description{
Stores a projected protein-domain table in the \code{projected_domains} slot so
the user can inspect or reuse it later.
}
