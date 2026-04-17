% Please edit documentation in R/syn-layer.R
\name{project_domains_to_genome}
\alias{project_domains_to_genome}
\title{Project protein-domain coordinates onto genomic coordinates}
\usage{
project_domains_to_genome(
  x,
  annotation = NULL,
  ids = NULL,
  domains = NULL,
  model = "all",
  motif = NULL,
  genes = NULL,
  transcripts = NULL,
  chr = NULL,
  start = NULL,
  end = NULL
)
}
\arguments{
\item{x}{A \code{SynIndividual} object.}

\item{annotation}{Optional name of an attached \code{SynProteinDomainAnnotation}
layer. Defaults to the first available protein-domain annotation.}

\item{ids}{Optional explicit identifier vector matched against the domain
annotation key column.}

\item{domains}{Optional domain names/accessions to filter.}

\item{model}{InterProScan analysis model(s) to keep. Accepts a single string,
a character vector, or \code{"all"}.}

\item{motif}{Optional motif name(s) used to filter the InterProScan table.}

\item{genes}{Optional gene identifiers used to limit the projected proteins.}

\item{transcripts}{Optional transcript identifiers used to limit the
projected proteins.}

\item{chr}{Optional chromosome name used to define the genomic window.}

\item{start, end}{Optional genomic window bounds.}
}
\value{
A data frame with projected genomic motif segments.
}
\description{
Uses transcript CDS structure from a \code{SynIndividual} to convert
protein-domain intervals into one or more genomic intervals.
}
