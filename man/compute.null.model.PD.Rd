% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Null_models_fcts.R
\name{compute.null.model.PD}
\alias{compute.null.model.PD}
\title{Compute null models for PD metrics}
\usage{
compute.null.model.PD(phylo_tree, sp_asb_df, nb_asb_rep)
}
\arguments{
\item{phylo_tree}{a \code{tree} object linking all the species from the
studied pool of species and ONLY species from the studied global pool.}

\item{sp_asb_df}{a data frame with species in columns and asb (grid cells)
in rows.}

\item{nb_asb_rep}{a number with the number of null assemblages to compute for
each grid cell.}
}
\value{
a list of data frames - as many as PD metrics - with the grid cells
in rows and the values of the metric for each null asb in columns.
}
\description{
Compute null models for PD metrics
}
