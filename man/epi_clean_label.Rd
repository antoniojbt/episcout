% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_clean_label.R
\name{epi_clean_label}
\alias{epi_clean_label}
\title{Apply Factor Levels and Labels Based on a Lookup Table}
\usage{
epi_clean_label(data_df, lookup_df)
}
\arguments{
\item{data_df}{A dataframe to which factor levels and labels will be applied.
This dataframe should contain columns that match the variable names specified
in the lookup dataframe.}

\item{lookup_df}{A lookup dataframe containing at least three columns: \code{variable},
\code{level}, and \code{label}, where \code{variable} is the name of the column in \code{data_df}
for which levels and labels are specified, \code{level} are the factor levels (as
character), and \code{label} are the corresponding labels for these levels.}
}
\value{
A modified version of \code{data_df} with specified columns converted to factors
with levels and labels as defined in \code{lookup_df}.
}
\description{
This function takes a dataframe and a lookup dataframe that contains the
mapping of factors' levels and labels for specific variables. It applies
these levels and labels to the corresponding variables in the target dataframe.
}
\examples{
# Assume data_df is already loaded and contains 'ORIGEN' and 'SECTOR' columns
# Assume lookup_df is set up with columns 'variable', 'level', and 'label'
# result_df <- epi_clean_label(data_df, lookup_df)

}
