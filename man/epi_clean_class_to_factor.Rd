% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_clean_class_to_factor.R
\name{epi_clean_class_to_factor}
\alias{epi_clean_class_to_factor}
\title{Convert column class to factor if unique values below cutoff}
\usage{
epi_clean_class_to_factor(df = NULL, cutoff_unique = 10)
}
\arguments{
\item{df}{A data frame object with columns to convert to factor}

\item{cutoff_unique}{A cutoff for the number of unique values present in the
column. Column class will be converted if there are less unique values than
the cutoff. Default is 10.}
}
\value{
The data frame passed with classes converted.
}
\description{
Convert column class to factor if column has less than
x unique results. Useful for handling data frames with many columns with some
automation for specifying column classes.
Assumes that columns with fewer unique values than the cutoff parameter
should be read as factors. All column classes other than dates are converted.
Assumes that eg integer columns with few values are in fact coded factors.
}
\note{
Avoids converting dates to factors if class is already Date.
Note that lubridate converted dates may return eg "POSIXct" "POSIXt"
which return a logical vector > 1 (for each element)
and give a warning message (that can be ignored):
"... the condition has length > 1 and only the first element will be used"
}
\examples{
\dontrun{
n <- 20
df_factor <- data.frame(
  var_id = rep(1:(n / 2), each = 2),
  var_to_rep = rep(c("Pre", "Post"), n / 2),
  x = rnorm(n),
  y = rbinom(n, 1, 0.50),
  z = rpois(n, 2)
)
df_factor$date_col <- seq(as.Date("2018/1/1"),
  by = "year", length.out = 5
) # nrow(df_factor))
# Check the current classes:
lapply(df_factor, class)
# Check how many unique values within each column:
lapply(df_factor, function(x) length(unique(x)))
# Check conditions:
i <- "date_col"
cutoff_unique <- 10
# if num. of unique values is less than cut-off:
length(unique(df_factor[[i]])) < cutoff_unique # should be TRUE
# and the class is not already a date:
class(df_factor[[i]]) != "Date" # should be FALSE
df_factor <- epi_clean_class_to_factor(df_factor, cutoff_unique = 10)
lapply(df_factor, class)
df_factor
}

}
\seealso{
\code{\link{epi_clean_count_classes}}, \code{\link{epi_clean_cond_date}},
\code{\link{epi_clean_cond_chr_fct}}, \code{\link{epi_clean_cond_numeric}}
}
\author{
Antonio Berlanga-Taylor \if{html}{\out{<\url{https://github.com/AntonioJBT/episcout}>}}
}
