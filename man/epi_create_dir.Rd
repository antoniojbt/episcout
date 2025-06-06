% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi_create_dir.R
\name{epi_create_dir}
\alias{epi_create_dir}
\title{Create a Directory and Nested Subdirectories}
\usage{
epi_create_dir(base_path, subdir = NULL)
}
\arguments{
\item{base_path}{A character string specifying the base path where the directory will be created.}

\item{subdir}{Optional. A character string specifying the name of the subdirectory. If not provided, the current date will be used. Can include nested paths (e.g., \code{results/today_xxx}).}
}
\value{
The full path to the created or existing directory.
Prints a message indicating whether the directory was created or already existed. Also prints the contents.
}
\description{
This function creates a directory at the specified path, including nested subdirectories if needed.
If the directory already exists, it does nothing. The directory name can include subdirectories (e.g., \verb{results/today_xxx/}), and these will be created recursively.
The subdirectory can either be a custom name or the current date (\code{dd_mm_yyyy} format).
}
\examples{
\dontrun{
# Create a directory named with the current date
base_dir <- "path/to/base_directory"
dated_dir <- epi_create_dir(base_dir)

# Create a directory with a custom name
custom_dir <- epi_create_dir(base_dir, subdir = "custom_name")

# Create a nested directory with today's date
nested_dir <- epi_create_dir(base_dir, subdir = "results/today_xxx")
}

}
