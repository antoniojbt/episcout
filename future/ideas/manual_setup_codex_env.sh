# manual setup codex environment

set -euxo pipefail

# System packages for R package development
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y \
  r-base \
  r-base-dev \
  build-essential \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libfreetype6-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  pandoc \
  qpdf \
  ghostscript \
  git \
  curl \
  ruby

# Core R development packages used by this repository
Rscript -e "install.packages(c(
  'remotes',
  'devtools',
  'roxygen2',
  'testthat',
  'covr',
  'lintr',
  'styler',
  'rcmdcheck'
), repos = 'https://cloud.r-project.org')"

# Install package dependencies from DESCRIPTION
Rscript -e "remotes::install_deps(dependencies = TRUE, upgrade = 'never')"

# Optional but useful sanity checks
Rscript --version
R CMD --version
ruby --version


# Core R development packages used by this repository
Rscript -e "install.packages(c(
  'remotes',
  'devtools',
  'roxygen2',
  'testthat',
  'covr',
  'lintr',
  'styler',
  'rcmdcheck',
  'lubridate',
  'vdiffr',
  'hmisc',
  'future' 
), repos = 'https://cloud.r-project.org')"

# Install package dependencies from DESCRIPTION
Rscript -e "remotes::install_deps(dependencies = TRUE, upgrade = 'never')"

# Optional but useful sanity checks
Rscript --version
R CMD --version
ruby --version

#Rscript -e "devtools::document()"
#Rscript -e "styler::style_pkg()"
#Rscript -e "lintr::lint_package()"
#Rscript -e "devtools::test(reporter = 'summary')"
#R CMD build . --no-manual --no-resave-data --compact-vignettes=gs+qpdf
#R CMD check --no-manual --as-cran episcout_*.tar.gz
