#' IMSS “official” colour palette
#'
#' Vector of IMSS brand colours.
#'
#' @return A character vector of hex colour codes.
#' @export
palette_IMSS <- c(
    "#911034",      # Original Red
    "#b12f45",      # Slightly brighter red
    "#c19a53",      # Original Gold
    "#e1b86e",      # Lighter gold/beige
    "#2a5c4b",      # Original Green
    "#3b755f",      # Brighter green
    "#DACBA1",      # Original Beige
    "#bba483",      # Darker beige
    "#602218",      # Original Brown
    "#7b3a2a"       # Rich brown
    )


#' Colour-blind–friendly IMSS palette
#'
#' A ten‐hex vector that’s easier to distinguish for viewers
#' with colour‐vision deficiencies.
#'
#' @return A character vector of hex colour codes.
#' @export
# Modified to be colour-blind friendly:
palette_IMSS_accessible <- c(
    "#911034",      # Original Red
    "#e69f00",      # Gold (more distinct from beige)
    "#56b4e9",      # Blue (replaces a green)
    "#009e73",      # Green (contrast with blue and yellow)
    "#f0e442",      # Yellow
    "#0072b2",      # Dark blue
    "#D55E00",      # Orange
    "#cc79a7",      # Purple (replaces a beige)
    "#602218",      # Original Brown
    "#7b3a2a"       # Rich brown
    )
