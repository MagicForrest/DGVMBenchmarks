#' Diverging red-white-blue fill scale with a widened white band around zero
#'
#' Behaves like scale_fill_gradient2(low = "red", high = "blue", mid = "white",
#' midpoint = 0), but instead of white occurring only exactly at zero, values
#' within +/- white_frac of the data range stay pure white before fading into colour.
#'
#' @param limits Numeric vector of length 2, the data range (as passed to scale_fill_gradient2).
#' @param breaks Optional break points for the legend.
#' @param white_frac Fraction of the range (limits[2] - limits[1]) that stays white on each side of zero. Default 0.05 (5%).
#' @param na.value Colour for NA values.
#'
#' @return A ggplot2 scale object.
#' @keywords internal
whiteBandFill <- function(limits, breaks = NULL, white_frac = 0.05, na.value = "black") {
  if (anyNA(limits) || length(limits) != 2) {
    stop("whiteBandFill: `limits` must be a numeric vector of length 2 with no NAs (got: ",
         paste(limits, collapse = ", "), ")")
  }
  range_width <- diff(range(limits))
  if (range_width == 0) {
    stop("whiteBandFill: `limits` must have non-zero width, both bounds are ", limits[1],
         " (check Difference_Limits in the yml for this benchmark)")
  }
  white_band <- white_frac * range_width

  zero_low <- max(limits[1], -white_band)
  zero_high <- min(limits[2], white_band)
  if (zero_high < zero_low) zero_high <- zero_low

  rescale_pos <- function(x) (x - limits[1]) / range_width

  ggplot2::scale_fill_gradientn(colours = c("red", "white", "white", "blue"),
                                values = rescale_pos(c(limits[1], zero_low, zero_high, limits[2])),
                                limits = limits,
                                breaks = breaks,
                                na.value = na.value)
}
