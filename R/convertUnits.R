#' Title
#'
#' @param df The fields you need to convert
#' @param from Current unit
#' @param to The unit you want to convert to
#' @param benchmark Holds information on which layer to convert
#'
#' @return Field with converted units
#' @export
#'
#' @examples convertUnits(Data.year.mean, "indiv/ha", "indiv/km^2")
convertUnits <- function(df, from, to, benchmark = this_benchmark) {
  
  units <- list("indiv/ha","indiv/km^2","indiv/m^2","kgC/ha","kgC/km^2", "kgC/m^2","kgC/ha/y","kgC/km^2/y","kgC/m^2/year","gC/ha/y", "gC/km^2/y", "gC/m^2/year", "cm","m","mm","dm")
  
  if (!(from %in% units)) {
    stop("Invalid unit for from. Please provide valid units.")
  }
  if (!(to %in% units)) {
    stop("Invalid unit for to. Please provide valid units.")
  }
  
  if (from == "indiv/ha" && to == "indiv/km^2") {
    conversion_factor <- 0.0001
  } else if (from == "indiv/m^2" && to == "indiv/ha") {
    conversion_factor <- 10000
  } else if (from == "indiv/m^2" && to == "indiv/km^2") {
    conversion_factor <- 1000000
  }
  else if (from == "cm" & to == "m") {
    conversion_factor <- 0.01
  } else if (from == "cm" & to == "mm") {
    conversion_factor <- 10
  } else if (from == "cm" & to == "dm") {
    conversion_factor <- 0.1
  } else if (from == "m" & to == "cm") {
    conversion_factor <- 100
  } else if (from == "m" & to == "mm") {
    conversion_factor <- 1000
  } else if (from == "m" & to == "dm") {
    conversion_factor <- 10
  } else if (from == "mm" & to == "cm") {
    conversion_factor <- 0.1
  } else if (from == "mm" & to == "m") {
    conversion_factor <- 0.001
  } else if (from == "mm" & to == "dm") {
    conversion_factor <- 0.01
  } else if (from == "dm" & to == "cm") {
    conversion_factor <- 10
  } else if (from == "dm" & to == "m") {
    conversion_factor <- 0.1
  } else if (from == "dm" & to == "mm") {
    conversion_factor <- 100
  } else if (from == "kgC/ha" & to == "kgC/km^2") {
    conversion_factor <- 0.0001
  } else if (from == "kgC/m^2" & to == "kgC/ha") {
    conversion_factor <- 10000
  } else if (from == "kgC/m^2" & to == "kgC/km^2") {
    conversion_factor <- 1e6
  } else if (from == "kgC/ha/y" & to == "kgC/km^2/y") {
    conversion_factor <- 0.0001
  } else if (from == "kgC/m^2/year" & to == "kgC/ha/y") {
    conversion_factor <- 10000
  } else if (from == "kgC/m^2/year" & to == "kgC/km^2/y") {
    conversion_factor <- 1e6
  } else if (from == "gC/ha/y" & to == "kgC/ha/y") {
    conversion_factor <- 0.001
  } else if (from == "gC/km^2/y" & to == "kgC/km^2/y") {
    conversion_factor <- 1e6
  } else if (from == "gC/m^2/year" & to == "kgC/m^2/year") {
    conversion_factor <- 0.001
  }
  
  df@data[[benchmark@guess_layers]] <- df@data[[benchmark@guess_layers]] / conversion_factor
  
  return(df)
}
  
  