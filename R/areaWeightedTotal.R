#' Calculate area-weighted total
#' 
#' Calculates area-weighted sum of a Field.  Useful for deriving global/regional GPP or other fluxes, C stocks etc.  
#' Note the Field variable must be on a per-unit area basis, with the area unit supplied as an argument (default is "m^2").
#' 
#' @param x A spatial Field for which the area-weighted summed flux
#' @param area_unit A character string specifying the unit the Field is in, default is "m^2" but can also be "km^2" or "ha" 
#' (see DGVMTools::addArea)
#' 
#' @name areaWeightedTotal
#' @rdname areaWeightedTotal
#' @import DGVMTools
#' @import data.table
#' @export
#' @return A DGVMTools::Field object the area-weighted summed data.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 

areaWeightedTotal <- function(x, area_unit = "m^2") {
  
  # for handling layer names of summed layers
  summed_prefix <- "Area_summed"
  all_summed_layers <- paste(summed_prefix, layers(x), sep = "_")
  
  # for doing multiple layerOp() in a pipe
  multiplyAllByArea <- function(x, prefix){
    for(this_layer in layers(x))  layerOp(x, "*", c(this_layer, "Area"), paste(prefix, this_layer, sep = "_"))
    return(x)
  }
  
  # suppress warnings because of potential warning when setting keys after averaging away all dimensions
  suppressWarnings(
    addArea(x, area_unit) %>%
      multiplyAllByArea(summed_prefix) %>%
      selectLayers(all_summed_layers) %>% 
      aggregateSpatial(method = "sum"))
  
}