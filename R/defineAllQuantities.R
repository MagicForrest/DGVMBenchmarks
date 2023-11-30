
#' Define all availiable quanteties and read correct format
#'
#' @param input Input yaml instruction file
#'
#' @return Updated quantity list for specific format
#' @export
#'
#' @examples defineAllQuantities(input = input_data)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
defineAllQuantities <- function(input){
# Extract the file name and unit element from json to define quantity
Names <- lapply(input[-1:-2], function(lst) lst[[1]])
Units <- lapply(input[-1:-2], function(lst) lst[[2]])

## Set correct format
if(input[["Directory"]][["Format"]] == "GUESS"){format <- GUESS}else if(input[["Directory"]][["Format"]] == "NetCDF"){format <- NetCDF}else if(input[["Directory"]][["Format"]] == "aDGVM"){format <- aDGVM}else if(input[["Directory"]][["Format"]] == "aDGVM2"){format <- aDGVM2}

## Defines new quantities
for (i in seq_along(Names)) {
  Quantity <- Names[[i]]
  Unit <- Units[[i]]
  format <- defineQuantity(id = Quantity, units = Unit, format = input[["Directory"]][["Format"]], add.to = format)
}
return(format)
}
