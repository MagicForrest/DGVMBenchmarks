#' Define FLUXNET sources. Create a FLUXNET source for your dataset. 
#'
#' @param input YML input to get your directory information
#' 
#'
#' @return List of lists 
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
define_FLUXNET_DatasetSource <- function(input){
  ## Set Model and reference sources ##
  all_FLUXNET_datasets <- list()
  if (is.na(input[["Directory"]][["Data"]]) || input[["Directory"]][["Data"]] == ""){input[["Directory"]][["Data"]] <- NULL}
  if (!is.null(input[["Directory"]][["Data"]])){
    Reference_data <- DGVMTools::defineSource(id = "Obs", dir = input[["Directory"]][["Data"]], format = FLUXNET, name = "Obs")
    all_FLUXNET_datasets <- list(Reference_data)}
  return(all_FLUXNET_datasets)
}