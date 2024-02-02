

#' Define ICOS sources. Create a ICOS source for your dataset. 
#'
#' @param input YML input to get your directory information
#' 
#'
#' @return List of lists 
#' @export
#'
#' @examples
define_ICOS_DatasetSource <- function(input){
  ## Set Model and reference sources ##
  all_ICOS_datasets <- list()
  if (is.na(input[["Directory"]][["Data"]]) || input[["Directory"]][["Data"]] == ""){input[["Directory"]][["Data"]] <- NULL}
  if (!is.null(input[["Directory"]][["Data"]])){
    Reference_data <- DGVMTools::defineSource(id = "Obs", dir = input[["Directory"]][["Data"]], format = ICOS, name = "Obs")
    all_ICOS_datasets <- list(Reference_data)}
  return(all_ICOS_datasets)
}