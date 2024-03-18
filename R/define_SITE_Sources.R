#' Define SITE sources. Create a SITE source for your dataset and simulations. 
#'
#' @param input 
#'
#' @return List of lists for the dataset source and the simulations sources
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
define_SITE_Sources <- function(input){
  ## Set Model and reference sources ##
  all_SITE_datasets <- list()
  if (is.na(input[["Directory"]][["Data"]]) || input[["Directory"]][["Data"]] == ""){input[["Directory"]][["Data"]] <- NULL}
  if (!is.null(input[["Directory"]][["Data"]])){
    Reference_data <- DGVMTools::defineSource(id = "Obs", dir = input[["Directory"]][["Data"]], format = SITE, name = "Obs")
    all_SITE_datasets <- list(Reference_data)}
  
  # Define source for latest model run
  New_run <- DGVMTools::defineSource(id = input[["Directory"]][["New_id"]], dir = input[["Directory"]][["New"]], format = SITE, name = input[["Directory"]][["New_id"]])
  # If previous model run is to be included, defined here. All sources added to list for benchmarking.
  if (is.na(input[["Directory"]][["Old"]]) || input[["Directory"]][["Old"]] == ""){input[["Directory"]][["Old"]] <- NULL}
  if (!is.null(input[["Directory"]][["Old"]])){Old_run <- DGVMTools::defineSource(id = input[["Directory"]][["Old_id"]], dir = input[["Directory"]][["Old"]], format = SITE, name = input[["Directory"]][["Old_id"]])
  all_SITE_simulation_Sources_list <- list(New_run, Old_run)
  }else{all_SITE_simulation_Sources_list <- list(New_run)}
  return(SITE_sources <- list(all_SITE_datasets,all_SITE_simulation_Sources_list))
}