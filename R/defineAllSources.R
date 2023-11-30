
#' Define all sources
#'
#' @param input yaml instruction file
#' @param format Holding format
#'
#' @return List of lists for the dataset source and the simulations sources
#' @export
#'
#' @examples defineAllSources(input = input_data, format = format)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
defineAllSources <- function(input, format){
  ## Set Model and reference sources ##
  all_datasets <- list()
  if (is.na(input[["Directory"]][["Data"]]) || input[["Directory"]][["Data"]] == ""){input[["Directory"]][["Data"]] <- NULL}
  if (!is.null(input[["Directory"]][["Data"]])){
  Reference_data <- DGVMTools::defineSource(id = "Obs", dir = input[["Directory"]][["Data"]], format = format, name = "Obs")
  all_datasets <- list(Reference_data)}

  # Define source for latest model run
  New_run <- DGVMTools::defineSource(id = "New Model Run", dir = input[["Directory"]][["New"]], format = format, name = "New Model Run")
  # If previous model run is to be included, defined here. All sources added to list for benchmarking.
  if (is.na(input[["Directory"]][["Old"]]) || input[["Directory"]][["Old"]] == ""){input[["Directory"]][["Old"]] <- NULL}
  if (!is.null(input[["Directory"]][["Old"]])){Old_run <- DGVMTools::defineSource(id = "Old Model Run", dir = input[["Directory"]][["Old"]], format = format, name = "Old Model Run")
  all_simulation_Sources_list <- list(New_run, Old_run)
  }else{all_simulation_Sources_list <- list(New_run)}
  return(sources <- list(all_datasets,all_simulation_Sources_list))
}
