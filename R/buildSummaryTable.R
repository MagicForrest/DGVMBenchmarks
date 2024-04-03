#' Build summary table from benchmark. Provides the spatial mean value of your data sources.
#'
#' @param benchmark Holds current benchmark instructions
#' @param all_sim_full list of all simulation fields
#' @param summary_col_names Column header names
#'
#' @return datatable which can be knitted into tabular output
#' @export
#'
#' @examples buildSummaryTable(benchmark = this_benchmark, all_sim_full, summary_col_names)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
buildSummaryTable <- function(benchmark = this_benchmark, all_sim_full, summary_col_names, input){
  
  summary_line <- as.list(rep("-", length(summary_col_names)))
  names(summary_line) <- summary_col_names
  if(length(benchmark@datasets[[1]]) != 0){
    for(this_dataset in benchmark@datasets) {
      if(summary_line$Dataset == "-"){summary_line$Dataset <- this_dataset@source@name}
    }}
  
  if(length(benchmark@datasets[[1]]) == 0){
      if(summary_line$Dataset == "-"){summary_line$Dataset <- "NA"}
    }
  summary_line$Quantity <- benchmark@description
  summary_line$Unit <- benchmark@agg.unit
  
  
  if(length(benchmark@datasets[[1]]) != 0){
    full_Field_ymean <- aggregateSpatial(unlist(benchmark@datasets[[1]]))
    
    summary_line$Data <- signif(full_Field_ymean@data[[benchmark@guess_layers]], 3)
    summary_line$`Dataset ref.` <- benchmark@dataset_source}
  
  if(length(benchmark@datasets[[1]]) == 0){
    summary_line$Data <- "NA"
    summary_line$`Dataset ref.` <- "NA"}
  
  for (i in seq_along(all_sim_full)){
    this_simulation <- all_sim_full[[i]]
    this_simulation_ymean <- suppressWarnings(aggregateSpatial(this_simulation, "mean"))
    summary_line[[this_simulation@source@name]] <- signif(this_simulation_ymean@data[[benchmark@guess_layers]], 3)
  }
  if (summary_line[[input[["Directory"]][["Sim1_id"]]]] != "-" || summary_line[[input[["Directory"]][["Sim2_id"]]]] != "-"){
    summary_table <- rbind(summary_table, summary_line)
    names(summary_table) <- summary_col_names}
  return(summary_table)
}
