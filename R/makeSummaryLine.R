#' Make a summary table line
#' 
#' SOme blah
#' @param benchmark An object of class Benchmark 
#' @param col_names A vector of character strings describing the column names 
#' 
#' @name makeSummaryLine
#' @rdname makeSummaryLine
#' @import DGVMTools

#' @export
#' @return A list which forms the lines of a benchmarking summary table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

makeSummaryLine <- function(benchmark, col_names) {
  
  summary_table_lines <- as.list(rep("-", length(col_names)))
  names(summary_table_lines) <- col_names
  for(this_dataset in benchmark@datasets) {
    
    # get the dataset name from either a Source or  Field
    # this is preferred
    if(is.Source(this_dataset)) dataset_name <- this_dataset@name
    # this is deprecated
    else if(is.Field(this_dataset)) dataset_name <- this_dataset@source@name
    
    if(summary_table_lines$Dataset == "-") summary_table_lines$Dataset <- dataset_name
    else summary_table_lines$Dataset <- paste0(summary_table_lines$Dataset, ", ", dataset_name) 
  }
  summary_table_lines$Quantity <- benchmark@description
  summary_table_lines$Unit <- benchmark@agg.unit
  summary_table_lines$Period <- paste(benchmark@first.year,benchmark@last.year, sep = "-")
  return(summary_table_lines)
}