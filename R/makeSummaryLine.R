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
    if(summary_table_lines$Dataset == "-") summary_table_lines$Dataset <- this_dataset@source@name
    else summary_table_lines$Dataset <- paste0(summary_table_lines$Dataset, ", ", this_dataset@source@name) 
  }
  summary_table_lines$Quantity <- benchmark@description
  summary_table_lines$Unit <- benchmark@agg.unit
  summary_table_lines$Period <- paste(benchmark@first.year,benchmark@first.year, sep = "-")
  return(summary_table_lines)
}