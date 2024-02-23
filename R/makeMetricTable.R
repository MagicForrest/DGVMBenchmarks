#' Make metric table lines
#' 
#' SOme blah
#' @param benchmark An object of class Benchmark 
#' @param col_names A vector of character strings describing the column names 
#' 
#' @name makeMetricTable
#' @rdname makeMetricTable
#' @import DGVMTools

#' @export
#' @return A list which forms the lines of a benchmarking summary table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

makeMetricTable <- function(benchmark, all_comparisons_list, simulation_sources, signif = 3) {
  
  
  # Prepare the (empty) table of benchmarking metrics
  col_names <- c("Dataset", "Metric", "Dimension", "Years")
  col_types <- c("character", "character", "character", "character")
  
  for(this_sim in simulation_sources) {
    col_names <- append(col_names, this_sim@name)
    col_types <- append(col_types, "numeric")
  }
  col_names <- append(col_names, c("Data bootstrap", "Data mean", "Dataset ref."))
  col_types <- append(col_types, c("numeric", "numeric", "character"))
  
  # make one empty line
  metric_table_line_template <- as.list(rep("-", length(col_names)))
  names(metric_table_line_template) <- col_names
  
  # file in the dataset name
  metric_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
  
  # for each dataset (in case there are multiple)
  for(this_dataset in benchmark@datasets) {
    # for each metric
    for(this_metric in benchmark@metrics) {
      this_line <- copy(metric_table_line_template)
      this_line$Dataset <- this_dataset@source@name
      this_line$Metric <- this_metric
      metric_table <- rbind(metric_table, data.frame(this_line))
    }
  }
  
  # set some columns to numerics  - maybe do this more efficiently 
  for(column_index in 1:length(col_types)){
    if(col_types[column_index] == "numeric") metric_table[, column_index] <- rep(NA_real_, nrow(metric_table))
  }
  
  # read the scores and spatiotemporal of comparison from the 
  for(this_dataset in benchmark@datasets) {
    for(this_sim_Source in simulation_sources) {
      for(this_metric in benchmark@metrics){
        
        metric_table[which(metric_table$Metric == this_metric), gsub(pattern = " ", replacement = ".", x = this_sim_Source@name)] <- signif(all_comparisons_list[["Values"]][[paste(this_sim_Source@name, "-", this_dataset@source@name)]]@stats[[this_metric]],3)
   print(all_comparisons_list[["Values"]][[paste(this_sim_Source@name, "-", this_dataset@source@name)]]@sta.info1)
   print(all_comparisons_list[["Values"]][[paste(this_sim_Source@name, "-", this_dataset@source@name)]]@sta.info2)
   
        
        
             common_sta <- commonSTAInfo(list(all_comparisons_list[["Values"]][[paste(this_sim_Source@name, "-", this_dataset@source@name)]]@sta.info1,
                                         all_comparisons_list[["Values"]][[paste(this_sim_Source@name, "-", this_dataset@source@name)]]@sta.info2))
       print(common_sta)
        
         metric_table[which(metric_table$Metric == this_metric), "Years"] <- paste(common_sta@first.year, common_sta@last.year, sep = "-")
        all_dims <- getDimInfo(all_comparisons_list[["Values"]][[paste(this_sim_Source@name, "-", this_dataset@source@name)]])
        domain <- list()
        print(all_dims)
        if("Lat" %in% all_dims & "Lat" %in% all_dims) domain[["Spatial"]] <- "Spatial"
        if("Year" %in% all_dims) domain[["Temporal"]] <- "Temporal"
        if("Month" %in% all_dims) domain[["Seasonal"]] <- "Seasonal"
        domain <- paste(domain, collapse = ",")
        print(domain)
        metric_table[which(metric_table$Metric == this_metric), "Dimension"] <- domain
        
      }
    }  
  }
  
  # do the NULL benchmarks
  
  
  names(metric_table) <- col_names
  return(metric_table)
  
}