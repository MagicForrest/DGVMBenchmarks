#' Full Spatial Comparison
#' 
#' SOme blah
#' @param benchmark An object of class Benchmark which described the benchmark to be performed.  Needed only for its "guess_var" and "datasets" slots.
#' @param all_maps A list of all spatial values Fields to be compared (both data and simulations, which of these are data are identified from the benchmark argument)
#' @param all_trends A list of all trend Fields to be compared (structure as above))
#' @param all_seasonal A list of all seasonal Fields to be compared (structure as above))
#' @param new_model,old_model Characters strings defing the new and old model runs from a "new minus old" comparisons)
#' 
#' @name fullSpatialComparison
#' @rdname fullSpatialComparison
#' @import DGVMTools

#' @export
#' @return A list'o'lists
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

fullSpatialComparison <- function(benchmark, all_maps, all_trends = NULL, all_seasonal = NULL, new_model = NULL, old_model = NULL) {
  
  # determine the dataset names
  all_datasets <- c()
  for(this_dataset in benchmark@datasets) {
    
    # get the dataset name from either a Source or  Field
    # this is preferred
    if(is.Source(this_dataset))  all_datasets <- append(all_datasets, this_dataset@name)
    # this is deprecated
    else if(is.Field(this_dataset))  all_datasets <- append(all_datasets, this_dataset@source@name)
    
  }
  
  # determine the simulation names
  all_sims <- c()
  for(this_field in all_maps) {
    if(!this_field@source@name %in% all_datasets) all_sims <- append(all_sims,this_field@source@name)
  }
  
  # Lists for all comparisons
  spatial_comparisons_list <- list()
  trend_comparisons_list <- list()
  seasonal_comparisons_list <- list()
  
  # loop through model runs to be processed and datasets to be compared
  for(this_sim in all_sims) {
    for(this_dataset in all_datasets) {
      
      # NOTE:  This needs to be exactly this form because it needs to match the panel names made by plotSpatialComparison() 
      comparision_name <- paste0(this_sim, " - ", this_dataset)
      
      #### SPATIAL VALUES COMPARISONS ####
      suppressWarnings(
        spatial_comparisons_list[[comparision_name]] <- compareLayers(field1 = all_maps[[this_sim]],
                                                                      field2 = all_maps[[this_dataset]],
                                                                      layers1 =  benchmark@guess_var, 
                                                                      layers2 = benchmark@guess_var, 
                                                                      show.stats = FALSE))
        
       
      
      
      #### TRENDS COMPARISONS ####
      if(!missing(all_trends) & !is.null(all_trends)) {
        suppressWarnings(trend_comparisons_list[[comparision_name]] <- compareLayers(field1 = all_trends[[this_sim]],
                                                                                     field2 = all_trends[[this_dataset]],
                                                                                     layers1 = "Trend",
                                                                                     layers2 = "Trend", 
                                                                                     show.stats = FALSE))
      }
      
      
      #### SEASONAL COMPARISONS ####
      # before doing comparison, set all (incorrectly) negative GPPs to zero
      if(!missing(all_seasonal) & !is.null(all_seasonal)) {
        this_simulation_only_positive <- layerOp(all_seasonal[[this_sim]],
                                                 operator = function(x){pmax(x,0)},
                                                 layers = benchmark@guess_var, 
                                                 new.layer = benchmark@guess_var)
        suppressWarnings(
          seasonal_comparisons_list[[comparision_name]] <- compareLayers(field1 = this_simulation_only_positive,
                                                                         field2 = all_seasonal[[this_dataset]],
                                                                         layers1 = benchmark@guess_var,
                                                                         layers2 = benchmark@guess_var, 
                                                                         do.seasonality = TRUE, 
                                                                         show.stats = FALSE))
      }
    }
  }
  
  
  # also do a new minus old model comparison if values supplied
  if(!is.null(old_model) && !is.null(new_model)) {
    
    
    # NOTE:  This needs to be exactly this form because it needs to match the panel names made by plotSpatialComparison() 
    comparision_name <- paste0(new_model, " - ",old_model)
    
    #### SPATIAL VALUES COMPARISONS ####
    suppressWarnings(
      spatial_comparisons_list[[comparision_name]] <- compareLayers(field1 = all_maps[[new_model]],
                                                                    field2 = all_maps[[old_model]],
                                                                    layers1 =  benchmark@guess_var, 
                                                                    layers2 = benchmark@guess_var, 
                                                                    show.stats = FALSE))
    
    #### TRENDS COMPARISONS ####
    if(!missing(all_trends) & !is.null(all_trends)) {
      suppressWarnings(trend_comparisons_list[[comparision_name]] <- compareLayers(field1 = all_trends[[new_model]],
                                                                                   field2 = all_trends[[old_model]],
                                                                                   layers1 = "Trend",
                                                                                   layers2 = "Trend", 
                                                                                   show.stats = FALSE))
    }
    
    
    #### SEASONAL COMPARISONS ####
    if(!missing(all_seasonal) & !is.null(all_seasonal)) {
      # before doing comparison, set all (incorrectly) negative GPPs to zero
      this_simulation1_only_positive <- layerOp(all_seasonal[[new_model]],
                                                operator = function(x){pmax(x,0)},
                                                layers = benchmark@guess_var, 
                                                new.layer = benchmark@guess_var)
      
      this_simulation2_only_positive <- layerOp(all_seasonal[[old_model]],
                                                operator = function(x){pmax(x,0)},
                                                layers = benchmark@guess_var, 
                                                new.layer = benchmark@guess_var)
      
      
      suppressWarnings(
        seasonal_comparisons_list[[comparision_name]] <- compareLayers(field1 = this_simulation1_only_positive,
                                                                       field2 = this_simulation2_only_positive,
                                                                       layers1 = benchmark@guess_var,
                                                                       layers2 = benchmark@guess_var, 
                                                                       do.seasonality = TRUE, 
                                                                       show.stats = FALSE))
    }
  }
  
  
  
  return(list(
    "Values" = spatial_comparisons_list,
    "Trend" = trend_comparisons_list,
    "Seasonal" = seasonal_comparisons_list
  ))
  
  
}