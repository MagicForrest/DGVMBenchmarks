#' Plot scatter/density plot of all simulation and dataset
#'
#' @param Benchmark Holds current becnhmark settings
#' @param all_sim_full_AGPP Holds list of all simulations
#'
#' @return Returns refined density plot of simulations vs dataset
#' @export
#'
#' @examples plotFullSpatial(Benchmark = this_benchmark, all_sim_full = all_sim_full_AGPP)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
plotFullScatter <- function(Benchmark, all_sim_full) {

  for (i in seq_along(all_sim_full)) {
    this_sim <- all_sim_full[[i]]


        # Join by gridcell only keep cells present in both Model and Reference layers
    data_df <- data.frame(Lat = this_sim@data$Lat, Lon = this_sim@data$Lon, Sim = this_sim@data[[Benchmark@guess_layers]]) #All plots!!!!
    data_df_obs<- data.frame( Lat = Data.year.mean@data$Lat, Lon = Data.year.mean@data$Lon , obs = Data.year.mean@data[[Benchmark@guess_layers]]) #All Plots !!!!!
    data<-dplyr::left_join(data_df_obs,data_df)
    data<- na.omit(data)


    #Plot dataframe as density plot
    densplot1<-ggplot(data = data, aes(x=obs, y=Sim)) +
      geom_bin2d(bins = 30) +
      scale_fill_continuous(type = "viridis") +
      theme_bw()+
      geom_point(alpha=0.08)+
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size=1)+
      xlim(Benchmark@ax_limits[[1]][1], Benchmark@ax_limits[[1]][2])+
      ylim(Benchmark@ax_limits[[1]][1], Benchmark@ax_limits[[1]][2])+
      labs(title = paste(this_sim@source@name, "vs", Benchmark@datasets[[1]]@source@name),
           x = paste(Benchmark@datasets[[1]]@source@name, Benchmark@id, Benchmark@unit),
           y = paste("Model", Benchmark@id, Benchmark@unit)) +
      theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.text = element_text(size = 15))+
      coord_fixed(ratio = 1)
    plot(densplot1)
  }
}
