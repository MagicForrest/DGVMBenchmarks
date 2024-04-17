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
    for (j in seq_along(Benchmark@datasets)){


        # Join by gridcell only keep cells present in both Model and Reference layers
    data_df <- data.frame(Lat = this_sim@data$Lat, Lon = this_sim@data$Lon, Sim = this_sim@data[[Benchmark@guess_layers]]) #All plots!!!!
    data_df_obs<- data.frame( Lat = Benchmark@datasets[[j]]@data$Lat, Lon = Benchmark@datasets[[j]]@data$Lon , obs = Benchmark@datasets[[j]]@data[[Benchmark@guess_layers]]) #All Plots !!!!!
    data<-dplyr::left_join(data_df_obs,data_df)
    data<- na.omit(data)

    xmin = min(data$Sim)
    xmax = max(data$Sim)
    ymin = min(data$obs)
    ymax = max(data$obs)
    if (!is.null(Benchmark@ax_limits) && length(Benchmark@ax_limits) != 0){
      xmin = Benchmark@ax_limits[[1]][1]
      xmax = Benchmark@ax_limits[[1]][2]
      ymin = Benchmark@ax_limits[[1]][1]
      ymax = Benchmark@ax_limits[[1]][2]
    }

    #Plot dataframe as density plot
    densplot1<-ggplot(data = data, aes(x=obs, y=Sim)) +
      geom_bin2d(bins = 30) +
      scale_fill_continuous(type = "viridis") +
      theme_bw()+
      geom_point(alpha=0.08)+
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size=1)+
      xlim(xmin, xmax)+
      ylim(ymin, ymax)+
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
  
  if(length(all_sim_full) > 1) {
  #Plot sim1 against sim2
  # Join by gridcell only keep cells present in both Model and Reference layers
  sim1 <- data.frame(Lat = all_sim_full[[1]]@data$Lat, Lon = all_sim_full[[1]]@data$Lon, Sim1 = all_sim_full[[1]]@data[[Benchmark@guess_layers]]) #All plots!!!!
  sim2<- data.frame( Lat = all_sim_full[[2]]@data$Lat, Lon = all_sim_full[[2]]@data$Lon , Sim2 = all_sim_full[[2]]@data[[Benchmark@guess_layers]]) #All Plots !!!!!
  data<-dplyr::left_join(sim2,sim1)
  data<- na.omit(data)
  
  xmin = min(data$Sim2)
  xmax = max(data$Sim2)
  ymin = min(data$Sim1)
  ymax = max(data$Sim1)
  if (!is.null(Benchmark@ax_limits) && length(Benchmark@ax_limits) != 0){
    xmin = Benchmark@ax_limits[[1]][1]
    xmax = Benchmark@ax_limits[[1]][2]
    ymin = Benchmark@ax_limits[[1]][1]
    ymax = Benchmark@ax_limits[[1]][2]
  }
  #Plot dataframe as density plot
  densplot1<-ggplot(data = data, aes(x=Sim2, y=Sim1)) +
    geom_bin2d(bins = 30) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()+
    geom_point(alpha=0.08)+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size=1)+
    xlim(xmin, xmax)+
    ylim(ymin, ymax)+
    labs(title = paste(all_sim_full[[1]]@source@name, "vs", all_sim_full[[2]]@source@name),
         x = paste(all_sim_full[[2]]@source@name, Benchmark@id, Benchmark@unit),
         y = paste(all_sim_full[[1]]@source@name, Benchmark@id, Benchmark@unit)) +
    theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text = element_text(size = 15))+
    coord_fixed(ratio = 1)
  plot(densplot1)
  }
}
