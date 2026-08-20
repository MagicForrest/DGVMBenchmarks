

#' FireTable
#'
#' @param param 
#' @param benchmark 
#' @param all_sim_full 
#'
#' @return
#' @export
#'
#' @examples
FireTable <- function(param = NULL, benchmark = this_benchmark, all_sim_full = all_sim_full){
  
  
  Dataset <- benchmark@datasets[[1]]@data
  colnames(Dataset) <- c("Lon","Lat","Year","Obs")
  
  sim1 <- all_sim_full[[1]]@data
  colnames(sim1) <- c("Lon","Lat","Year","Sim1")
  
  if (length(all_sim_full) == 2){
    sim2 <- all_sim_full[[2]]@data
    colnames(sim1) <- c("Lon","Lat","Year","Sim2")
    
    Fire_table <- merge(
      Dataset,
      sim1,
      by = c("Lon", "Lat", "Year")
    )
    Fire_table <- Fire_table %>%
      group_by(Year) %>%
      summarise(
        Obs_tot = sum(Obs,na.rm = T),
        Sim1_tot = sum(Sim1,na.rm = T),
        Sim2_tot = sum(Sim1,na.rm = T),
        Obs_mean = mean(Obs,na.rm = T),
        Sim1_mean = mean(Sim1,na.rm = T),
        Sim2_mean = mean(Sim1,na.rm = T),
        Obs_sd = sd(Obs,na.rm = T),
        Sim1_sd = sd(Sim1,na.rm = T),
        Sim2_sd = sd(Sim1,na.rm = T),
        .groups = "drop"
      )
    
    } else{
  
  Fire_table <- merge(
    Dataset,
    sim1,
    by = c("Lon", "Lat", "Year")
  )
  Fire_table <- Fire_table %>%
    group_by(Year) %>%
    summarise(
      Obs_tot = sum(Obs,na.rm = T),
      Sim1_tot = sum(Sim1,na.rm = T),
      Obs_mean = mean(Obs,na.rm = T),
      Sim1_mean = mean(Sim1,na.rm = T),
      Obs_sd = sd(Obs,na.rm = T),
      Sim1_sd = sd(Sim1,na.rm = T),
      
      .groups = "drop"
    )
}
  return(Fire_table)
  
}
