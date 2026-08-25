

#' FireTable
#'
#' @param param What metrics to produce, either "BurntFr"  or "Fire".
#' @param benchmark The current benchmark.
#' @param all_sim_full List holding simulations.
#'
#' @return
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se), Susanne Suvanto (susanne.suvanto@@luke.fi)
FireTable <- function(param = NULL, benchmark = this_benchmark, all_sim_full,new_model ,old_model){
    
    
    Dataset <- benchmark@datasets[[1]]@data
    colnames(Dataset) <- c("Lon", "Lat", "Year", "Obs")
   
    
    sim1 <- all_sim_full[[1]]@data
    colnames(sim1) <- c("Lon", "Lat", "Year", "Sim1")
    
    
    if (length(all_sim_full) == 2) {
      
      sim2 <- all_sim_full[[2]]@data
      colnames(sim2) <- c("Lon", "Lat", "Year", "Sim2")
      
      
      # Merge observations + new simulation
      Fire_data <- merge(
        Dataset,
        sim1,
        by = c("Lon", "Lat", "Year")
      )
      
      # Add old simulation
      Fire_data <- merge(
        Fire_data,
        sim2,
        by = c("Lon", "Lat", "Year")
      )
      
      
      # Calculate annual statistics
      Fire_table <- Fire_data %>%
        group_by(Year) %>%
        summarise(
          Obs_tot = sum(Obs, na.rm = TRUE),
          Sim1_tot = sum(Sim1, na.rm = TRUE),
          Sim2_tot = sum(Sim2, na.rm = TRUE),
          
          Obs_mean = mean(Obs, na.rm = TRUE),
          Sim1_mean = mean(Sim1, na.rm = TRUE),
          Sim2_mean = mean(Sim2, na.rm = TRUE),
          
          Obs_sd = sd(Obs, na.rm = TRUE),
          Sim1_sd = sd(Sim1, na.rm = TRUE),
          Sim2_sd = sd(Sim2, na.rm = TRUE),
          
          .groups = "drop"
        )
      
      
      # Rename simulation columns using model names
      colnames(Fire_table) <- c(
        "Year",
        "Obs_tot",
        paste0(new_model, "_tot"),
        paste0(old_model, "_tot"),
        "Obs_mean",
        paste0(new_model, "_mean"),
        paste0(old_model, "_mean"),
        "Obs_sd",
        paste0(new_model, "_sd"),
        paste0(old_model, "_sd")
      )
      
      
      
    } else {
      
      # Merge observations + new simulation
      Fire_data <- merge(
        Dataset,
        sim1,
        by = c("Lon", "Lat", "Year")
      )
      
      
      # Calculate annual statistics
      Fire_table <- Fire_data %>%
        group_by(Year) %>%
        summarise(
          Obs_tot = sum(Obs, na.rm = TRUE),
          Sim1_tot = sum(Sim1, na.rm = TRUE),
          
          Obs_mean = mean(Obs, na.rm = TRUE),
          Sim1_mean = mean(Sim1, na.rm = TRUE),
          
          Obs_sd = sd(Obs, na.rm = TRUE),
          Sim1_sd = sd(Sim1, na.rm = TRUE),
          
          .groups = "drop"
        )
      
      
      # Rename simulation columns using model name
      colnames(Fire_table) <- c(
        "Year",
        "Obs_tot",
        paste0(new_model, "_tot"),
        "Obs_mean",
        paste0(new_model, "_mean"),
        "Obs_sd",
        paste0(new_model, "_sd")
      )
    }
    
    
    return(Fire_table)
  }
