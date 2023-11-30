
#' Subset grid cells by cropping by extent
#'
#' @param input Holds information from the instruction yaml file
#'
#'
#' @return List of all cropped fields
#' @export
#'
#' @examples setGridCellExtent(input = input)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
setGridCellExtent <- function(input){

if (input[["Directory"]][["spatial_extent_id"]] == "Spain"){
spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_spain.txt", package = "DGVMBenchmarks"), header = TRUE)
}
  else if (input[["Directory"]][["spatial_extent_id"]] == "France"){
  spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_france.txt", package = "DGVMBenchmarks"), header = TRUE)
  }
    else if (input[["Directory"]][["spatial_extent_id"]] == "Switzerland"){
    spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_switzerland.txt", package = "DGVMBenchmarks"), header = TRUE)
    }
      else if (input[["Directory"]][["spatial_extent_id"]] == "Germany"){
      spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_belgium.txt", package = "DGVMBenchmarks"), header = TRUE)
      }
        else if (input[["Directory"]][["spatial_extent_id"]] == "Czech Republic"){
        spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_czech.txt", package = "DGVMBenchmarks"), header = TRUE)
        }
          else if (input[["Directory"]][["spatial_extent_id"]] == "Poland"){
          spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_poland.txt", package = "DGVMBenchmarks"), header = TRUE)
          }
            else if (input[["Directory"]][["spatial_extent_id"]] == "Belgium"){
            spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_belgium.txt", package = "DGVMBenchmarks"), header = TRUE)
            }
              else if (input[["Directory"]][["spatial_extent_id"]] == "Netherlands"){
              spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_netherlands.txt", package = "DGVMBenchmarks"), header = TRUE)
              }
                else if (input[["Directory"]][["spatial_extent_id"]] == "Sweden"){
                spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_sweden.txt", package = "DGVMBenchmarks"), header = TRUE)
                }
                  else if (input[["Directory"]][["spatial_extent_id"]] == "Finland"){
                  spatial_extent <- read.table(system.file("extdata","gridlists", "gridlist_finland.txt", package = "DGVMBenchmarks"), header = TRUE)
                  }
                    else if (input[["Directory"]][["spatial_extent_id"]] == "Europe"){
                    spatial_extent <- raster::extent(c(xmin = -10, ymin = 35, xmax = 35, ymax  = 72))
                    }
                      else if (input[["Directory"]][["spatial_extent_id"]] == "Temperate"){
                      spatial_extent <- raster::extent(c(xmin = -8.75, ymin = 41.75, xmax = 24.25, ymax  = 60.75))
                      }
                        else if (input[["Directory"]][["spatial_extent_id"]] == "Boreal"){
                        spatial_extent <- raster::extent(c(xmin = 11.75, ymin = 59.25, xmax = 31.25, ymax  = 69.75))
                        }
                          else if (input[["Directory"]][["spatial_extent_id"]] == "Mediterrainian"){
                          spatial_extent <- raster::extent(c(xmin = -7.25, ymin = 36.25, xmax = 9.25, ymax  = 44.75))
                          }
                            else if (input[["Directory"]][["spatial_extent_id"]] == "Custom") {
                            #  Extract custom extent values from the parameter file
                            custom_xmin <- input[["Directory"]][["custom_xmin"]]
                            custom_xmax <- input[["Directory"]][["custom_xmax"]]
                            custom_ymin <- input[["Directory"]][["custom_ymin"]]
                            custom_ymax <- input[["Directory"]][["custom_ymax"]]
                            spatial_extent <- raster::extent(c(xmin = custom_xmin, ymin = custom_ymin, xmax = custom_xmax, ymax = custom_ymax))
                            }
                              else if (input[["Directory"]][["spatial_extent_id"]] == "Gridlist"){
                              spatial_extent <- read.table(input[["Directory"]][["gridlist"]])
                              }
                                else if (input[["Directory"]][["spatial_extent_id"]] == "Full"){
                                  spatial_extent <- NULL
                                }


return(spatial_extent)
}
