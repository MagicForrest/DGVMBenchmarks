# *********************************************************
# ********  TELLME BENCHMARK SCRIPT FOR LPJ-GUESS  ********

#  TODO: Define and handle appropriate command line arguements 

#### LOAD LIBRARIES
library(DGVMTools)
library(DGVMBenchmarks)


#### DEFINE THE MODELS RUNS, COMMON TO ALL BENCHMARKS ####

# define the LPJ-GUESS run to be benchmarked
new_simulation_Source <- defineSource(id = "newrun",
                                      name = "New Run",
                                      format = GUESS,
                                      dir = "~/GuessRuns/trunk-v4.0/Standard/")


# define a reference LPJ-GUESS run 
# question: is this required?
old_simulation_Source <- defineSource(id = "oldrun",
                                      name = "Old Run",
                                      format = GUESS,
                                      dir = "~/GuessRuns/v4.1/PNV_BLAZE/")



#### GCP BENCHMARK ####

### First read the data
GCP_full_Field <- read_GCP()

# now read the LPJ-GUESS data
new_simulation_NEE <- getField(source = new_simulation_Source,
                               "cflux",
                               first.year = first_GCP_year,
                               last.year = last_GCP_year,
                               spatial.aggregate.method = "w.sum",
                               spatial.extent.id = "Global")

old_simulation_NEE <- getField(source = old_simulation_Source,
                               "cflux",
                               first.year = first_GCP_year,
                               last.year = last_GCP_year,
                               spatial.aggregate.method = "w.sum",
                               spatial.extent.id = "Global")

# divide by -10^12 for kg -> Tg and to reverse the sign
new_simulation_NEE <- layerOp(x = new_simulation_NEE, operator = "divc", layers = layers(new_simulation_NEE), new.layer = layers(new_simulation_NEE), constant = -1E12)
old_simulation_NEE <- layerOp(x = old_simulation_NEE, operator = "divc", layers = layers(new_simulation_NEE), new.layer = layers(new_simulation_NEE), constant = -1E12)

# calculate yearly means of all
new_simulation_NEE_ymean <- aggregateYears(new_simulation_NEE, "mean")
old_simulation_NEE_ymean <- aggregateYears(old_simulation_NEE, "mean")
GCP_full_Field_ymean  <- aggregateYears(GCP_full_Field, "mean")

# plot all together
NEE_plot <- plotTemporal(list(new_simulation_NEE,old_simulation_NEE, GCP_full_Field), 
                         col.by = "Source", 
                         layers = "NEE", 
                         title = "Global NEE", 
                         subtitle = NULL, 
                         y.label = "Global NEE (TgC/year)",
                         text.multiplier = 3, sizes = 2)
print(NEE_plot)

# make a simple data.frame to put numbers on the plot
global.numbers.df <- data.frame(x= rep(as.Date(paste(first_GCP_year), "%Y")), 
                                y = c(6, 5, 4), 
                                label = c(paste0("GCB residual: ", signif(GCP_full_Field_ymean@data[["NEE"]], 3)), 
                                          paste0(old_simulation_NEE_ymean@source@name, ": ", signif(old_simulation_NEE_ymean@data[["NEE"]], 3)),
                                          paste0(new_simulation_NEE_ymean@source@name, ": ", signif(new_simulation_NEE_ymean@data[["NEE"]], 3))))

NEE_plot <-  NEE_plot + geom_text(data = global.numbers.df,  mapping = aes(x = x, y = y, label = label), size = 10, hjust = 0, col = "black")
print(NEE_plot)
