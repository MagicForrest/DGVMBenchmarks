## Read input file, set parameters ##
input <- yaml::yaml.load_file(file.path(system.file("TellMeEurope.yml", package = "DGVMBenchmarks")))

sources <- DGVMBenchmarks::defineAllSources(input)
all_datasets <- sources[[1]]
all_simulation_Sources_list <- sources[[2]]

## Set the grid cell spatial extent see list of predefined options or choose "Full", "Custom" or grid list.
spatial.extent <- DGVMBenchmarks::setGridCellExtent(input = input)

## Set up spatial summary table will be built benchmark by-benchmark ##
summary_col_names <- c("Quantity", "Unit")
if (length(all_simulation_Sources_list[["GUESS"]]) != 0){
  for(this_sim in all_simulation_Sources_list[["GUESS"]]){summary_col_names <- append(summary_col_names, this_sim@name)}
  summary_col_names <- append(summary_col_names, c("Data", "Dataset", "Dataset ref."))
  summary_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
} else if (length(all_simulation_Sources_list[["NetCDF"]]) != 0){
  for(this_sim in all_simulation_Sources_list[["NetCDF"]]){summary_col_names <- append(summary_col_names, this_sim@name)}
  summary_col_names <- append(summary_col_names, c("Data", "Dataset", "Dataset ref."))
  summary_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)  
}

## This table is completely empty, it will by built benchmark by-benchmark ##
metric_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
Profound_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
ICOS_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
FLUXNET_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
Forest_fraction_table <- data.frame(check.names = F,stringsAsFactors = F)

## Inizialise the Benchmark class, to further customize your own benchmark add slot to use later. ##
setClass(
  "benchmark",
  slots = list(
    id = "character",
    name = "character",
    description = "character",
    file_name = "character",
    simulation = "character",
    spatial_extent_id = "character",
    spatial.extent = "ANY",
    guess_var = "ANY",
    guess_layers = "ANY",
    unit = "character",
    agg.unit = "character",
    datasets = "list",
    dataset_source = "character",
    first.year = "numeric",
    last.year = "numeric",
    difference_limits = "list",
    difference_breaks = "list",
    percentage.difference_limits = "list",
    percentage.difference_breaks = "list",
    values_limits = "list",
    values_breaks = "list",
    ax_limits = "list",
    metrics = "character",
    year.aggregate.method = "character",
    spatial.aggregate.method = "character",
    conversion_factor = "numeric",
    Layer_to_convert = "list",
    simulation_format = "character",
    adgvm.file.type = "character",
    adgvm.fire = "numeric",
    adgvm.climate = "numeric",
    adgvm2.scheme = "numeric",
    adgvm2.daily = "logical"
  )
)
# quick read switch and version label (for making quick read files)
quick_read <- TRUE
analysis_version <- "TellMe_Europe_v1"
spatial_extent_id <- input[["Directory"]][["spatial_extent_id"]]
version_label <- paste(analysis_version, spatial_extent_id, sep = "_")
# do verbose reads - this makes a lot of printed output which will appear in the report, only enable for developing/debugging the reading code
verbose_read <- TRUE
