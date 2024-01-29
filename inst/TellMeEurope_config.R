## Read input file, set parameters ##
input <- yaml::yaml.load_file(file.path(system.file("TellMeEurope.yml", package = "DGVMBenchmarks")))
## Extract the file name and unit element from yml to define quantity and format
format <- DGVMBenchmarks::defineAllQuantities(input = input)

## Set Model and reference sources ##
sources <- DGVMBenchmarks::defineAllSources(input = input,format = format)
all_datasets <- sources[[1]]
all_simulation_Sources_list <- sources[[2]]

## Set the grid cell spatial extent see list of predefined options or choose "Full" or "Custom"
spatial.extent <- DGVMBenchmarks::setGridCellExtent(input = input)

## Set up summary table will be built benchmark by-benchmark ##
summary_col_names <- c("Quantity", "Unit")
for(this_sim in all_simulation_Sources_list){summary_col_names <- append(summary_col_names, this_sim@name)}
summary_col_names <- append(summary_col_names, c("Data", "Dataset", "Dataset ref."))
summary_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)

## This table is completely empty, it will by built benchmark by-benchmark ##
metric_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
Profound_table <- data.frame(check.names = FALSE, stringsAsFactors = FALSE)
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
    guess_var = "character",
    guess_layers = "character",
    unit = "character",
    agg.unit = "character",
    datasets = "list",
    dataset_source = "character",
    first.year = "numeric",
    last.year = "numeric",
    limits = "list",
    breaks = "list",
    ax_limits = "list",
    metrics = "character"
  )
)
# quick read switch and version label (for making quick read files)
quick_read <- TRUE
analysis_version <- "TellMe_Europe_v1"
spatial_extent_id <- input[["Directory"]][["spatial_extent_id"]]
version_label <- paste(analysis_version, spatial_extent_id, sep = "_")
# do verbose reads - this makes a lot of printed output which will appear in the report, only enable for developing/debugging the reading code
verbose_read <- TRUE
