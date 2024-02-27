# DGVMBenchmarks
Benchmarking functionality for DGVMs/LSMs built on DGVMTools.

TellMeEurope:
TellMeEurope is a benchmarking report using the functionality of DGVMBenchmarks tailored for European scale data input and simulation output. The report handles both spatial (gridded) and temporal (time-series) evaluation. As of this version the available file formats compatible with the evaluation structure are LPJ-GUESS (.out), NetCDF (.nc), aDGVM, aDGVM2 and ICOS. ICOS is an internally defined format for ICOS dataset time-series input, this is processed from raw ICOS station input (.csv) (should not be altered before use). 

The report is in a three part structure where the 1st part is a YML instruction file, 2nd a configuration file and 3rd the actual report Rmd.

# TellMeEurope YAML
The YAML instruction file is where the user defines and structures their benchmarks. This is built as multiple benchmark objects that reads as a list of lists where each list is a certain benchmark. The first two objects that needs defining are *Directory* and *Switches*. 
In *Directory* the user defines their path´s to Data, simulation 1 and simulation 2 (Simulation 1 is mandatory for the report to run, however it is optional to use Data and sim 2, skip by leaving empty or setting NULL). The users pathways should lead to the directory containing the users different simulations a level above the actual input files as the report is adapted to evaluate multiple simulations in the same session. The user then has to define which simulations will be evaluated. The simulation is the name of the folder containing your files to be evaluated. 
Your path would hence be e.g. "/your/path/to/your/simulations". Here you have different folder for different simulations e.g. trunk, ICOS, Profound.... When running the report your path will be connected to the simulation of the specific benchmark hence if evaluation files from ICOS the report will connect your path with the simulation folder e.g. "/your/path/to/your/simulations/ICOS". Furthermore, the user is able to set the extent prior to evaluation either by choosing a pre-defined grid cell list (Most of the current available European countries) or using own grid list or setting extent limits. 

```yaml
# Directory (YML)
Directory:
  New: "your\path\to\simulation1"
  New_id: "Euapp probabilistic harvest"
  Data: "your\path\to\Data"
  Old: "your\path\to\simulation2"
  Old_id: "Euapp with thinning"
  Format: ["GUESS","ICOS"]
  Simulation_name: ["european_applications", "profound", "ICOS"]
  spatial_extent_id: "Full"
  custom_xmax:
  custom_xmin:
  custom_ymax:
  custom_ymin:
  gridlist:
  ```
In *Switches* you can turn on and off benchmark and choose to render plots or not. Logical *TRUE*/*FALSE* connected to the chunks in the Rmd. 
```yaml
##Switches (YML)
Switches:
  do_plots: TRUE
  do_agpp: TRUE
  do_cmass: FALSE
  do_stem_density: FALSE
  do_qdbh: FALSE
  do_woody_growth: TRUE
  do_profound: FALSE
  do_cmass_pft: TRUE
  do_cmass_pft1: FALSE
  do_ICOS_GPP: TRUE
  ```
The rest of the objects are benchmark specific and should be filled out as per benchmark.
Below is the object for a benchmark of annual GPP evaluated against NFI data. The information is used to build the benchmark using File_name, Unit, Id, first/last year and layer for processing the fields; limits, breaks and axis_lim for plotting; Name, source, description for tabular output.

```yaml
##Benchmark (YML)
AGPP:
  File_name: "agpp"
  Unit: "kg/m2/y"
  Id: "AGPP"
  Name: "AGPP"
  Description: "Annual GPP"
  First_year_Data: NULL
  Last_year_Data: NULL
  First_year: 2010
  Last_year: 2010
  Limits: [-1, 1]
  Breaks: [-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1]
  Axis_lim: [0, 3]
  Layer: "Forest_sum"
  Dataset_name: "LRF-GPP"
  Dataset_longname: "Light Response Function GPP (LRF-GPP)"
  Dataset_source: "Tagesson, T, et al.(2021). https://doi.org/10.1111/gcb.15424"
  Dataset_description: "Comparison of annual GPP for 2010 as produced by LPJ-GUESS to the light response function generated GPP based on satellite PAR data (Tagesson et al., 2020). The Light Response Function (LRF) GPP dataset at 0.05° × 0.05° resolution is modelled with an ecosystem-level physiological approach taking the asymptotic relationship between GPP and incoming photosynthetically active radiation (PAR) into account (Tagesson et al., 2020). The data was processed at 0.05° and aggregated to 0.5° taking the grid-cell mean value. 0.05° forest pixels were isolated before aggregation using the Corine Land Cover dataset (CLC) (100m) from Copernicus Land Monitoring Service 2018, European Environment Agency (EEA). Using CLC, a threshold was set to each GPP grid cell, removing all cells with less than 80% forest cover."
```

# TellMeEurope_config
The configuration script is the bridge between the instruction file and the Rmd. This scripts reads the YML file, defines your sources from you directories, sets the format, sets extent, initializes tabular output and most importantly initializes a benchmark object. The benchmark object is integral to bind the benchmark instructions to the actual benchmark in the report rendering. 

```yaml
##Initialize benchmark class
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
    limits = "list",
    breaks = "list",
    ax_limits = "list",
    metrics = "character"
  )
)
```
#TellMeEurope report
The TellMeEurope Rmd is where the rendering of the report takes place. This markdown script reads necessary libraries and initializes the connection between the instructions and the configuration by souring these when running. Its in the Rmd that the user make use of the DGVMBenchmark toolset to structure and evaluate their benchmarks. The report within the package displays the current version of the final rendered version. Make note of the markdown language to get a feel for how you want to structure your report. This version make use of tab-sets in order where each benchmark has its own section in the report with tabs to flick through the output. While creating your benchmark you may follow a workflow made to handle most input. Every benchmark has its own *Chunk* this means that each benchmark has an exclusive part of the code and should not be entangled with other benchmarks. Below is an example of how a benchmark chunk is structured. 

This example is the evaluation of Annual GPP between Data and two simulations. 

The if check is connected to the set *Switches* and if *FALSE* will skip to the next chunk.
You set the benchmark_name in order to connect to the correct benchmark object in the instruction file (must be same sting as the header of that YML object).

Then you process your data source, all necessary objects are made in the configuration but you need to now what format it is in. See all_GUESS_datasets, could also be all_NetCDF/all_ICOS_datasets.

Next step is to define the current benchmark. This is done using the benchmark_name to connect to the yml object (see YAML and setclass *benchmark* in configuration).

When you have a defined benchmark you are able to process the simulation fields based on the instructions from the yml and also sort your different fields (data and sim) into neat lists for later use. all_sim_full only contain simulations while all_Fields_list contain both data and simulations.

Its now time to create comparison objects from your fields. A comparison object is a field that combines two fields by dimensions Lat/Lon, Year, Day, Season and keeps the information to compare for both layers where both layers match in dimension. The spatial or temporal range will always be limited by the range in data, so for spatial comparisons only grid cells that are present in both fields will be kept and  in temporal comparisons only points in time present in both layers will be kept. fullSpatial/fullTemporalComparison will make a comparison object of all possible field combinations.

When all_comparisons have been created and filled whit comparison objects, its time to create output from these. There are many options available, for spatial output you would always use the same plotting tools as gridded fields will be handled the same. The options for spatial output is either to make difference maps; *plotAllSpatialComparisons*, or to make scatterplots; *plotFullScatter*. Temporal output may differ as information per station or in time may vary from source to source, as of this version there are plotting tools that are ICOS specific and Profound specific. This may change in future updates. Tabular output is created from the comparison objects where statistical metrics are calculated per automation. Use *buildSummaryTable* to get spatial mean´s of your fields to compare and *makeMetricTable* to extract the statistics you want to highlight. Both of these tables are initialized in the configuration where you are able to initialize multiple tables if you need to keep the tabular output to contain different sections and benchmarks.  

## AGPP difference Plots 
```{r Difference, echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
if (do_agpp){
  # Define which benchmark to do
benchmark_name <- "AGPP"

# Process your dataset
Data.year.mean <- processDataSource(all_datasets = all_GUESS_datasets, input, spatial.extent = spatial.extent, benchmark_name, simulation = 1)

# Build your current benchmark from input YAML file  
this_benchmark <- createBenchmark(benchmark_name, input, simulation = 1, Data.year.mean, spatial.extent)

# Process simulations and order together with dataset, the lists returned are needed in later functions.
field_lists <- DGVMBenchmarks::getAllFields(this_benchmark,all_GUESS_simulation_Sources_list)
all_sim_full <- field_lists[[1]]
all_Fields_list <- field_lists[[2]]

## Make comparison objects from your fields, used for comparison only!
all_comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark = this_benchmark, all_maps = all_Fields_list,       new_model = all_GUESS_simulation_Sources_list[[1]]@name, old_model = all_GUESS_simulation_Sources_list[[2]]@name)

## Spatial plotting of your comparisons, handels multiple layers to.
if(do_plots){DGVMBenchmarks::plotAllSpatialComparisons(Benchmark = this_benchmark, all_comparisons = all_comparisons)}

## Table making, extracts the spatial mean from your fields.
summary_table <- DGVMBenchmarks::buildSummaryTable(benchmark = this_benchmark, all_sim_full, summary_col_names, input = input)
## Extracts statistical metrics from your fields.
if(length(this_benchmark@datasets[[1]]) != 0){metric_table <- rbind(metric_table,
                     DGVMBenchmarks::makeMetricTable(benchmark = this_benchmark,
                                      all_comparisons_list = all_comparisons,
                                      simulation_sources = all_GUESS_simulation_Sources_list))}
}
```