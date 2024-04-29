# DGVMBenchmarks
Benchmarking functionality for DGVMs/LSMs built on DGVMTools.

DGVMBenchmark is a benchmarking toolset built on the logic of DGVMTools. It makes use of the ease and flexibility that DGVMTools allows for handling DGVM output. DGVMBenchmarks is a comprehensive benchmarking tool which aims for a standardized but flexible and user-friendly framework within which the user is able to evaluate their modeled output against data or against other models.
The report handles both spatial (gridded) and temporal (time-series) evaluation. As of this version the available file formats compatible with the evaluation structure are LPJ-GUESS (.out), NetCDF (.nc), aDGVM, aDGVM2, ICOS & FLUXNET.

Within the package structure are files and grid lists that can be used as predefined layers to set grid cell extents or correct mismatching Lat Lon for ICOS, FLUXNET and PROFOUND. This is found in /inst/extdata/.

# TellMeEurope

TellMeEurope is a benchmarking report using the functionality of DGVMBenchmarks tailored for European scale data input and simulation output. The report handles both spatial (gridded) and temporal (time-series) evaluation. As of this version the available file formats compatible with the evaluation structure are LPJ-GUESS (.out), NetCDF (.nc), aDGVM (.nc), aDGVM2 (.nc), ICOS, FLUXNET and SITE. ICOS and FLUXNET specially defined formats for ICOS and FLUXNET dataset time-series input, this is processed from raw station input (.csv) (should not be altered before use). SITE is a format to handle more general stations measurement data. For site the format handles .csv file types as most station data is in this format, additionally it also looks for a site name identifier "Site". 

Several different sets of benchmarks are defined. 

**Spatial benchmarks**

1.	Forest structure compared to National Forest Inventory and satellite data. These benchmarks are designed to compare a model simulation to the current structure and dynamics of European forests. For the fairest comparison, the model simulation should therefore be designed to give a closest representation of reality of current forests possible within that model structure. The data is provided on a 0.5° x 0.5° grid and covers the period ca. 2001-2010 and therefore model simulations should be carried out (or aggregated to) this scale and time period. Benchmarks include Woody biomass growth rate, stem density, quadratic mean diameter at breast height (QDBH) and satellite derived LRF-GPP (Annual gross primary productivity) following "Tagesson, T, et al. (2021). https://doi.org/10.1111/gcb.15424" is provided.	The satellite GPP has been masked to only cover forest areas based on a forest mask at a resolution of 0.05° x 0.05°, before being aggregated to the 0.5 x 0.5° grid.

2. Biomass by plant functional type. This is intended to show the potential biomass of each plant functional type in each grid cell of Europe. There is no comparison to observations for this benchmark. It requires a simulation that allows each plant functional type to grow to its full potential in each grid cell.

**Temporal benchmarks**

3.	Forest management test sites from the PROFOUND dataset (add reference). This includes X sites with variables of leaf area index (LAI), biomass, Stem density, QDBH and DBH. The benchmarks cover the period XXXX. Model simulations should be set up for the best possible representation of the test sites, as described in the paper above. 

4.	Eddy covariance flux sites from the ICOS network (covering gross primary productivity, net ecosystem exchange, total ecosystem respiration, evapotranspiration) and FLUXNET network (gross primary productivity, net ecosystem exchange, total ecosystem respiration). Model simulations should be set up to best represent the individual flux sites. Code for a bias correction approach for site climate with which to run the model simulations is available for FLUXNET , but not for ICOS (as the site time-series is currently too short). Additional integrated possibilities in ICOS and FLUXNET to plot against Land cover, Country, Climate and Station.

**For** each spatial benchmark it is possible to make maps of Absolute values, Difference maps and density plots. Additionally it is possible to make a summary table displaying the spatial mean for data and simulations as well as a metric table showcasing statistical metrics from comparison evaluation.
For temporal benchmarks it is possible to plot time-series Absolute values with uncertainty, time-series difference, scatter plots and more specific tailored plotting for ICOS and FLUXNET. Additionally it is possible to make tabular output of statistical metrics from the time-series comparisons.


# TellmeEurope structure

The report is in a three part structure where the 1st part is a YAML instruction file, 2nd a configuration file and 3rd the actual report in Rmd.

## TellMeEurope YAML
The YAML instruction file is where the user defines and structures their benchmarks. This is built as multiple benchmark objects that reads as a list of lists where each list is a certain benchmark. There are three types of objects that is defined within the instruction file. The first two are defined only once and are ***Directory*** and ***Switches***. In ***Directory*** the user defines settings that apply to the whole report such as directory paths and spatial extent. In ***Switches*** you can turn on and off benchmark and choose to render plots or not. Logical ***TRUE***/***FALSE*** connected to the chunks in the Rmd. The third object type is ***Benchmark*** here the user specifies in more detail how they want to process their inputs fo that specific benchmark. Each ***Benchmark*** object correspond to a specific benchmark. 

***Directory***

```yaml
# Directory (YML)
Directory:
  New: "your\\path\\to\\simulation1"
  New_id: "Euapp probabilistic harvest"
  Data: "your\\path\\to\\Data"
  Old: "your\\path\\to\\simulation2"
  Old_id: "Euapp with thinning"
  spatial_extent_id: "Full"
  custom_xmax:
  custom_xmin:
  custom_ymax:
  custom_ymin:
  gridlist:
  ```
 
***Switches*** 
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
Below is the object for a benchmark of modeled annual GPP evaluated against satellite-derived GPP. The information is used to build the benchmark using Unit, Id, Name, Description and Layer which apply for both data and simulation. Additionally there are settings for how the user wants to process their data and simulations in more detail. The processing settings handle sub-directory, file_name and Format as well as temporal range, aggregation methods and conversion factor. The benchmark is made to handle multiple datasets and also simulations with different formats per benchmark. To add more datasets or or simulations from different models you simply add an extra entry to the sub-directory, file name and format setting. For data there is also additional settings specific for processing ICOS and FLUXNET data.

```yaml
##Benchmark (YML)
AGPP:
  File_name: "agpp" # The name of the files to process, should be the same for data and sim. (If ICOS or FLUXNET use data name)
  Unit: "kg/m2/y" # The unit of the Benchmark
  Id: "AGPP" # The ID used for tabular output
  Name: "AGPP" # Used in plotting
  Description: "Annual GPP" # Used in tabular output
  Layer: "Forest_sum" # Layer/column in Fields to base the comparison on
  Data:   
    Datasets: ["european_application"] # Name of Dataset (name of subfolder in data source directory)
    First_year: NULL # Set first and last year to limit the temporal span, set to null takes the whole timespan 
    Last_year: NULL
    year.aggregate.method: "mean" # The method to aggregate time dimention 
    spatial.aggregate.method:
    Conversion_factor: NULL # Numeric() list() the factor needed for conversion of dataset, leave empty or NULL if not used
    Layer_to_convert: NULL # List holding the dataset names of the Fields to be converted
    Dataset_name: "LRF-GPP" # Used in plotting and tabular output
    Dataset_longname: "Light Response Function GPP (LRF-GPP)"
    Dataset_source: "Tagesson, T, et al.(2021). https://doi.org/10.1111/gcb.15424"
    Dataset_description: "Comparison of annual GPP for 2010 as produced by LPJ-GUESS to the light response function generated GPP based on satellite PAR data (Tagesson et al., 2020)."
    ICOS_FLUXNET_Specific:
      UT.threshold: NULL
      partition.method: NULL
      day.night.method: NULL
      NEE.day.night: NULL
      rm.leap: NULL
      data.cleaning: NULL
      qc.threshold: NULL # ICOS & FLUXNET specific settings.
  Simulations:
    Simulation: ["european_applications"]
    First_year: 2010
    Last_year: 2010
    year.aggregate.method: "mean"
    spatial.aggregate.method: 
    Conversion_factor: 
    Layer_to_convert: 
  Plotting:
    Spatial_Difference:
      Limits: [-1, 1]
      Breaks: [-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1]
    Spatial_Scatter:
      Limits: [0, 3] # Plotting settings limits and breaks list input []
  Statistics:
    ME: F
    NME: F
    NMSE: T
    RMSE: T 
    NME_2: F
    NMSE_2: F
    NME_3: F
    NMSE_3: F
    r: F
    r2: T
    r2_eff: F
```

## TellMeEurope_config
The configuration script is the bridge between the instruction file and the Rmd. This scripts reads the YML file, defines your sources from your directories, sets the format, sets extent, initializes tabular output and most importantly initializes a benchmark object. The benchmark object is integral to bind the benchmark instructions to the actual benchmark in the report rendering. 

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
    metrics = "character",
    year.aggregate.method = "character",
    spatial.aggregate.method = "character",
    conversion_factor = "numeric",
    Layer_to_convert = "list"
  )
)
```

## TellMeEurope report
The TellMeEurope Rmd is where the rendering of the report takes place. This markdown script reads necessary libraries and initializes the connection between the instruction file and the configuration file by sourcing these when running. Its in the Rmd that the user makes use of the DGVMBenchmark toolset to structure and evaluate their benchmarks. The report within the package displays the current version of the final rendered version. Make note of the markdown language to get a feel for how you want to structure your report. This version make use of tab-sets in order that each benchmark has its own section in the report with tabs to flick through the output. While creating your benchmark you may follow a workflow made to handle most input. Every benchmark has its own ***Chunk*** this means that each benchmark has an exclusive part of the code and should not be entangled with other benchmarks. Below is an example of how a benchmark chunk is structured. 
The general structure of a benchmark in the report would be as following:
Define the current benchmark -> process dataset(s) -> extract/build the current benchmark object -> process simulations -> make comparison objects -> visualize evaluation.


# Adding a benchmark

The following example is a benchmark of Annual GPP between Data and two simulations. In this example you will learn how to structure your directories prior to running the report, how to structure the YAML instructions, initialize tabular output and how to process/render your benchmark.

## File structure

Prior to running any process you file directory needs to be in order. In this example you are evaluating modeled Annual GPP output from 2 different simulation runs against data (Satellite-derived annual GPP). This means that we are pulling files containing GPP from 3 exclusive directories (Sim1, Sim2 & Data). Each directory should be structured as following:

Top-level directory
        Simulation sub-folder
                File containing GPP values
                
We use this structure in order to validate the pathway during processing and also to not having to define a new path for every benchmark.

## YAML Instruction file

When your directories are correctly structured its time to define your benchmark in the YAML instruction file.
Start by filling out your settings in the ***Directory*** object as these setting control the the whole report.

In the directory object you define your Top-level paths for each directory (Sim1, Sim2 & Data). Also name your simulations by setting an string ID in "Sim1_id" and "Sim2_id". Here you can also specify your spatial extent. For this there are a couple of options. "spatial_extent_id" handles all of these options, set to "Full" if you want the full extent of your files, "custom" if you want to define your own extent object (if chosen you must fill out xmax,xmin etc.), "gridlist" if you have your of gridlist that you want to use (if chosen give the path of your gridlist to "gridlist"). Additionally there are some country specific gridlists pre-defined in the package that you can use by setting "spatial_extent_id" to e.g. "Sweden".

```yaml
# Directory (YML)
Directory:
  Sim1: "your\\path\\to\\simulation1"
  Sim1_id: "Euapp probabilistic harvest"
  Data: "your\\path\\to\\Data"
  Sim2: "your\\path\\to\\simulation2"
  Sim2_id: "Euapp with thinning"
  spatial_extent_id: "Full"
  custom_xmax:
  custom_xmin:
  custom_ymax:
  custom_ymin:
  gridlist:
  ```

The next step is to add a Switch for your benchmark in the ***Switches*** object.
This is simply done by adding your own switch name it something appropriate (do_agpp) and then set it to ***TRUE*** if you want the report to render that benchmark or ***FALSE*** to skip it.

***Switches*** 
```yaml
##Switches (YML)
Switches:
  do_plots: TRUE
  do_agpp: TRUE
  ```

With that done you should now build your ***benchmark*** object.  
The benchmark object is nested, where the top level settings apply to the whole benchmark.
These settings are: Unit, Id, Name, Description and Layer. 
These are all flexible and you can specify exactly what you want see the comments in the code for what each setting does. "Layer" is the most important setting here, Layer specify which column in your GPP files to benchmark/evaluate.
This column needs to have the same name in all files so if they are different rename to match.

On the next level we have "Data" and "Simulations" these contain Data and Simulation specific processing settings.
Here you first connect your directory to the sub-directory by specifying within which folder your gpp files exist ("Datasets" for Data source and "Simulations" for simulation sources). Then you fill out "File_name", this is to connect the path to your sub-folder to the actual file that you want to access within the sub-folder. You are able to set the temporal range to process by setting First_year and Last_year, these can be left NULL to get the full range. 

In this example we have made told the benchmark to process the file "agpp" in the sub-directory "european_applications" in the "GUESS" format (conveniently both sub-directory and file name is named the same in all our source directories for this example). The dataset has been set to process the full temporal range and aggregated to the spatial mean over the period.
The simulations has been set to process for the year 2010 with the same aggregation method. Additionally we have set some plotting instructions as well as what statistical metrics to apply to the comparisons.

```yaml
##Benchmark (YML)
AGPP:
  Unit: "kg/m2/y"           # The unit of the Benchmark
  Id: "AGPP"                 # The ID used for tabular output
  Name: "AGPP"                # Used in plotting
  Description: "Annual GPP"    # Used in tabular output
  Layer: "Forest_sum"            # Layer/column in Fields to base the comparison on
  Data:   
    Datasets: ["european_application"]     # Name of Dataset (name of subfolder in data source directory)
    File_name: ["agpp"]                     # The name of the file to process.
    Format: ["GUESS"]
    First_year: NULL      # Set first and last year to limit the temporal span, set to null takes the whole timespan 
    Last_year: NULL
    year.aggregate.method: "mean"          # The method to aggregate time dimention 
    spatial.aggregate.method:
    Conversion_factor: NULL # Numeric() list() the factor needed for conversion of dataset, leave empty or NULL if not used
    Layer_to_convert: NULL # List holding the dataset names of the Fields to be converted
    Dataset_name: "LRF-GPP" # Used in plotting and tabular output
    Dataset_longname: "Light Response Function GPP (LRF-GPP)"
    Dataset_source: "Tagesson, T, et al.(2021). https://doi.org/10.1111/gcb.15424"
    Dataset_description: "Comparison of annual GPP for 2010 as produced by LPJ-GUESS to the light response function generated GPP based on satellite PAR data (Tagesson et al., 2020)."
    ICOS_FLUXNET_Specific:
      UT.threshold: NULL
      partition.method: NULL
      day.night.method: NULL
      NEE.day.night: NULL
      rm.leap: NULL
      data.cleaning: NULL
      qc.threshold: NULL # ICOS & FLUXNET specific settings.
  Simulations:
    Simulation: ["european_applications"]
    File_name: ["agpp"]
    First_year: 2010
    Last_year: 2010
    year.aggregate.method: "mean"
    spatial.aggregate.method: 
    Conversion_factor: 
    Layer_to_convert: 
  Plotting:
    Spatial_Difference:
      Limits: [-1, 1]
      Breaks: [-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1]
    Spatial_Scatter:
      Limits: [0, 3] # Plotting settings limits and breaks list input []
  Statistics:
    ME: F
    NME: F
    NMSE: T
    RMSE: T 
    NME_2: F
    NMSE_2: F
    NME_3: F
    NMSE_3: F
    r: F
    r2: T
    r2_eff: F
```

## Configuration file

As the configuration file is mainly the bridge between the report and the YAML instruction file, there is very little for the user to do here. Tho only thing to do is to initialize tabular output this is done as following:

``` 
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
```

Here we initialize an empty summary table for our spatial benchmarks and an empty metric table, these will be built up per benchmark in the report.

## TellMeEurope report

The report imports the defined sources, benchmark instructions and tables and now its time to actually process and render our example.

The full code example can be found below the text description. The description takes you line by line of how you would structure the ***chunk*** and process this AGPP example.

The if check is connected to the set ***Switches*** and if ***FALSE*** will skip to the next chunk.
You set the benchmark_name in order to connect to the correct benchmark object in the instruction file (must be same string as the header of that YML object).

Then you process your data source, all necessary lists and objects are made in the configuration.

Next step is to define the current benchmark. This is done using the benchmark_name to connect to the yml object (see YAML and setclass ***benchmark*** in configuration).

When you have a defined benchmark you are able to process the simulation fields based on the instructions from the YAML and also sort your different fields (data and sim) into neat lists for later use. all_sim_full only contain simulations while all_Fields_list contain both data and simulations.

Its now time to create comparison objects from your fields. A comparison object is a field that combines two fields by dimensions Lat/Lon, Year, Day, Season and keeps the information to compare for both layers where both layers match in dimension. The spatial or temporal range will always be limited by the range in data, so for spatial comparisons only grid cells that are present in both fields will be kept and  in temporal comparisons only points in time present in both layers will be kept. fullSpatial/fullTemporalComparison will make a comparison object of all possible field combinations.

When all_comparisons have been created and filled with comparison objects, its time to create output from these. There are several options available. For spatial output you would always use the same plotting tools as gridded fields will be handled the same. The options for spatial output is either to make difference maps; ***plotAllSpatialComparisons***, or to make scatterplots; ***plotFullScatter***. Temporal output may differ as information per station or in time may vary from source to source, as of this version there are plotting tools that are ICOS specific, FLUXNET specific and more general temporal plotting. Tabular output is created from the comparison objects where statistical metrics are calculated per automation. Use ***buildSummaryTable*** to get spatial mean´s of your fields to compare and ***makeMetricTable*** to extract the statistics you want to highlight. Both of these tables are initialized in the configuration where you are able to initialize multiple tables if you need to keep the tabular output to contain different sections and benchmarks.  

```{r Difference, echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
if (do_agpp){

  # Define which benchmark to do
benchmark_name <- "AGPP" # Connect to header name of benchmark object in YAML instruction file

# Process your dataset
Datasets <- processDataSource(all_datasets = all_datasets, input, spatial.extent = spatial.extent, benchmark_name = benchmark_name)

# Build your current benchmark from input YAML file  
this_benchmark <- createBenchmark(benchmark_name, input, Datasets, spatial.extent)

# Process simulations and order together with dataset, the lists returned are needed in later functions.
field_lists <- DGVMBenchmarks::getAllFields(this_benchmark,all_simulation_Sources_list)
all_sim_full <- field_lists[[1]]
all_Fields_list <- field_lists[[2]]

## Make comparison objects from your fields, used for comparison only!
all_comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark = this_benchmark, all_maps = all_Fields_list,       new_model = all_sim_fill[[1]]@name, old_model = all_sim_full[[2]]@name)

## Spatial plotting of your comparisons, handels multiple layers to.
if(do_plots){DGVMBenchmarks::plotAllSpatialComparisons(Benchmark = this_benchmark, all_comparisons = all_comparisons)}

## Table making, extracts the spatial mean from your fields.
summary_table <- DGVMBenchmarks::buildSummaryTable(benchmark = this_benchmark, all_sim_full, summary_col_names, input = input)
## Extracts statistical metrics from your fields.
if(length(this_benchmark@datasets[[1]]) != 0){metric_table <- rbind(metric_table,
                     DGVMBenchmarks::makeMetricTable(benchmark = this_benchmark,
                                      all_comparisons_list = all_comparisons,
                                      simulation_sources = all_simulation_Sources_list))}
}
```

