# Development Guide For DGVMBenchmarks and the Tellme scripts

### 1. Make sure you have the latest versions

There are three interacting pieces here make sure you have them all up-to-date and compatible.  In particular, it is possible that your Tellme script or DGVMBenchmarks package will depend on a feature or bugfix which is only recently implemented in DGVMTools.  So make sure:

1. You have installed the latest version of the `tellme-fixes` branches of DGVMTools.  This *might* be identical to the current master branch of DGVMTools, but maybe not.
1. You have installed the appropriate branch of `DGVMBenchmarks`.  Again this might be master, or it might be something specific to your Tellme script.
1. You are working on the correct version of your Tellme script.  Well duh.

### 2.  The structure of the YAML config file

The structure of YAML config file tries to combine a fair amount of standardisation with just enough flexibility.  The standardisation is to allow common plotting and processing functions to be re-used, so appropriate arguments must be defined in the YAML to guarantee that they work correctly.  In this case these are arguments are actually converted to formal S4 classes (referenced with `@` instead of `$`).  The flexibility is to allow full customisation.  This is implemented by the addition `custom` entry in the YAML file.  This entry will be converted to an R list (therefore indexed by `$`) and can be nested as deep as you like.  The gives potentially all the customisation you might like, and enough row to hang yourself.  As the developer of a given benchmark you are responsible for make sure that the `flex` arguments are included **and documented** in the YAML file, and that they are correctly and robustly interpreted in the benchmark code.