# Yield-Variability-Paper
The computational platform of this study consists of the following two steps:
## 1-	Models and Data
### Hydrologic and Agricultural Simulations:
The underlying hydrological and agricultural simulations of this study were done using the VIC-CropSyst model, which is a tightly-coupled, physically-based agro-hydrologic simulation tool. More information about VIC-CropSyst can be found in [Malek et al. (2017)](https://www.geosci-model-dev.net/10/3059/2017/). The VIC-CropSyst model will be made available upon email request to Keyvan Malek (km663@cornell.edu) and Jennifer Adam (jcadam@wsu.edu). 

The input files used to conduct VIC-CropSyst simulations can be found [here](https://figshare.com/articles/Input_and_Output_Files/6818810). These files include the global parameters file as well as the crop-related, vegetation-related, elevation/snow band, and soil input files.

### River Routing:
The routing code of Lohmann et al. (1996) was used to generate naturalized streamflow. The model can be found [here](https://rvic.readthedocs.io/en/latest/), and input information used in this simulation has been explained in [Wu et al (2012)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2012WR012313).

### Water Management Simulations:
The river system model used in this study (Yakima RiverWare: YAK-RW) was developed by the United States Bureau of Reclamation in partnership with the Washington State Department of Ecology and the HDR company. The [RiverWare](https://www.riverware.org/) model is a system dynamics platform specifically developed to simulate river systems; it is not an open source model. The input and output (proration rates) files of YAK-RW can be found [here](https://figshare.com/articles/Input_and_Output_Files/6818810).

## 2-	Post-Processing:

### First Step:
The main purpose of this post-processing code was to read VIC-CropSyst outputs and create crop response curves to water availability for each crop type in the YRB. The following script (written in R programming language) was used to make that happen. The final crop response curves and R-scripts used to generate these curves can be found here.

### Second Step:
In this step, we aggregate crop response curves over the entire Yakima River Basin.

### Third Step:
Finally, the following R-scripts were used to generate the figures that were displayed in the paper and supplemental materials.
