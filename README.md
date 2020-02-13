# **OpenLDM**
# Introduction
 The software is called **Open land use land cover Dynamic Modeling Platform** (**OpenLDMP**).This software is developed as an extension of requirements felt for land change analysis and modeling for Indian river basins, at Indian Institute of Remote Sensing ,Dehradun (IIRS) under the ISRO Geosphere Biosphere Program(ISRO-IGBP) project.
# Installation

## Download software
 1. Clone project directory 
 ```bash
  git clone https://github.com/ashutoshkumarjha/OpenLDM.git
 ```

## Installation Dependencies
 1. Download the [anaconda](https://enterprise-docs.anaconda.com/en/latest/)
 2. Follow the [instruction](http://docs.anaconda.com/anaconda/install)  to install
 3. Create a anaconda envrionment using the command 
 ```bash
   conda env create -f environment.yml
 ```
## Testing
### Command Line 
```bash
   cd src
   R -f runSteps1a.R
 ```
 On Successfull excution predicted file (_Test-neighwithNA-allocNA-134596782-randomForest-2005-re.tif_) in _output_ direcory would be generated.There will be also some other files with suffix _SM.tif_ also representing the suitability map for each class.
### Command Line 
For command line change the runSteps1a.R based on the new data sets and modeling conditions. Make sure to enable the _data consistency check_ line.Use the following command
```bash
   cd src
   R -f runSteps1a.R
 ```
### GUI mode
```bash
   cd src
   python runLULCgui5.py 
 ```
Just follow the GUI. For any help please browse help section.

# Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.
Please make sure to update tests as appropriate.

# Authors
* Ashutosh Kumar Jha 
# Cite 
## Software
  @misc{Jha2020,
  author = {Jha, Ashutosh Kumar },
  title = {Open land use land cover Dynamic Modeling Platform (**OpenLDMP**)},
  year = {2020},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\url{https://github.com/ashutoshkumarjha/OpenLDM}},
  commit = {29b942808ea9cd371fca4a0747e1e85452e02181}
}

## Cite Paper
* Under Review
# License
[GPL](https://www.gnu.org/licenses/gpl-3.0.en.html)
