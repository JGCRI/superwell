[![Build Status](https://travis-ci.org/JGCRI/superwell.svg?branch=master)](https://travis-ci.org/JGCRI/superwell)

# IN DEVELOPMENT!

# superwell: A model to estimate groundwater availability and extraction costs

The `superwell` package will provide functions to calculate estimates of the global volume and unit-costs of accessible groundwater production in 235 GCAM water regions.

## Description
The `superwell` package is used to determine...

## Get Started with `superwell`
`superwell` can be installed directly from its GitHub repository using the R `devtools` package. From an R prompt, run the command:

```r
devtools::install_github('IMMM-SFA/teleconnect')
```

### Inputs
The `superwell` code requires three input files:
- wellParams.yml
- GCAM_Electrical_Rates.yml
- Inputs.csv

File *wellParams.yml* contains values of the following parameters:
- Annual operation time
- Depletion Limit
- Energy cost rate
- Interest Rate
- Maintenance factor
- Max lifetime in years
- Pump efficiency
- Specific weight
- Static head
- Well diameter
- Well Install 10
- Well Install 20
- Well Install 30
- Initial Well Yield.

File *GCAM_Electrical_Rates.yml* contains assumptions on the cost of electricity in 172 countries.

File *Inputs.csv* contains data on permeability, porosity, depth, thickness, area and the WHY class of 235 water basins.

### Output
The output file contains the following:
- Number of iteration
- Time (1-20 years)
- Unit cost
- Hydraulic conductivity
- Radial extent
- Number of wells
- Volume of water produced
- Total volume of water produced
- Total volume of water Available
- Continent
- Object ID
- Country	GCAM ID
- Basin name
