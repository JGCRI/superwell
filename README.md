# superwell: A model to estimate groundwater availability and extraction costs

The `superwell` package will provide functions to calculate estimates of the global volume and unit-costs of accessible groundwater production in 235 GCAM water regions. 

## Inputs
The `superwell` code requires three input files: 
⋅⋅* wellParams.yml
⋅⋅* GCAM_Electrical_Rates.yml
⋅⋅* Inputs.csv

File *wellParams.yml* contains 14 variables: 
⋅⋅* Annual operation time 
⋅⋅* Depletion Limit 
⋅⋅* Energy cost rate
⋅⋅* Interest Rate
⋅⋅* Maintenance factor
⋅⋅* Max lifetime in years
⋅⋅* Pump efficiency
⋅⋅* Specific weight
⋅⋅* Static head
⋅⋅* Well diameter
⋅⋅* Well Install 10
⋅⋅* Well Install 20
⋅⋅* Well Install 30
⋅⋅* Initial Well Yield

File *Inputs.csv* contains data on permeability, porosity, depth, thickness, area and WHY class of 235 water basins 

## Output 
The output file contains the following: 
⋅⋅* The output file contains the following columns: 
⋅⋅* Iteration	
⋅⋅* t	
⋅⋅* Unit_Cost	
⋅⋅* Hydraulic_Conductivity	
⋅⋅* Radial_Extent	
⋅⋅* Wells	
⋅⋅* Volume_Produced	
⋅⋅* Total_Volume_Produced	
⋅⋅* Total_Volume_Available	
⋅⋅* Continent	
⋅⋅* ObjId	
⋅⋅* Country	GCAMid	
⋅⋅* Basin_Name
