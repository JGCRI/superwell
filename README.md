# superwell: A model to estimate groundwater availability and extraction costs

The `superwell` package will provide functions to calculate estimates of the global volume and unit-costs of accessible groundwater production in 235 GCAM water regions. 

### Inputs
The `superwell` code requires three input files: 
- wellParams.yml
- GCAM_Electrical_Rates.yml
- Inputs.csv

File *wellParams.yml* contains values of the following parameters: 
- Depletion limit - 5%, 25% and 40% from the initial volume in storage (the only parameter we change in this file)

- Annual operation time (s/year)
- Energy cost ($/KWh)
- Interest rate ($/year)
- Maintenance factor 
- Max lifetime (years)
- Pump efficiency (%)
- Specific weight ((kg/m3 * m/s^2))
- Static head (m)
- Well diameter (m)
- Well install 10 ($/m)
- Well install 20 ($/m)
- Well install 30 ($/m)
- Initial well yield (m^3/s).

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
- Total volume of water available	
- Continent	
- Object ID	
- Country	GCAM ID	
- Basin name
