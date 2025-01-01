# Superwell Inputs

[![DOI](https://data.msdlive.org/badge/DOI/10.57931/2484226.svg)](https://doi.org/10.57931/2484226)

The inputs for [`superwell`](https://github.com/JGCRI/superwell.git) primarily  comprises of aquifer properties on a 0.5° scale, including depth to groundwater, aquifer thickness, WHYMap aquifer classes, recharge rates, porosity, and permeability. These parameters have been digitized and geo-processed from sources like Fan et al. (2013), de Graaf et al. (2015), Richts et al. (2011),  Döll and Fiedler (2008), Gleeson et al., (2016), Messager et al., (2016), and Gleeson et al. (2014).

Gridded aquifer properties can be utilized independently to estimate global groundwater availability or as inputs for the [`superwell`](https://github.com/JGCRI/superwell.git) model. [`superwell`](https://github.com/JGCRI/superwell.git) simulates groundwater extraction, and estimates global extractable volumes and unit costs ($/km³) under various user-defined scenarios.

The [`inputs/`](./inputs/) folder is structured as follows:

| File                                            | Description                                           |
|-------------------------------------------------|-------------------------------------------------------|
| [`shapefiles/`](./shapefiles/)            | Spatial preprocessing for aquifer properties          |
| [GCAM_Electricity_Rates.csv](./GCAM_Electricity_Rates.csv) | Electricity cost assumptions in 172 countries         |
| [GW_cost_model_comparison_inputs.csv](./GW_cost_model_comparison_inputs.csv) | Input aquifer properties for model diagnostics        |
| [Theis_well_function_table.csv](./Theis_well_function_table.csv) | Lookup table for well function relation               |
| [basin_to_country_mapping.csv](./basin_to_country_mapping.csv) | Mapping between water basins and countries            |
| [continent_county_mapping.csv](./continent_county_mapping.csv) | Mapping between continents and countries              |
| [**inputs_recharge_lakes.csv**](./inputs_recharge_lakes.csv)                    | **Gridded aquifer properties**                            |
| [**params.csv**](./params.csv)                    | **Model settings and scenario assumptions**              |
| [prep_inputs.R](./prep_inputs.R)              | Script to generate `inputs` file from geo-processed [`shapefiles/`](./shapefiles/)          |
| [prep_inputs_recharge_lakes.R](./prep_inputs_recharge_lakes.R)              | Script to generate `inputs_recharge_lakes` file (with recharge and lakes) from geo-processed [`shapefiles/`](./shapefiles/)          |
| [sample_inputs.py](./sample_inputs.py)        | Script to sample from `inputs` dataset    |
| [sampled_data_100.csv](./sampled_data_100.csv) | Sampled set of aquifer properties for diagnostics     |
| [sampled_data_100.png](./sampled_data_100.png) | Distribution comparison of sampled data               |

## Model Settings and Scenario Assumptions  

[params.csv](./params.csv) defines model settings and scenario assumptions and contains values of the following parameters:

| Parameter                    | Description                              | Units               |
|------------------------------ |------------------------------------------|---------------------|
| `Country_filter`             | specify country filter if running for one country                           | N/A                 |
| `Basin_filter`            | specify basin name to run all cells within one basin. See `Basin_long_name` column in [inputs_recharge_lakes.csv](./inputs_recharge_lakes.csv)                         | N/A
| `Gridcell_filter`            | specify grid cell number to run one grid cell. See `GridCellID` column in [inputs_recharge_lakes.csv](./inputs_recharge_lakes.csv)                         | N/A                 |
| `Total_Simulation_Years`     | constrain total pumping lifetime                   | years               |
| `Pumping_Days`               | change pumping days per year                    | days/year           |
| `Depletion_Limit`               | Aquifer volume depletion limit                          | -              |
| `Recharge_Flag`               | Turn on/off recharge effects (default: 1 = on)                         | N/A              |
| `Shallow_Recharge_Ratio`               | part of recharge reducing pumping requirement (default: 0.2)                         | N/A               |
| `Shallow_Recharge_Threshold`               | part of ponded depth after which shallow recharge starts contributing to deep aquifer recharge (default: 0.75)                         | N/A               |
| `Specific_weight`            | Specific weight of water                    |  kg/(m²·s²)          |
| `Static_head`                | Static head                               | meters              |
| `Max_Initial_Sat_Thickness`  | Maximum initial saturated thickness     | meters              |
| `Max_Lifetime_in_Years`      | Maximum lifetime in years                | years               |
| `Well_Diameter`              | Well diameter                            | meters              |
| `Well_Yield`                 | Well yield                               | m³/s                |
| `Pump_Efficiency`            | Pump efficiency                          | unitless            |
| `Energy_cost_rate`           | Energy cost rate                         | $/KWh               |
| `Interest_Rate`              | Interest rate                            | unitless            |
| `Maintenance_factor`         | Maintenance factor                       | unitless            |
| `Well_Install_10`            | Installation cost for well depth of 10 m | $/m                 |
| `Well_Install_20`            | Installation cost for well depth of 20 m | $/m                 |
| `Well_Install_30`            | Installation cost for well depth of 30 m | $/m                 |

### Create Scenarios

- `Depletion_Limit` and `Ponded_Depth` could be altered to create a scenario for a pumping regime.
- `Country_filter` and `Gridcell_filter` could be changed to run for specific *spatial* delineation.
- `Total_Simulation_Years` and `Pumping_Days` could be changed to alter *temporal* extent or yearly pumping duration of groundwater extraction.
- Change `Recharge_Flag` to 0 to turn off recharge effects or alter `Shallow_Recharge_Ratio` and `Shallow_Recharge_Threshold` to change the recharge effects.
- All other variables in [params.csv](./params.csv) could also be varied to create alternative scenario assumptions.

## Aquifer Properties  

File [inputs_recharge_lakes.csv](./inputs_recharge_lakes.csv) contains data on permeability, porosity, depth to groundwater, total aquifer thickness, grid area, lake area, recharge rages, and the WHY class of 235 water basins [![DOI](https://data.msdlive.org/badge/DOI/10.57931/2484226.svg)](https://doi.org/10.57931/2484226).

| Variable           | Description                                                 |
|--------------------|------------------------------------------------------------|
| `GridCellID`       | Unique identifier for each (roughly 0.5°) grid cell         |
| `Continent`        | Continent name                                             |
| `Country`          | Country name                                               |
| `GCAM_basin_ID`    | Identifier for GCAM hydrologic basin                        |
| `Basin_long_name`  | Full name of the basin                                      |
| `WHYClass`         | Hydrogeologic classification based on WHYMap aquifer classes (Richts et al., 2011) |
| `Porosity`         | Soil porosity (%) (Gleeson et al., 2014)                    |
| `Permeability`     | Soil permeability (in square meters; Gleeson et al., 2014)  |
| `Aquifer_thickness` | Thickness of the aquifer (in meters; de Graaf et al., 2015) |
| `Depth_to_water`   | Depth to groundwater (in meters; Fan et al., 2013)          |
| `Grid_area`        | Area of the grid cell (in square meters)                    |
| `Lake_area`        | Area of the grid cell (in square meters)                    |
|                    | |

## Cite Inputs  

> *Cite input data as:* \
Niazi, H., Watson, D., Hejazi, M., Yonkofski, C., Ferencz, S., Vernon, C., Graham, N., Wild, T., & Yoon, J. (2024). [Global Geo-processed Data of Aquifer Properties by 0.5° Grid, Country and Water Basins](https://doi.org/10.57931/2484226). MultiSector Dynamics-Living, Intuitive, Value-adding, Environment. <https://doi.org/10.57931/2484226> \
[![DOI](https://data.msdlive.org/badge/DOI/10.57931/2484226.svg)](https://doi.org/10.57931/2484226)

### Model documentation

- Niazi, H., Ferencz, S., Graham, N., Yoon, J., Wild, T., Hejazi, M., Watson, D., & Vernon, C. (2025). [Long-term Hydro-economic Analysis Tool for Evaluating Global Groundwater Cost and Supply: Superwell v1.0](https://doi.org/10.5194/egusphere-2024-799). *Geoscientific Model Development*.

## References

- Fan, Y., Li, H., & Miguez-Macho, G. (2013). Global Patterns of Groundwater Table Depth. Science, 339(6122), 940-943. <https://doi.org/10.1126/science.1229881>
- de Graaf, I. E. M., Sutanudjaja, E. H., van Beek, L. P. H., & Bierkens, M. F. P. (2015). A high-resolution global-scale groundwater model. Hydrol. Earth Syst. Sci., 19(2), 823-837. <https://doi.org/10.5194/hess-19-823-2015>
- Gleeson, T., Moosdorf, N., Hartmann, J., & van Beek, L. P. H. (2014). A glimpse beneath earth's surface: GLobal HYdrogeology MaPS (GLHYMPS) of permeability and porosity. Geophysical Research Letters, 41(11), 3891-3898. <https://doi.org/10.1002/2014GL059856>
- Richts, A., Struckmeier, W. F., & Zaepke, M. (2011). WHYMAP and the Groundwater Resources Map of the World 1:25,000,000. In J. A. A. Jones (Ed.), Sustaining Groundwater Resources: A Critical Element in the Global Water Crisis (pp. 159-173). Springer Netherlands. <https://doi.org/10.1007/978-90-481-3426-7_10>
- Messager, M. L., Lehner, B., Grill, G., Nedeva, I., & Schmitt, O. (2016). Estimating the volume and age of water stored in global lakes using a geo-statistical approach. Nature Communications, 7(1), 13603. https://doi.org/10.1038/ncomms13603
- Döll, P., & Fiedler, K. (2008). Global-scale modeling of groundwater recharge. Hydrol. Earth Syst. Sci., 12(3), 863-885. https://doi.org/10.5194/hess-12-863-2008
- Gleeson, T., Befus, K. M., Jasechko, S., Luijendijk, E., & Cardenas, M. B. (2016). The global volume and distribution of modern groundwater. Nature Geoscience, 9(2), 161-167. https://doi.org/10.1038/ngeo2590 


