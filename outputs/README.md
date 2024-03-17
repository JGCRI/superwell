# Superwell Outputs

[![DOI](https://data.msdlive.org/badge/DOI/10.57931/2307832.svg)](https://doi.org/10.57931/2307832)

This folder provides simulated outputs from [`superwell`](https://github.com/JGCRI/superwell.git), a hydro-economic tool designed for the long-term assessment of groundwater cost and supply. It offers globally gridded data on extractable groundwater volumes and associated unit costs (USD/km³), driven by various user-defined depletion and ponded depth scenarios.

## Outputs Overview

Simulation outputs from [`superwell`](https://github.com/JGCRI/superwell.git) will be written to this folder in the format of `superwell_py_deep_all_0.*PD_0.*DL.csv`. These files detail the outputs of global groundwater extraction volumes and cost estimates on a 0.5° scale across user-defined scenarios.

Six standard scenarios that vary Ponded Depth (PD; 0.3 and 0.6 m) and Depletion Limit (DL; 5%, 25%, and 40% of available volume) have been provided in a data repository: [![DOI](https://data.msdlive.org/badge/DOI/10.57931/2307832.svg)](https://doi.org/10.57931/2307832). Two sample outputs include:

- `superwell_py_deep_all_0.3PD_0.25DL_sample_100.csv` presents superwell outputs for 100 sampled data points that match the distributions of global aquifer properties.
- `superwell_py_deep_all_0.3PD_0.25DL_Grid_72548.csv` provides superwell output for a single grid cell.

## Outputs Description

The output files contain various parameters, grouped as follows:

### Geographic and Aquifer Properties

- `year_number`: Year of pumping
- `depletion_limit`: Set depletion limit (DL) as a volume fraction of total available groundwater
- Geographic identifiers: `continent`, `country`, `gcam_basin_id`, `Basin_long_name`, `grid_id`
- `grid_area` (km²): Area of the grid cell
- `whyclass`: Hydrogeological classification of the aquifer
- Aquifer properties: `permeability` (m/day), `porosity` (%), `total_thickness` (m), `depth_to_water` (m)

### Hydraulic Properties and Extraction Metrics

- `orig_aqfr_sat_thickness` (m), `aqfr_sat_thickness` (m): Original and remaining/instantaneous saturated thickness of the aquifer
- `hydraulic_conductivity` (m/day), `transmissivity` (m²/day)
- `radius_of_influence` (m), `areal_extent` (km²): Well radius and area of influence from the well's center
- `number_of_wells`: Number of wells in a grid cell, determined by the ratio of well area to grid area
- `max_drawdown` (m), `drawdown` (m), `drawdown_interference` (m): Well and aquifer drawdown metrics during extraction
- `total_head` (m): Total lift for the groundwater
- `total_well_length` (m): Total depth of wells drilled
- `well_yield` (m³/day): Pumping rate or well yield

### Power and Energy Requirements

- `power` (kW), `energy` (kWh): Power and energy needed for groundwater pumping

### Volume and Cost Metrics

- `volume_produced_perwell` (m³), `cumulative_vol_produced_perwell` (m³): Production volume metrics per well
- `volume_produced_allwells` (m³), `cumulative_vol_produced_allwells` (m³): Total extraction volumes for all wells in a grid cell
- `available_volume` (m³): Available groundwater volume in storage for the grid cell
- `depleted_vol_fraction`: Fraction of total volume pumped over available volumes in a grid cell
- Cost-related outputs: `well_installation_cost` (USD), `annual_capital_cost`, `maintenance_cost`, `nonenergy_cost` (USD)
- `energy_cost_rate` (USD/kWh): Electricity rate
- `energy_cost` (USD): Energy cost for groundwater pumping
- Total cost metrics: `total_cost_perwell` (USD), `total_cost_allwells` (USD)
- `unit_cost` (USD/m³), `unit_cost_per_km3` (USD/km³), `unit_cost_per_acreft` (USD/acre-ft): Costs for pumping a unit volume of groundwater

## Cite Outputs

> _Cite output data as:_ \
Niazi, H., Ferencz, S., Yoon, J., Graham, N., Wild, T., Hejazi, M., Watson, D., & Vernon, C. (2024). [Globally Gridded Groundwater Extraction Volumes and Costs under Six Depletion and Ponded Depth Targets](https://doi.org/10.57931/2307832). MSD-LIVE Data Repository. <https://doi.org/10.57931/2307832> \
[![DOI](https://data.msdlive.org/badge/DOI/10.57931/2307832.svg)](https://doi.org/10.57931/2307832)

### Model documentation

> Niazi, H., Ferencz, S., Graham, N., Yoon, J., Wild, T., Hejazi, M., Watson, D., & Vernon, C. (2024; In-prep). [Long-term Hydro-economic Analysis Tool for Evaluating Global Groundwater Cost and Supply: Superwell v1.0](https://gmd.copernicus.org/preprints/). _Geoscientific Model Development_.
