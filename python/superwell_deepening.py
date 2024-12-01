# -*- coding: utf-8 -*-

# main superwell script with deepening and well replacement

# load libraries
import numpy as np
import pandas as pd
import math
import os

# TODO: create an outer scenario loop, or make this whole file a function and call it using a batch run script,
#  read the list of scenarios to be run from a file, create a directory for the scenario, copy the modified
#  params file, write outputs and save a few key plots e.g., a summary diagnostic plot and maps (volume, unit cost etc)

# Determine the relative path to the inputs directory
inputs_dir = '../inputs' if os.path.exists('../inputs') else 'inputs'

# TODO: can make this a function that reads the inputs and params files and returns the dataframes
# Reading CSV files with the determined base directory
grid_df = pd.read_csv(os.path.join(inputs_dir, 'inputs_recharge_lakes.csv'))
params = pd.read_csv(os.path.join(inputs_dir, 'params.csv'), index_col=0)
electricity_rates = pd.read_csv(os.path.join(inputs_dir, 'GCAM_Electricity_Rates.csv'), index_col=0, header=None)
W_lookup = pd.read_csv(os.path.join(inputs_dir, 'Theis_well_function_table.csv'), header="infer")
lookup_idx = pd.Index(W_lookup.W)

# define constants
MAX_INITIAL_SAT_THICKNESS = float(params.Val['Max_Initial_Sat_Thickness']) # maximum initial saturated thickness
DEEPENING_INCREMENT = float(params.Val['Well_Deepening_Increment']) # increment for deepening well (m)

DEPLETION_LIMIT = float(params.Val['Depletion_Limit'])  # depletion limit for this scenario
PONDED_DEPTH_TARGET = float(params.Val['Ponded_Depth'])  # annual ponded/irrigation depth target [m]

RECHARGE_FLAG = int(params.Val['Recharge_Flag'])  # decide if recharge is considered in the model (value zero or 1)
SHALLOW_RECHARGE_RATIO = float(params.Val['Shallow_Recharge_Ratio'])  # part of recharge reducing pumping requirement [-]
SHALLOW_RECHARGE_THRESHOLD = float(params.Val['Shallow_Recharge_Threshold'])  # % of ponded depth after which shallow
# recharge starts contributing to deep aquifer recharge [-]

SPECIFIC_WEIGHT = float(params.Val['Specific_weight'])  # specific weight of water
EFFICIENCY = float(params.Val['Pump_Efficiency'])  # well efficiency
ADJACENT_WELLS = int(params.Val['Adjacent_Wells'])  # number of adjacent wells to consider for drawdown interference

WELL_LIFETIME = float(params.Val['Max_Lifetime_in_Years'])
DEFAULT_ELECTRICITY_RATE = float(params.Val['Energy_cost_rate'])  # default electricity rate
INTEREST_RATE = float(params.Val['Interest_Rate'])
MAINTENANCE_RATE = float(params.Val['Maintenance_factor'])

COUNTRY_FILTER = params.Val['Country_filter']  # filter by the country
BASIN_FILTER = params.Val['Basin_filter']  # filter by the basin
GRIDCELL_FILTER = params.Val['Gridcell_filter']  # filter by the grid cell ID

# time constants 
NUM_YEARS = int(params.Val['Total_Simulation_Years'])  # maximum years of pumping
DAYS = int(params.Val['Pumping_Days'])  # days pumping per year
HOURS_IN_DAY = 24  # hours in a day
SECS_IN_DAY = 86400  # seconds in a day

# convert electricity rate dictionary
electricity_rate_dict = {}
for i in range(len(electricity_rates.iloc[:, 0])):
    country = electricity_rates.index[i]
    electricity_rate_dict[country.rstrip()] = electricity_rates.iloc[i, 0]

# TODO: ship the ancillary stuff like these filters or the headers for the output file to a separate file. The goal
#  should for this file to have main logic
# filter by basin, country and/or grid cell, change filters in params.csv
if COUNTRY_FILTER == 'all' and GRIDCELL_FILTER == 'all' and BASIN_FILTER == 'all':  # run global
    selected_grid_df = grid_df
    print('Run Global with ' + str(len(selected_grid_df)) + ' cells! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
elif COUNTRY_FILTER != 'all' and GRIDCELL_FILTER == 'all' and BASIN_FILTER == 'all':  # run country
    selected_grid_df = grid_df[grid_df['Country'] == COUNTRY_FILTER].reset_index(drop=True)
    print('Run Country with ' + str(len(selected_grid_df)) + ' cells! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
elif COUNTRY_FILTER == 'all' and GRIDCELL_FILTER == 'all' and BASIN_FILTER != 'all':  # run basin
    selected_grid_df = grid_df[grid_df['Basin_long_name'] == BASIN_FILTER].reset_index(drop=True)
    print('Run Basin with ' + str(len(selected_grid_df)) + ' cells! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
elif COUNTRY_FILTER == 'all' and GRIDCELL_FILTER != 'all' and BASIN_FILTER == 'all':  # run grid
    selected_grid_df = grid_df[grid_df['GridCellID'] == int(GRIDCELL_FILTER)].reset_index(drop=True)
    print('Run Grid with ' + str(len(selected_grid_df)) + ' cell! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
elif COUNTRY_FILTER != 'all' and GRIDCELL_FILTER == 'all' and BASIN_FILTER != 'all':  # run country and basin
    selected_grid_df = grid_df[(grid_df['Country'] == COUNTRY_FILTER) & (grid_df['Basin_long_name'] == BASIN_FILTER)].reset_index(drop=True)
    print('Run Country and Basin with ' + str(len(selected_grid_df)) + ' cells! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
elif COUNTRY_FILTER != 'all' and GRIDCELL_FILTER != 'all' and BASIN_FILTER == 'all':  # run country and grid cell
    selected_grid_df = grid_df[(grid_df['Country'] == COUNTRY_FILTER) & (grid_df['GridCellID'] == int(GRIDCELL_FILTER))].reset_index(drop=True)
    print('Run Country and Grid with ' + str(len(selected_grid_df)) + ' cells! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
elif COUNTRY_FILTER == 'all' and GRIDCELL_FILTER != 'all' and BASIN_FILTER != 'all':  # run basin and grid cell
    selected_grid_df = grid_df[(grid_df['Basin_long_name'] == BASIN_FILTER) & (grid_df['GridCellID'] == int(GRIDCELL_FILTER))].reset_index(drop=True)
    print('Run Basin and Grid with ' + str(len(selected_grid_df)) + ' cells! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)
else:
    selected_grid_df = grid_df[(grid_df['Country'] == COUNTRY_FILTER) & (grid_df['Basin_long_name'] == BASIN_FILTER) & (grid_df['GridCellID'] == int(GRIDCELL_FILTER))].reset_index(drop=True) # run basin, country and grid cell
    print('Run Basin, Country and Grid with ' + str(len(selected_grid_df)) + ' cell! Country: ' + COUNTRY_FILTER + (' Basin: ') + BASIN_FILTER + ' Grid: ' + GRIDCELL_FILTER)

# if selected_grid_df is empty, print error message that combination is not valid
if selected_grid_df.empty:
    print('ERROR: Filter combination is not valid. Please check the params.csv file to make sure the filter combinations are logical.')
    exit()

# define outputs file name
output_path = '../outputs' if os.path.exists('../outputs') else 'outputs'
output_name = ('superwell_py_deep_C_' + str.replace(COUNTRY_FILTER, ' ', '') + '_B_' +
               str.replace(BASIN_FILTER, ' ', '')  + '_G_' + str(GRIDCELL_FILTER) + '_' +
               str(PONDED_DEPTH_TARGET) + 'PD_' + str(DEPLETION_LIMIT) + 'DL_' + str(SHALLOW_RECHARGE_RATIO) + 'RR')

# header for output file
header_column_names = 'year_number,depletion_limit,continent,country,' \
                      'gcam_basin_id,Basin_long_name,grid_id,grid_area,permeability,porosity,' \
                      'total_thickness,depth_to_water,orig_aqfr_sat_thickness,aqfr_sat_thickness,' \
                      'hydraulic_conductivity,transmissivity,radius_of_influence,areal_extent,' \
                      'drawdown,drawdown_interference,total_head,well_yield,' \
                      'recharge,shallow_recharge_depth,threshold_depth,net_ponded_depth_target,' \
                      'excessive_recharge_depth,deep_recharge_vol,volume_deep_recharge,volume_produced_perwell,' \
                      'cumulative_vol_produced_perwell,number_of_wells,volume_produced_allwells,' \
                      'cumulative_vol_produced_allwells,available_volume,depleted_vol_fraction,' \
                      'well_installation_cost, annual_capital_cost,maintenance_cost,nonenergy_cost,' \
                      'power,energy,energy_cost_rate,energy_cost,total_cost_perwell,total_cost_allwells,' \
                      'unit_cost,unit_cost_per_km3,unit_cost_per_acreft,whyclass,total_well_length'

# write header to the output file
file = open(output_path + '/' + output_name + '.csv', 'w')
file.write(str(header_column_names))
file.write('\n')
file.close()

# TODO: we don't have max depletion ratio now, how do we calculate max drawdown now? # (originally it was supposed to
#  be max_drawdown = Max_Depletion * Original Aquifer Thickness ). It could be max_s_frac * selected_grid_df.Aquifer_thickness
#  (or the array tracking aquifer thickness sat_thickness_array?)

# define Theis function
def drawdown_theis(time, r, S, T, Q):
    """Theis solution for drawdown in a confined aquifer
    :param time:        time duration in seconds
    :param r:           distance from well in meters
    :param S:           storativity in [-]
    :param T:           transmissivity in m^2/s
    :param Q:           pumping rate in m^3/s
    :return:            drawdown (s) in meters
    """
    # calculate the well function argument (u)
    u = r ** 2 * S / (4 * T * time)

    # for large u values, W will be insignificant and drawdown (s) will ~= 0
    if u > 5.9:
        W = 0

    # use W(u) lookup table for intermediate values where approximation is insufficient
    elif 5.9 > u and u > .6:
        lookup_idx = pd.Index(W_lookup.u)
        lookup_loc = lookup_idx.get_indexer([u], method='nearest')
        W_index = lookup_loc[0] if lookup_loc[0] != -1 else np.argmin(np.abs(lookup_idx.to_series() - u))
        W = W_lookup.W[W_index]

    # use approximation for small u values
    else:
        W = -0.57721 - math.log(u) + u - u ** 2 / (2 * 2)

    # calculate drawdown (s)
    s = W * Q / (4 * 3.1416 * T)

    return (s)


# candidate well pumping rates (gallons per minute)
Q_array_gpm = [10, 20, 30, 40, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000, 1200, 1300, 1400, 1500]

# Convert candidate pumping rates to m^3/s
Q_array = np.array(Q_array_gpm) / (60 * 264.17)

# Notify user of the start of the simulation
print("Preamble complete. Beginning the simulation...")

# initialize counter to determine skipped cells due to screening criteria 
skipped_cells = 0

# %% superwell code block
for grid_cell in range(len(selected_grid_df.iloc[:, 0])):

    # TODO: can we make this progress printing a function?
    # print simulation progress
    total_cells = len(selected_grid_df.iloc[:, 0])
    progress_step_size = max(1, int(total_cells / 1000))  # change 1 to a bigger number if the progress bar is too fast

    # Print simulation progress
    if grid_cell == 0: # check if grid_cell is the first element
        print('Percent complete = ' + str(np.round(100 * grid_cell / total_cells, 1)) +
              ' | Processing Cell # ' + str(selected_grid_df.GridCellID[grid_cell]) + ' in '
              + str(selected_grid_df.Country[grid_cell]))
    elif (GRIDCELL_FILTER == 'all' and grid_cell % progress_step_size == 0) or (
            selected_grid_df.Country[grid_cell] != selected_grid_df.Country[grid_cell - 1]):
        print('Percent complete = ' + str(np.round(100 * grid_cell / total_cells, 1)) +
              ' | Processing Cell # ' + str(selected_grid_df.GridCellID[grid_cell]) + ' in '
              + str(selected_grid_df.Country[grid_cell]))


    ################ determine if grid cell is skipped ########################

    # skip grid areas less than 5x5 km (1% of a normal 50x50 grid size)
    if selected_grid_df.Grid_area[grid_cell] < 5 * 5 * (10 ** 6):
        skipped_cells += 1
        continue

    # skip if the lake covers more than 90% of the grid, otherwise we use the dry area (grid area - lake area) for calcs
    if selected_grid_df.Lake_area[grid_cell] / selected_grid_df.Grid_area[grid_cell] < 0.9:
        # selected_grid_df.Grid_area[grid_cell] = selected_grid_df.Grid_area[grid_cell] - selected_grid_df.Lake_area[grid_cell]
        selected_grid_df.loc[grid_cell, "Grid_area"] -= selected_grid_df.loc[grid_cell, "Lake_area"]
    else:
        skipped_cells += 1
        continue

    # depth to water table should we at least 1 meter
    if selected_grid_df.Depth_to_water[grid_cell] < 1:
        skipped_cells += 1
        continue

    # TODO: reevaluate if we need the permeability and porosity screening criteria. We should not overfilter the data
    #  given that model dynamics have significantly improved since the original implementation
    # limit low permeability values
    if selected_grid_df.Permeability[grid_cell] < -15:
        skipped_cells += 1
        continue

    # limit porosity to 5% voids at least
    if selected_grid_df.Porosity[grid_cell] < 0.05:
        skipped_cells += 1
        continue

    # correct aquifer thickness outliers, replace >1000m thickness with 1000m
    selected_grid_df.loc[selected_grid_df['Aquifer_thickness'] > 1000, 'Aquifer_thickness'] = 1000 # to avoid warning

    # skip grid cells where the depth is greater than thickness (negative transmissivity)
    if selected_grid_df.Aquifer_thickness[grid_cell] < selected_grid_df.Depth_to_water[grid_cell]:
        skipped_cells += 1
        continue


    ################ store grid cell attributes for output ####################

    # get the country name from selected_grid_df and check if the country is in electricity_rate_dict
    if str(selected_grid_df.Country[grid_cell]) in electricity_rate_dict:
        ELECTRICITY_RATE = electricity_rate_dict[str(selected_grid_df.Country[grid_cell])]
    else:
        ELECTRICITY_RATE = DEFAULT_ELECTRICITY_RATE  # give default electricity rate if missing cost data

    # extract total aquifer thickness from input data (m) and grid cell area (m^2)
    total_thickness = selected_grid_df.Aquifer_thickness[grid_cell]  # m
    grid_cell_area = selected_grid_df.Grid_area[grid_cell]

    # depth to water
    DTW_array = np.zeros(NUM_YEARS)  # tracks depth to water for each year
    DTW_array[0] = selected_grid_df.Depth_to_water[grid_cell]  # initial depth to water

    # saturated thickness: total aquifer thickness minus depth to piezometric surface
    initial_sat_thickness = selected_grid_df.Aquifer_thickness[grid_cell] - selected_grid_df.Depth_to_water[grid_cell]  # m

    sat_thickness_array = np.zeros(NUM_YEARS)
    well_length_array = np.zeros(NUM_YEARS)

    # adjust saturated thickness and well length for gradual deepening 
    if initial_sat_thickness > MAX_INITIAL_SAT_THICKNESS:
        sat_thickness_array[0] = MAX_INITIAL_SAT_THICKNESS  # m
        well_length_array[0] = sat_thickness_array[0] + DTW_array[0]  # m
    else:
        sat_thickness_array[0] = initial_sat_thickness  # m
        well_length_array[0] = total_thickness  # m

    # available volume (V=nAh) for the grid cell (m^3)
    available_volume = initial_sat_thickness * grid_cell_area * selected_grid_df.Porosity[grid_cell]

    # aquifer properties for Theis 
    S = selected_grid_df.Porosity[grid_cell]    # storativity [-] (same as porosity for confined aquifer)
    K = 10 ** selected_grid_df.Permeability[grid_cell] * 1e7  # hydraulic conductivity (m/s)
    T = K * sat_thickness_array[0]  # transmissivity (m^2/s)

    T_array = np.zeros(NUM_YEARS)  # tracks T for each year
    T_array[0] = T  # initial T


    #################### determine initial well Q #############################
    # Q refers to pumping rate or well yield (m^3/s) 

    # time and well radius for Theis solution
    time_Q = 2 * 365 * SECS_IN_DAY  # time period used for determining initial well Q (2 years in seconds)
    well_r = 0.5 * float(params.Val['Well_Diameter']) # well radius (m)

    # initialize drawdown and viable Q arrays
    s_array = np.zeros(len(Q_array))
    Q_viability = np.zeros(len(Q_array))

    # drawdown at t = 2 years for all candidate well Qs (m)
    for i, Q in enumerate(Q_array):
        s_array[i] = drawdown_theis(time_Q, well_r, S, T, Q)

    # drawdown criteria 
    max_s_frac = .40        # relative max drawdown as % of saturated thickness
    max_s_absolute = 80     # absolute max drawdown in m

    # find largest Q that meets drawdown criteria
    for i, s in enumerate(s_array):
        if s / initial_sat_thickness < max_s_frac and s < max_s_absolute:
            Q_viability[i] = 1

    # skip grid cell if no pumping rates are viable
    if np.sum(Q_viability) == 0:
        continue

    # find index and the value of the largest viable Q
    initial_Q_indx_arr = np.where(Q_viability == 1)
    initial_Q_indx = np.max(initial_Q_indx_arr[:])  # index of largest viable Q
    initial_Q = Q_array[initial_Q_indx]

    # initialize well Q array for each year and assign the Q for the first year
    Well_Q_array = np.zeros(NUM_YEARS)
    Well_Q_array[0] = initial_Q


    ################# recharge calculations and adjustments ###################

    recharge = selected_grid_df.Recharge[grid_cell]  # recharge rate (m/year)

    # turn recharge effects on and off
    if RECHARGE_FLAG == 0:
        NET_PONDED_DEPTH_TARGET = PONDED_DEPTH_TARGET  # no change in pumping target
        deep_recharge_vol = 0  # no change in depth to water
    else:
        # part of recharge that goes to shallow subsurface recharge and reduces pumping requirement
        shallow_recharge_depth = SHALLOW_RECHARGE_RATIO * recharge  # (m/year)

        # part of ponded depth the shallow recharge should reduce up to (m)
        threshold_depth = SHALLOW_RECHARGE_THRESHOLD * PONDED_DEPTH_TARGET

        # reduce ponded depth target with shallow recharge until threshold_depth is reached
        if shallow_recharge_depth <= threshold_depth:
            NET_PONDED_DEPTH_TARGET = PONDED_DEPTH_TARGET - shallow_recharge_depth
            excessive_recharge_depth = 0  # no excess recharge to go to deep aquifer
        else:
            # this actually never gets triggered in current settings of 0.2 ratio, 0.75 threshold. Max recharge 0.95, max shallow 0.19, threshold 0.225
            if threshold_depth == PONDED_DEPTH_TARGET:
                NET_PONDED_DEPTH_TARGET = 0.2 * PONDED_DEPTH_TARGET  # ensure 20% ponded depth is maintained
                # extra shallow recharge (a-b) plus the left over from maintaining 20% ponded depth (c)
                excessive_recharge_depth = shallow_recharge_depth - threshold_depth + NET_PONDED_DEPTH_TARGET
            else:
                NET_PONDED_DEPTH_TARGET = PONDED_DEPTH_TARGET - threshold_depth
                # excess shallow recharge beyond what is allowed to reduce PONDED_DEPTH_TARGET
                excessive_recharge_depth = shallow_recharge_depth - threshold_depth

        # part of recharge that contributes to an increase in deep subsurface volume
        # (contains partitioned deep recharge and leftover shallow recharge)
        deep_recharge_vol = ((1 - SHALLOW_RECHARGE_RATIO) * recharge + excessive_recharge_depth) * grid_cell_area


    ################## determine initial well area and roi #####################

    initial_well_area = initial_Q * DAYS * SECS_IN_DAY / NET_PONDED_DEPTH_TARGET  # initial well area A = Qt/d = V/d (m^2)
    initial_roi = (initial_well_area / math.pi) ** 0.5  # initial radius of influence (m)
    initial_num_wells = selected_grid_df.Grid_area[grid_cell] / initial_well_area # initial number of wells

    well_roi_array = np.zeros(NUM_YEARS)  # tracks roi for each year
    well_roi_array[0] = initial_roi

    well_area_array = np.zeros(NUM_YEARS)  # tracks well area for each year
    well_area_array[0] = initial_well_area

    # make sure depleted volume fraction does not exceed depletion limit
    # check for first year: ratio of estimated volume pumped in 1st year to available volume should be less than depletion limit
    # same as: initial_Q * DAYS * SECS_IN_DAY * initial_num_wells / available_volume > DEPLETION_LIMIT:
    if (((selected_grid_df.Grid_area[grid_cell] / well_area_array[0]) * Well_Q_array[0] * SECS_IN_DAY * DAYS) /
            available_volume > DEPLETION_LIMIT):
        continue


    ####################### annual pumping simulation loop ####################

    depleted_volume_fraction = np.zeros(NUM_YEARS)  # tracks depleted volume fraction for each year
    volume_all_wells = np.zeros(NUM_YEARS)  # tracks volume produced by all wells for each year
    volume_recharged = np.zeros(NUM_YEARS)  # tracks volume recharged for each year

    # simulate pumping for each year
    for year in range(NUM_YEARS):

        # make sure depleted volume fraction does not exceed depletion limit
        # check for last year: seize pumping if pumping for one more year would hit the depletion limit
        if depleted_volume_fraction[year - 1] + (volume_all_wells[year - 1] / available_volume) > DEPLETION_LIMIT:
            year = year - 1
            break

        # test viability pumping for current year 
        # initialize viability variables 
        s_theis = 0
        s_theis_interference = 0

        # simulate drawdown at t = 100 days of pumping
        s_theis = drawdown_theis(DAYS * SECS_IN_DAY, well_r, S, T_array[year], Well_Q_array[year])

        # account for interference from neighboring wells (drawdown at the edge of the well's radius of influence)
        s_theis_interference = drawdown_theis(DAYS * SECS_IN_DAY, well_roi_array[year] * 2, S, T_array[year], Well_Q_array[year])

        # total drawdown (well own drawdown + interference from neighboring wells)
        s_total = s_theis + ADJACENT_WELLS * s_theis_interference

        # check if drawdown constraints are violated by end of 100 day pumping period
        # if constraints violated: (1) first deepen well, (2) then reduce well pumping rate 
        if s_total > max_s_absolute or s_total / sat_thickness_array[year] > max_s_frac:

            # 1) first preference deepen well 
            if well_length_array[year] < total_thickness: # if well can be deepened
                # update well length if thickness allows to deepen by the increment 
                if well_length_array[year] + DEEPENING_INCREMENT < total_thickness:
                    well_length_array[year] = DEEPENING_INCREMENT + well_length_array[year]  # deepening well by 50m

                else: # if well cannot be deepened, update well length to total thickness
                    well_length_array[year] = total_thickness

                # update (increase) saturated thickness and T 
                sat_thickness_array[year] = well_length_array[year] - DTW_array[year]

                # deepening increases transmissivity which results in less drawdown for the same pumping rate
                T_array[year] = sat_thickness_array[year] * K

            # 2) once well cannot be deepened, reduce well pumping rate 
            else:
                # reinitialize drawdown to recalculate drawdown using values of the current year (e.g., T might have changed)
                s_array = np.zeros(len(Q_array))
                for i, Q in enumerate(Q_array):
                    s_array[i] = drawdown_theis(time_Q, well_r, S, T_array[year], Q)

                Q_viability = np.zeros(len(Q_array))

                # viable Qs for the current year
                for i, s in enumerate(s_array):
                    if s / sat_thickness_array[year] < max_s_frac and s < max_s_absolute:
                        Q_viability[i] = 1

                # exit pumping code block if no pumping rates are viable
                if np.sum(Q_viability) == 0:
                    break

                # find index and the value of the largest viable Q
                Q_indx_arr = np.where(Q_viability == 1)
                Q_indx = np.max(Q_indx_arr[:])  # index of largest viable Q
                new_Q = Q_array[Q_indx]  # new Q
                Well_Q_array[year] = new_Q  # update Q for current YEAR

                # update well area and roi for current year using new Q for current year
                well_area_array[year] = Well_Q_array[year] * DAYS * SECS_IN_DAY / NET_PONDED_DEPTH_TARGET
                well_roi = (well_area_array[year] / math.pi) ** 0.5
                well_roi_array[year] = initial_roi

        # if drawdown constraints are NOT violated, proceed to calculate output for pumping year  
        # simulate 100 days of pumping, with drawdown calculated every 10 days
        # TODO: make sure 10 day increment is robust to non-multiples of 10 (could be a constant or a parameter)
        s_theis_ts = np.zeros(int(DAYS / 10))
        s_theis_interference_ts = np.zeros(int(DAYS / 10))

        # calculate drawdown at every 10th day of pumping (e.g., t = 10, 20, 30, ... 100 days of pumping)
        for day in range(int(DAYS / 10)):
            s_theis_ts[day] = drawdown_theis((day + 1) * 10 * SECS_IN_DAY, well_r, S, T_array[year], Well_Q_array[year])
            s_theis_interference_ts[day] = drawdown_theis((day + 1) * 10 * SECS_IN_DAY, well_roi_array[year] * 2, S, T_array[year], Well_Q_array[year])

        # average drawdown over duration of pumping (e.g., 100 days)
        s_theis_avg = np.mean(s_theis_ts) + np.mean(ADJACENT_WELLS * s_theis_interference_ts)
        s_interference_avg = ADJACENT_WELLS * np.mean(s_theis_interference_ts)

        # apply Jacob correction to Theis drawdown to account for partial penetration (e.g., in unconfined aquifers)
        # convert to Jacob - coefficient of quadratic equation (ax^2 + bx + c = 0) 
        a = -1 / (2 * sat_thickness_array[year])
        b = 1
        c = -s_theis_avg

        # solve quadratic
        root_1 = (-b + (b ** 2 - 4 * a * c) ** 0.5) / (2 * a)
        root_2 = (-b - (b ** 2 - 4 * a * c) ** 0.5) / (2 * a)

        s_jacob = root_1

        # skip rest of the years after drawdown becomes zero
        if s_jacob == 0:
            continue


        ########################### compute outputs ###########################

        # save annual pumping values to arrays
        if year == 0: # for the first year
            drawdown = np.zeros(NUM_YEARS)
            drawdown_interference = np.zeros(NUM_YEARS)
            total_head = np.zeros(NUM_YEARS)
            volume_per_well = np.zeros(NUM_YEARS)
            volume_deep_recharge = np.zeros(NUM_YEARS)
            num_wells = np.zeros(NUM_YEARS)
            cumulative_volume_per_well = np.zeros(NUM_YEARS)
            cumulative_volume_all_wells = np.zeros(NUM_YEARS)
            depleted_volume_fraction = np.zeros(NUM_YEARS)

            volume_per_well[year] = Well_Q_array[year] * SECS_IN_DAY * DAYS # m^3
            num_wells[year] = selected_grid_df.Grid_area[grid_cell] / well_area_array[year] # number of wells [unitless]
            volume_all_wells[year] = volume_per_well[year] * num_wells[year] # m^3
            cumulative_volume_per_well[year] = volume_per_well[year] # m^3
            cumulative_volume_all_wells[year] = volume_all_wells[year]  # m^3
            depleted_volume_fraction[year] = cumulative_volume_all_wells[year] / available_volume # fraction of available volume that is already pumped out

        else: # for the rest of the pumping years
            volume_per_well[year] = Well_Q_array[year] * SECS_IN_DAY * DAYS
            num_wells[year] = selected_grid_df.Grid_area[grid_cell] / well_area_array[year]
            volume_all_wells[year] = volume_per_well[year] * num_wells[year]
            cumulative_volume_per_well[year] = volume_per_well[year] + cumulative_volume_per_well[year - 1]
            cumulative_volume_all_wells[year] = volume_all_wells[year] + cumulative_volume_all_wells[year - 1]
            depleted_volume_fraction[year] = cumulative_volume_all_wells[year] / available_volume

        drawdown[year] = s_jacob  # m
        drawdown_interference[year] = s_interference_avg  # m
        total_head[year] = s_jacob + DTW_array[year]  # m
        volume_deep_recharge[year] = min(deep_recharge_vol, volume_all_wells[year])

        # update variable arrays for next annual pumping iteration
        if year != NUM_YEARS - 1: # skip updating arrays for the last year
            # pass the same Q to the next year, checks before this determine if Q needs to be updated 
            Well_Q_array[year + 1] = Well_Q_array[year]
            # add the average depth of net groundwater pumped in current year to the previous depth to water
            DTW_array[year + 1] = DTW_array[year] + (volume_all_wells[year] - volume_deep_recharge[year]) / grid_cell_area / S
            # remaining length of well under water table
            sat_thickness_array[year + 1] = well_length_array[year] - DTW_array[year + 1]
            # updated transmissivity based on new saturated thickness
            T_array[year + 1] = K * sat_thickness_array[year + 1]
            # pass the same well characteristics to the next year, checks above determine if these need to be updated
            well_roi_array[year + 1] = well_roi_array[year]
            well_area_array[year + 1] = well_area_array[year]
            well_length_array[year + 1] = well_length_array[year]

    # skip calculating costs and writing outputs for the last year in which drawdown is zero
    if drawdown[year] == 0:
        continue


    ##################### annual costs and unit costs #########################

    # assign well unit cost based on WHY Class
    if selected_grid_df.WHYClass[grid_cell] == 10:
        well_unit_cost = float(params.Val['Well_Install_10'])
    elif selected_grid_df.WHYClass[grid_cell] == 20:
        well_unit_cost = float(params.Val['Well_Install_20'])
    else:
        well_unit_cost = float(params.Val['Well_Install_30'])

    # find indexes of years when number of wells increase due to pumping rate reduction 
    # along with pumping rate and corresponding number of wells
    pumping_years = year + 1
    well_count = np.unique(num_wells)
    if min(well_count) == 0:  # remove 0 from well_count array
        well_count = np.delete(well_count, 0)

    # calculate number of added wells for each year
    added_well_count = np.zeros(len(well_count))
    for i in range(len(added_well_count)):
        if i == 0:
            added_well_count[i] = well_count[i]
        else: # if well count has changed over years
            added_well_count[i] = well_count[i] - well_count[i - 1]

    # unique pumping rates and corresponding number of wells
    Q_vals = np.unique(Well_Q_array)
    Q_vals = np.sort(Q_vals[Q_vals != 0])[::-1]  # remove zeros and sort in descending order

    # indices where pumping rate and number of wells have changed
    Start_indx = np.zeros(len(Q_vals))
    if len(Start_indx) != 1: # if there are multiple unique pumping rates
        for i in range(pumping_years):
            if i == 0: # first year
                counter = 1
                continue
            if num_wells[i] - num_wells[i - 1] > 0: # if number of wells has increased compared to previous year
                Start_indx[counter] = int(i)
                counter += 1

    # initialize cost arrays to track annual non-energy costs for each group of added wells
    capital_cost_array = np.zeros((len(Start_indx), int(NUM_YEARS + WELL_LIFETIME)))
    maintenance_array = np.zeros((len(Start_indx), int(NUM_YEARS + WELL_LIFETIME)))

    # Calculate capital and maintenance costs as function of installation and initial costs
    def calculate_costs(added_wells, year, offset, install_cost, install_cost_for_maint, initial_cost):
        """Calculate capital and maintenance costs for each year and each group of added wells
        :param added_wells:             index of the group of added wells
        :param year:                    year of pumping
        :param offset:                  offset for the year
        :param install_cost:            installation cost for the well/grid cell
        :param install_cost_for_maint:  installation cost for maintenance 
        :param initial_cost:            initial cost for the year
        :return:                        capital and maintenance costs for the year     
        """

        capital_cost = initial_cost + added_well_count[added_wells] * install_cost * ((1 + INTEREST_RATE) ** WELL_LIFETIME) * INTEREST_RATE / ((1 + INTEREST_RATE) ** WELL_LIFETIME - 1)
        maintenance_cost = MAINTENANCE_RATE * install_cost_for_maint * added_well_count[added_wells]  # maintenance cost [% of initial cost]

        return capital_cost, maintenance_cost

    # calculate costs for each group of deepened, replaced, and added wells
    for added_wells in range(len(added_well_count)):
        offset = int(Start_indx[added_wells])
        for year in range(pumping_years):
            if year + offset == pumping_years:
                break

            # 1) no deepening, initial_sat_thickness < MAX_INITIAL_SAT_THICKNESS (pumping rate reduced)
            elif initial_sat_thickness < MAX_INITIAL_SAT_THICKNESS:
                install_cost = well_unit_cost * well_length_array[0]  # if no deepening, well install remains fixed
                install_cost_for_maint = well_unit_cost * well_length_array[0]
                capital_cost_array[added_wells, year + offset], maintenance_array[added_wells, year + offset] = calculate_costs(added_wells, year, offset, install_cost, install_cost_for_maint,0)

            # 2) deepening, initial_sat_thickness > MAX_INITIAL_SAT_THICKNESS
            elif year == 0:  # zero initial cost for first year
                install_cost = well_unit_cost * well_length_array[0]
                install_cost_for_maint = well_unit_cost * well_length_array[0]
                capital_cost_array[added_wells, year + offset], maintenance_array[added_wells, year + offset] = calculate_costs(added_wells, year + offset, offset, install_cost, install_cost_for_maint, 0)

            # replace well every n years (well lifetime), if pumping rate is reduced, unit cost would be less at 200 gpm and below
            elif (year + 1) % WELL_LIFETIME == 0:
                install_cost = well_unit_cost * well_length_array[year + offset]
                install_cost_for_maint = well_unit_cost * well_length_array[year + offset]
                capital_cost, maintenance_cost = calculate_costs(added_wells, year + offset, offset, install_cost,
                                                                 install_cost_for_maint, 0)
                # add to previously incurred costs
                capital_cost_array[added_wells, year + offset] += capital_cost
                maintenance_array[added_wells, year + offset] += maintenance_cost

            # deepening after the first year (main deepening block)
            elif well_length_array[year + offset] - well_length_array[year - 1 + offset] > 0:
                capital_cost_array[added_wells, (year + offset): int((year + offset + WELL_LIFETIME))] += well_unit_cost * (
                    well_length_array[year + offset] - well_length_array[year - 1 + offset]) * (
                    (1 + INTEREST_RATE) ** WELL_LIFETIME) * INTEREST_RATE / ((1 + INTEREST_RATE) ** WELL_LIFETIME - 1) * added_well_count[added_wells]
                install_cost_for_maint = well_unit_cost * well_length_array[year + offset]
                capital_cost, maintenance_cost = calculate_costs(added_wells, year + offset, offset, install_cost, install_cost_for_maint, 0)
                capital_cost_array[added_wells, year + offset] += capital_cost
                maintenance_array[added_wells, year + offset] += maintenance_cost

            # not deepening, not replacing in the current year
            # just accumulating costs without any additional initial costs
            else:
                capital_cost, maintenance_cost = calculate_costs(added_wells, year + offset, offset, install_cost, install_cost_for_maint, 0)
                capital_cost_array[added_wells, year + offset] += capital_cost
                maintenance_array[added_wells, year + offset] += maintenance_cost


    ####################### annual cost metrics ###########################

    annual_capital_cost = np.zeros(NUM_YEARS)
    maintenance_cost = np.zeros(NUM_YEARS)
    well_installation_cost = np.zeros(NUM_YEARS)
    nonenergy_cost = np.zeros(NUM_YEARS)
    power = np.zeros(NUM_YEARS)
    energy = np.zeros(NUM_YEARS)
    energy_cost_rate = np.zeros(NUM_YEARS)
    energy_cost = np.zeros(NUM_YEARS)
    total_cost_per_well = np.zeros(NUM_YEARS)
    total_cost_all_wells = np.zeros(NUM_YEARS)
    unit_cost = np.zeros(NUM_YEARS)
    unit_cost_per_km3 = np.zeros(NUM_YEARS)
    unit_cost_per_acreft = np.zeros(NUM_YEARS)

    # total annual capital and maintenance costs as costs could have been incurred due to different reasons in the same year 
    # e.g., both deepening and replacing or carrying forward a loan from previous deepening or replacement
    annual_capital_cost = np.sum(capital_cost_array, axis=0)
    maintenance_cost = np.sum(maintenance_array, axis=0)

    # calculate and store costs for a year
    for year in range(pumping_years):
        well_installation_cost[year] = well_unit_cost * well_length_array[year] # $
        nonenergy_cost[year] = annual_capital_cost[year] + maintenance_cost[year] # $
        power[year] = num_wells[year] * (SPECIFIC_WEIGHT * total_head[year] * Well_Q_array[year] / EFFICIENCY) / 1000  # kW
        energy[year] = power[year] * (DAYS * HOURS_IN_DAY)  # kWh 
        energy_cost_rate[year] = ELECTRICITY_RATE  # $ per kWh
        energy_cost[year] = energy[year] * energy_cost_rate[year]  # $/year
        total_cost_per_well[year] = (nonenergy_cost[year] + energy_cost[year]) / num_wells[year]
        total_cost_all_wells[year] = num_wells[year] * total_cost_per_well[year]

        # unit costs in different units 
        unit_cost[year] = total_cost_all_wells[year] / volume_all_wells[year]  # $/m^3
        unit_cost_per_km3[year] = unit_cost[year] * 10 ** 9  # $/km^3
        unit_cost_per_acreft[year] = unit_cost[year] * 1233.48  # $/acft


    ######################## save grid cell results ###########################

    """
    ['year_number', 'depletion_limit', 'continent', 'country', gcam_basin_id, 'Basin_long_name', 'grid_id', 
    'grid_area', 'permeability', 'porosity', 'total_thickness', 'depth_to_water', 'orig_aqfr_sat_thickness', 
    'aqfr_sat_thickness', 'hydraulic_conductivity', 'transmissivity', 'radius_of_influence', 'areal_extent', 
    'drawdown', 'drawdown_interference', 'total_head', 'well_yield', 'recharge', 'shallow_recharge_depth', 
    'threshold_depth', 'net_ponded_depth_target', 'excessive_recharge_depth', 'deep_recharge_vol', 
    'volume_deep_recharge', 'volume_produced_perwell', 'cumulative_vol_produced_perwell', 'number_of_wells', 
    'volume_produced_allwells', 'annual_capital_cost', 'maintenance_cost', 'nonenergy_cost', 'power', 'energy', 
    'energy_cost_rate', 'energy_cost', 'total_cost_perwell', 'total_cost_allwells', 'unit_cost', 'unit_cost_per_km3', 
    'unit_cost_per_acreft', 'whyclass', 'total_well_length']
    """

    # TODO: improve the way outputs are written to the file. Currently it is time consuming to write to the file for
    #  each grid cell and each year. It would be better to write to the file in chunks or at the end of the
    #  simulation. We can use some dataframes approach to store the results and write them at the end of the
    #  simulation. We can also use the pandas library to write to the file which is faster than the current method.
    #  Also, we can zip the outputs to reduce the file size.

    for year in range(pumping_years):
        if RECHARGE_FLAG == 0:
            shallow_recharge_str = 'recharge_off'
            threshold_depth_str = 'recharge_off'
            excessive_recharge_str = 'recharge_off'
        else:
            shallow_recharge_str = str(shallow_recharge_depth)
            threshold_depth_str = str(threshold_depth)
            excessive_recharge_str = str(excessive_recharge_depth)

        outputs = str(year + 1) + ', ' + \
                  str(DEPLETION_LIMIT) + ', ' + \
                  str(selected_grid_df.Continent[grid_cell]) + ', ' + \
                  str(selected_grid_df.Country[grid_cell]) + ', ' + \
                  str(int(selected_grid_df.GCAM_basin_ID[grid_cell])) + ', ' + \
                  str(selected_grid_df.Basin_long_name[grid_cell]) + ', ' + \
                  str(selected_grid_df.GridCellID[grid_cell]) + ', ' + \
                  str(grid_cell_area) + ', ' + \
                  str(selected_grid_df.Permeability[grid_cell]) + ', ' + \
                  str(selected_grid_df.Porosity[grid_cell]) + ', ' + \
                  str(selected_grid_df.Aquifer_thickness[grid_cell]) + ', ' + \
                  str(DTW_array[year]) + ', ' + \
                  str(initial_sat_thickness) + ', ' + \
                  str(sat_thickness_array[year]) + ', ' + \
                  str(K) + ', ' + \
                  str(T_array[year]) + ', ' + \
                  str(well_roi_array[year]) + ', ' + \
                  str(well_area_array[year]) + ', ' + \
                  str(drawdown[year]) + ', ' + \
                  str(drawdown_interference[year]) + ', ' + \
                  str(total_head[year]) + ', ' + \
                  str(Well_Q_array[year]) + ', ' + \
                  str(recharge) + ', ' + \
                  shallow_recharge_str + ', ' + \
                  threshold_depth_str + ', ' + \
                  str(NET_PONDED_DEPTH_TARGET) + ', ' + \
                  excessive_recharge_str + ', ' + \
                  str(deep_recharge_vol) + ', ' + \
                  str(volume_deep_recharge[year]) + ', ' + \
                  str(volume_per_well[year]) + ', ' + \
                  str(cumulative_volume_per_well[year]) + ', ' + \
                  str(num_wells[year]) + ', ' + \
                  str(volume_all_wells[year]) + ', ' + \
                  str(cumulative_volume_all_wells[year]) + ', ' + \
                  str(available_volume) + ', ' + \
                  str(depleted_volume_fraction[year]) + ', ' + \
                  str(well_installation_cost[year]) + ', ' + \
                  str(annual_capital_cost[year]) + ', ' + \
                  str(maintenance_cost[year]) + ', ' + \
                  str(nonenergy_cost[year]) + ', ' + \
                  str(power[year]) + ', ' + \
                  str(energy[year]) + ', ' + \
                  str(energy_cost_rate[year]) + ', ' + \
                  str(energy_cost[year]) + ', ' + \
                  str(total_cost_per_well[year]) + ', ' + \
                  str(total_cost_all_wells[year]) + ', ' + \
                  str(unit_cost[year]) + ', ' + \
                  str(unit_cost_per_km3[year]) + ', ' + \
                  str(unit_cost_per_acreft[year]) + ', ' + \
                  str(selected_grid_df.WHYClass[grid_cell]) + ', ' + \
                  str(well_length_array[year])

        # write outputs to the file
        file = open(output_path + '/' + output_name + '.csv', 'a')
        file.write(outputs)
        file.write('\n')
        file.close()

if GRIDCELL_FILTER == 'all':
    print(skipped_cells, 'grid cells out of ', grid_cell,' cells (',
      round(skipped_cells * 100 /grid_cell), '% ) were skipped due to screening criteria')

print('Results are saved in ', output_path + '/' + output_name + '.csv\nALL DONE!')

## END
