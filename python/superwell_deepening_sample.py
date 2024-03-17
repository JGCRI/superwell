# -*- coding: utf-8 -*-

# main superwell script

import numpy as np
import pandas as pd
import math

# TODO: create an outer scenario loop, or make this whole file a function and call it using a batch run script,
#  read the list of scenarios to be run from a file, create a directory for the scenario, copy the under modified
#  params file, write outputs and save a few key plots e.g., a summary diagnostic plot and maps (volume, unit cost etc)

# load data
grid_df = pd.read_csv('../inputs/sampled_data_100.csv')
params = pd.read_csv('../inputs/params.csv', index_col=0)
electricity_rates = pd.read_csv('../inputs/GCAM_Electrical_Rates.csv', index_col=0, header=None)
W_lookup = pd.read_csv('../inputs/Theis_well_function_table.csv', header="infer")
lookup_idx = pd.Index(W_lookup.W)


# define constants
MAX_INITIAL_SAT_THICKNESS = params.Val['Max_Initial_Sat_Thickness']  # maximum initial saturated thickness
DEFAULT_ELECTRICITY_RATE = params.Val['Energy_cost_rate']  # default electricity rate
DEPLETION_LIMIT = params.Val['Depletion_Limit']  # depletion limit for this scenario
IRR_DEPTH = params.Val['Irrigated_Depth']  # annual irrigation depth target [m]
NUM_YEARS = int(params.Val['Total_Simulation_Years'])  # maximum years of pumping
DAYS = int(params.Val['Pumping_Days'])  # days pumping per year
SPECIFIC_WEIGHT = params.Val['Specific_weight']  # specific weight of water
EFFICIENCY = params.Val['Pump_Efficiency']  # well efficiency
WELL_LIFETIME = params.Val['Max_Lifetime_in_Years']
INTEREST_RATE = params.Val['Interest_Rate']
MAINTENANCE_RATE = params.Val['Maintenance_factor']

# convert electricity rate dictionary
electricity_rate_dict = {}
for i in range(len(electricity_rates.iloc[:, 0])):
    country = electricity_rates.index[i]
    electricity_rate_dict[country.rstrip()] = electricity_rates.iloc[i, 0]

# filter by country, if desired
country = 'all'
if country == 'all':
    selected_grid_df = grid_df
else:
    selected_grid_df = grid_df[grid_df['Country'] == country].reset_index(drop=True)

# define outputs file name
output_path = '../outputs/'
output_name = 'superwell_py_deep_' + str.replace(country, ' ', '') + '_' + str(IRR_DEPTH) + 'IrD_' + str(
    DEPLETION_LIMIT) + 'DL_sample_100'

# header for output file
header_column_names = 'year_number,depletion_limit,continent,country,' \
                      'gcam_basin_id,Basin_long_name,grid_id,grid_area,permeability,porosity,' \
                      'total_thickness,depth_to_water,orig_aqfr_sat_thickness,aqfr_sat_thickness,' \
                      'hydraulic_conductivity,transmissivity,radius_of_influence,areal_extent,' \
                      'max_drawdown,drawdown,drawdown_interference,total_head,well_yield,volume_produced_perwell,' \
                      'cumulative_vol_produced_perwell,number_of_wells,volume_produced_allwells,' \
                      'cumulative_vol_produced_allwells,available_volume,depleted_vol_fraction,' \
                      'well_installation_cost, annual_capital_cost,maintenance_cost,nonenergy_cost,' \
                      'power,energy,energy_cost_rate,energy_cost,total_cost_perwell,total_cost_allwells,' \
                      'unit_cost,unit_cost_per_km3,unit_cost_per_acreft,whyclass,total_well_length'

# write header to the output file
file = open(output_path + output_name + '.csv', 'w')
file.write(str(header_column_names))
file.write('\n')
file.close()

# TODO: we don't have max depletion ratio now, how do we calculate max drawdown now? # (originally it was supposed to
#  be max_drawdown = Max_Depletion * Original Aquifer Thickness ). It could be max_s_frac * selected_grid_df.Aquifer_thickness
#  (or the array tracking aquifer thickness sat_thickness_array?)

# define Theis function
def drawdown_theis(time, r, S, T, Q):
    u = r ** 2 * S / (4 * T * time)

    if u > 5.9:  # for large u values, W will be insignificant and drawdown (s) will ~= 0
        W = 0

    elif 5.9 > u and u > .6:  # use W(u) lookup table for intermediate values where approximation is insufficient
        lookup_idx = pd.Index(W_lookup.u)
        lookup_loc = lookup_idx.get_indexer([u], method='nearest')
        W_index = lookup_loc[0] if lookup_loc[0] != -1 else np.argmin(np.abs(lookup_idx.to_series() - u))
        W = W_lookup.W[W_index]

    else:  # use approximation for small u values
        W = -0.57721 - math.log(u) + u - u ** 2 / (2 * 2)

    s = W * Q / (4 * 3.1416 * T)

    return (s)


# candidate well pumping rates (gallons per minute)
Q_array_gpm = [10, 20, 30, 40, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600, 700, 800, 900, 1000, 1200, 1300, 1400, 1500]

Q_array = np.array(Q_array_gpm) / (60 * 264.17)  # Convert candidate pumping rates to m^3/s

print("Preamble complete. Beginning the simulation...")

skipped_cells = 0

# %% superwell code block
for grid_cell in range(len(selected_grid_df.iloc[:, 0])):

    if grid_cell % int(len(selected_grid_df.iloc[:, 0]) / 10) == 0 or selected_grid_df.Country[
        grid_cell] != selected_grid_df.Country[grid_cell - 1]:
        print('Percent complete = ' + str(np.round(100 * grid_cell / len(selected_grid_df.iloc[:, 0]), 1)) +
              ' | Processing Cell # ' + str(selected_grid_df.GridCellID[grid_cell]) + ' in '
              + str(selected_grid_df.Country[grid_cell]))

    ################ determine if grid cell is skipped ########################

    # skip grid areas less than 5x5 km (1% of a normal 50x50 grid size)
    if selected_grid_df.Grid_area[grid_cell] < 5 * 5 * (10 ** 6):
        skipped_cells += 1
        continue

    # depth to water table should we at least 1 meter
    if selected_grid_df.Depth_to_water[grid_cell] < 1:
        skipped_cells += 1
        continue

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

    # Get the country name from selected_grid_df and check if the country is in electricity_rate_dict
    if str(selected_grid_df.Country[grid_cell]) in electricity_rate_dict:
        ELECTRICITY_RATE = electricity_rate_dict[str(selected_grid_df.Country[grid_cell])]
    else:
        ELECTRICITY_RATE = DEFAULT_ELECTRICITY_RATE  # give default electricity rate if missing cost data

    total_thickness = selected_grid_df.Aquifer_thickness[grid_cell]  # m
    grid_cell_area = selected_grid_df.Grid_area[grid_cell]

    # depth to water
    DTW_array = np.zeros(NUM_YEARS)  # tracks depth to water for each year
    DTW_array[0] = selected_grid_df.Depth_to_water[grid_cell]  # initial depth to water

    # saturated thickness: total aquifer thickness minus depth to piezometric surface
    initial_sat_thickness = selected_grid_df.Aquifer_thickness[grid_cell] - selected_grid_df.Depth_to_water[grid_cell]  # m

    sat_thickness_array = np.zeros(NUM_YEARS)
    well_length_array = np.zeros(NUM_YEARS)

    if initial_sat_thickness > MAX_INITIAL_SAT_THICKNESS:
        sat_thickness_array[0] = MAX_INITIAL_SAT_THICKNESS  # m
        well_length_array[0] = sat_thickness_array[0] + DTW_array[0]  # m
    else:
        sat_thickness_array[0] = initial_sat_thickness  # m
        well_length_array[0] = total_thickness  # m

    # available volume
    available_volume = initial_sat_thickness * grid_cell_area * selected_grid_df.Porosity[grid_cell]

    # aquifer properties for Theis 
    S = selected_grid_df.Porosity[grid_cell]  # [-]
    K = 10 ** selected_grid_df.Permeability[grid_cell] * 1e7  # m/s
    T = K * sat_thickness_array[0]  # m/s
    T_array = np.zeros(NUM_YEARS)  # tracks T for each year
    T_array[0] = T  # initial T

    #################### determine initial well Q #############################

    # time and well radius for Theis solution
    time_Q = 2 * 365 * 86400  # time period used for determining initial well Q
    well_r = 0.5 * params.Val['Well_Diameter']

    # drawdown at t = 2 years for all candidate well Qs 
    s_array = np.zeros(len(Q_array))
    for i, Q in enumerate(Q_array):
        s_array[i]= drawdown_theis(time_Q, well_r, S, T, Q)

    # find largest Q that meets screening criteria
    # screening criteria 
    max_s_frac = .40  # max drawdown as % of sat thickness
    max_s_absolute = 80  # max drawdown in m

    Q_viability = np.zeros(len(Q_array))

    for i, s in enumerate(s_array):
        if s / initial_sat_thickness < max_s_frac and s < max_s_absolute:
            Q_viability[i] = 1

    # skip grid cell if no pumping rates are viable
    if np.sum(Q_viability) == 0:
        continue

    initial_Q_indx_arr = np.where(Q_viability == 1)
    initial_Q_indx = np.max(initial_Q_indx_arr[:])  # index of largest viable Q
    initial_Q = Q_array[initial_Q_indx]
    Well_Q_array = np.zeros(NUM_YEARS)
    Well_Q_array[0] = initial_Q

    ###################### determine initial well Area ########################
    initial_well_area = initial_Q * DAYS * 86400 / (IRR_DEPTH)  # m^2
    initial_roi = (initial_well_area / math.pi) ** 0.5  # m
    initial_num_wells = selected_grid_df.Grid_area[grid_cell] / initial_well_area # initial number of wells
    well_roi_array = np.zeros(NUM_YEARS)
    well_roi_array[0] = initial_roi
    well_area_array = np.zeros(NUM_YEARS)
    well_area_array[0] = initial_well_area

    # make sure depleted volume fraction does not exceed depletion limit in the first year of pumping
    # ratio of volume pumped in first year to available volume
    # same as this initial_Q * DAYS * 86400 * initial_num_wells / available_volume > DEPLETION_LIMIT:
    if (((selected_grid_df.Grid_area[grid_cell] / well_area_array[0]) * Well_Q_array[0] * 86400 * DAYS) /
            available_volume > DEPLETION_LIMIT):
        continue

    ####################### annual pumping simulation loop ####################
    depleted_volume_fraction = np.zeros(NUM_YEARS)  # initialize
    volume_all_wells = np.zeros(NUM_YEARS)

    for year in range(NUM_YEARS):

        # make sure depleted volume fraction does not exceed depletion limit
        # check for last year: if we pump for one more year would we hit the depletion limit
        if depleted_volume_fraction[year - 1] + (volume_all_wells[year - 1] / available_volume) > DEPLETION_LIMIT:
            year = year - 1
            break

    # for year in range(NUM_YEARS):
    #     if depleted_volume_fraction[year - 1] > DEPLETION_LIMIT:
    #         year = year - 1
    #         break

        # test viability for current year (simulate drawdown at t = 100 days of pumping)
        # initialize viability variables 
        s_theis = 0
        s_theis_interference = 0

        s_theis = drawdown_theis(DAYS * 86400, well_r, S, T_array[year], Well_Q_array[year])
        s_theis_interference = drawdown_theis(DAYS * 86400, well_roi_array[year] * 2, S, T_array[year], Well_Q_array[year])
        s_total = s_theis + 4 * s_theis_interference  # total drawdown (well + interference)

        # check if drawdown constraints are violated by end of 100 day pumping period
        # if constraints violated: (1) first deepen well, (2) then reduce well pumping rate 
        if s_total > max_s_absolute or s_total / sat_thickness_array[year] > max_s_frac:

            # 1) first preference deepen well 
            if well_length_array[year] < total_thickness:
                # update well length
                if well_length_array[year] + 50 < total_thickness:
                    well_length_array[year] = 50 + well_length_array[year]

                else:
                    remaining_length = total_thickness - well_length_array[year]
                    well_length_array[year] = remaining_length + well_length_array[year]

                # update saturated thickness and T 
                sat_thickness_array[year] = well_length_array[year] - DTW_array[year]
                T_array[year] = sat_thickness_array[year] * K

            # 2) once well cannot be deepened, reduce well pumping rate 
            else:
                s_array = np.zeros(len(Q_array))
                for i, Q in enumerate(Q_array):
                    s_array[i] = drawdown_theis(time_Q, well_r, S, T_array[year], Q)

                Q_viability = np.zeros(len(Q_array))

                for i, s in enumerate(s_array):
                    if s / sat_thickness_array[year] < max_s_frac and s < max_s_absolute:
                        Q_viability[i] = 1

                # exit pumping code block if no pumping rates are viable
                if np.sum(Q_viability) == 0:
                    break

                Q_indx_arr = np.where(Q_viability == 1)
                Q_indx = np.max(Q_indx_arr[:])  # index of largest viable Q
                new_Q = Q_array[Q_indx]  # new Q
                Well_Q_array[year] = new_Q  # update Q for current YEAR

                # update roi
                well_area_array[year] = Well_Q_array[year] * DAYS * 86400 / IRR_DEPTH
                well_roi = (well_area_array[year] / math.pi) ** 0.5
                well_roi_array[year] = initial_roi

        # # exit pumping code block if no pumping rates are viable
        # if np.sum(Q_viability) == 0:
        #     break 

        # if constraints aren't violated, proceed to calculate output for pumping year  
        # simulate 100 days of pumping, with drawdown calculated every 10 days
        s_theis_ts = np.zeros(int(DAYS / 10))
        s_theis_interference_ts = np.zeros(int(DAYS / 10))

        for day in range(int(DAYS / 10)):
            s_theis_ts[day] = drawdown_theis((day + 1) * 10 * 86400, well_r, S, T_array[year], Well_Q_array[year])
            s_theis_interference_ts[day] = drawdown_theis((day + 1) * 10 * 86400, well_roi_array[year] * 2, S, T_array[year], Well_Q_array[year])

        # average drawdown
        s_theis_avg = np.mean(s_theis_ts) + np.mean(4 * s_theis_interference_ts)
        s_interference_avg = 4 * np.mean(s_theis_interference_ts)

        # convert to Jacob - solve quadratic
        a = -1 / (2 * sat_thickness_array[year])
        b = 1
        c = -s_theis_avg

        root_1 = (-b + (b ** 2 - 4 * a * c) ** 0.5) / (2 * a)
        root_2 = (-b - (b ** 2 - 4 * a * c) ** 0.5) / (2 * a)

        s_jacob = root_1

        # skip rest of the years after drawdown becomes zero
        if s_jacob == 0:
            continue

        ########################### compute outputs ###########################

        # save annual pumping values to arrays 
        if year == 0:
            drawdown = np.zeros(NUM_YEARS)
            drawdown_interference = np.zeros(NUM_YEARS)
            total_head = np.zeros(NUM_YEARS)
            volume_per_well = np.zeros(NUM_YEARS)
            num_wells = np.zeros(NUM_YEARS)
            # volume_all_wells = np.zeros(NUM_YEARS)
            cumulative_volume_per_well = np.zeros(NUM_YEARS)
            cumulative_volume_all_wells = np.zeros(NUM_YEARS)
            depleted_volume_fraction = np.zeros(NUM_YEARS)

            drawdown[year] = s_jacob
            drawdown_interference[year] = s_interference_avg
            total_head[year] = s_jacob + DTW_array[year]
            volume_per_well[year] = Well_Q_array[year] * 86400 * DAYS
            num_wells[year] = selected_grid_df.Grid_area[grid_cell] / well_area_array[year]
            volume_all_wells[year] = volume_per_well[year] * num_wells[year]
            cumulative_volume_per_well[year] = volume_per_well[year]
            cumulative_volume_all_wells[year] = volume_all_wells[year]
            depleted_volume_fraction[year] = cumulative_volume_all_wells[year] / available_volume

        else:
            drawdown[year] = s_jacob
            drawdown_interference[year] = s_interference_avg
            total_head[year] = s_jacob + DTW_array[year]
            volume_per_well[year] = Well_Q_array[year] * 86400 * DAYS
            num_wells[year] = selected_grid_df.Grid_area[grid_cell] / well_area_array[year]
            volume_all_wells[year] = volume_per_well[year] * num_wells[year]
            cumulative_volume_per_well[year] = volume_per_well[year] + cumulative_volume_per_well[year - 1]
            cumulative_volume_all_wells[year] = volume_all_wells[year] + cumulative_volume_all_wells[year - 1]
            depleted_volume_fraction[year] = cumulative_volume_all_wells[year] / available_volume

        # update variable arrays for next annual pumping iteration
        if year != NUM_YEARS - 1:
            Well_Q_array[year + 1] = Well_Q_array[year]
            DTW_array[year + 1] = DTW_array[year] + (volume_all_wells[year] / grid_cell_area) / S
            sat_thickness_array[year + 1] = well_length_array[year] - DTW_array[year + 1]
            T_array[year + 1] = K * sat_thickness_array[year + 1]
            well_roi_array[year + 1] = well_roi_array[year]
            well_area_array[year + 1] = well_area_array[year]
            well_length_array[year + 1] = well_length_array[year]

    # skip calculating costs and writing outputs for the last year in which drawdown is zero
    if drawdown[year] == 0:
        continue

    ##################### annual costs and unit costs ######################### 
    # assign well unit cost based on WHY Class
    if selected_grid_df.WHYClass[grid_cell] == 10:
        well_unit_cost = params.Val['Well_Install_10']
    elif selected_grid_df.WHYClass[grid_cell] == 20:
        well_unit_cost = params.Val['Well_Install_20']
    else:
        well_unit_cost = params.Val['Well_Install_30']

    # find indexes of years when number of wells increase due to pumping rate reduction 
    # along with pumping rate and corresponding number of wells
    pumping_years = year + 1
    well_count = np.unique(num_wells)
    if min(well_count) == 0:
        well_count = np.delete(well_count, 0)

    added_well_count = np.zeros(len(well_count))
    for i in range(len(added_well_count)):
        if i == 0:
            added_well_count[i] = well_count[i]
        else:
            added_well_count[i] = well_count[i] - well_count[i - 1]

    Q_vals = np.unique(Well_Q_array)
    Q_vals = np.sort(Q_vals[Q_vals != 0])[::-1]  # remove zeros and sort in descending order

    Start_indx = np.zeros(len(Q_vals))  # indexes where pumping rate and well num changes
    if len(Start_indx) != 1:
        for i in range(pumping_years):
            if i == 0:
                counter = 1
                continue
            if num_wells[i] - num_wells[i - 1] > 0:
                Start_indx[counter] = int(i)
                counter += 1

    # initialize cost arrays to track annual non-energy costs for each group of added wells
    capital_cost_array = np.zeros((len(Start_indx), int(NUM_YEARS + WELL_LIFETIME)))
    maintenance_array = np.zeros((len(Start_indx), int(NUM_YEARS + WELL_LIFETIME)))

    # Calculate capital and maintenance costs as function of installation and initial costs
    def calculate_costs(added_wells, year, offset, install_cost, install_cost_for_maint, initial_cost):
        # # TODO: determine initial cost here and remove the initial_cost argument
        # if year == 0:
        #     initial_cost = 0
        # else:
        #     initial_cost = capital_cost_array[added_wells, year + offset]
        capital_cost = initial_cost + added_well_count[added_wells] * install_cost * ((1 + INTEREST_RATE) ** WELL_LIFETIME) * INTEREST_RATE / ((1 + INTEREST_RATE) ** WELL_LIFETIME - 1)
        maintenance_cost = MAINTENANCE_RATE * install_cost_for_maint * added_well_count[added_wells]  # maintenance cost [% of initial cost]

        return capital_cost, maintenance_cost

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
                # capital_cost_array[added_wells, year + offset] = capital_cost
                # maintenance_array[added_wells, year + offset] = maintenance_cost

            elif (year + 1) % WELL_LIFETIME == 0:  # Replace well every n years (well lifetime), if reduced yield, cheaper unit cost at 200 gpm and below
                install_cost = well_unit_cost * well_length_array[year + offset]
                install_cost_for_maint = well_unit_cost * well_length_array[year + offset]
                capital_cost, maintenance_cost = calculate_costs(added_wells, year + offset, offset, install_cost, install_cost_for_maint, capital_cost_array[added_wells, year + offset])
                capital_cost_array[added_wells, year + offset] += capital_cost
                maintenance_array[added_wells, year + offset] += maintenance_cost

            elif well_length_array[year + offset] - well_length_array[year - 1 + offset] > 0:  # deepening
                # TODO: isn't really clear what this block is doing.
                #  Q: what does this elif argument mean? Is it checking if deepening has happened due to the increase in well length?
                #  Q: how would calculate_costs() get install_cost?
                #  Q: is the sequence of calculations important? Can we calculate install_costs at the start of this block?
                #  Q: why is the capital_cost_array being updated here/calculated twice?
                capital_cost, maintenance_cost = calculate_costs(added_wells, year + offset, offset, install_cost, install_cost_for_maint, capital_cost_array[added_wells, year + offset])
                capital_cost_array[added_wells, year + offset] += capital_cost
                capital_cost_array[added_wells, (year + offset): int((year + offset + WELL_LIFETIME))] += well_unit_cost * (
                        well_length_array[year + offset] - well_length_array[year - 1 + offset]) * (
                        (1 + INTEREST_RATE) ** WELL_LIFETIME) * INTEREST_RATE / ((1 + INTEREST_RATE) ** WELL_LIFETIME - 1) * added_well_count[added_wells]
                install_cost_for_maint = well_unit_cost * well_length_array[year + offset]
                maintenance_array[added_wells, year + offset] += maintenance_cost

            else:  # not deepening, not replacing in the current year
                capital_cost, maintenance_cost = calculate_costs(added_wells, year + offset, offset, install_cost, install_cost_for_maint, capital_cost_array[added_wells, year + offset])
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

    annual_capital_cost = np.sum(capital_cost_array, axis=0)
    maintenance_cost = np.sum(maintenance_array, axis=0)

    # calculate and store costs for a year
    for year in range(pumping_years):
        well_installation_cost[year] = well_unit_cost * well_length_array[year]
        nonenergy_cost[year] = annual_capital_cost[year] + maintenance_cost[year]
        power[year] = num_wells[year] * (SPECIFIC_WEIGHT * total_head[year] * Well_Q_array[year] / EFFICIENCY) / 1000  # kW
        energy[year] = power[year] * (DAYS * 24)  # kWh/year
        energy_cost_rate[year] = ELECTRICITY_RATE  # $ per kWh
        energy_cost[year] = energy[year] * energy_cost_rate[year]  # $/year
        total_cost_per_well[year] = (nonenergy_cost[year] + energy_cost[year]) / num_wells[year]
        total_cost_all_wells[year] = num_wells[year] * total_cost_per_well[year]

        unit_cost[year] = total_cost_all_wells[year] / volume_all_wells[year]  # $/m^3
        unit_cost_per_km3[year] = unit_cost[year] * 10 ** 9  # $/km^3
        unit_cost_per_acreft[year] = unit_cost[year] * 1233.48  # $/acft


    ######################## save grid cell results ###########################
    """
    ['year_number', 'depletion_limit', 'continent', 'country', 
    'gcam_basin_id', 'Basin_long_name', 'grid_id', 'grid_area', 'permeability', 'porosity', 
    'total_thickness', 'depth_to_water', 'orig_aqfr_sat_thickness', 'aqfr_sat_thickness', 
    'hydraulic_conductivity', 'transmissivity', 'radius_of_influence', 'areal_extent', 
    'max_drawdown', 'drawdown', 'drawdown_interference', 'total_head', 'well_yield', 'volume_produced_perwell', 
    'cumulative_vol_produced_perwell', 'number_of_wells', 'volume_produced_allwells', 
    'cumulative_vol_produced_allwells', 'available_volume', 'depleted_vol_fraction', 
    'well_installation_cost', 'annual_capital_cost', 'maintenance_cost', 'nonenergy_cost', 
    'power', 'energy', 'energy_cost_rate', 'energy_cost', 'total_cost_perwell', 'total_cost_allwells', 
    'unit_cost', 'unit_cost_per_km3', 'unit_cost_per_acreft', 'whyclass', 'total_well_length']
    """
    for year in range(pumping_years):

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
                  str('Max_Drawdown') + ', ' + \
                  str(drawdown[year]) + ', ' + \
                  str(drawdown_interference[year]) + ', ' + \
                  str(total_head[year]) + ', ' + \
                  str(Well_Q_array[year]) + ', ' + \
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
        file = open(output_path + output_name + '.csv', 'a')
        file.write(outputs)
        file.write('\n')
        file.close()

print(skipped_cells, 'grid cells out of ', grid_cell,' cells (',
      round(skipped_cells * 100 /grid_cell), '% ) were skipped due to screening criteria')

print('ALL DONE!')
## END
