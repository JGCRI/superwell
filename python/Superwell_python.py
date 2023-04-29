# -*- coding: utf-8 -*-

import numpy as np 
import pandas as pd
import matplotlib.pyplot as plt
import math 
import os 

#%%
#os.chdir('C:/Users/fere556/Desktop/Superwell/superwell')
os.chdir('C:/Users/niaz981/OneDrive - PNNL/Documents - GCIMS Water/1.4b Groundwater/2023_Niazi_Superwell/Code/superwell/python')
output_path = 'outputs'
output_name = 'superwell_python_test'  # file name for output

# load data 
os.chdir('inputs')
grid_df = pd.read_csv('inputs.csv')
well_params = pd.read_csv('Well_Params.csv', index_col = 0)
electricity_rates = pd.read_csv('GCAM_Electrical_Rates.csv', index_col = 0, header = None)
W_lookup = pd.read_csv('Theis_well_function_table.csv', header = "infer") 
lookup_idx = pd.Index(W_lookup.W)
os.chdir('../')

#%%
# define constants 

# user-defined 
IRR_DEPTH = 0.30 # annual irrigation depth target (m)
NUM_YEARS = 100 # maximum years of pumping 
DAYS = 100 # days pumping per year

# imported values 
ELECTRICITY_RATE =  well_params.Val['Energy_cost_rate'] # defualt electricity rate
DEPLETION_LIMIT = well_params.Val['Depletion_Limit'] # depletion limit for this scenario 
EFFICIENCY = well_params.Val['Pump_Efficiency'] # well efficiency
WELL_LIFETIME = well_params.Val['Max_Lifetime_in_Years']
INTEREST_RATE = well_params.Val['Interest_Rate']
MAINTENANCE_RATE = well_params.Val['Maintenance_factor']
SPECIFIC_WEIGHT = well_params.Val['Specific_weight'] # specific weight of water 

# convert electricity rate dictionary 
electricity_rate_dict = {}
for i in range(len(electricity_rates.iloc[:,0])):
    country = electricity_rates.index[i]
    electricity_rate_dict[country.rstrip()] = electricity_rates.iloc[i,0]

# filter by country, if desired
country = 'United States'
if country != 'all':
    selected_grid_df = grid_df.where(grid_df.CNTRY_NAME[:] == country)
    selected_grid_df = selected_grid_df.dropna(thresh = 4)
    selected_grid_df = selected_grid_df.reset_index(drop = True)

# define Theis function 
def drawdown_theis(time, r, S, T, Q):
    u = r**2 * S/(4 * T * time)
    
    if u > 5.9: # for large u values, W will be insignificant and drawdown (s) will ~= 0 
        W = 0
        
    elif 5.9 > u and u > .6: # use W(u) lookup table for intermediate values where approximation is insufficient 
        lookup_idx = pd.Index(W_lookup.u)
        lookup_loc = lookup_idx.get_loc(u, method = 'nearest')
        W = W_lookup.W[lookup_loc]
        
    else: # use approximation for small u values 
        W = -0.57721 - math.log(u) + u - u**2/(2*2)
        
    s = W * Q / (4 * 3.1416 * T)
    
    return(s, u)

# candidate well pumping rates (gallons per minute)
Q_array_gpm = [10, 20, 30, 40, 50, 100, 150, 200, 250, 300, 350, 400, 500, 600, 700, 
           800, 900, 1000, 1200, 1300, 1400, 1500]

Q_array = np.zeros(len(Q_array_gpm))

# convert candidate pumping rates to m^3/s
for i, Q in enumerate(Q_array_gpm):
    Q_array[i] = Q/(60*264.17)

# header for output file 
header_column_names = 'iteration,year_number,DEPLETION_LIMIT,continent,country_name,' \
        'gcam_basin_id,gcam_basin_name,well_id,grid_area,permeability,storativity,' \
        'total_thickness,depth_to_piez_surface,orig_aqfr_sat_thickness,aqfr_sat_thickness,' \
        'hydraulic_conductivity,transmissivity,radius_of_influence,areal_extent,' \
        'max_drawdown,drawdown,drawdown_interference,total_head,well_yield,volume_produced_perwell,' \
        'cumulative_vol_produced_perwell,number_of_wells,volume_produced_allwells,' \
        'cumulative_vol_produced_allwells,available_volume,depleted_vol_fraction,' \
        'well_installation_cost, annual_capital_cost,maintenance_cost,nonenergy_cost,' \
        'power,energy,energy_cost_rate,energy_cost,total_cost_perwell,total_cost_allwells,' \
        'unit_cost,unit_cost_per_km3,unit_cost_per_acreft,whyclass,total_well_length'

#%% superwell code block 
for grid_cell in range(len(selected_grid_df.iloc[:,0])):
    
    print('Percent complete = ' + str(100 * (grid_cell)/len(selected_grid_df.iloc[:,0])))

    ################ determine if grid cell is skipped ########################
    
    # skip grid areas less than 10x10 km
    if selected_grid_df.Area[grid_cell] < 1*10**7:
        continue
    
    # depth to water table should we at least 5 meters
    if selected_grid_df.Depth[grid_cell] < 1:
        continue
    
    # limit low permeability values
    if selected_grid_df.Permeability[grid_cell] < -15: 
        continue 
    
    # limit porosity to 5% voids at least
    if selected_grid_df.Porosity[grid_cell] < 0.05: 
        continue 
    
    # correct aquifer thickness outliers, replace >1000m thickness with 200m
    if selected_grid_df.Thickness[grid_cell] > 1000:
        selected_grid_df.Thickness[grid_cell] == 1000
        
    ################ store grid cell attributes for output ####################
    
    ELECTRICITY_RATE = electricity_rate_dict[str(selected_grid_df.CNTRY_NAME[grid_cell])]
    
    total_thickness = selected_grid_df.Thickness[grid_cell] # m
    well_length = total_thickness # m 
    grid_cell_area = selected_grid_df.Area[grid_cell]
    
    # depth to water 
    depth_to_peiz_surface = selected_grid_df.Depth[grid_cell] # m
    DTW_array = np.zeros(NUM_YEARS) # tracks depth to water for each year 
    DTW_array[0] = depth_to_peiz_surface # initial depth to water
    
    # saturated thickness 
    initial_sat_thickness = selected_grid_df.Thickness[grid_cell] - selected_grid_df.Depth[grid_cell] # m
    sat_thickness_array = np.zeros(NUM_YEARS)
    sat_thickness_array[0] = initial_sat_thickness # m
    
    # available volume
    available_volume = initial_sat_thickness * grid_cell_area * selected_grid_df.Porosity[grid_cell]
    
    # aquifer properties for Theis 
    S = selected_grid_df.Porosity[grid_cell] # [-]
    K = 10 ** selected_grid_df.Permeability[grid_cell] * 1e7 # m/s 
    T = K * sat_thickness_array[0] # m/s
    T_array = np.zeros(NUM_YEARS) # tracks T for each year 
    T_array[0] = T # initial T
    
    # assign well unit cost based on WHY Class 
    if selected_grid_df.WHYClass[grid_cell] == 10:    
        well_unit_cost = well_params.Val['Well_Install_10']
    elif selected_grid_df.WHYClass[grid_cell] == 20:
        well_unit_cost = well_params.Val['Well_Install_20']
    else:
        well_unit_cost = well_params.Val['Well_Install_30']
    
    #################### determine initial well Q #############################
    
    # time and well radius for Theis solution
    time_Q = 2 * 365 * 86400 # time period used for determining initial well Q
    well_r = 0.5 * well_params.Val['Well_Diameter']
    
    # drawdown at t = 2 years for all candidate well Qs 
    s_array = np.zeros(len(Q_array))
    u_array = np.zeros(len(Q_array))
    for i, Q in enumerate(Q_array):
        s_array[i], u_array[i] = drawdown_theis(time_Q, well_r, S, T, Q)

    # find largest Q that meets screening criteria
    # screening criteria 
    max_s_frac = .40     # max drawdown as % of sat thickness
    max_s_absolute = 80  # max drawdown in m
    
    Q_viability = np.zeros(len(Q_array))
    
    for i, s in enumerate(s_array):
        if s/initial_sat_thickness < max_s_frac and s < max_s_absolute:
            Q_viability[i] = 1
    
    # skip grid cell if no pumping rates are viable
    if np.sum(Q_viability) == 0:
        continue
    
    initial_Q_indx_arr = np.where(Q_viability == 1) 
    initial_Q_indx = np.max(initial_Q_indx_arr[:]) # index of largest viable Q
    initial_Q = Q_array[initial_Q_indx]
    Well_Q_array = np.zeros(NUM_YEARS)
    Well_Q_array[0] = initial_Q 
    
    ###################### determine initial well Area ########################
    initial_well_area = initial_Q * 100 * 86400 / (IRR_DEPTH) # m^2
    initial_roi = (initial_well_area/math.pi) ** 0.5 # m 
    well_roi_array = np.zeros(NUM_YEARS)
    well_roi_array[0] = initial_roi
    well_area_array = np.zeros(NUM_YEARS)
    well_area_array[0] = initial_well_area

    ####################### annual pumping simulation loop ####################
    depleted_volume_fraction = 0 # initialize 
    
    for year in range(NUM_YEARS):
        if depleted_volume_fraction > DEPLETION_LIMIT:
            break 
            
        # test viability for current year (simulate drawdown at t = 100 days of pumping)
        # initialize viability variables 
        s_theis = 0 
        u_theis = 0
        s_theis_interference = 0 
        u_theis_interference = 0
        
        s_theis, u_theis = drawdown_theis(DAYS * 86400, well_r, S, T_array[year], Well_Q_array[year])
        s_theis_interference, u_theis_interference = drawdown_theis(DAYS * 86400, well_roi_array[year] * 2, S, T_array[year], Well_Q_array[year])
        s_total = s_theis + 4 * s_theis_interference # total drawdown (well + interference)
        
        # check if drawdown constraints are violated by end of 100 day pumping period  
        if s_total > max_s_absolute or s_total/sat_thickness_array[year] > max_s_frac:
        
            # if constraints violated, find new Q
            s_array = np.zeros(len(Q_array))
            u_array = np.zeros(len(Q_array))
            for i, Q in enumerate(Q_array):
                s_array[i], u_array[i] = drawdown_theis(time_Q, well_r, S, T_array[year], Q)
        
            Q_viability = np.zeros(len(Q_array))
            
            for i, s in enumerate(s_array):
                if s/sat_thickness_array[year] < max_s_frac and s < max_s_absolute:
                    Q_viability[i] = 1
            
            # exit pumping code block if no pumping rates are viable 
            if np.sum(Q_viability) == 0:
                break 
            
            Q_indx_arr = np.where(Q_viability == 1) 
            Q_indx = np.max(Q_indx_arr[:]) # index of largest viable Q
            new_Q = Q_array[Q_indx] # new Q 
            Well_Q_array[year] = new_Q # uopdate Q for current YEAR 
            
            # update roi
            well_area_array[year] = Well_Q_array[year] * 100 * 86400 / (IRR_DEPTH)
            well_roi = (well_area_array[year] /math.pi) ** 0.5
            well_roi_array[year] = initial_roi
            
           
        # if constraints aren't violated, proceed to calculate output for pumping year  
        # simulate 100 days of pumping, with drawdown calculated every 10 days
        s_theis_ts = np.zeros(int(DAYS/10)) 
        u_theis_ts = np.zeros(int(DAYS/10)) 
        s_theis_interference_ts = np.zeros(int(DAYS/10))
        u_theis_interference_ts = np.zeros(int(DAYS/10))
        
        for day in range(int(DAYS/10)):
            s_theis_ts[day], u_theis_ts[day] = drawdown_theis((day+1) * 10 * 86400, well_r, S, T_array[year], Well_Q_array[year])
            s_theis_interference_ts[day], u_theis_interference_ts[day] = drawdown_theis((day+1) * 10 * 86400, well_roi_array[year] * 2, S, T_array[year], Well_Q_array[year])
        
        # average drawdown
        s_theis_avg = np.mean(s_theis_ts) + np.mean(4 * s_theis_interference_ts) 
        s_interference_avg = 4 * np.mean(s_theis_interference_ts) 
        
        # convert to Jacob - solve quadratic
        a = -1/(2*sat_thickness_array[year])
        b = 1
        c = -s_theis_avg
        
        root_1 = (-b + (b**2 - 4 * a * c) ** 0.5)/(2*a) 
        root_2 = (-b - (b**2 - 4 * a * c) ** 0.5)/(2*a) 
        
        s_jacob = root_1
        
        ########################### compute outputs ###########################

        drawdown = s_jacob
        total_head = s_jacob + DTW_array[year]
        volume_per_well = Well_Q_array[year] * 86400 * DAYS
        num_wells = selected_grid_df.Area[grid_cell]/well_area_array[year]
        volume_all_wells = volume_per_well * num_wells

        if year == 0:
            cumulative_volume_per_well = np.zeros(NUM_YEARS)
            cumulative_volume_all_wells = np.zeros(NUM_YEARS)
            cumulative_volume_per_well[year] = volume_per_well
            cumulative_volume_all_wells[year] =  volume_all_wells
        else:
            cumulative_volume_per_well[year] = volume_per_well + cumulative_volume_per_well[year-1]
            cumulative_volume_all_wells[year] =  volume_all_wells + cumulative_volume_all_wells[year-1]
       
        depleted_volume_fraction = cumulative_volume_all_wells[year]/available_volume
        well_installation_cost = well_unit_cost * well_length
        annual_capital_cost = well_installation_cost * ((1 + INTEREST_RATE) ** WELL_LIFETIME) * INTEREST_RATE/((1 + INTEREST_RATE) ** WELL_LIFETIME-1)
        maintenance_cost = MAINTENANCE_RATE * well_installation_cost
        nonenergy_cost = annual_capital_cost + maintenance_cost
        power = (SPECIFIC_WEIGHT * total_head * Well_Q_array[year]/EFFICIENCY)/1000 # kW 
        energy = power * (DAYS * 24) # kWh/year 
        energy_cost_rate = ELECTRICITY_RATE # $ per kWh
        energy_cost = energy * energy_cost_rate # $/year
        total_cost_per_well = nonenergy_cost + energy_cost 
        total_cost_all_wells = num_wells * total_cost_per_well 
        unit_cost = total_cost_all_wells/volume_all_wells # $/m^3
        unit_cost_per_km3 = unit_cost * 10**9 # $/km^3
        unit_cost_per_acreft = unit_cost * 1233.48 # $/acft
        
        # update variable arrays for next annual pumping iteration
        if year != NUM_YEARS-1:
            Well_Q_array[year+1] = Well_Q_array[year]
            DTW_array[year+1] = DTW_array[year] + (volume_all_wells/grid_cell_area)/S
            sat_thickness_array[year+1] = total_thickness - DTW_array[year+1]
            T_array[year+1] = K * sat_thickness_array[year+1]
            well_roi_array[year+1] = well_roi_array[year]
            well_area_array[year+1] = well_area_array[year]
        
        ######################## save time step results #######################
        """
        ['iteration', 'year_number', 'DEPLETION_LIMIT', 'continent', 'country_name', 
        'gcam_basin_id', 'gcam_basin_name', 'well_id', 'grid_area', 'permeability', 'storativity', 
        'total_thickness', 'depth_to_piez_surface', 'orig_aqfr_sat_thickness', 'aqfr_sat_thickness', 
        'hydraulic_conductivity', 'transmissivity', 'radius_of_influence', 'areal_extent', 
        'max_drawdown', 'drawdown', 'drawdown_interference', 'total_head', 'well_yield', 'volume_produced_perwell', 
        'cumulative_vol_produced_perwell', 'number_of_wells', 'volume_produced_allwells', 
        'cumulative_vol_produced_allwells', 'available_volume', 'depleted_vol_fraction', 
        'well_installation_cost', 'annual_capital_cost', 'maintenance_cost', 'nonenergy_cost', 
        'power', 'energy', 'energy_cost_rate', 'energy_cost', 'total_cost_perwell', 'total_cost_allwells', 
        'unit_cost', 'unit_cost_per_km3', 'unit_cost_per_acreft', 'whyclass', 'total_well_length']
        """
        
        outputs = str(1) + ', ' + \
                  str(year+1) + ', ' + \
                  str(DEPLETION_LIMIT) + ', ' + \
                  str(selected_grid_df.Continent[grid_cell]) + ', ' + \
                  str(selected_grid_df.CNTRY_NAME[grid_cell]) + ', ' + \
                  str(int(selected_grid_df.GCAM_ID[grid_cell])) + ', ' + \
                  str(selected_grid_df.Basin_Name[grid_cell]) + ', ' + \
                  str('Well_ID') + ', ' + \
                  str(grid_cell_area) + ', ' + \
                  str(selected_grid_df.Permeability[grid_cell]) + ', ' + \
                  str(selected_grid_df.Porosity[grid_cell]) + ', ' + \
                  str(selected_grid_df.Thickness[grid_cell]) + ', ' + \
                  str(DTW_array[year]) + ', ' + \
                  str(initial_sat_thickness) + ', ' + \
                  str(sat_thickness_array[year]) + ', ' + \
                  str(K) + ', ' + \
                  str(T_array[year]) + ', ' + \
                  str(well_roi_array[year]) + ', ' + \
                  str(well_area_array[year]) + ', ' + \
                  str('Max_Drawdown') + ', ' + \
                  str(drawdown) + ', ' + \
                  str(s_interference_avg) + ', ' + \
                  str(total_head) + ', ' + \
                  str(Well_Q_array[year]) + ', ' + \
                  str(volume_per_well) + ', ' + \
                  str(cumulative_volume_per_well[year]) + ', ' + \
                  str(num_wells) + ', ' + \
                  str(volume_all_wells) + ', ' + \
                  str(cumulative_volume_all_wells[year]) + ', ' + \
                  str(available_volume) + ', ' + \
                  str(depleted_volume_fraction) + ', ' + \
                  str(well_installation_cost) + ', ' + \
                  str(annual_capital_cost) + ', ' + \
                  str(maintenance_cost) + ', ' + \
                  str(nonenergy_cost) + ', ' + \
                  str(power) + ', ' + \
                  str(energy) + ', ' + \
                  str(energy_cost_rate) + ', ' + \
                  str(energy_cost) + ', ' + \
                  str(total_cost_per_well) + ', ' + \
                  str(total_cost_all_wells) + ', ' + \
                  str(unit_cost) + ', ' + \
                  str(unit_cost_per_km3) + ', ' + \
                  str(unit_cost_per_acreft) + ', ' + \
                  str(selected_grid_df.WHYClass[grid_cell]) + ', ' + \
                  str(selected_grid_df.Thickness[grid_cell]) 
                    
        if grid_cell == 0 and year == 0 :
            file = open(output_path + "\\" + output_name + '.csv', 'w') 
            file.write(str(header_column_names))
            file.write('\n')
            file.write(outputs)
            file.write('\n')
            file.close()
    
        else:
            file = open(output_path + "\\" + output_name + '.csv','a')
            file.write(outputs)
            file.write('\n')
            file.close()
    
    
    
#%% 


# u_vals = [0.00001, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 0.6, 1, 2, 3, 4, 5]
# W = np.zeros(len(u_vals))
# for i, u in enumerate(u_vals): 
#     W[i] = -0.57721 - math.log(u) + u - u**2/(2*2)

# import matplotlib.pyplot as plt
# plt.scatter(u_vals, W)
# plt.xscale('log')

# %%
