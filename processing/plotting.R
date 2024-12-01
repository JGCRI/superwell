# Plots key figures for the superwell manuscript
#
# Hassan Niazi, June 2023

# load libraries ----
{
  library(tidyverse)
  library(sf)
  library(ggplot2)
  # library(tmap)
  library(RColorBrewer)
  library(psych)
}

# load data ----
{
df_in <- read_csv("inputs/inputs.csv")
mappings_all <- read_csv("processing/basin_country_region_mapping.csv")
sf_in <- st_read("inputs/shapefiles/inputs.shp") %>% st_make_valid()

colnames(sf_in) <- c(colnames(df_in), "geometry") # correct col names in sf

df_in_R <- read_csv("inputs/inputs_recharge.csv")
sf_in_R <- st_read("inputs/shapefiles/inputs_recharge.shp") %>% st_make_valid()
colnames(sf_in_R) <- c(colnames(df_in_R), "geometry")

figs_dir <- "processing/figures/notimelimit/"
out_dir <- "outputs/superwell_py_deep_all_"

# main scenario, to load lesser rows n_max = 1000
df_0.3PD_0.25DL <- read_csv(paste0(out_dir, '0.3PD_0.25DL.csv')) %>% rename("GridCellID" = "grid_id")
print(paste0("Number of Grid Cells Processed: ", length(unique(df_0.3PD_0.25DL$GridCellID)),
             " out of ", length(unique(df_in$GridCellID)),
                                " (", round(length(unique(df_0.3PD_0.25DL$GridCellID))*100/length(unique(df_in$GridCellID)), 1), "%)"))

# base plot format
my_theme <- function () {
  theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      # axis.title = element_text(face="bold"),
      legend.position = c(0.91, 0.12),
      # legend.direction = "horizontal",
      legend.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      plot.tag = element_text(),
      plot.tag.position = c(0.01 , 0.99),
      plot.margin = margin(t = 1,  # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 3)  # Left margin
    )
}
saveformat <- ".png"
}

# read in all scenarios ----
# only read in for cost curves and volume statistics
{
df_0.3PD_0.05DL <- read_csv(paste0(out_dir, '0.3PD_0.05DL.csv')) %>% rename("GridCellID" = "grid_id")
df_0.3PD_0.4DL <- read_csv(paste0(out_dir, '0.3PD_0.4DL.csv')) %>% rename("GridCellID" = "grid_id")
df_0.6PD_0.05DL <- read_csv(paste0(out_dir, '0.6PD_0.05DL.csv')) %>% rename("GridCellID" = "grid_id")
df_0.6PD_0.25DL <- read_csv(paste0(out_dir, '0.6PD_0.25DL.csv')) %>% rename("GridCellID" = "grid_id")
df_0.6PD_0.4DL <- read_csv(paste0(out_dir, '0.6PD_0.4DL.csv')) %>% rename("GridCellID" = "grid_id")
}

# make a list of all scenarios
# scen_list <- c("0.3PD_0.05DL", "0.3PD_0.25DL", "0.3PD_0.4DL", "0.6PD_0.05DL", "0.6PD_0.25DL", "0.6PD_0.4DL")
# scen_names_list <- c("0.3PD_0.05DL", "0.3PD_0.25DL", "0.3PD_0.4DL", "0.6PD_0.05DL", "0.6PD_0.25DL", "0.6PD_0.4DL")
scen_names_list_labels <- c("0.3PD_0.05DL", "0.3PD_0.25DL", "0.3PD_0.4DL", "0.6PD_0.05DL", "0.6PD_0.25DL", "0.6PD_0.4DL")

# scen_list <- list("0.3PD_0.05DL" = df_0.3PD_0.05DL,
#                   "0.3PD_0.25DL" = df_0.3PD_0.25DL,
#                   "0.3PD_0.4DL" = df_0.3PD_0.4DL,
#                   "0.6PD_0.05DL" = df_0.6PD_0.05DL,
#                   "0.6PD_0.25DL" = df_0.6PD_0.25DL,
#                   "0.6PD_0.4DL" = df_0.6PD_0.4DL)

# output data ----
df_0.3PD_0.25DL %>% left_join(select(sf_in, c("GridCellID", "geometry")), by = "GridCellID") -> df_out

# clean data

# # determine unique combinations of country and GridCellID in df and in df_in
# df %>% as.data.frame() %>% select(COUNTRY, OriginalOb) %>% unique() -> df_unique
# df_in %>% select(CNTRY_NAME, OBJECTID) %>% unique() -> df_in_unique
# # determine the difference in both unique dataframes df and df_in
# df_unique %>% anti_join(df_in_unique, by = c("COUNTRY" = "CNTRY_NAME", "OriginalOb" = "OBJECTID")) -> df_diff


# high-level analysis on volume ----

df_in %>%  mutate(available_volume_allcells = Porosity * Grid_area * (Aquifer_thickness - Depth_to_water),
                  ponded_depth_avail = Porosity * (Aquifer_thickness - Depth_to_water)) -> df_in_vol


df_in %>%  mutate(Aquifer_thickness = case_when(Aquifer_thickness > 1000 ~ 1000,
                                                TRUE ~ Aquifer_thickness),
                  available_volume_allcells = Porosity * Grid_area * (Aquifer_thickness - Depth_to_water),
                  ponded_depth_avail = Porosity * (Aquifer_thickness - Depth_to_water)) -> df_in_vol_screened

# start analysis
#
# plot(df_in_vol$available_volume_allcells, df_in_vol$ponded_depth_avail)
{
  # global available volume
  print(paste0("Global Available Volume from inputs = ",
               round(sum(unique(df_in_vol$available_volume_allcells)) * 1e-15, 3), " million km3"))

  print(paste0("Global Available Volume from screened inputs = ",
               round(sum(unique(df_in_vol_screened$available_volume_allcells)) * 1e-15, 3), " million km3"))

  print(paste0("Global Accessible Volume in 0.3PD 0.05DL = ",
               round(sum(unique(df_0.3PD_0.05DL$available_volume)) * 1e-15, 3), " million km3"))
  print(paste0("Global Accessible Volume in 0.3PD 0.25DL = ",
               round(sum(unique(df_0.3PD_0.25DL$available_volume)) * 1e-15, 3), " million km3"))
  print(paste0("Global Accessible Volume in 0.3PD 0.40DL = ",
               round(sum(unique(df_0.3PD_0.4DL$available_volume)) * 1e-15, 3), " million km3"))
  print(paste0("Global Accessible Volume in 0.6PD 0.05DL = ",
               round(sum(unique(df_0.6PD_0.05DL$available_volume)) * 1e-15, 3), " million km3"))
  print(paste0("Global Accessible Volume in 0.6PD 0.25DL = ",
               round(sum(unique(df_0.6PD_0.25DL$available_volume)) * 1e-15, 3), " million km3"))
  print(paste0("Global Accessible Volume in 0.6PD 0.40DL = ",
               round(sum(unique(df_0.6PD_0.4DL$available_volume)) * 1e-15, 3), " million km3"))

  # global volume produced
  print(paste0("Global Produced Volume in 0.3PD 0.05DL = ",
               round(sum(df_0.3PD_0.05DL$volume_produced_allwells) * 1e-15, 4), " million km3"))
  print(paste0("Global Produced Volume in 0.3PD 0.25DL = ",
               round(sum(df_0.3PD_0.25DL$volume_produced_allwells) * 1e-15, 4), " million km3"))
  print(paste0("Global Produced Volume in 0.3PD 0.40DL = ",
               round(sum(df_0.3PD_0.4DL$volume_produced_allwells) * 1e-15, 4), " million km3"))
  print(paste0("Global Produced Volume in 0.6PD 0.05DL = ",
               round(sum(df_0.6PD_0.05DL$volume_produced_allwells) * 1e-15, 4), " million km3"))
  print(paste0("Global Produced Volume in 0.6PD 0.25DL = ",
               round(sum(df_0.6PD_0.25DL$volume_produced_allwells) * 1e-15, 4), " million km3"))
  print(paste0("Global Produced Volume in 0.6PD 0.40DL = ",
               round(sum(df_0.6PD_0.4DL$volume_produced_allwells) * 1e-15, 4), " million km3"))
}



## summary stats of available, accessible and pumped volumes ----

# since there are duplicate available vol values, we need to group by grid cell before unique
length(unique(df_in_vol$GridCellID))
length(unique(df_in_vol$available_volume_allcells))

global_avail <- df_in_vol %>% select(GridCellID, available_volume_allcells) %>% group_by(GridCellID) %>% unique() %>% ungroup() %>%
  summarise(available = sum(available_volume_allcells) * 1e-15)

# define get function to get the data frame from the environment
get <- function(x) eval(as.name(x), envir = .GlobalEnv)

# function to calculate accessible and pumped volumes
calculate_stats <- function(scen_list) {
  vols <- data.frame(scenario = character(),
                      accessible = numeric(),
                      pumped = numeric()
                      )

  scen_names_list <- scen_list

    for (scenario in scen_names_list) {
    # get the data frame loaded in the environment if it is equal to scenario name
    df <- get(paste0("df_", scenario))

    # length(unique(df$available_volume)) - nrow(df %>% select(GridCellID, available_volume) %>% group_by(GridCellID) %>% unique())

    # accessible <- sum(unique(df$available_volume)) * 1e-15 # assuming every grid cell has a different accessible volume
    accessible <- as.numeric(df %>% select(GridCellID, available_volume) %>% group_by(GridCellID) %>% unique() %>% ungroup() %>%
      summarise(available = sum(available_volume) * 1e-15))

    # length(unique(df_0.6PD_0.4DL$GridCellID)) - length(unique(df_0.6PD_0.4DL$volume_produced_allwells))
    pumped <- sum(df$volume_produced_allwells) * 1e-15
    # pumped <- df %>% select(GridCellID, cumulative_vol_produced_allwells) %>% group_by(GridCellID) %>% filter(cumulative_vol_produced_allwells == max(cumulative_vol_produced_allwells)) %>% ungroup() %>% summarise(pumped = sum(cumulative_vol_produced_allwells) * 1e-12)

    vols <- as.data.frame(rbind(vols, data.frame(scenario, accessible, pumped)))
  }
  return(vols)
}

# Getting fractions
fractions <- calculate_stats(scen_names_list_labels) %>%
  mutate(available = global_avail$available,
         accessible_perc = (accessible*100)/available,
         pumped_perc = (pumped*100)/available,
         pumped_accessible_perc = (pumped*100)/accessible)
print(fractions)


# write fraction as a csv
write.csv(fractions, "outputs/fractions.csv", row.names = FALSE)

# mean and std dev across scenarios
sapply(fractions[, -1], function(x) c(mean = mean(x), sd = sd(x)))



# timeseries by scenario ----
# some tests
df_0.3PD_0.05DL %>% select(year_number, volume_produced_allwells, total_cost_allwells, unit_cost) %>%
  mutate(unit_cost_calc = total_cost_allwells/volume_produced_allwells,
         diff_uc = unit_cost_calc - unit_cost) -> a

a %>% group_by(year_number) %>%
  summarise(global_vol_prod = sum(volume_produced_allwells),
            global_total_cost = sum(total_cost_allwells),
            global_unit_cost = mean(unit_cost)) %>%
  mutate(global_unit_cost_calc = global_total_cost/global_vol_prod,
         diff_uc_global = global_unit_cost_calc - global_unit_cost) -> b

plot(b$year_number, b$global_vol_prod, type = "l", xlab = "Year", ylab = "Global Volume Produced (km3)")
plot(b$year_number, b$global_total_cost, type = "l", xlab = "Year", ylab = "Global Total Cost ($)")
plot(b$year_number, b$global_unit_cost, type = "l", xlab = "Year", ylab = "Global Unit Cost ($/km3)")
plot(b$year_number, b$global_unit_cost_calc, type = "l", xlab = "Year", ylab = "Global Unit Cost ($/km3)")

# plot drawdown vs depth to water to see the difference between water depth decrease at well head and over water table decrease
plot(df_0.3PD_0.05DL$depth_to_water, df_0.3PD_0.05DL$drawdown, xlab = "Depth to Water (m)", ylab = "Drawdown (m)", pch = 20, cex = 0.5)

# global timeseries df by scenario
# function to calculate global timeseries df
calculate_global_timeseries <- function(scen_list) {
  global_timeseries <- data.frame()

  for (scenario in scen_list) {
    # get the data frame loaded in the environment if it is equal to scenario name
    df <- get(paste0("df_", scenario))

    # calculate global timeseries df
    df %>% group_by(year_number) %>%
      summarise(global_vol_prod = sum(volume_produced_allwells) * 1e-9,
                global_dep_vol_frac_avg = mean(depleted_vol_fraction),

                global_aqfr_sat_thickness_avg = mean(aqfr_sat_thickness),
                global_transmissivity_avg = mean(transmissivity),

                global_depth_to_water_avg = mean(depth_to_water),
                global_drawdown_avg = mean(drawdown),
                global_total_head_avg = mean(total_head),
                global_total_well_length_avg = mean(total_well_length),

                global_energy = sum(energy),
                global_energy_avg = mean(energy),
                global_power = sum(power),
                global_power_avg = mean(power),

                global_energy_cost = sum(energy_cost),
                global_energy_cost_avg = mean(energy_cost),
                global_nonenergy_cost = sum(nonenergy_cost),
                global_nonenergy_cost_avg = mean(nonenergy_cost),

                global_number_of_wells_avg = mean(number_of_wells),
                global_well_installation_cost_avg = mean(well_installation_cost),
                global_annual_capital_cost_avg = mean(annual_capital_cost),
                global_maintenance_cost_avg = mean(maintenance_cost),


                global_total_cost_perwell = sum(total_cost_perwell),
                global_total_cost_perwell_avg = mean(total_cost_perwell),
                global_total_cost = sum(total_cost_allwells),
                global_total_cost_avg = mean(total_cost_allwells),

                energy2nonenergy_ratio = sum(energy_cost)/sum(nonenergy_cost),
                global_unit_cost_avg = mean(unit_cost)) %>%
      mutate(scenario = scenario) -> df

    global_timeseries <- as.data.frame(rbind(global_timeseries, df))
  }
  return(global_timeseries)
}

global_timeseries <- calculate_global_timeseries(scen_names_list_labels)

# make sure scenarios are not reordered
global_timeseries$scenario <- factor(global_timeseries$scenario,
                                        levels = c("0.3PD_0.05DL", "0.6PD_0.05DL", "0.3PD_0.25DL",
                                                   "0.6PD_0.25DL", "0.3PD_0.4DL", "0.6PD_0.4DL"))


# plot timeseries of all variables in global_timeseries as figure. x = year_number, y = variable, color = scenario
global_timeseries_long <- tidyr::pivot_longer(global_timeseries, cols = -c(year_number, scenario), names_to = "variable", values_to = "value")

ggplot(global_timeseries_long, aes(x = year_number, y = value, color = scenario, linetype = scenario)) +
  geom_line(size = 1) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Time Series of Global Variables", x = "Year", y = "Value") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  my_theme() +
  theme(legend.position = c(0.875, 0.06), legend.box = "horizontal")

ggsave(paste0(figs_dir, "timeseries/global_timeseries.png"), width = 16, height = 9, units = "in", dpi = 300)


# plot(global_timeseries$year_number, global_timeseries$energy2nonenergy_ratio, type = "l", xlab = "Year", ylab = "Global Energy to Nonenergy Cost Ratio")

# function to plot timeseries by scenario. Arguments y variable, y label, and legend position
plot_timeseries <- function(y_var, multiplier, y_lab, legend_pos, tag, logbool = F) {
  gg <- ggplot(global_timeseries) +
    geom_line(aes(x = year_number, y = get(y_var) * multiplier, color = scenario, linetype = scenario), linewidth = 1) +
    labs(x = 'Pumping Year', y = y_lab, color = "Scenario", linetype = "Scenario", tag = tag) +
    # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.01)) + # Setting breaks at squares and labels at the square roots
    # scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
    # annotation_logticks(base = 10, sides = "b") +
    scale_color_brewer(palette = "Paired") +
    scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      # axis.title = element_text(face="bold"),
      legend.position = if (legend_pos == "r") {c(0.875, 0.78)} else {c(0.125, 0.78)}, # c(0.875, 0.78) for right  c(0.125, 0.78) for left
      # legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      plot.tag = element_text(),
      plot.tag.position = c(0.01 , 0.99),
      legend.key.size =  unit(0.2, "in"),
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.4, "in"),
      plot.margin = margin(t = 1,  # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 3)  # Left margin
    )

  if (logbool == T) {
    gg <- gg + scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
       annotation_logticks(base = 10, sides = "b")
  }
  ggsave(paste0(figs_dir, "timeseries/time_", y_var, "_all_scen.png"), width = 6, height = 4, units = "in", dpi = 300)
  return(gg)
}


{ # for SI

  # global_vol_prod = sum(volume_produced_allwells) * 1e-9,
  # global_dep_vol_frac_avg = mean(depleted_vol_fraction),
  #
  # global_aqfr_sat_thickness_avg = mean(aqfr_sat_thickness),
  # global_transmissivity_avg = mean(transmissivity),
  #
  # global_depth_to_water_avg = mean(depth_to_water),
  # global_drawdown_avg = mean(drawdown),
  # global_total_head_avg = mean(total_head),
  # global_total_well_length_avg = mean(total_well_length),
  #
  # global_energy = sum(energy),
  # global_energy_avg = mean(energy),
  # global_power = sum(power),
  # global_power_avg = mean(power),
  #
  # global_energy_cost = sum(energy_cost),
  # global_energy_cost_avg = mean(energy_cost),
  #
  # global_number_of_wells_avg = mean(number_of_wells),
  # global_well_installation_cost_avg = mean(well_installation_cost),
  # global_annual_capital_cost_avg = mean(annual_capital_cost),
  # global_maintenance_cost_avg = mean(maintenance_cost),
  # global_nonenergy_cost = sum(nonenergy_cost),
  # global_nonenergy_cost_avg = mean(nonenergy_cost),
  #
  # global_total_cost_perwell = sum(total_cost_perwell),
  # global_total_cost_perwell_avg = mean(total_cost_perwell),
  # global_total_cost = sum(total_cost_allwells),
  # global_total_cost_avg = mean(total_cost_allwells),
  #
  # energy2nonenergy_ratio = sum(energy_cost)/sum(nonenergy_cost),
  # global_unit_cost_avg = mean(unit_cost)

  # volume and hydraulics
  plot_timeseries("global_vol_prod", 1e-3, expression('Global Volume Produced (1,000 km'^3*')'), "r", "(a)", T)
  plot_timeseries("global_dep_vol_frac_avg", 1, expression('Depleted Volume Fraction (-)'), "l", "(b)", T)

  # T = Kb
  plot_timeseries("global_aqfr_sat_thickness_avg", 1, expression('Aquifer Saturated Thickness (m)'), "l", "(a)", T)
  plot_timeseries("global_transmissivity_avg", 86400, expression('Transmissivity (m'^2*'/day)'), "r", "(b)", T)

  # depths
  plot_timeseries("global_depth_to_water_avg", 1, expression('Global Mean Depth to Water (m)'), "l", "(a)")
  plot_timeseries("global_drawdown_avg", 1, expression('Global Mean Drawdown (m)'), "r", "(b)")
  plot_timeseries("global_total_head_avg", 1, expression('Global Mean Total Head (m)'), "l", "(c)")
  plot_timeseries("global_total_well_length_avg", 1, expression('Global Mean Total Well Length (m)'), "l", "(d)")

  # energy and power
  plot_timeseries("global_energy", 1e-12, expression('Global Energy (million GWh)'), "r", "(a)", T) # K to giga, million
  plot_timeseries("global_energy_avg", 1e-6, expression('Global Mean Energy (GWh)'), "l", "(b)", T)
  plot_timeseries("global_power", 1e-6, expression('Global Power (GW)'), "r", "(c)", T)
  plot_timeseries("global_power_avg", 1e-6, expression('Global Mean Power (GW)'), "l", "(d)", T)

  # energy and non energy costs
  plot_timeseries("global_energy_cost", 1e-12, expression('Global Energy Cost (trillion $)'), "r", "(a)", T)
  plot_timeseries("global_energy_cost_avg", 1e-6, expression('Global Mean Energy Cost (million $)'), "l", "(b)", T)
  plot_timeseries("global_nonenergy_cost", 1e-12, expression('Global Nonenergy Cost (trillion $)'), "r", "(c)", T)
  plot_timeseries("global_nonenergy_cost_avg", 1e-6, expression('Global Mean Nonenergy Cost (million $)'), "l", "(d)", T)

  # nonenergy costs components
  plot_timeseries("global_number_of_wells_avg", 1e-3,  expression('Global Mean Number of Wells (x1000)'), "r", "(a)", F)
  plot_timeseries("global_well_installation_cost_avg", 1e-6, expression('Global Mean Well Installation Cost (million $)'), "l", "(b)", F)
  plot_timeseries("global_annual_capital_cost_avg", 1e-6, expression('Global Mean Annual Capital Cost (million $)'), "r", "(c)", F)
  plot_timeseries("global_maintenance_cost_avg", 1e-6, expression('Global Mean Maintenance Cost (million $)'), "r", "(d)", F)

  # total costs
  plot_timeseries("global_total_cost_perwell", 1e-6, expression('Global Total Cost per Well (million $)'), "r", "(a)", T)
  plot_timeseries("global_total_cost_perwell_avg", 1e-6, expression('Global Mean Total Cost per Well (million $)'), "l", "(b)", T)
  plot_timeseries("global_total_cost", 1e-12, expression('Global Total Cost (trillion $)'), "r", "(c)", T)
  plot_timeseries("global_total_cost_avg", 1e-6, expression('Global Mean Total Cost (million $)'), "l", "(d)", T)

  # cost ratios
  plot_timeseries("energy2nonenergy_ratio", 1, expression('Global Energy to Nonenergy Cost Ratio (-)'), "l", "(a)")
  plot_timeseries("global_unit_cost_avg", 1, expression('Global Mean Unit Cost ($/m'^3*')'), "l", "(b)")

}

# IGNORE THE FOLLOWING. THE FUNCTION ABOVE IS NOW USED TO GENERATE THE PLOTS
### timeseries volume produced by scenario ----
ggplot(global_timeseries) +
  geom_line(aes(x = year_number, y = global_vol_prod * 1e-3, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Volume Produced (1,000 km'^3*')'), color = "Scenario", linetype = "Scenario", tag = "(c)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.01)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    legend.position = c(0.875, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_vol_prod_all_scen.png"), width = 6, height = 4, units = "in", dpi = 300)

# cumulative volume produced
ggplot(global_timeseries %>%
         group_by(scen) %>% mutate(global_vol_prod_cumsum = cumsum(global_vol_prod) * 1e-6)) +
  geom_line(aes(x = year_number, y = global_vol_prod_cumsum, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Cumulative Volume Produced (mln km'^3*')'), color = "Scenario", linetype = "Scenario", tag = "(d)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.1)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    legend.position = c(0.125, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_vol_prod_all_scen_cumulative.png"), width = 6, height = 4, units = "in", dpi = 300)


### timeseries energy to nonenergy cost ratio by scenario ----
ggplot(global_timeseries) +
  geom_line(aes(x = year_number, y = energy2nonenergy_ratio, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Energy to Nonenergy Cost Ratio)'), color = "Scenario", linetype = "Scenario", tag = "(c)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.01)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    legend.position = c(0.125, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_energy2nonenergy_ratio_all_scen.png"), width = 6, height = 4, units = "in", dpi = 300)

### timeseries total cost by scenario ----
ggplot(global_timeseries) +
  geom_line(aes(x = year_number, y = global_total_cost * 1e-9, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Total Cost (billion $)'), color = "Scenario", linetype = "Scenario", tag = "(c)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.01)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    legend.position = c(0.875, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_total_cost_all_scen.png"), width = 6, height = 4, units = "in", dpi = 300)

# cumulative volume produced
ggplot(global_timeseries %>%
         group_by(scen) %>% mutate(global_total_cost_cumsum = cumsum(global_total_cost) * 1e-12)) +
  geom_line(aes(x = year_number, y = global_total_cost_cumsum, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Cumulative Total Cost (trillion $)'), color = "Scenario", linetype = "Scenario", tag = "(d)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.1)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    legend.position = c(0.125, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_total_cost_all_scen_cumulative.png"), width = 6, height = 4, units = "in", dpi = 300)



### timeseries unit cost ----
ggplot(global_timeseries) +
  geom_line(aes(x = year_number, y = global_unit_cost, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Unit Cost ($/m'^3*')'), color = "Scenario", linetype = "Scenario", tag = "(c)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.01)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    # legend.position = c(0.875, 0.78),
    legend.position = c(0.125, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_unit_cost_all_scen.png"), width = 6, height = 4, units = "in", dpi = 300)


# cumulative unit cost
# this probably doesn't mean anything in reality
ggplot(global_timeseries %>%
         group_by(scen) %>% mutate(global_unit_cost_cumsum = cumsum(global_unit_cost))) +
  geom_line(aes(x = year_number, y = global_unit_cost_cumsum, color = scen, linetype = scen), linewidth = 1) +
  labs(x = 'Pumping Year', y = expression('Global Cumulative Unit Cost ($/m'^3*')'), color = "Scenario", linetype = "Scenario", tag = "(d)") +
  # scale_x_sqrt(breaks = c(1, 5, 10, 25, 50, 100, 300), expand = c(0.01,0.1)) + # Setting breaks at squares and labels at the square roots
  scale_x_log10(expand = c(0, 0.035), breaks = scales::log_breaks(n = 10)) + #-0.12
  annotation_logticks(base = 10, sides = "b") +
  scale_color_brewer(palette = "Paired") +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # axis.title = element_text(face="bold"),
    legend.position = c(0.125, 0.78),
    # legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    plot.tag = element_text(),
    plot.tag.position = c(0.01 , 0.99),
    legend.key.size =  unit(0.2, "in"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(0.4, "in"),
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 3)  # Left margin
  )

ggsave(paste0(figs_dir, "global_unit_cost_all_scen_cumulative.png"), width = 6, height = 4, units = "in", dpi = 300)


# stacked bar charts of costs over time ----
df_0.3PD_0.25DL %>%
  select(year_number, number_of_wells, annual_capital_cost, maintenance_cost, energy_cost) %>%
  mutate(annual_capital_cost = annual_capital_cost/number_of_wells,
         maintenance_cost = maintenance_cost/number_of_wells,
         energy_cost = energy_cost/number_of_wells) %>%
  group_by(year_number) %>% summarise_all(mean) %>% ungroup() %>% mutate(scen = "0.3PD_0.25DL") %>%
  pivot_longer(cols = c(annual_capital_cost, maintenance_cost, energy_cost), names_to = "cost_type", values_to = "cost") %>%
  mutate(cost_type = case_when(cost_type == "annual_capital_cost" ~ "Capital",
                               cost_type == "maintenance_cost" ~ "Maintenance",
                               cost_type == "energy_cost" ~ "Energy"),
         cost_type = factor(cost_type, levels = c("Capital", "Maintenance", "Energy")) %>% fct_rev()) %>%
  ggplot() +
  geom_bar(aes(fill = cost_type, y = cost, x = year_number), position = "stack", stat = "identity") +
  labs(x = 'Pumping Year', y = expression('Global Annual Average Cost per Well ($/well)'), fill = "Cost Type", tag = "(c)") +
  scale_fill_manual(values = c("#00b0f0", "#f4aa00", "#c10435")) + my_theme() +
  theme(legend.position = c(0.125, 0.85),
        legend.title = element_text(face = "bold"))

ggsave(paste0(figs_dir, "global_annual_cost_perwell_0.3PD_0.25DL.png"), width = 6, height = 4, units = "in", dpi = 300)


# one grid cell
df_0.3PD_0.25DL %>% filter(GridCellID == "39689") %>% # 72548 for deepen and added wells, 12422 south africa for 199th year, US 96934, Amu Darya 39689 nice balanced graph, Nile 19565
  select(year_number, number_of_wells, annual_capital_cost, maintenance_cost, energy_cost) %>%
  mutate(annual_capital_cost = annual_capital_cost/number_of_wells,
         maintenance_cost = maintenance_cost/number_of_wells,
         energy_cost = energy_cost/number_of_wells) %>%
  group_by(year_number) %>% summarise_all(mean) %>% ungroup() %>% mutate(scen = "0.3PD_0.25DL") %>%
  pivot_longer(cols = c(annual_capital_cost, maintenance_cost, energy_cost), names_to = "cost_type", values_to = "cost") %>%
  mutate(cost_type = case_when(cost_type == "annual_capital_cost" ~ "Capital",
                               cost_type == "maintenance_cost" ~ "Maintenance",
                               cost_type == "energy_cost" ~ "Energy"),
         cost_type = factor(cost_type, levels = c("Capital", "Maintenance", "Energy")) %>% fct_rev()) %>%
  ggplot() +
  geom_bar(aes(fill = cost_type, y = cost, x = year_number), position = "stack", stat = "identity") +
  labs(x = 'Pumping Year', y = expression('Grid Annual Cost per Well ($/well)'), fill = "Cost Type", tag = "(a)") +
  scale_fill_manual(values = c("#00b0f0", "#f4aa00", "#c10435")) + my_theme() +
  theme(legend.position = c(0.125, 0.85),
        legend.title = element_text(face = "bold"))

ggsave(paste0(figs_dir, "grid_annual_cost_perwell_0.3PD_0.25DL.png"), width = 6, height = 4, units = "in", dpi = 300)

# one grid with deepened and added wells
# select grid cell that has both added wells and deepening
df_0.3PD_0.25DL %>%
  # select(GridCellID, well_yield, number_of_wells) %>%
  select(GridCellID, year_number, well_yield, number_of_wells) %>%
  filter(year_number > 19) %>% select(-year_number) %>%
  # group_by(GridCellID) %>%
    unique() %>%
  group_by(GridCellID) %>% mutate(n = n()) %>% ungroup() %>% filter(n > 1)  -> df_addedwells
# why is that the cells which reduce the pumping rate don't go beyond 50 years?

df_0.3PD_0.25DL %>% filter(GridCellID == "82509") %>% # 82509 (seems to be better to use), 72548
  select(GridCellID, year_number, number_of_wells, well_yield, total_well_length, depth_to_water, total_head, annual_capital_cost, maintenance_cost, energy_cost, total_cost_allwells) -> df_deepen_addedwells

# write_csv(df_0.3PD_0.25DL %>% filter(GridCellID == "72548"), "df_0.3PD_0.25DL_grid_id_72548.csv")

# one grid cell
# read output file of one grid cell
df_0.3PD_0.25DL_Grid_72548 <- read_csv(paste0(out_dir, '0.3PD_0.25DL_Grid_72548.csv')) %>% rename("GridCellID" = "grid_id")
df_0.3PD_0.25DL_Grid_72548 <- df_0.3PD_0.25DL %>% filter(GridCellID == "72548")
# df_0.3PD_0.25DL
df_0.3PD_0.25DL %>% filter(GridCellID == "72548") %>% # 82509 3 pumping rates, 2450 added wells only, 72548 for deepen and added wells, 12422 south africa for 199th year, US 96934, Amu Darya 39689 nice balanced graph, Nile 19565
  #
  select(year_number, number_of_wells, annual_capital_cost, maintenance_cost, energy_cost) %>%
  mutate(annual_capital_cost = annual_capital_cost/number_of_wells,
         maintenance_cost = maintenance_cost/number_of_wells,
         energy_cost = energy_cost/number_of_wells) %>%
  group_by(year_number) %>% summarise_all(mean) %>% ungroup() %>% mutate(scen = "0.3PD_0.25DL") %>%
  pivot_longer(cols = c(annual_capital_cost, maintenance_cost, energy_cost), names_to = "cost_type", values_to = "cost") %>%
  mutate(cost_type = case_when(cost_type == "annual_capital_cost" ~ "Capital",
                               cost_type == "maintenance_cost" ~ "Maintenance",
                               cost_type == "energy_cost" ~ "Energy"),
         cost_type = factor(cost_type, levels = c("Capital", "Maintenance", "Energy")) %>% fct_rev()) %>%
  ggplot() +
  geom_bar(aes(fill = cost_type, y = cost, x = year_number), position = "stack", stat = "identity") +
  labs(x = 'Pumping Year', y = expression('Grid Annual Cost per Well ($/well)'), fill = "Cost Type", tag = "(b)") +
  scale_fill_manual(values = c("#00b0f0", "#f4aa00", "#c10435")) + my_theme() +
  theme(legend.position = c(0.125, 0.85),
        legend.title = element_text(face = "bold"))

# ggsave(paste0(figs_dir, "grid_annual_cost_perwell_0.3PD_0.25DL_addedwells.png"), width = 6, height = 4, units = "in", dpi = 300)

df_0.3PD_0.25DL_Grid_72548 %>% filter(GridCellID == "72548") %>% # 82509 3 pumping rates, 2450 added wells only, 72548 for deepen and added wells, 12422 south africa for 199th year, US 96934, Amu Darya 39689 nice balanced graph, Nile 19565
  #
  select(year_number, number_of_wells, annual_capital_cost, maintenance_cost, energy_cost) %>%
  # mutate(annual_capital_cost = annual_capital_cost/number_of_wells,
  #        maintenance_cost = maintenance_cost/number_of_wells,
  #        energy_cost = energy_cost/number_of_wells) %>%
  group_by(year_number) %>% summarise_all(mean) %>% ungroup() %>% mutate(scen = "0.3PD_0.25DL") %>%
  pivot_longer(cols = c(annual_capital_cost, maintenance_cost, energy_cost), names_to = "cost_type", values_to = "cost") %>%
  mutate(cost_type = case_when(cost_type == "annual_capital_cost" ~ "Capital",
                               cost_type == "maintenance_cost" ~ "Maintenance",
                               cost_type == "energy_cost" ~ "Energy"),
         cost_type = factor(cost_type, levels = c("Capital", "Maintenance", "Energy")) %>% fct_rev()) %>%
  ggplot() +
  geom_bar(aes(fill = cost_type, y = cost * 1e-6, x = year_number), position = "stack", stat = "identity") +
  labs(x = 'Pumping Year', y = expression('Grid Annual Costs (million $)'), fill = "Cost Type", tag = "(d)") +
  scale_fill_manual(values = c("#00b0f0", "#f4aa00", "#c10435")) + my_theme() +
  theme(legend.position = c(0.125, 0.85),
        legend.title = element_text(face = "bold"))

# ggsave(paste0(figs_dir, "grid_annual_cost_0.3PD_0.25DL_addedwells.png"), width = 6, height = 4, units = "in", dpi = 300)


# single well dynamics ----
df_single <- df_0.3PD_0.25DL_Grid_72548

plot_single_well <- function(data, x_var = "year_number", y_var, y_scale = 1, y_label, color = "red", tag = "") {
  gg <- ggplot(data, aes_string(x = x_var, y = paste0(y_var, " * ", y_scale))) +
    geom_point(color = color) +
    labs(x = "Pumping Year", y = y_label, tag = tag) + my_theme()

  ggsave(paste0(figs_dir, "singlecell/cell_", y_var, ".png"), plot = gg, width = 6, height = 4, units = "in", dpi = 300)

  return(gg)
}

{
  # depths
  plot_single_well(df_single, "year_number", "total_thickness", 1, expression('Total Aquifer Thickness (m)'), "blue2", "(a)")
  plot_single_well(df_single, "year_number", "depth_to_water", 1, expression('Depth to Water (m)'), "red2", "(b)")
  plot_single_well(df_single, "year_number", "orig_aqfr_sat_thickness", 1, expression('Original Aquifer Saturated Thickness (m)'), "green3", "(c)")
  plot_single_well(df_single, "year_number", "total_well_length", 1, expression('Total Well Length (m)'), "orange", "(d)")
  plot_single_well(df_single, "year_number", "aqfr_sat_thickness", 1, expression('Aquifer Saturated Thickness (m)'), "purple", "(e)")
  plot_single_well(df_single, "year_number", "total_head", 1, expression('Total Head (m)'), "brown", "(f)")

  # T=Kb
  plot_single_well(df_single, "year_number", "hydraulic_conductivity", 86400, expression('Hydraulic Conductivity (m/day)'), "red2", "(a)")
  plot_single_well(df_single, "year_number", "transmissivity", 86400, expression('Transmissivity (m'^2*'/day)'), "green4", "(b)")

  # well hydraulics
  plot_single_well(df_single, "year_number", "well_yield", 1, expression('Well Yield (m'^3*'/day)'), "orange2", "(a)")
  plot_single_well(df_single, "year_number", "areal_extent", 1, expression('Areal Extent (m'^2*')'), "royalblue2", "(b)")
  plot_single_well(df_single, "year_number", "number_of_wells", 1, 'Number of Wells', "red2", "(c)")
  plot_single_well(df_single, "year_number", "drawdown", 1, expression('Drawdown (m)'), "green3", "(d)")

  # volumes
  plot_single_well(df_single, "year_number", "volume_produced_perwell", 1e-6, expression('Volume Produced Per Well (million m'^3*')'), "blue3", "(a)")
  plot_single_well(df_single, "year_number", "cumulative_vol_produced_perwell", 1e-6, expression('Cumulative Volume Produced Per Well (million m'^3*')'), "red3", "(b)")
  plot_single_well(df_single, "year_number", "volume_produced_allwells", 1e-9, expression('Volume Produced All Wells (billion m'^3*')'), "green3", "(c)")
  plot_single_well(df_single, "year_number", "cumulative_vol_produced_allwells", 1e-9, expression('Cumulative Volume Produced All Wells (billion m'^3*')'), "orange", "(d)")
  plot_single_well(df_single, "year_number", "available_volume", 1e-9, expression('Available Volume (billion m'^3*')'), "purple", "(e)")
  plot_single_well(df_single, "year_number", "depleted_vol_fraction", 1, 'Depleted Volume Fraction', "brown", "(f)")

  # power and energy
  plot_single_well(df_single, "year_number", "power", 1e-6, expression('Power (GW)'), "olivedrab3", "(a)")
  plot_single_well(df_single, "year_number", "energy", 1e-6, expression('Energy (GWh)'), "darkorchid4", "(b)")

  # energy cost
  plot_single_well(df_single, "year_number", "energy_cost_rate", 1, expression('Electricity Cost Rate (USD/KWh)'), "darkorange", "(a)")
  plot_single_well(df_single, "year_number", "energy_cost", 1e-6, expression('Energy Cost (million $)'), "green", "(b)")

  # nonenergy cost components
  plot_single_well(df_single, "year_number", "well_installation_cost", 1e-6, expression('Well Installation Cost (million $)'), "darkslateblue", "(a)")
  plot_single_well(df_single, "year_number", "annual_capital_cost", 1e-6, expression('Annual Capital Cost (million $)'), "cadetblue", "(b)")
  plot_single_well(df_single, "year_number", "maintenance_cost", 1e-6, expression('Maintenance Cost (million $)'), "darkmagenta", "(c)")
  plot_single_well(df_single, "year_number", "nonenergy_cost", 1e-6, expression('Nonenergy Cost (million $)'), "khaki3", "(d)")

  # total costs
  plot_single_well(df_single, "year_number", "total_cost_perwell", 1e-6, expression('Total Cost Per Well (million $)'), "mediumpurple", "(a)")
  plot_single_well(df_single, "year_number", "total_cost_allwells", 1e-6, expression('Total Cost All Wells (million $)'), "hotpink4", "(b)")

  # unit cost
  plot_single_well(df_single, "year_number", "unit_cost", 1, expression('Unit Cost (USD/m'^3*')'), "chartreuse3", "(a)")
  plot_single_well(df_single, "year_number", "unit_cost_per_acreft", 1, expression('Unit Cost (USD/acre-ft)'), "darkolivegreen4", "(b)")
}


# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$energy_cost * 1e-6, type = "p", col = "red", xlab = "Pumping Year", ylab = "Energy Costs (million $)")
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$annual_capital_cost * 1e-6, type = "p", col = "blue2", xlab = "Pumping Year", ylab = "Capital Costs (million $)")
# # maintenance cost
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$maintenance_cost * 1e-6, type = "p", col = "green4", xlab = "Pumping Year", ylab = "Maintenance Costs (million $)")
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$nonenergy_cost * 1e-6, type = "p", col = "blue2", xlab = "Pumping Year", ylab = "Nonenergy Costs (million $)")
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$total_cost_allwells * 1e-6, type = "p", col = "green4", xlab = "Pumping Year", ylab = "Total Cost (million $)")
# # plot transmissivity
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$transmissivity * 86400, type = "p", col = "gold", xlab = "Pumping Year", ylab = "Transmissivity (m2/day)")
#
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$well_yield * 86400, type = "p", col = "dodgerblue", xlab = "Pumping Year", ylab = "Well Yield (m3/day)")
# plot(x = df_0.3PD_0.25DL_Grid_72548$year_number, y = df_0.3PD_0.25DL_Grid_72548$areal_extent * 1e-6, type = "p", col = "darkorange", xlab = "Pumping Year", ylab = "Well Area (km2)")

# plot all input variables -----------------------------------------------

# setting up the plot
plot_input <- sf_in_R %>% # filter(Country == "United States") %>%
  mutate(Grid_area_km = Grid_area/10^6, # convert to km2
         Aquifer_thickness = case_when(Aquifer_thickness > 1000 ~ 1000,
                                       TRUE ~ Aquifer_thickness)) %>% # replace all MEAN_thk_m values greater than 1000 with 1000
  select(c("Porosity", "Permeability", "Aquifer_thickness",
           "Depth_to_water", "Grid_area_km", "Recharge", "WHYClass")) # filter only relevant ones
  # rename(c("storativity" = "MEAN_Poros", "permeability" = "MEAN_Perme",
  #          "grid_area_km" = "CalcArea_m", "total_thickness" = "MEAN_thk_m",
  #          "depth_to_water" = "MEAN_Depth", "country_name" = "COUNTRY",
  #          "GridCellID" = "OriginalOb")) # rename to make the join with output smooth


# legendnames <- c("Porosity" = "Porosity (-)", "Permeability" = "Permeability (m2)",
legendnames <- c("Porosity" = "Porosity (-)", "Permeability" = expression('Permeability (m'^2*')'),
                 "Aquifer_thickness" = "Aquifer Thickness (m)", "Depth_to_water" = "Depth to Water (m)",
                 "Grid_area_km" = expression('Grid Area (km'^2*')'), "Recharge" = "Recharge (m/yr)", "WHYClass" = "WHY Class") # to label legend properly
# see all palettes available: tmaptools::palette_explorer()
pal <- c("Porosity" = "BuPu", "Permeability" = "YlGn", "Grid_area_km" = "BuGn",
         "Aquifer_thickness" = "YlOrRd", "Depth_to_water" = "Blues", "Recharge" = "Blues", "WHYClass" = "PuBuGn")

# max(plot_input$Permeability)

# plot all input variables using ggplot
for (plot in colnames(plot_input)[6:(length(plot_input) - 2)]) { # plot all input variables except last two
  ggplot(plot_input) +
    geom_sf(aes(fill = .data[[plot]]), lwd = 0, colour = NA) +
    scale_fill_gradientn(name = legendnames[plot],
                         colors = c(brewer.pal(9, pal[plot])),
                         values = scales::rescale(sort(c(min(plot_input[[plot]]),
                                                         kmeans(plot_input[[plot]], centers = 7)$center, # breaks by kmeans clustering
                                                         max(plot_input[[plot]]))))
                         ) +
    coord_sf(expand = FALSE) + # makes sure the plot doesn't extend beyond Earth (also plots y axis ticks and crops)
    theme_minimal() + theme(plot.margin = grid::unit(c(-4, 0, -4, 0), "cm"),
                            legend.justification = c(0,0), legend.position = c(0,0),
                            # legend.title = element_text(face = "bold"),
                            panel.grid = element_blank())

    # save each plot
    plotname <- paste0(figs_dir, "map_in_", plot, ".png")
    ggsave(plotname, width = 11, height = 3.5, units = "in", dpi = 300)
}



# for WHYClass
ggplot(plot_input) +
  geom_sf(lwd = 0, aes(fill =  as.factor(WHYClass))) +
  scale_fill_manual(name = "WHY Class", values = c("#377eb8", "#e41a1c", "#4daf4a")) +
  coord_sf(expand = FALSE) + # makes sure the plot doesn't extend beyond Earth (also plots y axis ticks and crops)
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = grid::unit(c(-4, 0, -4, 0), "cm"),
                          legend.justification = c(0,0), legend.position = c(0,0)
                          # legend.title = element_text(face = "bold")
                          )
ggsave(paste0(figs_dir, "map_in_WHYClass.png"), width = 11, height = 3.5, units = "in", dpi = 300)


# for recharge
ggplot(sf_in_R) +
  geom_sf(lwd = 0, aes(fill =  Recharge), lwd = 0, colour = NA) +
  scale_fill_gradientn(name = "Recharge (m/yr)",
                       colors = c(brewer.pal(9, "Blues")),
                       values = scales::rescale(sort(c(min(sf_in_R$Recharge),
                                                       kmeans(sf_in_R$Recharge, centers = 2)$center, # breaks by kmeans clustering
                                                       max(sf_in_R$Recharge))))
  ) +
  coord_sf(expand = FALSE) + # makes sure the plot doesn't extend beyond Earth (also plots y axis ticks and crops)
  theme_minimal() + theme(plot.margin = grid::unit(c(-4, 0, -4, 0), "cm"),
                          legend.justification = c(0,0), legend.position = c(0,0),
                          panel.grid = element_blank())

# save each plot
ggsave(paste0(figs_dir, "map_in_Recharge.png"), width = 11, height = 3.5, units = "in", dpi = 300)




# plot outputs #################################################################

add_geometry <- function (df) {
  df %>% left_join(select(sf_in, c("GridCellID", "geometry")), by = "GridCellID") %>% st_as_sf()
  }


# for old inputs and outputs
# df_0.3PD_0.25DL %>% filter(country_name %in% c("United States", "Canada")) %>% #%in% c("United States", "Canada")
#   left_join(select(input, c("country_name", "GridCellID", "grid_area_km", "permeability",
#                             "storativity", "depth_to_water", "total_thickness",
#                             "Shape_Leng", "Shape_Area", "geometry")),
#             by = c("country_name", "GridCellID")) %>%
#             select(-contains(".y")) %>%
#             rename_at(vars(contains(".x")), ~ str_remove(., ".x")) %>%
#   st_as_sf() -> df_out

# summary_df_0.3PD_0.25DL <- psych::describe(df_out)


# function to plot maps for outputs
plot_map <- function (plot_data, plot_var, legend_var, color_var, kcenters = 7) {
  # plot <-
  ggplot(plot_data) +
  geom_sf(aes(fill = .data[[plot_var]]), color = NA, lwd = 0) +
  scale_fill_gradientn(name = legend_var,
                       colors = c(brewer.pal(9, color_var)),
                       values = scales::rescale(sort(c(min(plot_data[[plot_var]]),
                                                       kmeans(plot_data[[plot_var]], centers = kcenters)$center, # breaks by kmeans clustering
                                                       max(plot_data[[plot_var]]))))) +
  coord_sf(expand = FALSE) + # makes sure the plot doesn't extend beyond Earth (also plots y axis ticks and crops)
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = grid::unit(c(-4, 0, -4, 0), "cm"),
                          legend.justification=c(0,0), legend.position=c(0,0)
                          # legend.title = element_text(face = "bold")
                          )

  # save each plot
  plotname <- paste0(figs_dir, "map_out_", plot_var, ".png")
  ggsave(plotname, width = 11, height = 3.5, units = "in", dpi = 300)

  # return(plot)
}

## available water map ----

### using inputs (df_in_vol_screened) and covert to ponded depth

# cells above 200 ponded depth
# plot(df_in_vol$ponded_depth_avail > 200, breaks = 100, col = brewer.pal(9, "Blues"), main = "Available Groundwater as Ponded Depth (m)")
# df_in_vol %>% filter(ponded_depth_avail > 200) -> ponded_200
# plot(x = ponded_200$ponded_depth_avail,
#      y = ponded_200$Aquifer_thickness,
#      col = brewer.pal(9, "Blues"), main = "Available Groundwater as Ponded Depth (m)")

plot_map(df_in_vol_screened %>% add_geometry() %>% select(c("GridCellID", "ponded_depth_avail", "geometry")),
         "ponded_depth_avail", "Available Groundwater as \nPonded Depth (m)", "Blues")

### using outputs
system.time(df_out %>% select(c("GridCellID", "available_volume", "geometry")) %>% #unique() %>%
              mutate(available_volume = available_volume * 1e-9) %>%
              st_as_sf() %>% unique() -> df_plot)

system.time(plot_map(df_plot, "available_volume", expression('Available Volume (km'^3*')'), "Blues"))


## total volume produced map ----
system.time(df_out %>% select(c("GridCellID", "volume_produced_allwells", "geometry")) %>%
              group_by(GridCellID) %>%
              summarize(volume_produced_allwells = sum(volume_produced_allwells) * 1e-9) %>%
              ungroup() %>% add_geometry() -> df_out_vol)


print(paste0("Global Pumped Volume in 0.3PD 0.25DL = ",
             round(sum(df_out_vol$volume_produced_allwells) * 1e-6, 3), " million km3"))


system.time(plot_map(df_out_vol, "volume_produced_allwells", expression('Volume Produced\n(million km'^3*')'), "OrRd"))

## total volume produced map as ponded depth
system.time(df_out %>% select(c("GridCellID", "volume_produced_allwells", "grid_area", "geometry")) %>%
              mutate(ponded_depth_prod = volume_produced_allwells/grid_area) %>%
              group_by(GridCellID) %>%
              summarize(volume_produced_allwells_depth = sum(ponded_depth_prod)) %>%
              ungroup() %>% add_geometry() -> df_out_vol_pdepth)


system.time(plot_map(df_out_vol_pdepth, "volume_produced_allwells_depth", "Groundwater Produced \nas Ponded Depth (m)", "OrRd"))



## pumped to available fraction ----
plot_map(df_out %>% select(c("GridCellID", "available_volume", "cumulative_vol_produced_allwells", "geometry")) %>% #unique() %>%
           group_by(GridCellID) %>%
           summarize(available_volume = max(available_volume),
                     cumulative_vol_produced_allwells = max(cumulative_vol_produced_allwells)) %>%
           mutate(pump_frac = cumulative_vol_produced_allwells * 100 / available_volume) %>% ungroup() %>% add_geometry(),
         "pump_frac", "Pumping Efficiency (%)", "BuPu")


## unit cost map ----
# unit costs: mean unit cost over pumping period
plot_map(df_out %>% select(c("GridCellID", "unit_cost", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(unit_cost_avg = mean(unit_cost)) %>% ungroup() %>% add_geometry(),
         "unit_cost_avg", expression('Mean Unit Cost ($/m'^3*')'), "YlOrRd")

## make two year-wise unit cost maps to show change in cost over pumping period
# first for the first year i.e. year_number = 1
plot_map(df_out %>% select(c("year_number","GridCellID", "unit_cost", "geometry")) %>%
           filter(year_number == 1) %>% rename("unit_cost_y1" = "unit_cost") %>% st_as_sf(),
         "unit_cost_y1", expression('Unit Cost in \nFirst year ($/m'^3*')'), "YlOrRd")

# the second for last year i.e. max(year_number)
plot_map(df_out %>% select(c("GridCellID", "year_number", "unit_cost", "geometry")) %>%
           group_by(GridCellID) %>%
           filter(year_number == max(year_number)) %>%
           rename("unit_cost_ymax" = "unit_cost") %>% ungroup() %>% st_as_sf(),
         "unit_cost_ymax", expression('Unit Cost in \nLast year ($/m'^3*')'), "YlOrRd")

## energy costs
plot_map(df_out %>% select(c("GridCellID", "energy_cost", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(energy_cost_avg = mean(energy_cost) * 1e-6) %>% ungroup() %>% add_geometry(),
         "energy_cost_avg", "Mean Energy Cost\n(million $)", "Reds")

plot_map(df_out %>% select(c("GridCellID", "energy_cost", "number_of_wells", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(energy_cost_avg_perwell = mean(energy_cost)/mean(number_of_wells)) %>% ungroup() %>% add_geometry(),
         "energy_cost_avg_perwell", "Mean Energy Cost\nper Well ($/well)", "YlGnBu")


## nonenergy costs
plot_map(df_out %>% select(c("GridCellID", "nonenergy_cost", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(nonenergy_cost_avg = mean(nonenergy_cost) * 1e-6) %>% ungroup() %>% add_geometry(),
         "nonenergy_cost_avg", "Mean Nonenergy Cost\n(million $)", "Purples")


plot_map(df_out %>% select(c("GridCellID", "nonenergy_cost", "number_of_wells", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(nonenergy_cost_avg_perwell = mean(nonenergy_cost)/mean(number_of_wells)) %>% ungroup() %>% add_geometry(),
         "nonenergy_cost_avg_perwell", "Mean Nonenergy Cost\nper Well ($/well)", "Reds")



## cost vs vol at a cost cutoff map ----
# cutoff by global median unit cost: total volume produced as ponded depth until median unit cost in each grid cell
plot_map(df_out %>% select(c("GridCellID", "grid_area", "unit_cost", "volume_produced_allwells", "geometry")) %>%
           filter(unit_cost <= median(unit_cost)) %>%
           mutate(ponded_depth_prod = volume_produced_allwells/grid_area) %>%
           group_by(GridCellID) %>%
           summarize(ponded_depth_prod_cutoff_global = sum(ponded_depth_prod)) %>%
           ungroup() %>% add_geometry(),
         "ponded_depth_prod_cutoff_global", "Groundwater Produced as \nPonded Depth under \nGlobal Median \nUnit Cost (m)", "YlGnBu")


# global volume under median cost in million km3
df_out %>% select(c("GridCellID", "unit_cost", "volume_produced_allwells")) %>%
  filter(unit_cost <= median(unit_cost)) %>%
  summarize(ponded_depth_prod_cutoff_global = sum(volume_produced_allwells) * 1e-15) %>% ungroup()


# cutoff by median unit cost in each grid cell
plot_map(df_out %>% select(c("GridCellID", "grid_area", "unit_cost", "volume_produced_allwells", "geometry")) %>%
           group_by(GridCellID) %>%
           filter(unit_cost <= median(unit_cost)) %>%
           mutate(ponded_depth_prod = volume_produced_allwells/grid_area) %>%
           summarize(ponded_depth_prod_cutoff_grid = sum(ponded_depth_prod)) %>%
           ungroup() %>% add_geometry(),
         "ponded_depth_prod_cutoff_grid", "Groundwater Produced as \nPonded Depth under \nGrid Median \nUnit Cost (m)", "YlGnBu")

# global volume under median cost of each grid cell in million km3
df_out %>% select(c("GridCellID", "unit_cost", "volume_produced_allwells")) %>%
  group_by(GridCellID) %>%
  filter(unit_cost <= median(unit_cost)) %>% ungroup() %>%
  summarize(ponded_depth_prod_cutoff_global = sum(volume_produced_allwells) * 1e-15)

# percent of available and produced volume in 0.3 25%
# > 0.408/5.225
# [1] 0.07808612
# > 0.396/5.225
# [1] 0.07578947
# > 0.408/.782
# [1] 0.5217391
# > 0.396/.782
# [1] 0.5063939

## more diagnostic maps ----
# pumping period
plot_map(df_out %>% select(c("GridCellID", "year_number", "depleted_vol_fraction", "geometry")) %>%
           group_by(GridCellID) %>%
           filter(year_number == max(year_number)) %>% ungroup() %>% add_geometry(),
         "year_number", "Pumping Lifetime (yr)", "GnBu")

# Q: why do d_v_frac has same values for adjacent cells
# df_out %>% select(c("year_number", "depleted_vol_fraction","GridCellID", "geometry")) %>%
#   group_by(GridCellID) %>%
#   filter(year_number == max(year_number)) %>% ungroup() -> a

## grid cell ID
plot_map(df_out %>% select(c("GridCellID", "geometry")) %>% unique() %>% add_geometry(),
         "GridCellID", "Grid Cell ID", "YlGnBu")


# depletion volume fraction
plot_map(df_out %>% select(c("GridCellID", "depleted_vol_fraction", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(depleted_vol_fraction = mean(depleted_vol_fraction) * 100) %>% ungroup() %>% add_geometry(),
         "depleted_vol_fraction", "Mean Depleted\nVolume Fraction (%)", "YlOrRd")

plot_map(df_out %>% select(c("GridCellID", "depleted_vol_fraction", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(depleted_vol_fraction = max(depleted_vol_fraction) * 100) %>% ungroup() %>% add_geometry(),
         "depleted_vol_fraction", "Max Depleted\nVolume Fraction (%)", "YlOrRd")


## aquifer saturated thickness
plot_map(df_out %>% select(c("GridCellID", "aqfr_sat_thickness", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(aquifer_saturated_thickness = mean(aqfr_sat_thickness)) %>% ungroup() %>% add_geometry(),
         "aquifer_saturated_thickness", expression('Aquifer Saturated \nThickness (m)'), "YlGnBu")

## hydraulic_conductivity
plot_map(df_out %>% select(c("hydraulic_conductivity", "geometry")) %>% unique() %>%
           mutate(hydraulic_conductivity = hydraulic_conductivity * 86400) %>% st_as_sf(),
         "hydraulic_conductivity", "Hydraulic Conductivity\n(m/day)", "PuBuGn")

## transmissivity
plot_map(df_out %>% select(c("GridCellID", "transmissivity", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(transmissivity = mean(transmissivity) * 86400) %>% ungroup() %>% add_geometry(),
           # mutate(transmissivity = transmissivity) %>% st_as_sf(),
         "transmissivity", expression('Transmissivity(m'^2*'/day)'), "YlOrRd")

## well_yield
plot_map(df_out %>% select(c("GridCellID", "well_yield", "geometry")) %>%
           group_by(GridCellID) %>% unique() %>%
           summarize(well_yield = mean(well_yield) * 86400) %>% ungroup() %>% add_geometry(),
         "well_yield", expression('Mean Well Yield (m'^3*'/day)'), "PuBu")

## well_depth
plot_map(df_out %>% select(c("GridCellID", "total_well_length", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(well_depth = mean(total_well_length)) %>% ungroup() %>% add_geometry(),
         "well_depth", expression('Mean Well Depth (m)'), "YlGnBu")

## well_area
plot_map(df_out %>% select(c("GridCellID", "areal_extent", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(well_area = mean(areal_extent) * 1e-6) %>% ungroup() %>% add_geometry(),
         "well_area", expression('Mean Well Area (km'^2*')'), "PuBu")

## energy cost rate
plot_map(df_out %>% select(c("GridCellID", "energy_cost_rate", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(energy_cost_rate = mean(energy_cost_rate)) %>% ungroup() %>% add_geometry(),
         "energy_cost_rate", expression('Energy Cost Rate \n($/KWh)'), "YlOrRd")


## drawdown
plot_map(df_out %>% select(c("GridCellID", "drawdown", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(drawdown = mean(drawdown)) %>% ungroup() %>% add_geometry(),
         "drawdown", expression('Mean Drawdown (m)'), "YlOrRd")

## number of wells
plot_map(df_out %>% select(c("GridCellID", "number_of_wells", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(number_of_wells = mean(number_of_wells) * 1e-3) %>% ungroup() %>% add_geometry(),
         "number_of_wells", expression('Mean Number of Wells\n(1,000)'), "YlGnBu")


## annual_capital_cost
plot_map(df_out %>% select(c("GridCellID", "annual_capital_cost", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(annual_capital_cost = mean(annual_capital_cost) * 1e-6) %>% ungroup() %>% add_geometry(),
         "annual_capital_cost", expression('Mean Annual Capital \nCost ($M)'), "YlOrRd")

## annual_capital_cost (per well)
plot_map(df_out %>% select(c("GridCellID", "annual_capital_cost", "number_of_wells", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(annual_capital_cost_per_well = mean(annual_capital_cost)/mean(number_of_wells)) %>% ungroup() %>% add_geometry(),
         "annual_capital_cost_per_well", expression('Mean Annual Capital \nCost ($/well)'), "YlOrRd")


## energy
plot_map(df_out %>% select(c("GridCellID", "energy", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(energy = mean(energy) * 1e-6) %>% ungroup() %>% add_geometry(),
         "energy", expression('Mean Pumping Energy \n(GWh)'), "YlOrRd")

## power
plot_map(df_out %>% select(c("GridCellID", "power", "geometry")) %>%
           group_by(GridCellID) %>%
           summarize(power = mean(power) * 1e-6) %>% ungroup() %>% add_geometry(),
         "power", expression('Mean Pumping Power \n(GW)'), "YlGnBu")

#'##################### correlations ###########################################
# correlations ----
# select a small country to avoid memory issues
df_corr <- df_out %>% filter(country == "Netherlands") %>%
  select(c("year_number", "permeability", "porosity", "total_thickness", "depth_to_water", "whyclass", "grid_area",
           "aqfr_sat_thickness", "available_volume", "hydraulic_conductivity", "transmissivity",
           "well_yield", "areal_extent", "number_of_wells",
           "drawdown", "total_head", "total_well_length",
           "volume_produced_perwell", "cumulative_vol_produced_perwell", "volume_produced_allwells", "cumulative_vol_produced_allwells",
           "depleted_vol_fraction",
           "well_installation_cost", "annual_capital_cost", "maintenance_cost", "nonenergy_cost",
           "power", "energy", "energy_cost",
           "total_cost_perwell", "total_cost_allwells", "unit_cost"
           )
         )

# plot pair wise correlations of key variables

{
  png(paste0(figs_dir, "correlations.png"), width = 17, height = 17, units = "in", res = 300)
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  df_corr %>%
    cor() %>% corrplot::corrplot(method = "number", type = "upper", order = "hclust", tl.col = "black")
  dev.off()
}

{
  png(paste0(figs_dir, "correlations_spearman.png"), width = 17, height = 17, units = "in", res = 300)
  df_corr %>%
    cor(method = "spearman") %>% corrplot::corrplot(method = "number", type = "upper", order = "hclust", tl.col = "black")
  dev.off()
}

# NO NEED TO PLOT THIS - TOO MANY VARIABLES
# pretty pair panel (smoothed), but confusing
png(paste0(figs_dir, "correlations_psych_smooth.png"), width = 17, height = 17, units = "in", res = 300)
psych::
pairs.panels(df_corr[1:100,],
             method = "spearman",   # correlation method
             hist.col = "gold",     # color of histogram
             rug = FALSE,           # show marginal rug plots
             show.points= FALSE,    # show data points
             smoother = T,
             smooth = TRUE,
             scale = TRUE,
             density = TRUE,       # show density plots
             ellipses = TRUE       # show correlation ellipses
)
dev.off()

#'##################### scatter plots ###########################################

# Scatter plots: ----
# (1a) K or k vs. Well pumping rate
# (1b) T vs. Well pumping rate
# combine (1) and (2) to show T vs. Pumping rate with color = K and size = saturated thickness
# (2) Well pumping rate vs. Well area
# (3) Depth to water vs. Energy cost (per well)
# (4) Well depth vs Nonenergy cost (per well) with Z color WHYMap class
# (5) Pumping rate vs. Unit cost (color = depth to water, size = pumping rate)
# (6) pumped volume vs. change in water depth in 1 yr (color by porosity)
# Variables: K, Q, T, Aoi, DTW, energy $/well, total_well_length, nonenergy $/well, unit $, vol, depth

## choose scatter df ----
df_out %>% filter(country == "United States") -> df_scatter

# # read in model run using sampled inputs
# df_scatter <- read_csv(paste0(out_dir, '0.3PD_0.25DL_sample_100.csv')) %>% rename("GridCellID" = "grid_id") %>% left_join(select(sf_in, c("GridCellID", "geometry")), by = "GridCellID")

# num of grid cells for scatter plot
print(paste0("Number of Grid Cells for Scatter Plots: ", length(unique(df_scatter$GridCellID)),
             " out of ", length(unique(df_in$GridCellID)),
             " (", round(length(unique(df_scatter$GridCellID))*100/length(unique(df_in$GridCellID)), 1), "%)"))

StatClipEllipse <- ggproto("StatClipEllipse", Stat,
                           required_aes = c("x", "y"),
                           compute_group = function(data, scales, type = "t", level = 0.99,
                                                    segments = 51, na.rm = FALSE) {
                             xx <- ggplot2:::calculate_ellipse(data = data, vars = c("x", "y"), type = type,
                                                               level = level, segments = segments)
                             xx %>% mutate(x = pmax(x, 0), y = pmax(0, y)) # 1250 for nonenergy costs
                           }
)

stat_clip_ellipse <- function(mapping = NULL, data = NULL,
                              geom = "path", position = "identity",
                              ...,
                              type = "t",
                              level = 0.99,
                              segments = 51,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              fill = NA) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatClipEllipse,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      level = level,
      segments = segments,
      na.rm = na.rm,
      ...
    )
  )
}


#TODO: Let's get rid of the states of Q_array and make it smooth, let's only control the bounds using Q-array

## (1a) hydraulic_conductivity (K) vs. Well pumping rate (Q)
df_scatter %>% select(c("GridCellID", "hydraulic_conductivity", "well_yield")) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = hydraulic_conductivity * 86400, y = well_yield * 86400)) +
  geom_point(size = 1) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_scatter$hydraulic_conductivity) * 86400),
                  ylim = c(0, max(df_scatter$well_yield) * 87400)) + #, expand = F
  # scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = -Inf, hjust = 1.3, vjust = -0.9,
            size = 4, fontface = "italic") +
  labs(x = "Hydraulic Conductivity (m/day)", y = expression('Well Yield (m'^3*'/day)'), tag = "(a)") +
  my_theme()

# ggsave(paste0(figs_dir, "scatter_", "hydraulic_conductivity", "_", "well_yield", saveformat),
#        width = 7.0, height = 5, units = "in", dpi = 300)

# ggExtra::ggMarginal(p, color = NA, fill = "red3", lwd = 1, size = 10, alpha = 0.9, outlier.shape = NA, notch = T) # type = "boxplot"


## a  K vs. Pumping rate with color = T and size = saturated thickness ----
df_scatter %>% select(c("GridCellID", "hydraulic_conductivity", "transmissivity", "well_yield", "aqfr_sat_thickness")) %>%
  # sample_n(10000) %>%
  group_by(GridCellID) %>% #unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>% #head(500) %>%
  ggplot(aes(x = hydraulic_conductivity * 86400, y = well_yield * 86400)) +
  geom_point(aes(color = transmissivity * 86400, size = aqfr_sat_thickness), alpha = 0.75) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  scale_color_gradientn(colors = c(brewer.pal(9, "YlOrRd"))[3:9]) +
  scale_size(range = c(0.01, 4)) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_scatter$hydraulic_conductivity) * 86400),
                  ylim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 2.65, vjust = 12.75,
            size = 4, fontface = "italic") +
  labs(x =  "Hydraulic Conductivity (m/day)", y = expression('Well Yield (m'^3*'/day)'), tag = "(a)",
       color = expression("Transmissivity (m"^2*"/day)"), size = "Saturated Thickness (m)") +
  my_theme() + theme(legend.position = c(0.8, 0.33))

ggsave(paste0(figs_dir, "scatter_", "hydraulic_conductivity", "_", "well_yield_colored_scaled", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

## (1b) T vs. Well pumping rate
df_scatter %>% select(c("GridCellID", "transmissivity", "well_yield")) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = transmissivity * 86400, y = well_yield * 86400)) +
  geom_point(size = 1) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_scatter$transmissivity) * 86400),
                  ylim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = -Inf, hjust = 1.3, vjust = -0.9,
            size = 4, fontface = "italic") +
  labs(x = "Transmissivity [m2/day]", y = expression('Well Yield (m'^3*'/day)'), tag = "(b)") +
  my_theme()

ggsave(paste0(figs_dir, "scatter_", "transmissivity", "_", "well_yield", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)


## option 2: combine (1a) and (1b) to show T vs. Pumping rate with color = K and size = saturated thickness
df_scatter %>% select(c("GridCellID", "hydraulic_conductivity", "transmissivity", "well_yield", "aqfr_sat_thickness")) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>% # head(5000) %>%
  ggplot(aes(x = transmissivity * 86400, y = well_yield * 86400)) +
  geom_point(aes(color = hydraulic_conductivity * 86400, size = aqfr_sat_thickness), alpha = 0.5) +
  scale_color_gradientn(colors = c(brewer.pal(9, "Oranges"))[1:9]) +
  scale_size(range = c(0.01, 5)) +
  # scale_size(range = c(0.25, 2.5)) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  # coord_cartesian(xlim = c(0, max(df_scatter$transmissivity) * 86400), ylim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 2.8, vjust = 12.75,
            size = 4, fontface = "italic") +
  labs(x = expression("Transmissivity (m"^2*"/day)"), y = expression('Well Yield (m'^3*'/day)'), tag = "(a)",
       color = "Hydraulic Conductivity (m/day)", size = "Saturated Thickness (m)") +
  my_theme() + theme(legend.position = c(0.8, 0.33))

ggsave(paste0(figs_dir, "scatter_", "transmissivity", "_", "well_yield_colored_scaled", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

## c transmissivity x axis, drawdown y axis, colored by well pumping rate ----
df_scatter %>% select(c("GridCellID", "transmissivity", "drawdown", "well_yield")) %>%
  # sample_n(1000) %>%
  group_by(GridCellID) %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = transmissivity * 86400, y = drawdown)) +
  geom_point(aes(color = well_yield * 86400), alpha = 0.5, size = 0.5) +
  scale_color_gradientn(colors = c(brewer.pal(9, "Blues"))[2:9]) +
  scale_size(range = c(0.01, 5)) +
  # scale_size(range = c(0.25, 2.5)) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  # coord_cartesian(xlim = c(0, max(df_scatter$transmissivity) * 86400), ylim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.85, vjust = 1.95,
            size = 4, fontface = "italic") +
  labs(x = expression("Transmissivity (m"^2*"/day)"), y = "Drawdown (m)", tag = "(c)",
       color = expression('Well Yield (m'^3*'/day)'), size = "Saturated Thickness (m)") +
  my_theme() + theme(legend.position = c(0.87, 0.75))

ggsave(paste0(figs_dir, "scatter_", "transmissivity", "_", "drawdown", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

# hydraulic conductivity vs drawdown, colored by well pumping rate
df_scatter %>% select(c("GridCellID", "hydraulic_conductivity", "drawdown", "well_yield")) %>%
  #sample_n(1000) %>%
  group_by(GridCellID) %>%
  mutate(plength = nrow(.)) %>% ungroup() %>% # head(5000) %>%
  ggplot(aes(x = hydraulic_conductivity * 86400, y = drawdown)) +
  geom_point(aes(color = well_yield * 86400), alpha = 0.5) +
  scale_color_gradientn(colors = c(brewer.pal(9, "YlGnBu"))[2:9]) +
  scale_size(range = c(0.01, 5)) +
  # scale_size(range = c(0.25, 2.5)) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  # coord_cartesian(xlim = c(0, max(df_scatter$transmissivity) * 86400), ylim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.7, vjust = 1.75,
            size = 4, fontface = "italic") +
  labs(x = expression("Hydraulic Conductivity (m/day)"), y = "Drawdown (m)", tag = "(c)",
       color = expression('Well Yield (m'^3*'/day)'), size = "Saturated Thickness (m)") +
  my_theme() + theme(legend.position = c(0.89, 0.75))

ggsave(paste0(figs_dir, "scatter_", "hydraulic_conductivity", "_", "drawdown", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)


## b Well pumping rate vs. Well area ----
df_scatter %>% select(c("GridCellID", "areal_extent", "well_yield")) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = areal_extent * 1e-6, y = well_yield * 86400)) +
  geom_point(size = 1.95) +
  # geom_point(aes(size = well_yield * 86400)) +
  geom_smooth(linetype = "dashed", fill = "lightblue", size = 0.5, alpha = 0.5) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_scatter$areal_extent) * 1e-6),
                  ylim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  geom_text(aes(label = paste0("n = ", plength, " \nn (Q~A) = 22")),
            x = Inf, y = -Inf, hjust = 1.3, vjust = -0.9,
            size = 4, fontface = "italic") +
  labs(x = expression("Well Area (km"^2*")"), y = expression('Well Yield (m'^3*'/day)'), tag = "(b)") +
  my_theme()

ggsave(paste0(figs_dir, "scatter_", "areal_extent", "_", "well_yield", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)


## e Depth to water vs. Energy cost (per well) ----
df_scatter %>% select(c("GridCellID", "well_yield", "depth_to_water", "total_well_length",  "energy_cost", "nonenergy_cost", "number_of_wells", "whyclass")) %>% #sample_n(1000) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(energy_cost_perwell = energy_cost/number_of_wells,
         nonenergy_cost_perwell = nonenergy_cost/number_of_wells,
         plength = nrow(.)) %>% ungroup() -> df_cost_dtw

df_cost_dtw %>%
  ggplot(aes(x = depth_to_water, y = energy_cost_perwell)) +
  geom_point(aes(color = well_yield * 86400), alpha = 0.75, size = 0.5) +
  scale_color_gradientn(colors = c(brewer.pal(9, "Blues"))[2:9]) +
  # scale_color_gradientn(colors = c(brewer.pal(9, "Blues"))[4:9]) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_cost_dtw$depth_to_water)),
                  ylim = c(0, max(df_cost_dtw$energy_cost_perwell))) + #, expand = F
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.3, vjust = 1.75,
            size = 4, fontface = "italic") +
  labs(x = "Depth to Water (m)", y = "Energy Cost per Well ($/well)", tag = "(e)", color = expression('Well Yield (m'^3*'/day)')) +
  my_theme() +
  # theme(legend.position = c(0.87, 0.75))
  theme(legend.position = c(0.25, 0.94), legend.direction = "horizontal")

ggsave(paste0(figs_dir, "scatter_", "depth_to_water", "_", "energy_cost_perwell", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)


## Depth to water vs Nonenergy cost (per well) with Z color WHYMap class
df_cost_dtw %>% #head(1000) %>%
  ggplot(aes(x = depth_to_water, y = nonenergy_cost_perwell)) +
  geom_point(aes(color = as.factor(whyclass), shape = factor(whyclass)), size = 0.5, alpha = 0.75) +
  scale_color_manual(values = c("goldenrod3", "dodgerblue4", "green4")) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_cost_dtw$depth_to_water)),
                  ylim = c(0, max(df_cost_dtw$nonenergy_cost_perwell))) + #, expand = F
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.3, vjust = 39,
            size = 4, fontface = "italic") +
  labs(x = "Depth to Water (m)", y = "Nonenergy Cost per Well ($/well)", tag = "(d)", color = "WHY Class") +
  my_theme() + theme(legend.position = c(0.24, 0.97), legend.direction = "horizontal") +
  guides(shape = "none", color = guide_legend(override.aes = list(shape = c(19, 17, 15), size = 2)))

ggsave(paste0(figs_dir, "scatter_", "depth_to_water", "_", "nonenergy_cost_perwell", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

## f total well depth vs nonenergy cost per well by WHY class ----
df_cost_dtw %>%
  # sample_n(1000) %>%
  ggplot(aes(x = total_well_length, y = nonenergy_cost_perwell)) +
  geom_point(aes(color = as.factor(whyclass), shape = factor(whyclass)), size = 0.5, alpha = 0.75) +
  scale_color_manual(values = c("goldenrod2", "dodgerblue4", "green4")) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_cost_dtw$total_well_length)),
                  ylim = c(0, max(df_cost_dtw$nonenergy_cost_perwell))) + #, expand = F
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.3, vjust = 39,
            size = 4, fontface = "italic") +
  labs(x = "Total Well Depth (m)", y = "Nonenergy Cost per Well ($/well)", tag = "(f)", color = "WHY Class") +
  my_theme() + theme(legend.position = c(0.24, 0.97), legend.direction = "horizontal") +
  guides(shape = "none", color = guide_legend(override.aes = list(shape = c(19, 17, 15), size = 2)))

ggsave(paste0(figs_dir, "scatter_", "total_well_depth", "_", "nonenergy_cost_perwell", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

## d Pumping rate vs. Unit cost (color = depth to water, size = pumping rate) ----
df_scatter %>% select(c("GridCellID", "unit_cost", "well_yield", "total_cost_perwell","depth_to_water")) %>%
  # sample_n(1000) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>% #head(5000) %>%
  ggplot(aes(y = unit_cost, x = well_yield * 86400)) +
  geom_point(aes(color = depth_to_water, size = total_cost_perwell), alpha = 0.75) +
  scale_color_gradientn(colors = c(brewer.pal(9, "YlGnBu"))[2:9]) +
  scale_size(range = c(0.01, 4)) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(ylim = c(0, max(df_scatter$unit_cost)),
                  xlim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  scale_y_sqrt() + scale_x_sqrt() +
  geom_text(aes(label = paste0("n = ", plength, "\nn (Q) = 22  ")),
            x = Inf, y = Inf, hjust = 3.3, vjust = 1.55,
            size = 4, fontface = "italic") +
  labs(y = expression("Unit Cost ($/m"^3*")"), x = expression('Well Yield (m'^3*'/day)'), tag = "(d)",
       color = "Depth to\nWater (m)", size = "Total Cost per\nWell ($/well)") +
  my_theme() + theme(legend.position = c(0.89, 0.645), legend.title.align = 0.5)

ggsave(paste0(figs_dir, "scatter_", "unit_cost", "_", "well_yield_cost", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

# cost diagnostic plots
# plot(df_scatter$total_cost_perwell, df_scatter$energy_cost)
# plot(df_scatter$total_cost_perwell, df_scatter$well_installation_cost)
# plot(df_scatter$total_cost_perwell, df_scatter$number_of_wells)
# plot(df_scatter$total_cost_allwells, df_scatter$number_of_wells)



# unit cost vs well yield: color by WHY class size by depth to water
df_scatter %>% select(c("GridCellID", "unit_cost", "well_yield", "depth_to_water", "whyclass")) %>%
  # sample_n(10000) %>%
  group_by(GridCellID) %>%
  mutate(plength = nrow(.)) %>% ungroup() %>% #head(5000) %>%
  ggplot(aes(y = unit_cost, x = well_yield * 86400)) +
  geom_point(aes(shape = factor(whyclass), color = factor(whyclass), size = depth_to_water), alpha = 0.75) +
  scale_color_manual(values = c("goldenrod1", "dodgerblue3", "green4")) +
  # scale_color_gradientn(colors = c(brewer.pal(9, "PuBu"))[1:9]) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(ylim = c(0, max(df_scatter$unit_cost)),
                  xlim = c(0, max(df_scatter$well_yield) * 87400 * 1)) + #, expand = F
  # scale_y_continuous(sec.axis = sec_axis(~.x * 0.1835, name = "Well Yield [gallons/min]")) +
  scale_y_sqrt() + scale_x_sqrt() +
  geom_text(aes(label = paste0("n = ", plength, " \nn (Q) = 22 ")),
            x = Inf, y = Inf, hjust = 1.3, vjust = 1.75,
            size = 4, fontface = "italic") +
  labs(y = expression("Unit Cost ($/m"^3*")"), x = expression('Well Yield (m'^3*'/day)'), tag = "(b)", size = "Depth to\nWater (m)") +
  my_theme() + theme(legend.position = c(0.89, 0.65), legend.title.align = 0.5) +
  guides(shape = "none", color = guide_legend(override.aes = list(shape = c(19, 17, 15), size = 2)))


ggsave(paste0(figs_dir, "scatter_", "unit_cost", "_", "well_yield_whyclass", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

## pumped volume vs. change in water depth in 1 yr (color by porosity)
df_scatter %>% select(c("GridCellID", "year" = "year_number","volume_produced_allwells", "depth_to_water", "porosity")) %>%
  group_by(GridCellID) %>% filter(year %in% c(1, 2)) %>%
  mutate(change_in_water_depth = depth_to_water - lead(depth_to_water)) %>% drop_na() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = volume_produced_allwells * 1e-6, y = change_in_water_depth)) +
  geom_point(aes(color = porosity), size = 1) +
  scale_color_gradientn(colors = c(brewer.pal(9, "Oranges"))[4:9]) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_scatter$volume_produced_allwells * 1e-6)),
                  ylim = c(-6.5, -1)) + #, expand = F
  # scale_y_sqrt() + scale_x_sqrt() +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.3, vjust = 1.75,
            size = 4, fontface = "italic") +
  labs(x = expression('Volume Produced (km'^3*')'), y = "Change in Water Depth in Year 1 (m)", tag = "(f)", color = "Porosity (-)") +
  my_theme() + theme(legend.position = c(0.8, 0.05), legend.direction = "horizontal")

ggsave(paste0(figs_dir, "scatter_", "volume_produced_allwells", "_", "change_in_water_depth", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

# porosity vs change in water depth
df_scatter %>% select(c("GridCellID", "year" = "year_number","volume_produced_allwells", "depth_to_water", "porosity")) %>%
  group_by(GridCellID) %>% filter(year %in% c(1, 2)) %>%
  mutate(change_in_water_depth = depth_to_water - lead(depth_to_water)) %>% drop_na() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = porosity, y = change_in_water_depth)) +
  geom_point(aes(color = volume_produced_allwells  * 1e-6), size = 1) +
  scale_color_gradientn(colors = c(brewer.pal(9, "Oranges"))[3:9]) +
  # geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  coord_cartesian(xlim = c(0, max(df_scatter$porosity)),
                  ylim = c(-6.5, -1)) + #, expand = F
  # scale_y_sqrt() + scale_x_sqrt() +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = Inf, hjust = 1.3, vjust = 1.75,
            size = 4, fontface = "italic") +
  labs(x = "Porosity (-)", y = "Change in Water Depth in Year 1 (m)", tag = "(f)", color = expression('Volume Produced (km'^3*')')) +
  my_theme() + theme(legend.position = c(0.75, 0.05), legend.direction = "horizontal")
  # guides(color = guide_colourbar(barwidth = 20))

ggsave(paste0(figs_dir, "scatter_", "porosity", "_", "change_in_water_depth", saveformat),
       width = 7.0, height = 5, units = "in", dpi = 300)

## energy costs vs total_head ----
df_scatter %>% select(c("GridCellID", "energy_cost", "total_head", "number_of_wells")) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = total_head, y = energy_cost)) +
  # geom_point(size = 1, alpha = 0.05) +
  geom_smooth(linetype = "dashed", fill = "lightblue", linewidth = 1) +
  # stat_clip_ellipse(level = 0.99, color = "red3", size = 1) +
  # coord_cartesian(xlim = c(0, max(df_scatter$hydraulic_conductivity) * 86400),
  #                 ylim = c(0, max(df_scatter$well_yield) * 87400)) + #, expand = F
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = -Inf, hjust = 1.3, vjust = -0.9,
            size = 4, fontface = "italic") +
  labs(x = "Lift (m)", y = "Energy Cost ($)") +
  my_theme()


#'################# end of scatter / start of cost curves #######################

# Cost curves ----

df_unit <- df_0.3PD_0.25DL %>%
  select(c("GridCellID", "year_number", "depletion_limit", "continent", "country", "gcam_basin_id", "Basin_long_name",
           "volume_produced_allwells", "unit_cost", "grid_area")) %>%
  arrange(unit_cost) %>%
  bind_rows(select(df_0.3PD_0.05DL, all_of(names(.))) %>% arrange(unit_cost),
            select(df_0.3PD_0.4DL, all_of(names(.))) %>% arrange(unit_cost))
            # select(df_0.6PD_0.05DL, all_of(names(.))), select(df_0.6PD_0.25DL, all_of(names(.))), select(df_0.6PD_0.4DL, all_of(names(.))))

c(min(df_unit$unit_cost), max(df_unit$unit_cost))
describe(df_unit$unit_cost)
describe(df_unit$volume_produced_allwells)


# breaks for the log-scale bins
log_breaks <- unique(c(seq(0.0001, 0.01, by = 0.0025),
                       seq(0.01, 1, by = 0.005),
                       seq(1, 10, by = 0.01),
                       seq(10, 100, by = 1)))
log_bins <- data.frame(index = seq_along(log_breaks), unit_cost_bin = log_breaks)
# plot(log_breaks)
# plot(log_bins)


## global ----

# cut unit_cost into the defined bins
df_unit_bin_global <- df_unit %>%
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index") # %>% arrange(desc(depletion_limit))


df_unit_bin_global$depletion_limit <- factor(df_unit_bin_global$depletion_limit,
                                             levels = c(0.4, 0.25, 0.05))

# plot global cost curve
ggplot(df_unit_bin_global) +
  # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit))) +
  geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
  scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  # scale_y_continuous(expand = c(0, 3000)) +
  scale_x_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0)) + #-0.12
  # coord_cartesian(xlim = c(0, 1)) +
  labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Global Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(b)") +
  # annotation_logticks(sides = "b") +
  my_theme() + theme(legend.position = c(0.9, 0.875)) #,legend.title = element_text(face = "bold")

ggsave(paste0(figs_dir, "unit_cost_global_density", saveformat), width = 7, height = 5, units = "in", dpi = 300)

# flipped axes
ggplot(df_unit_bin_global) +
  # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit))) +
  geom_polygon(aes(y = unit_cost_bin, x = volume_produced_allwells_bin, fill = as.factor(depletion_limit))) + #alpha = as.factor(depletion_limit)
  scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  # scale_y_continuous(expand = c(0, 3000)) +
  scale_y_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0)) + #-0.12
  # coord_cartesian(xlim = c(0, 1)) +
  labs(y = expression('Unit Cost ($/m'^3*')'), x = expression('Global Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(b)") +
  # annotation_logticks(sides = "b") +
  my_theme() + theme(legend.position = c(0.9, 0.875)) #,legend.title = element_text(face = "bold")



## cumulative
df_unit_bin_global %>% group_by(depletion_limit) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) -> df_unit_bin_global_cumsum

ggplot(df_unit_bin_global_cumsum) +
  geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
  scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_x_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0.01)) + #-0.12
  # annotation_logticks(sides = "b") +
  labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Global Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(a)") +
  my_theme() + theme(legend.position = c(0.1, 0.875))

ggsave(paste0(figs_dir, "unit_cost_global_cumsum", saveformat), width = 7, height = 5, units = "in", dpi = 300)


# flipped axes
ggplot(df_unit_bin_global_cumsum) +
  geom_line(aes(y = unit_cost_bin, x = cumm_volume_produced_allwells_bin * 1e-3,
                color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
  scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  scale_y_continuous(expand = c(0, 0.05)) +
  scale_y_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0.01)) + #-0.12
  # annotation_logticks(sides = "b") +
  labs(y = expression('Unit Cost ($/m'^3*')'), x = expression('Global Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(a)") +
  my_theme() + theme(legend.position = c(0.1, 0.875))


# for large scale cascade plot
plot_global <- egg::ggarrange(
  ggplot(df_unit_bin_global) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0)) + #-0.12
    # coord_cartesian(xlim = c(0, 1)) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(b)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none") #,legend.title = element_text(face = "bold")

  ,

  ggplot(df_unit_bin_global_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                  color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0.01)) + #-0.12
    # annotation_logticks(sides = "b") +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(a)") +
    my_theme() + theme(legend.position = "none"),
  nrow = 1, top = "Global Cost Curves"
)

ggsave(plot = plot_global, paste0(figs_dir, "unit_cost_global", saveformat), width = 9, height = 4, units = "in", dpi = 300)



## continents ----
continent_names <- c("Af" = "Africa", "As" = "Asia", "Au" = "Australia",
                     "Eu" = "Europe", "NAm" = "North America", "SA" = "South America", "Oc" = "Australia")

# cut unit_cost into the defined bins
df_unit_bin_continent <- df_unit %>%
  mutate(continent = recode(continent, !!!continent_names)) %>%
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, continent, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index")

df_unit_bin_continent$depletion_limit <- factor(df_unit_bin_continent$depletion_limit,
                                                       levels = c(0.4, 0.25, 0.05))

df_unit_bin_continent %>% group_by(depletion_limit, continent) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(depletion_limit) -> df_unit_bin_continent_cumsum

df_unit_bin_continent_cumsum$depletion_limit <- factor(df_unit_bin_continent_cumsum$depletion_limit,
                                             levels = c(0.4, 0.25, 0.05))


# plot continental cost curve
plot_cons <- egg::ggarrange(
  ggplot(df_unit_bin_continent) +
  # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, color = as.factor(depletion_limit))) +
  geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
  scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  # scale_y_continuous(expand = c(0, 3000)) +
  # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
  scale_x_sqrt() +
  facet_wrap(~continent, ncol = 6) +
  labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(c)") +
  # annotation_logticks(sides = "b") +
  my_theme() + theme(legend.position = "bottom")
,

ggplot(df_unit_bin_continent_cumsum) +
  geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
  scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
  # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
  # scale_y_continuous(expand = c(0, 0.05)) +
  scale_x_sqrt() +
  # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
  facet_wrap(~continent, ncol = 6) +
  labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(d)") +
  # annotation_logticks(sides = "b") +
  my_theme() + theme(legend.position = "bottom")

, ncol = 1)

ggsave(plot = plot_cons, paste0(figs_dir, "unit_cost_continents", saveformat),
       width = 14, height = 7, units = "in", dpi = 300)


## for scale cascade plot
plot_continent <- egg::ggarrange(
  ggplot(df_unit_bin_continent %>% filter(continent == "Australia")) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, color = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    scale_x_sqrt() +
    # facet_wrap(~continent, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(c)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")

  ,

  ggplot(df_unit_bin_continent_cumsum %>% filter(continent == "Australia")) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                  color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    # facet_wrap(~continent, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(d)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  ,
  nrow = 1, top = "Continental Cost Curves: Australia"
)

ggsave(plot = plot_continent, paste0(figs_dir, "unit_cost_continent", saveformat), width = 9, height = 4, units = "in", dpi = 300)




## country ----

# cut unit_cost into the defined bins
df_unit_bin_country <- df_unit %>% filter(country == "United States") %>%
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, country, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index")

df_unit_bin_country$depletion_limit <- factor(df_unit_bin_country$depletion_limit,
                                                levels = c(0.4, 0.25, 0.05))

df_unit_bin_country %>% group_by(depletion_limit, country) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(depletion_limit) -> df_unit_bin_country_cumsum

df_unit_bin_country_cumsum$depletion_limit <- factor(df_unit_bin_country_cumsum$depletion_limit,
                                                       levels = c(0.4, 0.25, 0.05))


# plot country cost curves
plot_country <- egg::ggarrange(
  ggplot(df_unit_bin_country) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, color = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    scale_x_sqrt() +
    # facet_wrap(~country, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(c)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  ,

  ggplot(df_unit_bin_country_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                  color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    # facet_wrap(~country, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(d)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  , nrow = 1, top = "Country Cost Curves: United States")


ggsave(plot = plot_country, paste0(figs_dir, "unit_cost_country", saveformat), width = 9, height = 4, units = "in", dpi = 300)


## region ----
df_unit_bin_region <- df_unit %>%
  left_join(mappings_all %>%
              filter(region == "Africa_Northern") %>%
              select(gcam_basin_id = GCAM_basin_ID, region), by = "gcam_basin_id") %>%
  drop_na(region) %>%
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, region, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index")

df_unit_bin_region$depletion_limit <- factor(df_unit_bin_region$depletion_limit,
                                            levels = c(0.4, 0.25, 0.05))

df_unit_bin_region %>% group_by(depletion_limit, region) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(depletion_limit) -> df_unit_bin_region_cumsum

df_unit_bin_region_cumsum$depletion_limit <- factor(df_unit_bin_region_cumsum$depletion_limit,
                                                   levels = c(0.4, 0.25, 0.05))


# plot region cost curves
plot_region <- egg::ggarrange(
  ggplot(df_unit_bin_region) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, color = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    scale_x_sqrt() +
    # facet_wrap(~region, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(c)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  ,

  ggplot(df_unit_bin_region_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin * 1e-3,
                  color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    # facet_wrap(~region, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (million km'^3*')'), color = "Depletion Limit", tag = "(d)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  , nrow = 1, top = "Regional Cost Curves: Northern Africa")


ggsave(plot = plot_region, paste0(figs_dir, "unit_cost_region", saveformat), width = 9, height = 4, units = "in", dpi = 300)


## basin ----

# cut unit_cost into the defined bins
df_unit_bin_basin <- df_unit %>% filter(Basin_long_name == "Indus") %>%
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, Basin_long_name, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-12) %>% ungroup() %>%
  left_join(log_bins, by = "index")

df_unit_bin_basin$depletion_limit <- factor(df_unit_bin_basin$depletion_limit,
                                              levels = c(0.4, 0.25, 0.05))

df_unit_bin_basin %>% group_by(depletion_limit, Basin_long_name) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(depletion_limit) -> df_unit_bin_basin_cumsum

df_unit_bin_basin_cumsum$depletion_limit <- factor(df_unit_bin_basin_cumsum$depletion_limit,
                                                     levels = c(0.4, 0.25, 0.05))


# plot basin cost curves
plot_basin <- egg::ggarrange(
  ggplot(df_unit_bin_basin) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, color = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    scale_x_sqrt() +
    # facet_wrap(~basin, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (1,000 km'^3*')'), fill = "Depletion Limit", tag = "(c)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  ,

  ggplot(df_unit_bin_basin_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin,
                  color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    # facet_wrap(~basin, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (1,000 km'^3*')'), color = "Depletion Limit", tag = "(d)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  , nrow = 1, top = "Basin Cost Curves: Indus")


ggsave(plot = plot_basin, paste0(figs_dir, "unit_cost_basin", saveformat), width = 9, height = 4, units = "in", dpi = 300)


## grid  ----
# select a good looking grid cell
df_unit %>%
  # filter or select cells that have all 3 depletion limits
  group_by(GridCellID) %>%
  summarise(n_depletion_limits = n_distinct(depletion_limit)) %>%
  filter(n_depletion_limits == 3) %>% ungroup() %>% select(-n_depletion_limits) %>%
  left_join(df_unit, by = "GridCellID") %>%
  filter(country %in% c("South Africa"), #"South Africa", "Namibia", "Botswana"
         year_number > 20,
         depletion_limit == 0.05) -> select_grid


# breaks for the log-scale bins
log_breaks_grid <- unique(c(seq(0.0001, 0.01, by = 0.00025),
                            seq(0.01, 1, by = 0.0005),
                            seq(1, 10, by = 0.01),
                            seq(10, 100, by = 1)))
log_bins_grid <- data.frame(index = seq_along(log_breaks_grid), unit_cost_bin = log_breaks_grid)

# cut unit_cost into the defined bins
df_unit_bin_grid <- df_unit %>% filter(GridCellID == c(12429)) %>% # ok one 12429 for now
  group_by(depletion_limit) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(depletion_limit, GridCellID, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-9) %>% ungroup() %>%
  left_join(log_bins, by = "index")

df_unit_bin_grid$depletion_limit <- factor(df_unit_bin_grid$depletion_limit,
                                              levels = c(0.4, 0.25, 0.05))

df_unit_bin_grid %>% group_by(depletion_limit, GridCellID) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(depletion_limit) -> df_unit_bin_grid_cumsum

df_unit_bin_grid_cumsum$depletion_limit <- factor(df_unit_bin_grid_cumsum$depletion_limit,
                                                     levels = c(0.4, 0.25, 0.05))


# plot grid cost curves
plot_grid <- egg::ggarrange(
  ggplot(df_unit_bin_grid) +
    # geom_line(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, color = as.factor(depletion_limit))) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(depletion_limit)), position = 'identity') + #alpha = as.factor(depletion_limit)
    scale_fill_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 3000)) +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    scale_x_sqrt() +
    # facet_wrap(~GridCellID, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (km'^3*')'), fill = "Depletion Limit", tag = "(c)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")
  ,

  ggplot(df_unit_bin_grid_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin,
                  color = as.factor(depletion_limit)), size = 1.25) + #alpha = as.factor(depletion_limit)
    scale_color_manual(values = c("red3", "black", "dodgerblue2")) +
    # scale_alpha_manual(values = c(0.5, 1, 0.5), guide = 'none') +
    # scale_y_continuous(expand = c(0, 0.05)) +
    scale_x_sqrt() +
    # scale_x_log10(breaks = scales::log_breaks(n = 5), expand = c(0, 0)) + #-0.12
    # facet_wrap(~GridCellID, ncol = 6) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (km'^3*')'), color = "Depletion Limit", tag = "(d)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "none")

  , nrow = 1, top = "Grid Cost Curve: ID-12429 in South Africa")


ggsave(plot = plot_grid, paste0(figs_dir, "unit_cost_grid", saveformat), width = 9, height = 4, units = "in", dpi = 300)


## scale cascade map ----

# prep df with geometry for region using mapping
sf_in %>% select(GridCellID, GCAM_basin_ID, Continent, Country, Basin_long_name) %>%
  left_join(mappings_all %>%
              filter(region == "Africa_Northern") %>%
              select(GCAM_basin_ID = GCAM_basin_ID, region), by = "GCAM_basin_ID") %>% drop_na(region) -> scale_map_region

# plot a blank map with borders of Globe, United States, Australia, Indus Basin, West Africa
unit_cost_scale_map <- ggplot() +
  # borders("world", fill="white",colour="black") +
  geom_sf(data = sf_in, fill = "grey85", color = "grey85", size = 0.25) + # global
  geom_sf(data = scale_map_region, fill = "red4", color = "red4") + # region
  geom_sf(data = sf_in %>% filter(Continent == "Au"), fill = "gold2", color = "gold2") + # continent
  geom_sf(data = sf_in %>% filter(Country == "United States"), fill = "darkblue", color = "darkblue") + # country
  geom_sf(data = sf_in %>% filter(Basin_long_name == "Indus"), fill = "darkgreen", color = "darkgreen") + # basin
  # geom_sf(data = sf_in %>% filter(GridCellID %in% c("12429")), fill = "black", color = "black", size = 10) +
  geom_sf(data = sf_in %>% filter(GridCellID == "12416"), fill = "black", color = "black", size = 10) + # grid, full area
  coord_sf(expand = FALSE) + # makes sure the plot doesn't extend beyond Earth (also plots y axis ticks and crops)
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = grid::unit(c(-4, 0, -4, 0), "cm"),
                          legend.justification=c(0,0), legend.position= "none"
                          # legend.title = element_text(face = "bold")
  )

ggsave(unit_cost_scale_map, paste0(figs_dir, "unit_cost_scale_map", saveformat), width = 11, height = 3.5, units = "in", dpi = 300)










# ARCHIVE ##################################################
ggplot(input) +
  geom_histogram(aes(x = MEAN_Poros), bins = 10)


# individually plot map usin ggplot
ggplot(df_out %>% select(c("available_volume", "geometry")) %>%
         mutate(available_volume = available_volume * 1e-9)) +
  geom_sf(aes(fill = .data[["available_volume"]]), color = NA) +
  scale_fill_gradientn(name = "Available Volume (km3)",
                       colors = c(brewer.pal(9, "Blues")),
                       values = scales::rescale(sort(c(min(df_out[["available_volume"]]),
                                                       kmeans(df_out[["available_volume"]], centers = 4)$center, # breaks by kmeans clustering
                                                       max(df_out[["available_volume"]]))))) +
  coord_sf(expand = FALSE) + # makes sure the plot doesn't extend beyond Earth (also plots y axis ticks and crops)
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.margin = grid::unit(c(-5, 0, -5, 0), "cm"),
                          legend.justification=c(0,0), legend.position=c(0,0),
                          legend.title = element_text(face = "bold"))

# tmap stuff
method     <- "kmeans"

# single map ----
tm <- tm_shape(input) +
  tm_fill("MEAN_Perme", style = "pretty") +
  tm_borders(col = "transparent") +
  tm_layout(frame = FALSE,
            legend.position = c("left", "bottom"),
            inner.margins = c(0, 0, 0, 0))

tm
# tmap_save(tm, filename = "processing/map_porosity.png", width = 12, height = 5, units = "in", dpi = 300)

# using tmap
for (plot in colnames(plot_input)[1:(length(plot_input) - 5)]) { # plot all input variables except the last
  tm_plot <- tm_shape(input) +
    tm_fill(plot, style = method, palette = pal[plot], legend.title = F) +  # Use the color palette from 'pal'
    tm_borders(col = "transparent") +
    tm_layout(frame = FALSE,
              legend.position = c("left", "bottom"),
              inner.margins = c(0, 0, 0, 0)) +
    tm_add_legend(type = "fill", title = legendname[plot])

  # save each plot
  # plotname <- paste0("processing/map_", gsub(" ", "", legendname[plot]), "_", method, ".png")
  # tmap_save(tm_plot, filename = plotname, width = 11, height = 4.5, units = "in", dpi = 300)
}

tm_plot

# single scatter plot
## (1a) hydraulic_conductivity (K) vs. Well pumping rate (Q)
df_scatter %>% select(c("GridCellID", "hydraulic_conductivity", "well_yield")) %>%
  group_by(GridCellID) %>% unique() %>%
  mutate(plength = nrow(.)) %>% ungroup() %>%
  ggplot(aes(x = hydraulic_conductivity * 86400, y = well_yield * 86400)) +
  geom_point(size = 1) +
  geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
  stat_ellipse(level = 0.99, color = "red3", size = 1) +
  geom_text(aes(label = paste0("n = ", plength)),
            x = Inf, y = -Inf, hjust = 1.3, vjust = -0.9,
            size = 4, fontface = "italic") +
  labs(x = "Hydraulic Conductivity (m/day)", y = expression('Well Yield (m'^3*'/day)'), tag = "(a)") +
  my_theme()

ggsave(paste0(figs_dir, "scatter_", "hydraulic_conductivity_", "well_yield", saveformat),
       width = 5.5, height = 5, units = "in", dpi = 300)

ggExtra::ggMarginal(p, color = NA, fill = "red3", lwd = 1, size = 10, alpha = 0.9,
                    outlier.shape = NA, notch = T) # type = "boxplot"

# function for scatter plots

plot_scatter <- function(data, x_var, y_var, labels) {
  p <- data %>%
    select(GridCellID, {{x_var}}, {{y_var}}) %>%
    group_by(GridCellID) %>%
    unique() %>%
    mutate(plength = nrow(.)) %>% ungroup() %>%
    ggplot(aes(x = {{x_var}} * 86400, y = {{y_var}} * 86400)) +
    geom_point(size = 1) +
    geom_smooth(linetype = "dashed", fill = "lightblue", size = 1) +
    stat_clip_ellipse(level = 0.99, geom="polygon", color = "red3", size = 1, fill = NA) +
    geom_text(aes(label = paste0("n = ", plength)), x = Inf, y = -Inf, hjust = 1.3, vjust = -0.9,
              size = 4, fontface = "italic") +
    labs(x = labels[1], y = labels[2], tag = labels[3]) +
    my_theme()

  ggsave(paste0(figs_dir, "scatter_", substitute(x_var), "_", substitute(y_var), saveformat),
         width = 5.5, height = 5, units = "in", dpi = 300)

  return(p)
}

## (1a) hydraulic_conductivity (K) vs. Well pumping rate (Q)
plot_scatter(df_scatter, hydraulic_conductivity, well_yield,
             c("Hydraulic Conductivity (m/day)", expression('Well Yield (m'^3*'/day)'), "(a)"))

# (1b) T vs. Well pumping rate
plot_scatter(df_scatter, transmissivity, well_yield,
             c(expression("Transmissivity (m"^2*"/day)"), expression('Well Yield (m'^3*'/day)'), "(b)"))


# unit cost diagnostics
# scatter
ggplot(df_unit, aes(x = unit_cost, y = volume_produced_allwells * 1e-9)) +
  geom_point(size = 1, color = "steelblue2") +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Volume Produced (km3)") +
  my_theme()

# probability density function (PDF)
ggplot(df_unit, aes(x = volume_produced_allwells * 1e-9)) +
  geom_density(fill = "steelblue2", alpha = 0.5) +
  labs(x = "Volume Produced (km3)", y = "Probability Density") +
  my_theme()

# cumulative distribution function (CDF)
ggplot(df_unit, aes(x = volume_produced_allwells * 1e-9)) +
  stat_ecdf(geom = "step", color = "red4") +
  labs(x = "Volume Produced (km3)", y = "Cumulative Probability") +
  my_theme()

ggplot(df_unit, aes(x = unit_cost)) +
  stat_ecdf(geom = "step", color = "red4") +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Cumulative Probability") +
  my_theme()

# 2D probability density
ggplot(df_unit, aes(x = unit_cost, y = volume_produced_allwells * 1e-9)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon", color = NA, alpha = 0.3) +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Volume Produced (km3)") +
  my_theme()

# histograms and column plots
ggplot(df_unit) +
  geom_col(aes(x = unit_cost, y = volume_produced_allwells * 1e-9)) +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Volume Produced (km3)") +
  my_theme()

ggplot(df_unit, aes(x = unit_cost, y = volume_produced_allwells * 1e-9)) +
  geom_histogram(stat = "identity", color = "black", fill = "lightblue", bins = 10) +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Volume Produced (km3)") +
  my_theme()


# global cost curves
ggplot(df_unit_bin_global) +
  geom_col(aes(x = unit_cost_bin, y = volume_produced_allwells_bin), color = "black") +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Volume Produced (km3)") +
  my_theme()

ggplot(df_unit_bin_global %>% filter(depletion_limit == 0.25), aes(x = unit_cost_bin, y = volume_produced_allwells_bin)) +
  geom_area(stat = "identity", color = "black", fill = "gray15", size = 1) +
  labs(x = expression("Unit Cost ($/m"^3*")"), y = "Volume Produced (km3)") +
  scale_y_continuous(expand = c(0, 3000)) + scale_x_log10(breaks = scales::log_breaks(n = 10), expand = c(0, 0)) +
  annotation_logticks(sides = "b") +
  my_theme()
