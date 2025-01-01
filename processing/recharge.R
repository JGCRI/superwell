# test initial experiments with recharge

# load libraries
library(tidyverse)

# load data
no_recharge <- read_csv("outputs/vsuperwell_py_deep_C_all_B_all_G_91942_0.3PD_0.25DL_0.0RR.csv")
# recharge <- read_csv("outputs/vsuperwell_py_deep_C_all_B_all_G_91942_0.3PD_0.25DL_0.1RR.csv")
real_recharge_LO <- read_csv("outputs/superwell_py_deep_C_all_B_all_G_91942_0.3PD_0.25DL_0.2RR.csv") # this run has very low recharge
# real_recharge_HI <- read_csv("outputs/superwell_py_deep_C_all_B_all_G_9743_0.3PD_0.25DL_0.2RR.csv") # this run has very high recharge
# real_recharge_HI_Shallow <- read_csv("outputs/superwell_py_deep_C_all_B_all_G_9743_0.3PD_0.25DL_0.9RR.csv")

df_recharge <- bind_rows(no_recharge, real_recharge_LO)

# Define the variables to plot ------------------------------------------------
vars_to_plot <- c("depth_to_water", "aqfr_sat_thickness", "drawdown", "total_head",
                  "total_well_length", "volume_recharged", "volume_produced_allwells",
                  "cumulative_vol_produced_allwells", "nonenergy_cost", "energy_cost",
                  "total_cost_allwells", "unit_cost")

# Select the relevant columns including year_number and recharge_ratio
df_long <- df_recharge %>%
  select(year_number, recharge_ratio, all_of(vars_to_plot)) %>%
  pivot_longer(cols = all_of(vars_to_plot), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = vars_to_plot))

# Plot the data
ggplot(df_long, aes(x = year_number, y = value, color = factor(recharge_ratio))) +
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = c("black", "red2", "dodgerblue3", "forestgreen")) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Model Year Number", y = "Value", color = "Recharge Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))



# Cost curves ---------------------------------------------------------
# simple plots
plot(recharge$unit_cost, recharge$cumulative_vol_produced_allwells * 1e-9, type = "l", col = "red", xlab = "Unit Cost ($/m^3)", ylab = "Cumulative Volume Produced (km^3)", main = "Cost Curve: 10% recharge")

plot(no_recharge$unit_cost, no_recharge$cumulative_vol_produced_allwells * 1e-9, type = "l", col = "red", xlab = "Unit Cost ($/m^3)", ylab = "Cumulative Volume Produced (km^3)", main = "Cost Curve: No recharge")


# cost curve using binnings --------------------------------------------
df_unit <- df_recharge %>%
  select(c("grid_id", "year_number", "recharge_ratio", "continent", "country", "gcam_basin_id", "Basin_long_name",
           "volume_produced_allwells", "unit_cost", "grid_area")) %>%
  arrange(unit_cost)

# breaks for the log-scale bins
log_breaks_grid <- unique(c(seq(0.0001, 0.01, by = 0.00025),
                            seq(0.01, 1, by = 0.00025),
                            seq(1, 10, by = 0.01),
                            seq(10, 100, by = 1)))
log_bins_grid <- data.frame(index = seq_along(log_breaks_grid), unit_cost_bin = log_breaks_grid)

# cut unit_cost into the defined bins
df_unit_bin_grid <- df_unit %>% # filter(grid_id == c(12429)) %>% # ok one 12429 for now
  group_by(recharge_ratio) %>%
  mutate(index = cut(unit_cost, breaks = log_breaks_grid, include.lowest = TRUE, labels = FALSE)) %>%
  group_by(recharge_ratio, grid_id, index) %>%
  summarise(volume_produced_allwells_bin = sum(volume_produced_allwells) * 1e-9) %>% ungroup() %>%
  left_join(log_bins_grid, by = "index")

df_unit_bin_grid$recharge_ratio <- factor(df_unit_bin_grid$recharge_ratio,
                                           levels = c(0, 0.1))

df_unit_bin_grid %>% group_by(recharge_ratio, grid_id) %>% arrange(unit_cost_bin) %>%
  mutate(cumm_volume_produced_allwells_bin = cumsum(volume_produced_allwells_bin)) %>%
  arrange(recharge_ratio) -> df_unit_bin_grid_cumsum

df_unit_bin_grid_cumsum$recharge_ratio <- factor(df_unit_bin_grid_cumsum$recharge_ratio,
                                                 levels = c(0, 0.1))


## plot grid cost curves -----
plot_grid <- egg::ggarrange(
  ggplot(df_unit_bin_grid) +
    geom_area(aes(x = unit_cost_bin, y = volume_produced_allwells_bin, fill = as.factor(recharge_ratio)), position = 'identity') + #alpha = as.factor(recharge_ratio)
    scale_fill_manual(values = c("red3", "dodgerblue2")) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Volume Produced (km'^3*')'), fill = "Recharge Ratio", tag = "(c)") +
    my_theme() + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ,

  ggplot(df_unit_bin_grid_cumsum) +
    geom_line(aes(x = unit_cost_bin, y = cumm_volume_produced_allwells_bin,
                  color = as.factor(recharge_ratio)), size = 1.25) + #alpha = as.factor(recharge_ratio)
    scale_color_manual(values = c("red3", "dodgerblue2")) +
    labs(x = expression('Unit Cost ($/m'^3*')'), y = expression('Cumulative Volume Produced (km'^3*')'), color = "Recharge Ratio", tag = "(d)") +
    # annotation_logticks(sides = "b") +
    my_theme() + theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

  , nrow = 1, top = "Grid Cost Curve")















# ARCHIVE ##############################################
# plot cost curve (this shows a kink, that's why we need the binning)
ggplot(df_recharge) +
  geom_point(aes(x = unit_cost, y = cumulative_vol_produced_allwells * 1e-9, color = factor(recharge_ratio)), size = 1) +
  scale_color_manual(values = c("red2", "dodgerblue3")) +
  labs(x = "Unit Cost ($/m^3)", y = "Cumulative Volume Produced (km^3)", color = "Recharge Ratio") +
  theme_bw() +
  theme(legend.position = "bottom")


