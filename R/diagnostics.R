# PREAMBLE    ########
# Diagnostics and post processing of superwell outputs
#
# Hassan Niazi, hassan.niazi@pnnl.gov
# Jan 2023, JGCRI, PNNL

library(tidyverse)
library(ggplot2)
library(GGally)
library(ggpubr)
library(plotly)
library(psych)

#rm(list = ls())

# General types of plots:
# single variables plots for all key variables (static and dynamic)
# two variable plots for key variables (static and dynamic)
# multi-variable matrix plots (static only)

# load data     ########
country <- "United States"
output_filename <- paste0(gsub(" ", "_", tolower(country)), ".csv")
output_dir <- "outputs/"
read.csv(paste0(output_dir, output_filename)) -> outdata_all
#read.csv("outputs/united_states.csv") -> outdata_all
#read.csv("outputs/2023-04-14_pakistan.csv") -> outdata_all
#read.csv("outputs/2023-04-0_united_states.csv") -> outdata_all

outdata_all %>% select(!c(gcam_basin_name, country_name, continent)) -> outdata

# new
ggplot() +
  # geom_histogram(aes(x = irrigated_depth)) +
  geom_point(aes(x = outdata$cumulative_vol_produced_allwells, y = outdata$unit_cost), shape = ".", alpha = 1) +
  # stat_ecdf(aes(x = irrigated_depth), geom = "step") +
  labs(x = "Cumulative Vol Produced All Wells", y = "Unit Cost") +
  theme_bw()

# old
ggplot() +
  # geom_histogram(aes(x = irrigated_depth)) +
  geom_point(aes(x = outdata$total_volume_produced, y = outdata$unit_cost), shape = ".", alpha = 1) +
  # stat_ecdf(aes(x = irrigated_depth), geom = "step") +
  labs(x = "Cumulative Vol Produced All Wells", y = "Unit Cost") +
  theme_bw()

# plot irrigated depth
outdata %>% mutate(irrigated_depth = volume_produced_perwell/areal_extent) -> irr_depth

ggplot() +
  # geom_histogram(aes(x = irr_depth$irrigated_depth)) +
  geom_point(aes(x = irr_depth$irrigated_depth, y = 1:nrow(irr_depth))) +
  # stat_ecdf(aes(x = irr_depth$irrigated_depth), geom = "step") +
  labs(x = "Irrigated Depth", y = "Count") +
  theme_bw()

max(irr_depth$irrigated_depth)
plot(irr_depth$irrigated_depth)


outdata %>% filter(number_of_wells > 5000) %>% arrange(number_of_wells) %>% mutate(Nwells = "Nwells_HI") -> outdata_Nwells_hi
outdata %>% filter(number_of_wells < 10) %>% arrange(number_of_wells) %>% mutate(Nwells = "Nwells_LO") -> outdata_Nwells_lo
outdata_Nwells <- bind_rows(outdata_Nwells_hi, outdata_Nwells_lo)

dfhistplot <- outdata_Nwells_hi
ggplot(dfhistplot) +
  geom_histogram(aes(x = dfhistplot$number_of_wells)) +
  geom_point(aes(y = 1:nrow(dfhistplot), x = dfhistplot$number_of_wells)) +
  labs(x = "Number of Wells", y = "Count") +
  theme_bw()

dfplot_Nwells <- outdata_Nwells
headers_all <- colnames(dfplot_Nwells)
dpNwells <- ggplot(dfplot_Nwells) + theme_bw() + theme(axis.title.x = element_blank())
outplots_pts_Nwells <- list()
for (head in headers_all) {
  outplots_pts_Nwells[[head]] <- dpNwells +
    geom_point(aes(x = 1:nrow(dfplot_Nwells), y = .data[[head]], colour = Nwells)) + #), shape = ".", alpha = 1)
    scale_color_manual(values = c("darkred","yellow2")) +
    labs(y = head) +
  theme(legend.position="none")
  # uncomment to save all individual plots
  # ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_", head, ".png"))
}

ggarrange(plotlist = outplots_pts_Nwells, ncol = 9, nrow = 5)
#ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_pts_Nwells_all_v3.png"), width = 16, height = 9, units = "in")

# plot all cdfs

outdata_all %>% select(!c(gcam_basin_name, country_name, continent, depletion_limit, gcam_basin_id, energy_cost_rate, unit_cost_per_km3)) -> outdata_hist

dfplot_histcdf <- outdata_hist
headers_histcdf <- colnames(dfplot_histcdf)

dp_histcdf <- ggplot(dfplot_histcdf) + theme_bw() + theme(axis.title.x = element_blank())

outplots_pts_histcdf <- list()
outplots_bars_histcdf <- list()
outplots_cdf_histcdf <- list()

for (head in headers_histcdf) {
  outplots_pts_histcdf[[head]] <- dp_histcdf +
    geom_point(aes(x = 1:nrow(dfplot_histcdf), y = .data[[head]])) +
    labs(y = head)

  outplots_bars_histcdf[[head]] <- dp_histcdf +
    geom_histogram(aes(x = .data[[head]])) +
    labs(y = head)

  #stat_ecdf(aes(x = dfhistplot$number_of_wells), geom = "step") +
  outplots_cdf_histcdf[[head]] <- dp_histcdf +
    stat_ecdf(aes(x = .data[[head]]), geom = "step") +
    labs(y = head)


  # uncomment to save all individual histograms
  # ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_hist_", head, ".png"))
}

# ggarrange(plotlist = outplots_pts_histcdf, ncol = 10, nrow = 4)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_histcdf_pts_v0.png"), width = 16, height = 9, units = "in")

ggarrange(plotlist = outplots_bars_histcdf, ncol = 10, nrow = 4)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_histcdf_bars_v1.png"), width = 16, height = 9, units = "in")

ggarrange(plotlist = outplots_cdf_histcdf, ncol = 10, nrow = 4)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_histcdf_cdf_v1.png"), width = 16, height = 9, units = "in")


# STATIC PLOTS  ########
## plot all headers ########
dfplot <- outdata
headers_all <- colnames(dfplot)

dp <- ggplot(dfplot) + theme_bw() + theme(axis.title.x = element_blank())

outplots_pts_all <- list()
outplots_bars_all <- list()

for (head in headers_all) {
  outplots_pts_all[[head]] <- dp +
    geom_point(aes(x = 1:nrow(dfplot), y = .data[[head]])) +
    labs(y = head)

  # uncomment to save all individual plots
  # ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_", head, ".png"))
}

ggarrange(plotlist = outplots_pts_all, ncol = 9, nrow = 5)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_pts_allv2.png"), width = 16, height = 9, units = "in")



 ## plot meaningful variables only ########
headers <-
  c("iteration",
    "year_number",
    "area",
    "aqfr_sat_thickness",
    "thickness",
    "total_head",
    "storativity",
    "hydraulic_conductivity",
    "well_id",
    "radius_of_influence",
    #"drawdown_roi",            # minor rounding variation
    #"radial_extent",           # same as radius of influence
    #"areal_extent",            # quadratic of radial extent
    "number_of_wells",
    "total_volume_produced",
    "volume_produced",
    "available_volume",
    #"continent",
    #"country_name",
    #"gcam_basin_id",          # TODO: arrange the input data so outputs are more organized/ordinal
    #"gcam_basin_name",
    "depletion_limit",  # controlled by depletion_limit but is it controlling depth -> appears so
    "drawdown",
    #"depletion_limit",
    "depth_to_piez_surface",
    #"well_installation_cost",
    "annual_capital_cost",
    #"maintenance_cost",
    "nonenergy_cost", #
    "cost_of_energy",
    #"energy_cost_rate",
    #"electric_energy",         # useful for energy cost section, but not for diagnostics -> function of total_head and well_yield
    "unit_cost"
  )

outplots_pts <- list()
outplots_bars <- list()

for (head in headers) {
  outplots_pts[[head]] <- dp +
    geom_point(aes(x = 1:nrow(dfplot), y = .data[[head]])) +
    labs(y = head)

  outplots_bars[[head]] <- dp +
    geom_histogram(aes(x = .data[[head]])) +
    labs(y = head)

  # uncomment to save all individual histograms
  # ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_hist_", head, ".png"))
  }

ggarrange(plotlist = outplots_pts, ncol = 7, nrow = 3)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_pts_v2.png"), width = 16, height = 9, units = "in")

ggarrange(plotlist = outplots_bars, ncol = 7, nrow = 3)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_bars.png"), width = 16, height = 9, units = "in")

## matrix of variables pairs ########
pairs_static <- ggpairs(outdata[headers]) + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png(filename = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel.png"), width = 2133, height = 1200, units = "px")
#pdf(file = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel.pdf"), width = 16, height = 9)
pairs.panels(outdata[headers],
             method = "pearson",   # correlation method
             hist.col = "dodgerblue",
             rug = FALSE,
             #show.points= FALSE,
             #jiggle = TRUE,
             smooth = TRUE,
             scale = TRUE,
             density = TRUE,       # show density plots
             ellipses = TRUE       # show correlation ellipses
             )
dev.off()

# pretty pair panel (smoothed), but confusing
png(filename = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel_sm.png"), width = 2133, height = 1200, units = "px")
#pdf(file = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel_sm.pdf"), width = 16, height = 9)
pairs.panels(outdata[headers],
             method = "pearson",   # correlation method
             hist.col = "gold",
             rug = FALSE,
             show.points= FALSE,
             smoother = T,
             smooth = TRUE,
             scale = TRUE,
             density = TRUE,       # show density plots
             ellipses = TRUE       # show correlation ellipses
)
dev.off()

# screened variables for pair plots
headers_pairs <-
  c(#"iteration",
    #"year_number",
    "area",
    "aqfr_sat_thickness",
    #"thickness",
    "total_head",
    "storativity",
    "hydraulic_conductivity",
    #"well_id",
    "radius_of_influence",
    #"drawdown_roi",            # minor rounding variation
    #"radial_extent",           # same as radius of influence
    #"areal_extent",            # quadratic of radial extent
    "number_of_wells",
    #"total_volume_produced",
    "volume_produced",
    "available_volume",
    #"continent",
    #"country_name",
    #"gcam_basin_id",          # TODO: arrange the input data so outputs are more organized/ordinal
    #"gcam_basin_name",
    "exploitable_groundwater",  # controlled by depletion_limit but is it controlling depth -> appears so
    "drawdown",
    #"depletion_limit",
    "depth_to_piez_surface",
    #"well_installation_cost",  # why is it varying? Should be somehow linked to well depth?
    #"annual_capital_cost",
    #"maintenance_cost",
    "Total_nonenergy_cost",
    "cost_of_energy",
    #"energy_cost_rate",
    #"electric_energy",         # useful for energy cost section, but not for diagnostics -> function of total_head and well_yield
    "unit_cost"
  )

pairs_static_screened <- ggpairs(outdata[headers_pairs]) + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png(filename = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel_scr.png"), width = 2133, height = 1200, units = "px")
#pdf(file = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel_scr.pdf"), width = 16, height = 9)
pairs.panels(outdata[headers_pairs],
             method = "pearson",   # correlation method
             hist.col = "dodgerblue",
             rug = FALSE,
             #show.points= FALSE,
             #jiggle = TRUE,
             smooth = TRUE,
             scale = TRUE,
             density = TRUE,       # show density plots
             ellipses = TRUE       # show correlation ellipses
)
dev.off()

# pretty pair panel (smoothed), but confusing
png(filename = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel_sm_scr.png"), width = 2133, height = 1200, units = "px")
#pdf(file = paste0("outputs/outplots-diag/", tolower(country), "_pairs_panel_sm_scr.pdf"), width = 16, height = 9)
pairs.panels(outdata[headers_pairs],
             method = "pearson",   # correlation method
             hist.col = "gold",
             rug = FALSE,
             show.points= FALSE,
             smoother = TRUE,
             smooth = TRUE,
             scale = TRUE,
             density = TRUE,       # show density plots
             ellipses = TRUE       # show correlation ellipses
)
dev.off()

# DYNAMIC PLOTS ########

## one-variable single plot ########
plot_ly(outdata) %>%
  add_trace(x = ~1:nrow(outdata),
            y = ~outdata$volume_produced,
            color = ~as.factor(outdata$gcam_basin_id),
            text = ~outdata$well_id,
            type = "scatter",
            mode = "markers")

oneVarDynamic <- function(data, var) {
  plot_ly(data) %>%
    add_trace(x = 1:nrow(data),
              y = enquo(var),
              color = as.factor(data$gcam_basin_id),
              text = data$well_id,
              type = "scatter",
              mode = "markers")
  }

oneVarDynamic(outdata, drawdown)
oneVarDynamic(outdata, volume_produced)
oneVarDynamic(outdata, radius_of_influence)
oneVarDynamic(outdata, number_of_wells)
oneVarDynamic(outdata, Total_nonenergy_cost)


## two-variable single plot interactive ########

plot_ly(outdata) %>%
  add_trace(x = ~outdata$radius_of_influence,
            y = ~outdata$volume_produced,
            color = ~as.factor(outdata$gcam_basin_id),
            text = ~outdata$well_id,
            type = "scatter",
            mode = "markers")

twoVarDynamic <- function(data, xvar , yvar, colorvar) {
  # set flexible coloring variable, but set basin ID as default
  if (missing(colorvar)) {
    colorvar = as.factor(data$gcam_basin_id)
  } else { #TODO: faulty, colorvar does not get passed as a column header
    colorvar = as.factor(data$colorvar)
  }
  # plot
  ply2var <- plot_ly(data) %>%
    add_trace(x = enquo(xvar),
              y = enquo(yvar),
              color = colorvar,
              text = data$well_id,
              type = "scatter",
              mode = "markers")
  return(ply2var)
  }

twoVarDynamic(outdata, radius_of_influence, drawdown)
twoVarDynamic(outdata, radius_of_influence, number_of_wells)
twoVarDynamic(outdata, drawdown, depth_to_piez_surface)
twoVarDynamic(outdata, area, year_number)
twoVarDynamic(outdata, area, available_volume)
twoVarDynamic(outdata, year_number, well_id)
twoVarDynamic(outdata, available_volume, year_number)
twoVarDynamic(outdata, radius_of_influence, volume_produced)
twoVarDynamic(outdata, volume_produced, total_volume_produced)

## matrix of variable pairs: interactive ########

for (head in headers_all) {
  outplots_pts_all_ply[[head]] <-
    plot_ly(outdata) %>%
    add_trace(x = ~1:nrow(outdata),
              y = ~.data[[head]], # or just use the variable name
              type = "scatter",
              mode = "markers"
    )

}


outplots_pts_all_intr <- plot_ly(outdata) %>%
  add_trace(x = ~outdata$radius_of_influence,
            y = ~outdata$volume_produced,
            color = ~as.factor(outdata$gcam_basin_id),
            type = "scatter",
            mode = "markers")

# meaningful variables only: interactive

pairs_dynamic <- ggplotly(pairs_static_screened)


## Model comparison metrics
read.csv("outputs/united_states-maxdep1-scen1.csv") -> outdatamip

outdatamip %>% group_by(well_id) %>% mutate(areaPerWell = area/sum(number_of_wells)) -> areaPerWell

# ARCHIVE ######################################################################

#headers <-  c("iteration", "year_number", "area", "radius_of_influence", "drawdown_roi", "areal_extent", "total_head", "aqfr_sat_thickness", "storativity", "thickness", "unit_cost", "hydraulic_conductivity", "radial_extent", "number_of_wells", "volume_produced", "total_volume_produced", "available_volume", "continent", "well_id", "country_name", "gcam_basin_id", "gcam_basin_name", "exploitable_groundwater", "well_installation_cost", "annual_capital_cost", "Total_nonenergy_cost", "maintenance_cost", "cost_of_energy", "energy_cost_rate", "electric_energy", "drawdown", "depletion_limit", "depth_to_piez_surface")
