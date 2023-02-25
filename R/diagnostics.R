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
country <- "all"
output_filename <- paste0(gsub(" ", "_", tolower(country)), ".csv")
output_dir <- "outputs/"
read.csv(paste0(output_dir, output_filename)) -> outdata
#read.csv("outputs/pakistan.csv") -> outdata
outdata %>% select(!gcam_basin_name) -> outdata

# STATIC PLOTS  ########
## plot all headers ########
headers_all <- colnames(outdata)

dp <- ggplot(outdata) + theme_bw() + theme(axis.title.x = element_blank())

outplots_pts_all <- list()
outplots_bars_all <- list()

for (head in headers_all) {
  outplots_pts_all[[head]] <- dp +
    geom_point(aes(x = 1:nrow(outdata), y = .data[[head]])) +
    labs(y = head)

  # uncomment to save all individual plots
  # ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_", head, ".png"))
}

ggarrange(plotlist = outplots_pts_all, ncol = 8, nrow = 4)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_pts_all.png"), width = 16, height = 9, units = "in")

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
    "exploitable_groundwater",  # controlled by depletion_limit but is it controlling depth -> appears so
    "drawdown",
    #"depletion_limit",
    "depth_to_piez_surface",
    #"well_installation_cost",  # why is it varying? Should be somehow linked to well depth?
    "annual_capital_cost",
    #"maintenance_cost",
    "total_nonenergy_cost", # TODO: change to total_nonenergy_cost
    "cost_of_energy",
    #"energy_cost_rate",
    #"electric_energy",         # useful for energy cost section, but not for diagnostics -> function of total_head and well_yield
    "unit_cost"
  )

outplots_pts <- list()
outplots_bars <- list()

for (head in headers) {
  outplots_pts[[head]] <- dp +
    geom_point(aes(x = 1:nrow(outdata), y = .data[[head]])) +
    labs(y = head)

  outplots_bars[[head]] <- dp +
    geom_histogram(aes(x = .data[[head]])) +
    labs(y = head)

  # uncomment to save all individual histograms
  # ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_hist_", head, ".png"))
  }

ggarrange(plotlist = outplots_pts, ncol = 7, nrow = 3)
# ggsave(filename = paste0("outputs/outplots-diag/", tolower(country), "_pts.png"), width = 16, height = 9, units = "in")

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
    "Total_nonEnergy_Cost",
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
oneVarDynamic(outdata, total_nonenergy_cost)


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




# ARCHIVE ######################################################################

#headers <-  c("iteration", "year_number", "area", "radius_of_influence", "drawdown_roi", "areal_extent", "total_head", "aqfr_sat_thickness", "storativity", "thickness", "unit_cost", "hydraulic_conductivity", "radial_extent", "number_of_wells", "volume_produced", "total_volume_produced", "available_volume", "continent", "well_id", "country_name", "gcam_basin_id", "gcam_basin_name", "exploitable_groundwater", "well_installation_cost", "annual_capital_cost", "total_nonenergy_cost", "maintenance_cost", "cost_of_energy", "energy_cost_rate", "electric_energy", "drawdown", "depletion_limit", "depth_to_piez_surface")
