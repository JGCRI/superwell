# Plots key aquifer properties
#
# Hassan Niazi, June 2023

# load libraries ----
{
library(tidyverse)
library(sf)
library(ggplot2)
# library(tmap)
library(RColorBrewer)
}

# load data ----
{
df_in <- read_csv("inputs/aquifer_properties.csv")
sf_in <- st_read("inputs/shapefiles/inputs.shp") %>% st_make_valid()
colnames(sf_in) <- c(colnames(df_in), "geometry") # correct col names in sf

figs_dir <- "processing/figures/"

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

# high-level analysis on volume ----

df_in %>%  mutate(available_volume_allcells = Porosity * Grid_area * (Aquifer_thickness - Depth_to_water),
                  ponded_depth_avail = Porosity * (Aquifer_thickness - Depth_to_water)) -> df_in_vol


df_in %>%  mutate(Aquifer_thickness = case_when(Aquifer_thickness > 1000 ~ 1000,
                                                TRUE ~ Aquifer_thickness),
                  available_volume_allcells = Porosity * Grid_area * (Aquifer_thickness - Depth_to_water),
                  ponded_depth_avail = Porosity * (Aquifer_thickness - Depth_to_water)) -> df_in_vol_screened

# plot(df_in_vol$available_volume_allcells, df_in_vol$ponded_depth_avail)
{
  # global available volume
  print(paste0("Global Available Volume from inputs = ",
               round(sum(unique(df_in_vol$available_volume_allcells)) * 1e-15, 3), " million km3"))

  print(paste0("Global Available Volume from screened inputs = ",
               round(sum(unique(df_in_vol_screened$available_volume_allcells)) * 1e-15, 3), " million km3"))
}

## summary stats of available, accessible and pumped volumes ----

# since there are duplicate available vol values, we need to group by grid cell before unique
length(unique(df_in_vol$GridCellID))
length(unique(df_in_vol$available_volume_allcells))

global_avail <- df_in_vol %>% select(GridCellID, available_volume_allcells) %>% group_by(GridCellID) %>% unique() %>% ungroup() %>%
  summarise(available = sum(available_volume_allcells) * 1e-15)


# plot all input variables -----------------------------------------------

# setting up the plot
plot_input <- sf_in %>% # filter(Country == "United States") %>%
  mutate(Grid_area_km = Grid_area/10^6, # convert to km2
         Aquifer_thickness = case_when(Aquifer_thickness > 1000 ~ 1000,
                                       TRUE ~ Aquifer_thickness)) %>% # replace all MEAN_thk_m values greater than 1000 with 1000
  select(c("Porosity", "Permeability", "Aquifer_thickness",
           "Depth_to_water", "Grid_area_km", "WHYClass")) # filter only relevant ones
  # rename(c("storativity" = "MEAN_Poros", "permeability" = "MEAN_Perme",
  #          "grid_area_km" = "CalcArea_m", "total_thickness" = "MEAN_thk_m",
  #          "depth_to_water" = "MEAN_Depth", "country_name" = "COUNTRY",
  #          "GridCellID" = "OriginalOb")) # rename to make the join with output smooth


# legendnames <- c("Porosity" = "Porosity (-)", "Permeability" = "Permeability (m2)",
legendnames <- c("Porosity" = "Porosity (-)", "Permeability" = expression('Permeability (m'^2*')'),
                 "Aquifer_thickness" = "Aquifer Thickness (m)", "Depth_to_water" = "Depth to Water (m)",
                 "Grid_area_km" = expression('Grid Area (km'^2*')'), "WHYClass" = "WHY Class") # to label legend properly
# see all palettes available: tmaptools::palette_explorer()
pal <- c("Porosity" = "BuPu", "Permeability" = "YlGn", "Grid_area_km" = "BuGn",
         "Aquifer_thickness" = "YlOrRd", "Depth_to_water" = "Blues", "WHYClass" = "PuBuGn")

max(plot_input$Permeability)

# plot all input variables using ggplot
for (plot in colnames(plot_input)[1:(length(plot_input) - 2)]) { # plot all input variables except last two
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

## END