# prep superwell inputs with recharge
#
# Hassan Niazi, Sept 2024


library(tidyverse)
library(sf)

df_RL <- st_read("inputs/shapefiles/inputs_recharge_plus_lakes.shp") # %>% st_make_valid()

# correct the col names
# "GridCellID"        "Continent"         "Country"           "GCAM_basin_ID"     "Basin_long_name"   "WHYClass"          "Porosity"          "Permeability"      "Aquifer_thickness" "Depth_to_water"    "Grid_area"   "Recharge"    "Lake_area"   "geometry"
colnames(df_RL) <- c("GridCellID", "Continent", "Country", "GCAM_basin_ID", "Basin_long_name", "WHYClass", "Porosity", "Permeability", "Aquifer_thickness", "Depth_to_water", "Grid_area", "Recharge", "Lake_area", "geometry")

write_csv(st_drop_geometry(df_RL), "inputs/inputs_recharge_lakes.csv")




# recharge processing --------------------------------------------------------------
df_R <- st_read("inputs/shapefiles/superwell_inputs_recharge.shp") # %>% st_make_valid()
df_in <- read_csv("inputs/inputs.csv")
st_in <- st_read("inputs/shapefiles/inputs.shp")

# create weighted intersection area (Intersecti) recharge (Doll_Recha) and aggregated to original superwell grid (OBJECTID)
df_R_weighted <- st_drop_geometry(df_R) %>%
  mutate(weighted_recharge = Intersecti * Doll_Recha) %>%
  group_by(OBJECTID) %>%
  summarise(RechargeAvg = sum(weighted_recharge) / sum(Intersecti))

# join recharge to superwell grid
df_in_R <- df_in %>%
  left_join(df_R_weighted, by = c("GridCellID" = "OBJECTID")) %>%
  # filter(is.na(RechargeAvg)) %>% # about 5,544 NAs in 16 countries (so all over the place, can try redoing the ArcGIS process OR even better to try st_intersection in R. Code below for reference)
  # replace all NAs and negative values with 0
  mutate(RechargeAvg = if_else(is.na(RechargeAvg) | RechargeAvg < 0, 0, RechargeAvg))

# attach geometry
st_in_R <- df_in_R %>% left_join(st_in %>% select(GrdClID, geometry), by = c("GridCellID" = "GrdClID"))

# write to csv
write_csv(df_in_R, "inputs/inputs_recharge.csv")
st_write(st_in_R, "inputs/shapefiles/inputs_recharge.shp", delete_layer = TRUE, trunc_width = Inf, delete_dsn = FALSE)

# plot recharge
# df_in_R <- read_csv("inputs/inputs_recharge.csv")
# st_in_R <- st_read("inputs/shapefiles/inputs_recharge.shp")
# histogram of recharge values
ggplot(df_in_R, aes(x = Recharge)) + geom_histogram(bins = 50, fill = "dodgerblue3") +
  labs(x = "Recharge (mm/yr)", y = "Frequency") + theme_bw()

# get average recharge for Godavari 123, Morocco 86, California 217
df_in_R %>% filter(GCAM_basin_ID %in% c(123, 86, 217)) %>% select(GCAM_basin_ID, Basin_long_name, Recharge) %>%
  group_by(GCAM_basin_ID, Basin_long_name) %>% summarise(Recharge = mean(Recharge)) %>% sort(Recharge)

################################################################################
# processing attempt, but then we processed using QGIS so reading the output data above directly
# EXCLUDE LAKES --------------------------------------------------------------
st_hydrolakes <- st_read("inputs/shapefiles/HydroLAKES_polys_v10.shp") %>% st_make_valid()
st_in_R <- st_read("inputs/shapefiles/inputs_recharge.shp") %>% st_transform(crs = st_crs(st_hydrolakes)) %>% st_make_valid()

# print head of st_hydrolakes to see major attributes
head(st_hydrolakes)
colnames(st_hydrolakes)

# we have to maintain st_in_R and attach st_hydrolakes. If the area of st_hydrolakes covers more than 90% of st_in_R then write "1" in lakes_flag column

# intersect superwell grid with lakes and calculate area
st_in_R_lakes <- st_intersection(st_in_R, st_hydrolakes %>% select(Hylak_id, Lake_area, geometry)) %>%
  mutate(area_intersect = st_area(.) %>% as.numeric())


# ARCHIVE --------------------------------------------------------------
# # read the shape file and correct them
# Doll_global <- st_read("../../Data/superwell_inputs/Doll_Recharge_Data/Doll_Global.shp") %>% st_make_valid()
# all_merged <- st_read("inputs/shapefiles/All_merged.shp") %>% st_transform(crs = st_crs(Doll_global)) %>% st_make_valid()
#
# # intersect to get recharge cells within superwell grid
# intersected <- st_intersection(all_merged, Doll_global) %>%
#   mutate(area_intersect = st_area(.) %>% as.numeric())
#
# # area-weighted average recharge for each superwell grid
# result <- intersected %>%
#   group_by(OBJECTID) %>%
#   summarise(recharge = sum(area_intersect * Doll_Recha) / sum(area_intersect))
#
# st_write(result, "inputs/shapefiles/inputs_recharge_R.shp")
