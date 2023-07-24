# prepare inputs file for superwell
#
# Input: a shapefile containing hydro-geological input data processed using GIS
#
# List of issues this script resolves:
# - disputed territory between China and India
# - recalculates grid area using coordinates
# - adds continents to the shape file
#   - assigns unique continent to countries intersecting adjacent continents
#   - renames NA to NAm to avoid getting NaN
# - renames Côte d'Ivoire to avoid special characters
# - makes sure grid cell IDs are unique
# - renames parameters to be consistent
# - removes any , in basin name to avoid issues with reading in csvs
# - prepares mapping files to be used later
#
# Output: input.csv, input.shp
#
# Hassan Niazi, July 2023

library(tidyverse)
library(sf)

# load data
df <- st_read("inputs/shapefiles/All_merged.shp") %>% st_make_valid()
basin_mapping <- read_csv("inputs/basin_to_country_mapping.csv") %>%
  mutate(Basin_long_name = str_replace_all(Basin_long_name, ",", "_"))
continent_mapping <- read_csv("inputs/continent_county_mapping.csv")

############ checks, cleaning data, fill NAs etc ###############################
length(unique(df$OBJECTID)) == length(df$OBJECTID) # should be TRUE to make sure all grid cell IDs are unique

# check if grid area and shape area are different and which one to take
diff_area <- df %>% filter(COUNTRY == "Australia", CalcArea_m != Shape_Area) %>%
  mutate(areadiff = (CalcArea_m - Shape_Area)*100/Shape_Area,
         Calc_area_geom = st_area(geometry)) %>%
  filter(abs(areadiff) > 0.1)

unique(diff_area$COUNTRY)
plot(diff_area %>% filter(COUNTRY == "Australia") %>% select(areadiff)) # biggest errors
plot(diff_area %>% filter(COUNTRY == "India") %>% select(areadiff)) # normal errors
# flag the area mismatch cells with a new column and plot them to see if there is any spatial coherency to the errors
# decision for now: take CalcArea_m because it reasonable, Shape_Area has a "step" on some latitude but is also reproducible from coordinates so there's a tradeoff

plot(df %>% filter(COUNTRY == "Australia") %>% select(CalcArea_m) %>% mutate(CalcArea_m = CalcArea_m * 1e-6))
plot(df %>% filter(COUNTRY == "Australia") %>% select(Shape_Area) %>% mutate(Shape_Area = Shape_Area * 1e-6))

{# run
# handle NAs
na_check <- function (df) {
  apply(df, 2, function(x) any(is.na(x))) %>% # check which columns have NAs
  as.data.frame() %>% filter(. == T) %>% rownames() # filter the column name with NAs
  }

na_check(df)

replace_missing_country <- function(df, COUNTRY = Country) {
  df %>% mutate(Country = if_else(is.na({{ COUNTRY }}), "China", {{ COUNTRY }}))
  }
}
# filter and plot data with NAs
df_filtered <- df %>% filter(if_any(everything(), is.na))
plot(df_filtered)

{# run
# fill NAs with the nearest country (currently it's disputed part of China and India)
df_fill <- df %>%
  # rename and drop some columns
  select(GridCellID = OBJECTID, Country = COUNTRY, GCAM_basin_ID = GCAM_ID_1,
         WHYClass, Porosity = MEAN_Poros, Permeability = MEAN_Perme, Aquifer_thickness = MEAN_thk_m,
         Depth_to_water = MEAN_Depth, Grid_area = CalcArea_m) %>% # note we took Shape_Area instead of CalcArea
  replace_missing_country() %>%
  mutate(Country = str_replace(Country, "Côte d'Ivoire", "Cote d'Ivoire")) # avoid special characters
}

na_check(df_fill) # should be NULL or character(0)

########### attach continents ##################################################

{# run
# prepare continent to country mapping file
df_in_map <- read_csv("inputs/inputs_old.csv") %>%
  select(Continent, Country = CNTRY_NAME, GCAM_basin_ID = GCAM_ID, WHYClass) %>%
  # rename(Country = CNTRY_NAME, GCAM_basin_ID = GCAM_ID) %>%
  unique() %>%
  # mutate(Country = ifelse(is.na(Country), "China", Country)) %>%
  replace_missing_country() %>%
  mutate(Continent = case_when(
    Country %in% c("Colombia", "Panama") ~ "SA",
    Country %in% c("Egypt", "Israel", "Palestinian Territory") ~ "Af",
    Country %in% c("Spain", "Greece", "Bulgaria") ~ "Eu",
    Country %in% c("Turkey", "Russian Federation", "Kazakhstan", "Georgia", "Azerbaijan") ~ "As",
    Country %in% c("Papua New Guinea") ~ "Oc",
    is.na(Continent) ~ "NAm",
    TRUE ~ Continent)) %>% unique() %>%
  left_join(basin_mapping, by = "GCAM_basin_ID")
}

na_check(df_in_map)

# write_csv(df_in_map, "inputs/continent_county_mapping.csv")
# continent_mapping <- read_csv("inputs/continent_county_mapping.csv")

{# run
# attach continents to input file
df_all <- df_fill %>% left_join(df_in_map, by = c("Country", "GCAM_basin_ID", "WHYClass")) %>%
  select(GridCellID, Continent, Country, GCAM_basin_ID, Basin_long_name, WHYClass,
         Porosity, Permeability, Aquifer_thickness, Depth_to_water, Grid_area, geometry)
}

na_check(df_all)

# duplicate grid cell IDs in df_all
length(df_all$GridCellID) - length(df_fill$GridCellID)
diff_df <- df_all %>% group_by(GridCellID) %>% filter(n() > 1)

unique(diff_df$Country)

# list of countries that have intersection with two continents -> Corrected the mapping file
# [1] "Colombia"              "Panama"                "Egypt"                 "Israel"                "Palestinian Territory" "Spain"
# [7] "Kazakhstan"            "Russian Federation"    "Papua New Guinea"      "Turkey"                "Azerbaijan"            "Greece"
# [13] "Georgia"               "Bulgaria"
# plot(df_all %>% filter(Country == "Colombia") %>% select(Continent))
# plot(df_all %>% filter(Country == "Russian Federation") %>% select(Continent))

plot(df_all %>% filter(Country == "Pakistan") %>% select(Porosity, Permeability, Aquifer_thickness, Depth_to_water))

############# write the inputs file ############################################
{# run
write_csv(st_drop_geometry(df_all), "inputs/inputs.csv")

st_write(df_all, "inputs/shapefiles/inputs.shp", delete_layer = TRUE, trunc_width = Inf, delete_dsn = FALSE)
}
## END
