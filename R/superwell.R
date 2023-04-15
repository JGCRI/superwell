# Superwell: A Hydro-Economic Model for Groundwater Supply and Cost
#
# It uses the functions provided hereunder to load the data and simulate. These
# functions are called later in the code to execute the simulation. The
# simulation resolution is flexible to run on any scale (basin, country, grid
# etc) provided in the input data file.
#
# Document outline could be used to navigate through functions (Crtl+Shift+O).
#
# Superwell team (alphabetical): Catherine Yonkofski, Chris Vernon, David J
# Watson, Hassan Niazi, Jim Yoon, Mohamad Hejazi, Neal Graham, Stephen Ferencz,
# Thomas Wild
#
# Contact: Hassan Niazi, hassan.niazi@pnnl.gov
#
# January 2023, Joint Global Climate Research Institute, Pacific Northwest
# National Laboratory, College Park, MD, USA

# TODO: eventually separate superwell into superwell_fun and superwell_run
# TODO: make depletion limit a separate file or dynamic so the code always runs for
# all depletion limits not just one in wellParams.yml

# load functions to start and run superwell in the last line. See man/ for
# documentation of all functions


devtools::load_all(".")

library(yaml)
library(dplyr)

options(stringsAsFactors = FALSE)
#rm(list = ls())


# Constants ----
{
  EULER_CONST     <- 0.5772156649   # to be used in Theis solution
  pi              <- 3.14159
  CONV_KM3_M3  <- 1e+9
  CONV_M3_KM3  <- 1/CONV_KM3_M3
  CONV_M3_AcreFt  <- 0.000810714
  g               <- 9.80665
  errFactor       <- 0.1
}


# Data loading functions ----

#' Load well parameters
#'
#' Load a total of 14 well-specific parameters (e.g., constants, dimensions, costs, etc.) from a wellparam file
#'
#' @param well_param_file Full path with file name and extension to the input well parameters YAML file
#' @return Data frame of well parameters
#' @import  yaml
#' @author Superwell team; hassan.niazi@pnnl.gov
#' @export

load_well_data <- function(well_param_file) {
  #well_param_file = "inputs/wellParams.yml"
  wp <- yaml.load_file(well_param_file)

  # Store well yield data from the yaml file because we will need to go back to it with every node
  wp[["Initial_Well_Yield"]] <- wp$Well_Yield

  # Store a "generic" electricity cost rate in case of an error
  wp[["global_energy_cost_rate"]] <- wp$Energy_cost_rate

  return(wp)
}


#' Load aquifer data by country
#'
#' Read in the aquifer data for countries
#'
#' @param config_file Full path with file name and extension to the input configuration CSV file
#' @return hydro-geological data for each aquifer of the specified country
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter
#' @author Superwell team; hassan.niazi@pnnl.gov
#' @export
# TODO: setup global 'all' as default. load_config <- function(config_file, country = 'United States') {
load_config <- function(config_file, country) {
  # Node-specific input (permeability, porosity, thickness, etc.) are in "Inputs.csv"

  if (country == "All") {
    df <- read.csv(config_file)
    }
    else {
      df <- read.csv(config_file) %>%
      filter(CNTRY_NAME %in% (country))
    }

  return (df)
}


#' Load electricity cost data for each countries
#'
#' Read in the cost of electricity by GCAM basin (USD/Unit for 172 countries)
#'
#' @param el_cost_file Full path with file name and extension to the input GCAM electrical rates YAML file
#' @return electricity energy cost for a country
#' @import yaml
#' @author Superwell team; hassan.niazi@pnnl.gov
#' @export

load_elec_data <- function(el_cost_file) {
  # el_cost_file = "GCAM_Electrical_Rates.yml"
  ec <- yaml.load_file(el_cost_file)

  return(ec)
}


## Ancillary functions
# percent function
percent <- function(x,
                    digits = 2,
                    format = "f",
                    width = 5,
                    ...) {
  paste0(formatC(100 * x, format = format, digits = digits, width = width, ...), "%")
}

# Groundwater calculation functions ----

#' Theis solution
#'
#' Calculates drawdown using Theis solution
#'
#' @details affected by initial yield, annual operational time, and aquifer properties (T and S)
#' @param wp well parameters from wellParams.yml
#' @param t annual operational time, comes from wellParams.yml (currently fixed at 365 days i.e., 31536000 s/yr)
#' @param rw radius of well initially, but radius of influence eventually.
#'
#' @return drawdown using Theis solution
#' @author Superwell team; hassan.niazi@pnnl.gov
#' @export

# To run as a standalone function
# wp  <- load_well_data(well_params)
# t   <- wp$Max_Lifetime_in_Years
# rw  <- wp$Well_Diameter * 0.5      # only to calculate at the edge of the well (actually radius of influence is used)
# df  <- load_config(config, country)
# i = 20
# wp[["Storativity"]]             <- df[i, "Porosity"]
# wp[["Hydraulic_Conductivity"]]  <- (10 ^ df[i, "Permeability"]) * g / (1.7918 * 1e-6)
# wp[["Transmissivity"]]          <- wp$Hydraulic_Conductivity * wp$Aqfr_Sat_Thickness


calcWellsTheis <- function(t, rw, wp) {
  # Pumping well drawdown <- 2.3Q/4piT*log(2.25Tt/r2S)
  W <- 0.0
  u <- rw ^ 2 * wp$Storativity / (4 * wp$Transmissivity * t * wp$Annual_Operation_time) # r2S/4Tt (t = lifetime * annualOperationTime)

  j <- 1
  term <- -u
  W <- -EULER_CONST - log(u) - term       # well function

  while (abs(term) > 0.00000001 && term != Inf) {
    term <- -u * j / (j + 1) ^ 2 * term
    W <- W - term
    j <- j + 1
  }

  s <- (wp$Well_Yield / (4.0 * pi * wp$Transmissivity) * W)     # drawdown from Theis [m]

  result <- list(s = s, t = t, W = W)
  return(result)
}


#' Main Superwell function
#'
#' Calculates cost of supplying groundwater (i.e. cost curves)
#'
#' @param well_params load all well parameters from load_well_data function which takes wellParams.yml as an input
#' @param elec_rates load all GCAM electricity costs from load_elec_data which takes GCAM_Electrical_Rates.yml as an input
#' @param config_file load all aquifer data for a country from load_config function which takes inputs.csv as an input. inputs.csv is generated from digitized maps from 4 datasets on aquifer properties
#' @param output_dir specifies output directory
#'
#' @return cost curves of nonrenewable groundwater in basins of a country
#' @author Superwell team; hassan.niazi@pnnl.gov
#' @export

superwell <- function(well_params, elec_rates, config_file, runcountry, output_dir) {

  # prep data ----
  #
  # well specific parameters (e.g., costs, etc.) are in "wellParams.yml" which is in yaml format
  wp <- load_well_data(well_params)

  # electrical USD/Unit costs by GCAM basin number
  ec <- load_elec_data(elec_rates)

  # store the well yield from the yaml file because we will need to go back to it with every node
  wp[["Initial_Well_Yield"]] <- wp$Well_Yield

  # store a "generic" Electricity cost rate, irrespective of the country being processed, in case a country specific cost estimate is not available. However, country-specific takes precedence if available
  wp[["global_energy_cost_rate"]] <- wp$Energy_cost_rate

  # specify country name if running for a country, otherwise 'All' in config will run globally
  country <- runcountry
  print(paste0("Processing:  ", country))
  # "inputs.csv" contains all the node specific input (permeability, porosity, thickness, etc.)
  df <- load_config(config, country)

  # Set up the output file.
  # This file will be written to after the completion of every node so the
  # output may be copied and used at any point during the run.

  #fileName <- paste(df[1, "CNTRY_NAME"], "_WellResults.csv")
  #fileName <- paste("MENA", "_WellResults.csv")
  output_filename <- paste0(Sys.Date(),"_", gsub(" ", "_", tolower(country)),".csv")
  #output_filename <- paste0(format(Sys.time(), "%Y-%m-%d-t%H.%M"),"_", gsub(" ", "_", tolower(country)),".csv")
  output_file <- file.path(output_dir, output_filename)

  con <- file(output_file, "w") # open the file to write

  # write the headers for the output file
    cat("iteration, year_number, depletion_limit, continent, country_name, gcam_basin_id, gcam_basin_name, well_id, grid_area, permeability, storativity, total_thickness, depth_to_piez_surface, orig_aqfr_sat_thickness, aqfr_sat_thickness, hydraulic_conductivity, transmissivity, radius_of_influence, areal_extent, max_drawdown, drawdown, total_head, well_yield, volume_produced_perwell, cumulative_vol_produced_perwell, number_of_wells, volume_produced_allwells, cumulative_vol_produced_allwells, available_volume, depleted_vol_fraction, well_installation_cost, annual_capital_cost, maintenance_cost, nonenergy_cost, power, energy, energy_cost_rate, energy_cost, total_cost_perwell, total_cost_allwells, unit_cost, unit_cost_per_km3, unit_cost_per_acreft, whyclass, screen_length, casing_length, total_well_length\n",
    file = con
  )

  # sims per cell ----
  #
  # each row in df is a different grid node in the model domain
  for (i in 1:(nrow(df))) {

    # print progress to the console
    if (i < nrow(df) && i %in% seq(1, nrow(df), by=round(nrow(df)/20))) {
      prog <- percent(c(i/nrow(df)))
      print(paste(prog,"--",formatC(i,format="G",digits=6),"/",nrow(df),"--","Processing",df[i,"CNTRY_NAME"]))
    } else if (i == nrow(df)) {
      print("ALL DONE!")
      print(paste("Output file is in folder:",output_dir,"named as:",output_filename))
      print("Model time in seconds:")
    }

    ## corrections, constraints and checks ----

    skip <- 0

    # skip grid areas less than 10x10 km
    # these are on lower tail resulting from abnormal intersections of grid and WHYMAP area class map in GIS
    if (df[i, "Area"] <= 1e+8){
      skip = skip + 1
      next
    }

    # depth to water table should we at least 5 meters
    else if (df[i, "Depth"] < 5){
      #df[i, "Depth"] <- 5
      skip = skip + 1
      next
    }

    # limit low permeability values
    # which cause low hydraulic conductivity, low transmissivity, low well yield,
    # small radius of influence, hence large number of wells in a grid cell,
    # resulting in huge capital and total costs
    else if (df[i, "Permeability"] < -15 ){
      #df[i, "Permeability"] <- -15      # limits K to 1e-08
      skip = skip + 1
      next
    }

    # limit porosity to 5% voids at least
    else if (df[i, "Porosity"] < 0.05){
      #df[i, "Porosity"] <- 0.05     # limits K to 1e-08
      next #TODO: cite number of cell skipped overall
      skip = skip + 1
    }

    # correct aquifer thickness outliers, replace >1000m thickness with 200m
    else if (df[i, "Thickness"] > 1000){
      df[i, "Thickness"] <- 200
    }

    # use global electricity cost if country information is missing
    if (is.null(ec[[df[i, "CNTRY_NAME"]]]) == FALSE) {
      wp[["Energy_cost_rate"]] <- ec[[df[i, "CNTRY_NAME"]]]
    } else {
      wp[["Energy_cost_rate"]] <- wp$global_energy_cost_rate
    }

    # grid prep ----
    #
    # Calculate and store grid specific attributes
    wp[["Well_Yield"]]      <- wp$Initial_Well_Yield
    wp[["roi_boundary"]]    <- 1

    wp[["Total_Thickness"]] <- df[i, "Thickness"]
    wp[["Depth_to_Piezometric_Surface"]] <- df[i, "Depth"]
    wp[["Orig_Aqfr_Sat_Thickness"]] <- wp$Total_Thickness - wp$Depth_to_Piezometric_Surface # m
    wp[["Aqfr_Sat_Thickness"]]  <- wp$Orig_Aqfr_Sat_Thickness # m (initialized but would be updated using pumping info)

    wp[["Screen_length"]]   <- wp$Orig_Aqfr_Sat_Thickness * 0.3           # m TODO: assumption, why 30%?
    wp[["Casing_length"]]   <- wp$Orig_Aqfr_Sat_Thickness + wp$Depth_to_Piezometric_Surface - wp$Screen_length # m
    wp[["Total_Well_Length"]]   <- wp$Casing_length + wp$Screen_length    # m

    wp[["Storativity"]]     <- df[i, "Porosity"] # assuming storativity is equal to porosity
    wp[["Hydraulic_Conductivity"]] <- (10 ^ df[i, "Permeability"]) * 1e7
    #wp[["Hydraulic_Conductivity"]] <- (10 ^ df[i, "Permeability"]) * g / (1.7918 * 1e-6) #TODO: check with kinematic viscosity = 1e6 or 1.7918 * 1e-6
    wp[["Transmissivity"]]  <- wp$Hydraulic_Conductivity * wp$Aqfr_Sat_Thickness   # T = Kb [m2/s]


    # different installation cost based on geologically different WHYclasses
    if (df[i, "WHYClass"] == 10) {
      wp[["Well_Installation_Cost"]] <- wp$Well_Install_10 * wp$Total_Well_Length
    } else {
      if (df[i, "WHYClass"] == 20) {
        wp[["Well_Installation_Cost"]] <- wp$Well_Install_20 * wp$Total_Well_Length
      } else {
        wp[["Well_Installation_Cost"]] <- wp$Well_Install_30 * wp$Total_Well_Length
      }
    }

    #TODO: test with multiple ratios, use an array initially e.g. [0.2, 0.8] but later make it probabilistic (Normal distribution + Monte Carlo)
    maxDepthRatio = 0.66
    wp$Max_Drawdown = maxDepthRatio * wp$Orig_Aqfr_Sat_Thickness  # two-thirds depletion limit

    # minimum saturated aquifer thickness at the well head (total aquifer depth - max drawdown)
    drawdownLimit <- wp$Orig_Aqfr_Sat_Thickness - wp$Max_Drawdown

    # # simulation loop ----

    # Initializations
    outputList <- list()

    wp[["Depleted_Vol_Fraction"]] <- 0
    wp[["Cumulative_Vol_Produced_allWells"]] <- 0
    wp[["Total_Head"]] <- 0
    wp[["Drawdown"]]   <- 0

    TotTime       <- 0       # counter that allows maximum iterations
    run           <- 0
    NumIterations <- 1

    while (run == 0) {

      ### iterate on depth, drawdown, and runtime ----
      # depth of depletion limit is higher than available groundwater depth
      while ((wp$Depleted_Vol_Fraction < wp$Depletion_Limit) &&
             (wp$Max_Drawdown >= 1) && (run == 0) && (TotTime <= 800)) { #800 allows at least 20 iterations in year 20
        # initialize
        s <- 0
        t_max <- wp$Max_Lifetime_in_Years

        # Jacob correction for observed drawdown. This is the drawdown to be used with the Theis solution.
        sadj <- wp$Max_Drawdown - ((wp$Max_Drawdown ^ 2) / (2 * wp$Aqfr_Sat_Thickness))

        # first: compute drawdown with initial Q guess at well head ----

        # TODO: we can also simply solve Theis for Q directly rather than
        # guessing a Q and then adjusting till the Q produces drawdown within
        # (0.1 m) error factor of the target max drawdown

        rw <- wp$Well_Diameter * 0.5
        calcResults <- calcWellsTheis(t_max, rw, wp)
        t_max <- calcResults$t
        s <- calcResults$s
        W <- calcResults$W

        # recalculate actual well yield using actual drawdown at the well head. this well replace initial well yield guess
        wp[["Well_Yield"]] <- (sadj * (4.0 * pi * wp$Transmissivity)) / W     # rearranged from Theis: s = Q*W(u)/4piT

        # we probably don't need this now as the line above does the same job of calculating the Q which will produce MaxDrawdown at the well head
        # # initialize Q loop
        # inRange <- TRUE
        # while (inRange == TRUE) {
        #   inRange = (abs(sadj - s) > errFactor)
        #   wp[["Well_Yield"]] <- wp$Well_Yield * (abs(sadj / s))
        #   s <- (wp$Well_Yield / (4.0 * pi * wp$Transmissivity) * W)   # s = Q*W(u)/4piT
        # }

        # second: roi ----
        #
        # Guess the radius of influence given actual well yield Q
        if (NumIterations < 2) { # makes sure we are calculating roi only in the first time step

          roi <- ((wp$Well_Yield * t_max * wp$Annual_Operation_time) / (pi * wp$Orig_Aqfr_Sat_Thickness * wp[["Storativity"]])) ^ 0.5

          # the following is the actual equation but since roi is just a guess
          # we are ignoring some terms, the following while loop adjusts roi
          # based on a sroi = 1 limit.

          # roi <- ((wp$Well_Yield * t * wp$Annual_Operation_time * W * u) / (pi * (wp[["Storativity"]]) * wp$roi_boundary)) ^ 0.5

          # TODO: the approach could be changed to actually solve for roi instead of iterating
          # 1) calc W based on well Q, aquifer props, and drawdown of 1m
          # 2) lookup u value
          # 3) solve r = ((Q * t_roi * W(u) * u ) / (S^2 * pi)) ^ 0.5
          sroi <- wp$roi_boundary + errFactor + 1

          inRange <- TRUE
          while (inRange == TRUE) {
            inRange = (abs(sroi - wp$roi_boundary) > errFactor)
            if (sroi < 0) {
              roi = roi * 0.75
            } else {
              roi = roi * (sroi / wp$roi_boundary) ^ 0.033
            }
            calcResults <- calcWellsTheis(t_max, roi, wp)
            t_max <- calcResults$t
            sroi <- calcResults$s
          }

          wp[["radial_extent"]] <- roi
          wp[["Drawdown_roi"]] <- sroi
          wp[["Areal_Extent"]] <- pi * (wp$radial_extent ^ 2) # m3

          # scale down max drawdown if well area is larger than grid area
          # TODO: do not scale
          if (wp$Areal_Extent > (df[i, "Area"] + errFactor)) {
            # scales down max drawdown using well areal extent and grid area ratio
            wp[["Max_Drawdown"]] <- wp$Max_Drawdown * (abs(df[i, "Area"] / wp$Areal_Extent))
            drawdownLimit = wp$Orig_Aqfr_Sat_Thickness - wp$Max_Drawdown
            break
          }
        }

        ### drawdown and volume over time with costs ----
        #
        # Calculate drawdown at the well over time with costs

        # Initialize drawdown from pumping well and image wells and time
        s <- 0
        t <- 0

        # Run code while drawdown in pumping well is at max possible drawdown

        # iterate through each year of pumping up to the max life time and depletion limit
        while (t < wp$Max_Lifetime_in_Years &&    # time is lower than maximum pumping duration
               wp$Depleted_Vol_Fraction < wp$Depletion_Limit && # pumped volume fraction is lower than the depletion limit
               wp$Well_Yield > 0) { # something is pumped

          t <- t + 1      # next year if conditions are met

          # guess initial drawdown based on radius of the well
          calcResults <- calcWellsTheis(t, rw, wp)
          s <- calcResults$s
          t <- calcResults$t
          w <- calcResults$W

          ### jacob correction ----
          #
          # Solve quadratic of Jacob correction for observed drawdown in well
          #(s_obs^2)/(2h)-s_obs+s=0
          a <- 1 / (2 * wp$Aqfr_Sat_Thickness)
          b <- -1
          c <- s
          det <- (b ^ 2) - (4 * a * c)

          if (det > 0) {
            root1 <- (-b + sqrt(det)) / (2 * a)
            root2 <- (-b - sqrt(det)) / (2 * a)
          } else if (det == 0) {
            root1 <- (-b) / 2 * a
            root2 <- root1
          } else {
            root1 <- "error"
            root2 <- "error"
          }

          if (root1 > wp$Max_Drawdown + errFactor || root1 < 0) {
            root1 <- 0
          }

          if (root2 > wp$Max_Drawdown + errFactor || root2 < 0) {
            root2 <- 0
          }

          sobs = root1

          if (sobs < root2) {
            sobs <- root2
          }


          ### output calculations ----
          wp[["Drawdown"]]              <- sobs # m
          wp[["Total_Head"]]            <- sobs + wp$Depth_to_Piezometric_Surface

          wp[["Volume_Produced_perWell"]] <- wp$Annual_Operation_time * wp$Well_Yield # m^3
          wp[["Cumulative_Vol_Produced_perWell"]] <- t * wp$Annual_Operation_time * wp$Well_Yield # m^3

          wp[["Power"]]                 <- (wp$Specific_weight * wp$Total_Head * wp$Well_Yield / wp$Pump_Efficiency) / 1000 # KW
          wp[["Energy"]]                <- wp$Power * (wp$Annual_Operation_time / 3600) # KWh/year (converted from sec to hours)

          wp[["Annual_Capital_Cost"]]   <- wp$Well_Installation_Cost * (1 + wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years * wp$Interest_Rate / (((1 + wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years) - 1)  #
          wp[["Maintenance_Cost"]]      <- wp$Maintenance_factor * wp$Well_Installation_Cost # $
          wp[["NonEnergy_Cost"]]        <- wp$Annual_Capital_Cost + wp$Maintenance_Cost # $
          wp[["Energy_Cost"]]           <- wp$Energy * wp$Energy_cost_rate # $ (electricity cost for now)
          wp[["Total_Cost_perWell"]]    <- wp$Energy_Cost + wp$NonEnergy_Cost # $
          wp[["Unit_Cost"]]             <- wp$Total_Cost_perWell / wp$Volume_Produced_perWell # $/m3
          wp[["Unit_Cost_per_km3"]]     <- wp$Unit_Cost / CONV_M3_KM3 # $/km3
          wp[["Unit_Cost_per_AcreFt"]]  <- wp$Unit_Cost / CONV_M3_AcreFt # $/acFt

          # Add the year's output to a list for export to file later
          wp[["NumWells"]] <- df[i, "Area"] / wp$Areal_Extent     # number of wells
          wp[["Volume_Produced_allWells"]] <- wp$Volume_Produced_perWell * wp$NumWells
          wp[["Cumulative_Vol_Produced_allWells"]] <- wp$Cumulative_Vol_Produced_allWells + wp$Volume_Produced_allWells
          wp[["Total_Cost_allWells"]]   <- wp$Total_Cost_perWell * wp$NumWells # $

          wp[["Available_Volume"]]      <- df[i, "Area"] * wp$Orig_Aqfr_Sat_Thickness * wp$Storativity # m3
          wp[["Depleted_Vol_Fraction"]] <- wp$Cumulative_Vol_Produced_allWells / wp$Available_Volume

          TotTime = NumIterations * 2 * t

          # out list ----
          # initialize the output file in first time step
          if (t == 1) {
            outputList <- list()
          }

          outputList[[paste("line", as.character(t))]] <-
            paste(       NumIterations,                  # iteration
              ",",       t,                              # year_number
              ",",       wp$Depletion_Limit,             # depletion_limit
              ",",       df[i, "Continent"],             # continent
              ",",       df[i, "CNTRY_NAME"],            # country_name
              ",",       df[i, "GCAM_ID"],               # gcam_basin_id
              ",",       df[i, "Basin_Name"],            # gcam_basin_name
              ",",       df[i, "OBJECTID"],              # well_id
              ",",       df[i, "Area"],                  # grid_area (m2)
              ",",       df[i, "Permeability"],          # permeability
              ",",       wp$Storativity,                 # storativity (-)
              ",",       wp$Total_Thickness,             # total_thickness
              ",",       wp$Depth_to_Piezometric_Surface,# depth_to_piez_surface (m)
              ",",       wp$Orig_Aqfr_Sat_Thickness,     # orig_aqfr_sat_thickness
              ",",       wp$Aqfr_Sat_Thickness,          # aqfr_sat_thickness
              ",",       wp$Hydraulic_Conductivity,      # hydraulic_conductivity
              ",",       wp$Transmissivity,              # transmissivity
              ",",       roi,                            # radius_of_influence
              ",",       wp$Areal_Extent,                # areal_extent
              ",",       wp$Max_Drawdown,                # max_drawdown
              ",",       wp$Drawdown,                    # drawdown
              ",",       wp$Total_Head,                  # total_head
              ",",       wp$Well_Yield,                  # well_yield
              ",",       wp$Volume_Produced_perWell,     # volume_produced_perwell
              ",",       wp$Cumulative_Vol_Produced_perWell,  # cumulative_vol_produced_perwell
              ",",       wp$NumWells,                    # number_of_wells
              ",",       wp$Volume_Produced_allWells,    # volume_produced_allwells
              ",",       wp$Cumulative_Vol_Produced_allWells, # cumulative_vol_produced_allwells
              ",",       wp$Available_Volume,            # available_volume
              ",",       wp$Depleted_Vol_Fraction,       # depleted_vol_fraction
              ",",       wp$Well_Installation_Cost,      # well_installation_cost
              ",",       wp$NumWells,                    # annual_capital_cost
              ",",       wp$Annual_Capital_Cost,         # maintenance_cost
              ",",       wp$NonEnergy_Cost,              # nonenergy_cost
              ",",       wp$Power,                       # power
              ",",       wp$Energy,                      # energy KW/yr
              ",",       wp$Energy_cost_rate,            # energy_cost_rate
              ",",       wp$Energy_Cost,                 # energy_cost
              ",",       wp$Total_Cost_perWell,          # total_cost_perwell
              ",",       wp$Total_Cost_allWells,         # total_cost_allwells
              ",",       wp$Unit_Cost,                   # unit_cost
              ",",       wp$Unit_Cost_per_km3,           # unit_cost_per_km3
              ",",       wp$Unit_Cost_per_AcreFt,        # unit_cost_per_acreft
              ",",       df[i, "WHYClass"],              # whyclass
              ",",       wp$Screen_length,               # screen_length
              ",",       wp$Casing_length,               # casing_length
              ",",       wp$Total_Well_Length,           # total_well_length
              "\n"
            )

          #loop back to next year
        }

        # initialize next 20 years ----
        #wp[["Total_Volume_Produced"]] <- wp$Total_Volume_Produced + wp$Well_Yield * wp$Max_Lifetime_in_Years * wp$Annual_Operation_time
        wp[["Total_Volume_Produced"]] <- wp$Total_Volume_Produced + (wp$Volume_Produced * NumWells)
        wp[["Aqfr_Sat_Thickness"]] <- wp$Total_Thickness - wp$Total_Volume_Produced / (3.14159 * wp$radial_extent ^ 2 * wp$Storativity)
        wp[["Available_volume"]] <- (wp$Areal_Extent * wp$Orig_Aqfr_Sat_Thickness * wp$Storativity)
        wp[["Exploitable_GW"]] <- wp$Total_Volume_Produced / wp$Available_volume
        wp[["Depth_to_Piezometric_Surface"]] <- wp$Total_Thickness - wp$Aqfr_Sat_Thickness
        wp[["Max_Drawdown"]] <- wp$Aqfr_Sat_Thickness - WT
        wp[["Total_Head"]] <- sobs + wp$Depth_to_Piezometric_Surface

        NumIterations <- NumIterations + 1
        TotTime = NumIterations * 2 * t

        #append results to output file
        for (name in names(outputList)) {
          cat(outputList[[name]], file = con)
        }

        # loop back to depleted volume fraction and max depletion limit
      }
      run <- 1
      # break the while run = 0 loop because run = 1
    }
    # loop back to next grid cell
  }
  close(con)
}


# Execution ----
#
## load file paths ----
{
well_params <- "inputs/wellParams.yml"
elec_rates <- "inputs/GCAM_Electrical_Rates.yml"
config <- "inputs/inputs.csv"
#config <- "inputs/GW_cost_model_comparison_inputs.csv"

output_dir <- "outputs"
}
# specify country name if running for a country, otherwise 'All' will run globally
runcountry <- "United States"


#TODO: Make temporal resolution flexible too
#TODO: Give option to choose between running fully global inputs and filtered grid cells

# temp data load (main superwell() will also load these)
country <- runcountry
wp <- load_well_data(well_params)   # well specific parameters
ec <- load_elec_data(elec_rates)    # electrical USD/Unit costs by GCAM basin number

## run superwell ----
system.time(superwell(well_params, elec_rates, config, runcountry, output_dir))


# Tests ########################################################################

# speed test between indexing methods (indexing using $ is MUCH faster)
# x = 1:10
# system.time({for(i in 1:1e5) identity(x)*wp$Storativity})
# system.time({for(i in 1:1e5) identity(x)*df[i, "Porosity"]})

## END
