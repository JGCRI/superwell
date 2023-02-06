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

#TODO: eventually separate superwell into superwell_fun and superwell_run

# load functions to start and run superwell in the last line. See man/ for
# documentation of all functions


devtools::load_all(".")

library(yaml)
library(dplyr)

options(stringsAsFactors = FALSE)
#rm(list = ls())

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
#load_config <- function(config_file, country = 'United States') {
load_config <- function(config_file, country) {
  #load_config <- function(config_file, country) {
  # Node-specific input (permeability, porosity, thickness, etc.) are in "Inputs.csv"
  ## check for one country (Iran)
  #config_file <- 'inputs.csv'
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
                    ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Groundwater calculation functions ----

#' Theis solution
#'
#' Calculates drawdown using Theis solution
#'
#' @details affected by initial yield, annual operational time, and aquifer properties (T and S)
#' @param t annual operational time, comes from wellParams.yml (currently fixed at 365 days i.e., 31536000 s/yr)
#' @param rw radius of well. *TODO REDFLAG should be radius of influence. CHECK*
#' @param wp well parameters from wellParams.yml
#'
#' @return drawdown using Theis solution
#' @author Superwell team; hassan.niazi@pnnl.gov
#' @export

calcWellsTheis <- function(t, rw, wp) {
  #Pumping well drawdown <-2.3Q/4piT*log(2.25Tt/r2S)
  W <- 0.0
  u <- rw ^ 2 * wp$Storativity / (4 * wp$Transmissivity * t * wp$Annual_Operation_time) #'*TODO REDFLAG : rw is supposed to be radius of influence (roi) NOT radius of well (dia/2), check if this is only used as initialization *

  j <- 1
  term <- -u
  W <- -0.5772156649 - log(u) - term       # well function

  while (abs(term) > 0.00000001 && term != Inf) {
    term <- -u * j / (j + 1) ^ 2 * term
    W <- W - term
    j <- j + 1
  }
  s <- (wp$Well_Yield / (4.0 * 3.14159 * wp$Transmissivity) * W)     # drawdown from Theis [m]
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

superwell <- function(well_params,
                 elec_rates,
                 config_file,
                 resolution,
                 output_dir) {

  # prep data ----
  #
  # well specific parameters (e.g., costs, etc.) are in "wellParams.yml" which is in yaml format
  wp <- load_well_data(well_params)

  # electrical USD/Unit costs by GCAM basin number
  ec <- load_elec_data(elec_rates)

  # store the well yield from the yaml file because we will need to go back to it with every node
  wp[["Initial_Well_Yield"]] <- wp$Well_Yield

  # store a "generic" Electricity cost rate in case of an error. Irrespective of the country being processed
  wp[["global_energy_cost_rate"]] <- wp$Energy_cost_rate

  # specify country name if running for a country, otherwise 'All' will run globally
  country <- resolution
  print(paste0("Processing:  ", country))
  # "inputs.csv" contains all the node specific input (permeability, porosity, thickness, etc.)
  df <- load_config(config, country)

  #Set up the output file. This file will be written to after the completion of
  #every node so the output may be copied and used at any point during the run.
  #fileName<-paste(df[1, "CNTRY_NAME"], "_WellResults.csv")
  #fileName<-paste("MENA", "_WellResults.csv")
  output_filename <- paste0(gsub(" ", "_", tolower(country)), ".csv")
  output_file <- file.path(output_dir, output_filename)

  con <- file(output_file, "w") #open the file to write

  # write the headers for the output file
  cat("iteration, year_number, area, radius_of_influence, drawdown_roi, areal_extent, total_head, aqfr_sat_thickness, storativity,thickness, unit_cost, hydraulic_conductivity, radial_extent, number_of_wells, volume_produced, total_volume_produced, available_volume,continent, well_id, country_name, gcam_basin_id, exploitable_groundwater, well_installation_cost, annual_capital_cost, total_cost, maintenance_cost, cost_of_energy, energy_cost_rate, electric_energy, drawdown, depletion_limit, depth_to_piez_surface\n",
    file = con
  )

  # sims per cell loop ----
  #
  # each row in df is a different grid node in the model domain
  for (i in 1:(nrow(df))) {
    #calculate total progress
    #prog <- percent(c(i/nrow(df)))
    #Print total progress to the console
    #print(paste(i,"/",nrow(df),prog,df[i,"CNTRY_NAME"]))

    ## grid prep ----
    #
    # Calculate and store other node specific attributes
    wp[["Well_Yield"]] <- wp$Initial_Well_Yield
    if (is.null(ec[[df[i, "CNTRY_NAME"]]]) == FALSE) {
      wp[["Energy_cost_rate"]] <- ec[[df[i, "CNTRY_NAME"]]]
    } else {
      wp[["Energy_cost_rate"]] <- wp$global_energy_cost_rate
    }

    wp[["Storativity"]] <- df[i, "Porosity"] #assuming storativity is equal to porosity
    wp[["Depth_to_Piezometric_Surface"]] <- df[i, "Depth"]
    wp[["Aqfr_Sat_Thickness"]] <- df[i, "Thickness"]                    # m
    wp[["Orig_Aqfr_Sat_Thickness"]] <- df[i, "Thickness"]               # m
    wp[["Screen_length"]] <- wp$Orig_Aqfr_Sat_Thickness * 0.3           # m assumption
    wp[["Casing_length"]] <- wp$Orig_Aqfr_Sat_Thickness + wp$Depth_to_Piezometric_Surface - wp$Screen_length # m
    #	wp[["Max_Drawdown"]]<-0.66 * wp$Aqfr_Sat_Thickness                # m
    wp[["Total_Well_Length"]] <- wp$Casing_length + wp$Screen_length    # m
    wp[["Hydraulic_Conductivity"]] <- (10 ^ df[i, "Permeability"]) * 1e7
    wp[["roi_boundary"]] <- 1
    #wp[["Transmissivity"]]<-wp$Hydraulic_Conductivity*wp$Screen_length     ###this changed. Is it right? # m2/s
    wp[["Transmissivity"]] <- wp$Hydraulic_Conductivity * wp$Aqfr_Sat_Thickness

    if (df[i, "WHYClass"] == 10) {
      wp[["Well_Installation_cost"]] <- wp$Well_Install_10 * wp$Total_Well_Length
    } else {
      if (df[i, "WHYClass"] == 20) {
        wp[["Well_Installation_cost"]] <- wp$Well_Install_20 * wp$Total_Well_Length
      } else {
        wp[["Well_Installation_cost"]] <- wp$Well_Install_30 * wp$Total_Well_Length
      }
    }
    # Initialization
    wp[["Exploitable_GW"]] <- 0
    wp[["Total_Volume_Produced"]] <- 0
    wp[["Total_Head"]] <- 0
    wp[["Drawdown"]] <- 0
    TotTime = 0
    #wp[["Areal_Extent"]] <- df[i, "Area"]

    outputList <- list()

    wp$Max_Drawdown = 0.66 *  wp$Orig_Aqfr_Sat_Thickness  # two-thirds depletion limit
    WT <- wp$Orig_Aqfr_Sat_Thickness - wp$Max_Drawdown    # water table
    wp$Total_Thickness = wp$Depth_to_Piezometric_Surface + wp$Orig_Aqfr_Sat_Thickness

    run <- 0
    while (run == 0) {
      NumIterations <- 1

      # First: iterate on depth, drawdown, and runtime ----
      while ((wp$Exploitable_GW < wp$Depletion_Limit) &&
             (wp$Max_Drawdown >= 1) && (run == 0) && (TotTime <= 200)) {
        # initialize
        s <- 0
        t <- wp$Max_Lifetime_in_Years
        errFactor <- 0.1

        # Jacob correction for observed drawdown. This is the drawdown to be used with the Theis solution.
        sadj <- wp$Max_Drawdown - ((wp$Max_Drawdown ^ 2) / (2 * wp$Aqfr_Sat_Thickness))

        # First: compute drawdown with initial Q guess
        rw <- wp$Well_Diameter * 0.5 #REDFLAG
        calcResults <- calcWellsTheis(t, rw, wp)
        t <- calcResults$t
        s <- calcResults$s
        W <- calcResults$W

        # Second: iterate on Q ----
        # initialize Q loop
        inRange <- TRUE

        while (inRange == TRUE) {
          inRange = (abs(sadj - s) > errFactor)
          wp[["Well_Yield"]] <- wp$Well_Yield * (abs(sadj / s))
          s <- (wp$Well_Yield / (4.0 * 3.14159 * wp$Transmissivity) * W)
        }


        # roi ----
        #
        # Guess the radius of influence of well yield Q
        if (NumIterations < 2) {
          roi <- (wp$Well_Yield * t * wp$Annual_Operation_time / (3.14159 * wp$Orig_Aqfr_Sat_Thickness * df[i, "Porosity"])) ^ 0.5
          sroi <- wp$roi_boundary + errFactor + 1

          inRange <- TRUE
          while (inRange == TRUE) {
            inRange = (abs(sroi - wp$roi_boundary) > errFactor)
            if (sroi < 0) {
              roi = roi * 0.75 #TODO: what are these assumptions?
            } else {
              roi = roi * (sroi / wp$roi_boundary) ^ 0.033 #TODO: what are these assumptions?
            }
            calcResults <- calcWellsTheis(t, roi, wp)
            t <- calcResults$t
            sroi <- calcResults$s
          }

          wp[["radial_extent"]] <- roi
          wp[["Drawdown_roi"]] <- sroi
          wp[["Areal_Extent"]] <- 3.14159 * (wp$radial_extent ^ 2) # m3

          if (wp$Areal_Extent > (df[i, "Area"] + errFactor)) {
            wp[["Max_Drawdown"]] <- wp$Max_Drawdown  * (abs(df[i, "Area"] / wp$Areal_Extent))
            WT = wp$Orig_Aqfr_Sat_Thickness - wp$Max_Drawdown
            break
          }
        }

        # Calculate drawdown at the well over time with costs
        # Initialize drawdown from pumping well and image wells and time
        s <- 0
        t <- 0

        # Run code while drawdown in pumping well is gt max possible drawdown
        # iterate through each year of pumping up to the max life time in years
        #while ((t < wp$Max_Lifetime_in_Years) && (wp$Well_Yield > 0)){
        while ((t < wp$Max_Lifetime_in_Years) &&
               (wp$Well_Yield > 0) && (wp$Exploitable_GW < wp$Depletion_Limit)) {
          t <- t + 1
          calcResults <- calcWellsTheis(t, rw, wp)
          t <- calcResults$t
          s <- calcResults$s
          w <- calcResults$W

          # jacob correction ----
          #
          #Solve quadratic of Jacob correction for observed drawdown in well
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

          #Output (one row per year)
          wp[["Drawdown"]] <- s # m
          wp[["Total_Head"]] <- sobs + wp$Depth_to_Piezometric_Surface
          wp[["Volume_Produced"]] <- t * wp$Annual_Operation_time * wp$Well_Yield #m^3
          wp[["Power"]] <- (wp$Specific_weight * wp$Total_Head * wp$Well_Yield / wp$Pump_Efficiency) / 1000 #KW
          wp[["Electric_Energy"]] <- wp$Power * (wp$Annual_Operation_time / 3600) #(CONVERT(1,"hr","sec"))) #KWh/year
          wp[["Annual_Capital_Cost"]] <- wp$Well_Installation_cost * (1 + wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years * wp$Interest_Rate / ((1 + wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years - 1)
          wp[["Maintenance_Cost"]] <- wp$Maintenance_factor * wp$Well_Installation_cost # $
          wp[["Total_Cost"]] <- wp$Annual_Capital_Cost + wp$Maintenance_Cost # $
          wp[["Cost_of_Energy"]] <- (wp$Electric_Energy * wp$Energy_cost_rate) # $
          wp[["Unit_cost"]] <- (wp$Total_Cost + wp$Cost_of_Energy) / (wp$Well_Yield * wp$Annual_Operation_time)
          wp[["Cost_per_ac_ft"]] <- wp$Unit_cost / 0.000810714


          # out list ----
          #Add the year's output to a list for export to file later
          NumWells <- df[i, "Area"] / wp$Areal_Extent
          wp[["Total_Volume_Produced"]] <- wp$Total_Volume_Produced + (wp$Volume_Produced * NumWells)
          wp[["Available_volume"]] <- (wp$Areal_Extent * wp$Orig_Aqfr_Sat_Thickness * wp$Storativity) # $/acFt
          wp[["Exploitable_GW"]] <- wp$Total_Volume_Produced / wp$Available_volume
          TotTime = NumIterations * 2 * t

          if (t == 1) {
            outputList <- list()
          }

          outputList[[paste("line", as.character(t))]] <-
            paste(
              NumIterations,                  # iteration
              ",",
              t,                              # year_number
              ",",
              df[i, "Area"],                  # area
              ",",
              roi,                            # radius_of_influence
              ",",
              sroi,                           # drawdown_roi
              ",",
              wp$Areal_Extent,                # areal_extent
              ",",
              wp$Total_Head,                  # total_head
              ",",
              wp$Aqfr_Sat_Thickness,          # aqfr_sat_thickness
              ",",
              wp$Storativity,                 # storativity
              ",",
              wp$Total_Thickness,             # thickness
              ",",
              wp$Unit_cost,                   # unit_cost
              ",",
              wp$Hydraulic_Conductivity,      # hydraulic_conductivity
              ",",
              wp$radial_extent,               # radial_extent
              ",",
              NumWells,                       # number_of_wells
              ",",
              wp$Annual_Operation_time * wp$Well_Yield,                   # volume_produced
              ",",
              #(wp$Volume_Produced * NumWells),                           # total_volume_produced
              (wp$Volume_Produced * NumWells) + wp$Total_Volume_Produced, # total_volume_produced
              ",",
              #wp$Areal_Extent*wp$Orig_Aqfr_Sat_Thickness*wp$Storativity, # available_volume
              df[i, "Area"] *
                wp$Orig_Aqfr_Sat_Thickness * wp$Storativity,              # available_volume
              ",",
              df[i, "Continent"],
              ",",
              df[i, "OBJECTID"],              # well_id
              ",",
              df[i, "CNTRY_NAME"],            # country_name
              ",",
              df[i, "GCAM_ID"],               # gcam_basin_id
              ",",
              #df[i,"Basin_Name"], #          # gcam_basin_name
              #",",
              ((wp$Volume_Produced * NumWells) + wp$Total_Volume_Produced) /
                (wp$Areal_Extent * wp$Orig_Aqfr_Sat_Thickness * wp$Storativity),

              # exploitable_groundwater
              ## CHANGE MADE BY NTG
              ## add previous iterations volume produced to current 20 year time period
              ",",
              wp$Well_Installation_cost,      # well_installation_cost
              ",",
              wp$Annual_Capital_Cost,         # annual_capital_cost
              ",",
              wp$Total_Cost,                  #  total_cost
              ",",
              wp$Maintenance_Cost,            # maintenance_cost
              ",",
              wp$Cost_of_Energy,              # cost_of_energy
              ",",
              wp$Energy_cost_rate,            # energy_cost_rate
              ",",
              wp$Electric_Energy,             # electric_energy
              ",",
              wp$Drawdown,                    #  drawdown (m)
              ",",
              wp$Depletion_Limit,             # depletion limit
              ",",
              wp$Total_Thickness - wp$Aqfr_Sat_Thickness,   # depth_to_piez_surface (m)
              "\n"
            )

          #loop back to next year
        }

        # initialize next 20 years
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

        #loop back to exploitable gw
        #append results to output file
        for (name in names(outputList)) {
          cat(outputList[[name]], file = con)
        }
      }
      run <- 1
    }
  }
  close(con)
}


# Execution ----
#
## load file paths ----
well_params <- "inputs/wellParams.yml"
elec_rates <- "inputs/GCAM_Electrical_Rates.yml"
config <- "inputs/inputs.csv"
output_dir <- "outputs/"

# specify country name if running for a country, otherwise 'All' will run globally
cntry_resolution <- "India"

#TODO: Make temporal resolution flexible too

## run superwell ----
system.time(superwell(well_params, elec_rates, config, cntry_resolution, output_dir))

