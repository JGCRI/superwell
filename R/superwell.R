# Estimate global volume and unit-costs of accessible groundwater production in 235 GCAM water regions

# Modified code originally created by DJ Watson
# File retrieved from file://pnl/projects/JGCRI_YONK/JGCRI/DJ_Share/R%20code/ on July 31, 2019
# Updated the outpus structure as shown in /R Code/MENA_Results_161216/updatedformat/5/superwell.R

# Input files: wellParams.yml, GCAM_Electrical_Rates.yml, Inputs.csv
# Output file: ***_WellResults.csv (as in original code)
# Variables from test yml
# well_parameters[["Annual_Operation_time"]] s/year
# well_parameters[["Depletion_Limit"]]       $/KWh
# well_parameters[["Energy_cost_rate"]]      $/year
# well_parameters[["Interest_Rate"]]         n/a
# well_parameters[["Maintenance_factor"]]    years
# well_parameters[["Max_Lifetime_in_Years"]] n/a
# well_parameters[["Pump_Efficiency"]]       (kg/m3*m/s2)
# well_parameters[["Specific_weight"]]       m
# well_parameters[["Static_head"]]           m
# well_parameters[["Well_Diameter"]]         $/m
# well_parameters[["Well_Install_10"]]       $/m
# well_parameters[["Well_Install_20"]]       $/m
# well_parameters[["Well_Install_30"]]       $/m
# well_parameters[["Well_Yield"]]            m3/s

# Other parameters in the script
# @param Max_Drawdown <- 0.66 (commented out in the script)
# @param Screen_length_of_original_Aqfr_Sat_Thickness <- 0.3
# @param WHYClass - 10, 20 or 30
# @param Max_Drawdown_to_Orig_Aqfr_Sat_Thickness <- 0.66 (line 139)
# @param radius of influence of Q <- 0.75

options(stringsAsFactors = FALSE)

# Hard-coded variables:
# ------
# Constants
Euler_constant <- 0.5772156649
pi <- 3.14159
conversion_factor <- 0.000810714
# Other Variables
errFactor <- 0.1
# ------

#' Load well parameters
#'
#' Load well-specific parameters (e.g., costs, etc.) from "wellParams.yml" (total 14 well parameters)
#'
#' @param well_param_file Full path with file name and extension to the input well parameters YAML file
#' @return Data frame of well parameters
#' @import  yaml
#' @export
load_well_parameters <- function(well_param_file) {

  well_parameters <- yaml.load_file(well_param_file)

  # Store well yield data from the yaml file because we will need to go back to it with every node
  well_parameters[["Initial_Well_Yield"]] <- well_parameters$Well_Yield

  # Store a "generic" electricity cost rate in case of an error
  well_parameters[["global_energy_cost_rate"]] <- well_parameters$Energy_cost_rate

  return(well_parameters)
}

#' Load electricity data by GCAM basin
#'
#' Read in the cost of electricity by GCAM basin (USD/Unit for 172 countries)
#'
#' @param electricity_cost_file Full path with file name and extension to the input GCAM electrical rates YAML file
#' @return loaded file
#' @import yaml
#' @export
load_elec_data <- function(electricity_cost_file) {

  # return(yaml.load_file(electricity_cost_file))
  return(data.table(melt(yaml.load_file(elec_rates)))[, .('CNTRY_NAME' = L1, 'value' = elec_rate)])
}

#' Load electricity data by GCAM basin
#'
#' Read in the cost of electricity by GCAM basin (USD/Unit for 172 countries)
#'
#' @param config_file Full path with file name and extension to the input configuration CSV file
#' @return data.table Input file data
#' @importFrom data.table fread
#' @export
load_config <- function(config_file, country = 'Iran') {

  return(fread(config_file)[CNTRY_NAME %in% (country)])
}

#' Need description
#'
#' Need long description
#'
#' @param time Passing through the previously loaded well_parameters data
#' @param well_radius Passing through the previously loaded well_data object
#' @param well_params initial Q guess
#' @param well_data Passing through the main dataframe object
#' @return result - need return info
#' @export
calc_wells <- function(time, well_radius, well_params, well_data)
{
  # Pumping well drawdown <-2.3Q/4*pi*T*log(2.25Tt/r2S)
  # transient Theis solution
  # s - the drawdown (m);
  # t - time (years)
  # rw - well radius

  W <- 0.0   # W(u) - the Well Function (exponential integral)
  u <- well_radius ^ 2 * well_data$Storativity / (4 * well_data$Transmissivity * time * well_params$Annual_Operation_time)
  j <- 1
  term <- -u
  W <- -Euler_constant - log(u) - term

  while (abs(term) > 0.00000001 && !is.infinite(term))
  {
    term <- -u * j / (j + 1) ^ 2 * term
    W <- W - term
    j <- j + 1
  }

  s <- (well_params$Well_Yield / (4.0 * pi * well_data$Transmissivity) * W)
  result <- list(drawdown = s, t_years = time, W = W)

  return(result)
}

#' Calculate well drawdown data
#'
#' This function calculates well specific data and delivers the output for writing to csv file
#'
#' @param well_parameters Passing through the previously loaded well_parameters data
#' @param well_data Passing through the previously loaded well_data object
#' @param rw initial Q guess
#' @param df Passing through the main dataframe object
#' @param i Passing through i from the main loop
#' @param num_iterations Passing through num_iterations from the main loop
#' @return outputList - returns the formatted list of output variables for writing to excel
#' @author Jason Evanoff; jason.evanoff@pnnl.gov
#' @export
calculate_output <- function(well_parameters, well_data, rw, df, i, num_iterations)
{

  #Calculate drawdown at the well over time with costs
  #Initialize drawdown from pumping well and image wells and time
  drawdown <- 0
  t_time <<- 0

  while ((t_time < well_parameters$Max_Lifetime_in_Years) && (well_parameters$Well_Yield > 0))
  {
    t_time <<- t_time + 1
    well_calculations <- calc_wells(t_time, rw, well_parameters, well_data)
    t_time <<- well_calculations$t_years
    drawdown <- well_calculations$drawdown
    w <- well_calculations$W

    #Solve quadratic of jacob correction for observed drawdown in well
    #(s_obs^2)/(2h)-s_obs+s=0
    a <- 1 / (2 * well_data$Aqfr_Sat_Thickness)
    b <- -1
    c <- drawdown

    det <- (b ^ 2) - (4 * a * c)
    if (det > 0)
    {
      root1 <- (-b + sqrt(det)) / (2 * a)
      root2 <- (-b - sqrt(det)) / (2 * a)
    }
    else if (det == 0)
    {
      root1 <- (-b) / 2 * a
      root2 <- root1
    }
    else
    {
      root1 <- "error"
      root2 <- "error"
    }

    # Problematic above and below
    if (root1 > well_data$Max_Drawdown + errFactor || root1 < 0)
    {
      root1 <- 0
    }
    if (root2 > well_data$Max_Drawdown + errFactor || root2 < 0)
    {
      root2 <- 0
    }
    sobs <<- root1
    if (sobs < root2)
    {
      sobs <<- root2
    }

    #Output (one row per year)
    well_data[["Drawdown"]] <- drawdown
    well_data[["Total_Head"]] <- sobs + well_data$Depth_to_Piezometric_Surface
    well_data[["Volume_Produced"]] <- t_time * well_parameters$Annual_Operation_time * well_parameters$Well_Yield                                # m^3
    well_data[["Power"]] <- (well_parameters$Specific_weight * well_data$Total_Head * well_parameters$Well_Yield / well_parameters$Pump_Efficiency) / 1000      # KW
    well_data[["Electric_Energy"]] <- well_data$Power *(well_parameters$Annual_Operation_time/3600) #(CONVERT(1,"hr","sec")))         # KWh/year
    well_data[["Annual_Capital_Cost"]] <- well_data$Well_Installation_cost *
      (1 + well_parameters$Interest_Rate) ^ well_parameters$Max_Lifetime_in_Years * well_parameters$Interest_Rate /
      ((1+well_parameters$Interest_Rate) ^ well_parameters$Max_Lifetime_in_Years - 1)
    well_data[["Maintenance_Cost"]] <- well_parameters$Maintenance_factor * well_data$Well_Installation_cost                          # $
    well_data[["Total_Cost"]] <- well_data$Annual_Capital_Cost + well_data$Maintenance_Cost                                     # $
    well_data[["Cost_of_Energy"]] <- (well_data$Electric_Energy * well_data$Energy_cost_rate)                                   # $
    well_data[["Unit_cost"]] <- (well_data$Total_Cost + well_data$Cost_of_Energy) / (well_parameters$Well_Yield * well_parameters$Annual_Operation_time)
    well_data[["Cost_per_ac_ft"]] <- well_data$Unit_cost / conversion_factor                                                   # $/acFt
    #Add the year's output to a list for export to file later
    #browser()
    NumWells <- df[i,"Area"] / well_data$Areal_Extent
    TotTime <<- num_iterations * 2 * t_time

    if (t_time == 1)
    {
      outputList <- data.frame()
    }

    outputList <- dplyr::bind_rows(outputList,
                                   c(Continent = df[i,"Continent"], ObjID = df[i,"OBJECTID"], Country = df[i,"CNTRY_NAME"], Time = t_time, Drawdown = well_data$Drawdown,
                                     Observed_Drawdown = sobs, Volume = well_data$Volume_Produced, Element_Area = df[i,"Area"], Areal_Extent = well_data$Areal_Extent,
                                     Total_Head = well_data$Total_Head, Power = well_data$Power, Electric_Energy = well_data$Electric_Energy,
                                     Energy_Cost_Rate = well_data$Energy_cost_rate, Cost_of_Energy = well_data$Cost_of_Energy, Unit_Cost = well_data$Unit_cost,
                                     Cost_Per_Ac_Ft = well_data$Cost_per_ac_ft, Interest_Rate = well_parameters$Interest_Rate,
                                     Max_Lifetime_in_Years = well_parameters$Max_Lifetime_in_Years, Maintenance_factor = well_parameters$Maintenance_factor,
                                     Well_Yield = well_parameters$Well_Yield, Annual_Operation_time = well_parameters$Annual_Operation_time,
                                     Total_Well_Length = well_data$Total_Well_Length, WHYClass =  trunc(df[i,"WHYClass"] / 10 * 10), Available_Volume = well_data$Available_volume,
                                     Number_of_Wells = NumWells, Total_Time = TotTime, Basin_ID = df[i,"GCAM_ID"], Basin_Name = df[i,"Basin_Name"]) )
  }
  #loop back to next year
  # (well_parameters, well_data, rw, df, i, num_iterations)
  return(list(outputList, well_data))
}

#' Assign the electricity cost to each country in the data frame
#'
#' Assign the electricity cost to each country in the data frame.  If a price is not
#' present, assign a global substitute
#'
#' @param df Passing through the previously loaded data frame
#' @param well_parameters Passing through the previously loaded well_parameters object
#' @param electricity_cost_data Passing through the main dataframe object
#' @param country_field field name from the input csv file for country
#' @param energy_cost_field field name for the energy cost rate field
#' @return return_df - returns the row binded dataframe
#' @export
add_energy_cost_rate <- function(df, well_parameters, electricity_cost_data, country_field = "CNTRY_NAME",
                                 energy_cost_field = "Energy_cost_rate") {

  # if the country name in the input data row has a lookup value electric price...
  if (is.null(electricity_cost_data[[df[i, country_field]]]) == FALSE ) {

    df[energy_cost_field] <- ec[[df[i, country_field]]]
  }
  else {

    well_data[[energy_cost_field]] <- well_parameters$global_energy_cost_rate
  }
}

process_all <- function(df, well_parameters, ec) {

  # set the ON clause as keys of the tables:
  setkey(ec, CNTRY_NAME)
  setkey(df, CNTRY_NAME)

  # perform the join, eliminating not matched rows from Right
  well_data <- df[ec, nomatch=0]

  well_data[["Available_volume"]] <- 0

  well_data[["Available_volume"]] <- 0
  # Initalize empty node dependant parameters here. I believe these should be here for reinitialization for each node.
  #well_parameters[["Well_Yield"]] <- well_parameters$Initial_Well_Yield
  well_data[["Exploitable_GW"]] <- 0
  well_data[["Total_Volume_Produced"]] <- 0
  well_data[["Total_Head"]] <- 0
  well_data[["Drawdown"]] <- 0
  well_data[["roi_boundary"]] <- 1

  # Set up node dependant variables
  well_data[["Storativity"]] <- well_data[["Porosity"]]
  well_data[["Depth_to_Piezometric_Surface"]] <- well_data[["Depth"]]
  well_data[["Aqfr_Sat_Thickness"]] <- well_data[["Thickness"]]                                          # m
  well_data[["Orig_Aqfr_Sat_Thickness"]] <- well_data[["Thickness"]]                                     # m

  # Calculated variables
  well_data[["Max_Drawdown"]] <- 0.66 * well_data$Aqfr_Sat_Thickness                                # m
  well_data[["Hydraulic_Conductivity"]] <- (10 ^ df[["Permeability"]]) * 1e7
  well_data[["Screen_length"]] <- well_data$Orig_Aqfr_Sat_Thickness * 0.3                           # m
  well_data[["Casing_length"]] <- well_data$Orig_Aqfr_Sat_Thickness +
    well_data$Depth_to_Piezometric_Surface - well_data$Screen_length  # m
  well_data[["Total_Well_Length"]] <- well_data$Casing_length + well_data$Screen_length             # m
  well_data[["Transmissivity"]] <- well_data$Hydraulic_Conductivity * well_data$Screen_length       # m2/s

  #well_data[["Areal_Extent"]] <- df[i, "Area"]
  WT <- well_data$Orig_Aqfr_Sat_Thickness - well_data$Max_Drawdown
  well_data$Total_Thickness <- well_data$Depth_to_Piezometric_Surface + well_data$Orig_Aqfr_Sat_Thickness

  # TODO:  set to conditional statement similar to np.where
  well_data[["Well_Installation_cost"]] <- well_parameters$Well_Install_10 * well_data$Total_Well_Length



}

#' Process the well data input data frame
#'
#' This function calculates well specific data and delivers the output for writing to csv file
#'
#' @param df Passing through the previously loaded data frame
#' @param well_parameters Passing through the previously loaded well_parameters object
#' @param ec Passing through the main dataframe object
#' @return return_df - returns the row binded dataframe
#' @author Jason Evanoff; jason.evanoff@pnnl.gov
#' @export
process_data <- function(df, well_parameters, ec)
{

  # New variable to store well data/parameters that are not read in and are calculated/changed
  well_data <- list()

  well_data[["Available_volume"]] <- 0

  # Dataframe to collect all the write operations and perform at once
  return_df <- data.frame()

  # -----
  # Calculations
  # -----
  # Each row in df is a different grid node in the model domain
  for(i in 1:(nrow(df)))
  {
    # Time tracking variables
    TotTime <- 0
    num_iterations <- 1

    # Output variable
    outputList <- list()
    # Calculate and store other node specific attributes

    # if the country name in the input data row has a lookup value electric price...
    if (is.null(ec[[df[i,"CNTRY_NAME"]]]) == FALSE )
    {
      well_data[["Energy_cost_rate"]] <- ec[[df[i,"CNTRY_NAME"]]]
    }
    else
    {
      well_data[["Energy_cost_rate"]] <- well_parameters$global_energy_cost_rate
    }

    # Initalize empty node dependant parameters here. I believe these should be here for reinitialization for each node.
    #well_parameters[["Well_Yield"]] <- well_parameters$Initial_Well_Yield
    well_data[["Exploitable_GW"]] <- 0
    well_data[["Total_Volume_Produced"]] <- 0
    well_data[["Total_Head"]] <- 0
    well_data[["Drawdown"]] <- 0
    well_data[["roi_boundary"]] <- 1

    # Set up node dependant variables
    well_data[["Storativity"]] <- df[i,"Porosity"]
    well_data[["Depth_to_Piezometric_Surface"]] <- df[i,"Depth"]
    well_data[["Aqfr_Sat_Thickness"]] <- df[i,"Thickness"]                                            # m
    well_data[["Orig_Aqfr_Sat_Thickness"]] <- df[i,"Thickness"]                                       # m

    # Calculated variables
    well_data[["Max_Drawdown"]] <- 0.66 * well_data$Aqfr_Sat_Thickness                                # m
    well_data[["Hydraulic_Conductivity"]] <- (10 ^ df[i,"Permeability"]) * 1e7
    well_data[["Screen_length"]] <- well_data$Orig_Aqfr_Sat_Thickness * 0.3                           # m
    well_data[["Casing_length"]] <- well_data$Orig_Aqfr_Sat_Thickness +
      well_data$Depth_to_Piezometric_Surface - well_data$Screen_length  # m
    well_data[["Total_Well_Length"]] <- well_data$Casing_length + well_data$Screen_length             # m
    well_data[["Transmissivity"]] <- well_data$Hydraulic_Conductivity * well_data$Screen_length       # m2/s
    #well_data[["Areal_Extent"]] <- df[i, "Area"]
    WT <- well_data$Orig_Aqfr_Sat_Thickness - well_data$Max_Drawdown
    well_data$Total_Thickness <- well_data$Depth_to_Piezometric_Surface + well_data$Orig_Aqfr_Sat_Thickness

    if (df[i,"WHYClass"] == 10)
    {
      well_data[["Well_Installation_cost"]] <- well_parameters$Well_Install_10 * well_data$Total_Well_Length
    }
    else
    {
      if (df[i,"WHYClass"] == 20)
      {
        well_data[["Well_Installation_cost"]] <- well_parameters$Well_Install_20 * well_data$Total_Well_Length
      }
      else
      {
        well_data[["Well_Installation_cost"]] <- well_parameters$Well_Install_30 * well_data$Total_Well_Length
      }
    }
    return_vals <- calculate_drawdown(well_data, well_parameters, WT, num_iterations, df, i, TotTime, t_time)
    well_data <- return_vals[[2]]
    return_df <- rbind(return_df, return_vals[[1]])
  }
  return(return_df)
}

#' Title
#'
#' @param well_data fill
#' @param well_parameters fill
#' @param WT fill
#' @param num_iterations fill
#' @param df fill
#' @param i fill
#' @param TotTime fill
#' @param t_time fill
#'
#' @return fill
#' @export
calculate_drawdown <- function(well_data, well_parameters, WT, num_iterations, df, i, TotTime, t_time)
{

  # Dataframe to collect all the write operations and perform at once
  return_df <- data.frame()

  while ((well_data$Exploitable_GW < well_parameters$Depletion_Limit) && (well_data$Max_Drawdown >= 1) && TotTime <= 200)
  {
    #initialize
    drawdown <- 0
    t_time <- well_parameters$Max_Lifetime_in_Years
    errFactor <- 0.1
    #k <- 0
    #Jacob correction for observed drawdown. This is the drawdown to be used with the Theis solution.
    sadj <- well_data$Max_Drawdown - ((well_data$Max_Drawdown ^ 2) / (2 * well_data$Aqfr_Sat_Thickness))
    #First compute drawdown with initial Q guess
    rw <- well_parameters$Well_Diameter * 0.5

    well_calculations <- calc_wells(t_time, rw, well_parameters, well_data)

    t_time <- well_calculations$t_years
    drawdown <- well_calculations$drawdown
    W <- well_calculations$W
    #Second: Iterate on Q.
    #initialize Q loop
    inRange <- TRUE

    # Added this so well_data can be used instead of well_parameters for the well_yield. It was recursive so needed to initialize here
    well_data[["Well_Yield"]] <- well_parameters$Well_Yield

    while(inRange == TRUE)
    {
      inRange <- (abs(sadj - drawdown) > errFactor)
      well_parameters[["Well_Yield"]] <- well_parameters$Well_Yield * (abs(sadj / drawdown))
      drawdown <- (well_parameters$Well_Yield / (4.0 * pi * well_data$Transmissivity) * W)
    }
    # browser()
    # Guess the radius of influence of Q
    well_data <- guess_Q(num_iterations, well_parameters, well_data, df, i, inRange, WT, t_time)

    # Run code while drawdown in pumping well is gt max possible drawdown
    # iterate through each year of pumping up to the max life time in years
    # Added this line because sobs == error is causing problems in the line below setting total_head
    sobs <<- 0
    return_vals <- calculate_output(well_parameters, well_data, rw, df, i, num_iterations)
    output_list <- return_vals[[1]]
    well_data <- return_vals[[2]]

    #initialize next 20 years
    well_data[["Available_volume"]] <- well_data$Areal_Extent * well_data$Orig_Aqfr_Sat_Thickness * well_data$Storativity
    well_data[["Total_Volume_Produced"]] <- well_data$Total_Volume_Produced + well_parameters$Well_Yield * well_parameters$Max_Lifetime_in_Years * well_parameters$Annual_Operation_time
    well_data[["Exploitable_GW"]] <- well_data$Total_Volume_Produced / well_data$Available_volume
    well_data[["Aqfr_Sat_Thickness"]] <- well_data$Total_Thickness - well_data$Total_Volume_Produced / (pi * well_data$radial_extent ^ 2 * well_data$Storativity)
    well_data[["Depth_to_Piezometric_Surface"]] <- well_data$Total_Thickness - well_data$Aqfr_Sat_Thickness
    well_data[["Max_Drawdown"]] <- well_data$Aqfr_Sat_Thickness - WT
    well_data[["Total_Head"]] <- sobs + well_data$Depth_to_Piezometric_Surface
    num_iterations <- num_iterations + 1
    TotTime <- num_iterations * 2 * t_time

    return_df <- rbind(return_df, output_list)
  }
  return(list(return_df, well_data))
}

#' Guess the radius of influence of Q
#'
#' Long description
#'
#' @param num_iterations: passed in from previous function - used as a counter
#' @param well_parameters: well parameters read in from the well_params file
#' @param well_data: well data that is calculated during the run, not read in from the file
#' @param df: main input dataframe
#' @param inRange: used in the while loop as a tracking mechanism
#' @param WT: unsure
#'
#' @return  fill
#' @author Jason Evanoff; jason.evanoff@pnnl.gov
#' @export
#'
guess_Q <- function(num_iterations, well_parameters, well_data, df, i, inRange, WT, t_time)
{
  # Guess the radius of influence of Q
  if (num_iterations < 2)
  {
    return_on_investment <- (well_parameters$Well_Yield * t_time * well_parameters$Annual_Operation_time /
                               (pi * well_data$Orig_Aqfr_Sat_Thickness * df[i,"Porosity"])) ^ 0.5
    social_roi <- well_data$roi_boundary + errFactor + 1
    inRange <- TRUE

    while(inRange == TRUE)
    {
      inRange <- (abs(social_roi - well_data$roi_boundary) > errFactor)
      if (social_roi < 0)
      {
        return_on_investment <- return_on_investment * 0.75
      }
      else
      {
        return_on_investment <- return_on_investment * (social_roi / well_data$roi_boundary) ^ 0.033
      }
      well_calculations <- calc_wells(t_time, return_on_investment, well_parameters, well_data)
      t_time <- well_calculations$t_years
      social_roi <- well_calculations$drawdown
    }

    well_data[["radial_extent"]] <- return_on_investment
    well_data[["Drawdown_roi"]] <- social_roi
    well_data[["Areal_Extent"]] <- pi * (well_data$radial_extent ^ 2)                                                                                                                                         #m3
    if(well_data$Areal_Extent > (df[i,"Area"] + errFactor))
    {
      well_data[["Max_Drawdown"]] <- well_data$Max_Drawdown * (abs(df[i,"Area"] / well_data$Areal_Extent))
      WT = well_data$Orig_Aqfr_Sat_Thickness - well_data$Max_Drawdown
      # break
    }
  }
  return(well_data)
}

#' Main
#'
#' @param well_param_file Full path with file name and extension to the input well parameters YAML file
#' @param electric_cost_file Full path with file name and extension to the input GCAM electrical rates YAML file
#' @param config_file Full path with file name and extension to the input configuration CSV file
#' @param output_csv Full path with file name and extension to the output CSV file
#' @return There is no return value. A csv file is created as the output
#' @author <name>; <email>
#' @export
main <- function(well_param_file, electric_cost_file, config_file, output_csv)
{
  well_parameters <- load_well_parameters(well_param_file)
  electric_price_data <- load_elec_data(electric_cost_file)
  dt <- load_config(config_file)

  write_df <- process_data(df, well_parameters, electric_price_data)

  # Set up the output file. This file is written to after the completion of every node so the output may be copied and used throughout
  con <- file(output_csv, "w")
  write.csv(write_df, file = con,row.names = FALSE)
  close(con)

}
