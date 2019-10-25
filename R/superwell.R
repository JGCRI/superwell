# Estimate global volume and unit-costs of accessible groundwater production in 235 GCAM water regions

# Modified code originally created by DJ Watson
# File retrieved from file://pnl/projects/JGCRI_YONK/JGCRI/DJ_Share/R%20code/ on July 31, 2019
# Updated the outpus structure as shown in /R Code/MENA_Results_161216/updatedformat/5/superwell.R

# Input files: wellParams.yml, GCAM_Electrical_Rates.yml, Inputs.csv
# Output file: ***_WellResults.csv (as in original code)

options(stringsAsFactors = FALSE)


# -----
#Variables
# -----

# Hard-coded variables:
# ---

# Constants
Euler_constant <- 0.5772156649
pi <- 3.14159
conversion_factor <- 0.000810714
# ------

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
#' @author <name>; <email>
#' @export
load_well_data <- function(well_param_file)
{
  # Parameters in wellParams.yml
  # @param Initial_Well_Yield <- Well_Yield <- 5e-05
  #well_param_file = "wellParams.yml"

  tryCatch(
    {
      wp <- yaml.load_file(well_param_file)
    },
    error = function(err)
    {
      # Log or display error in this block
    }
  )

  # Store well yield data from the yaml file because we will need to go back to it with every node
  wp[["Initial_Well_Yield"]] <- wp$Well_Yield

  # Store a "generic" electricity cost rate in case of an error
  wp[["global_energy_cost_rate"]] <- wp$Energy_cost_rate

  # Variables from test yml
  # wp[["Annual_Operation_time"]] <- wp$Annual_Operation_time  #s/year
  # wp[["Depletion_Limit"]]       <- wp$Depletion_Limit  #$/KWh
  # wp[["Energy_cost_rate"]]      <- wp$Energy_cost_rate  #$/year
  # wp[["Interest_Rate"]]         <- wp$Interest_Rate  #n/a
  # wp[["Maintenance_factor"]]    <- wp$Maintenance_factor  #years
  # wp[["Max_Lifetime_in_Years"]] <- wp$Max_Lifetime_in_Years #n/a
  # wp[["Pump_Efficiency"]]       <- wp$Pump_Efficiency   #(kg/m3*m/s2)
  # wp[["Specific_weight"]]       <- wp$Specific_weight   #m
  # wp[["Static_head"]]           <- wp$Static_head   #m
  # wp[["Well_Diameter"]]         <- wp$Well_Diameter  #$/m
  # wp[["Well_Install_10"]]       <- wp$Well_Install_10  #$/m
  # wp[["Well_Install_20"]]       <- wp$Well_Install_20  #$/m
  # wp[["Well_Install_30"]]       <- wp$Well_Install_30   #$/m
  # wp[["Well_Yield"]]            <- wp$Well_Yield   #m3/s

  return(wp)
}


#' Load electricity data by GCAM basin
#'
#' Read in the cost of electricity by GCAM basin (USD/Unit for 172 countries)
#'
#' @param el_cost_file Full path with file name and extension to the input GCAM electrical rates YAML file
#' @return <fill in>
#' @import yaml
#' @author <name>; <email>
#' @export
load_elec_data <- function(el_cost_file)
{

  # el_cost_file = "GCAM_Electrical_Rates.yml"
  ec <- yaml.load_file(el_cost_file)

  return(ec)
}


#' Load electricity data by GCAM basin
#'
#' Read in the cost of electricity by GCAM basin (USD/Unit for 172 countries)
#'
#' @param config_file Full path with file name and extension to the input configuration CSV file
#' @return <fill in>
#' @author <name>; <email>
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter
#' @export
load_config <- function(config_file, country = 'Iran')
{
  # Node-specific input (permeability, prosity, thickness, etc.) are in "Inputs.csv"
  ## check for one country (Iran)
  #config_file <- 'inputs.csv'
  df <- read.csv(config_file) %>%
      filter(CNTRY_NAME %in% (country))

  return (df)
}




# Other parameters in the script
# @param Max_Drawdown <- 0.66 (commented out in the script)
# @param Screen_length_of_original_Aqfr_Sat_Thickness <- 0.3
# @param WHYClass - 10, 20 or 30
# @param errFactor <- 0.1
# @param Max_Drawdown_to_Orig_Aqfr_Sat_Thickness <- 0.66 (line 139)
# @param radius of influence of Q <- 0.75


calc_wells <- function(time, well_radius, well_params, well_data)
{
# Pumping well drawdown <-2.3Q/4*pi*T*log(2.25Tt/r2S)
    # transient Theis solution
    # s - the drawdown (m);
    # t - time (years)
    # rw - well radius

  # See no point to this line(?):
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
  result <- list(s = s, t = time, W = W)

  return(result)
}

calculate_drawdown <- function(wp, well_data, rw, df, i, num_iterations)
{

  #Calculate drawdown at the well over time with costs
  #Initialize drawdown from pumping well and image wells and time
  s <- 0
  t <- 0

  #--- WHILE 5 START
  while ((t < wp$Max_Lifetime_in_Years) && (wp$Well_Yield > 0))
  {
    # print(paste("while5 ", i))
    t <- t + 1
    well_calculations <- calc_wells(t, rw, wp, well_data)
    t <- well_calculations$t
    s <- well_calculations$s
    w <- well_calculations$W

    #Solve quadratic of jacob correction for observed drawdown in well
    #(s_obs^2)/(2h)-s_obs+s=0
    a <- 1 / (2 * well_data$Aqfr_Sat_Thickness)
    b <- -1
    c <- s
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
    sobs <- root1
    if (sobs < root2)
    {
      sobs <- root2
    }
    # Added this code because sobs == error is causing problems in the line below setting total_head
    # Not sure why it's failing now after I've moved it to a function, attempting to compensate
    #Output (one row per year)
    well_data[["Drawdown"]] <- s
    well_data[["Total_Head"]] <- sobs + well_data$Depth_to_Piezometric_Surface
    well_data[["Volume_Produced"]] <- t * wp$Annual_Operation_time * wp$Well_Yield                                # m^3
    well_data[["Power"]] <- (wp$Specific_weight * well_data$Total_Head * wp$Well_Yield / wp$Pump_Efficiency) / 1000      # KW
    well_data[["Electric_Energy"]] <- wp$Power *(wp$Annual_Operation_time/3600) #(CONVERT(1,"hr","sec")))         # KWh/year
    well_data[["Annual_Capital_Cost"]] <- wp$Well_Installation_cost *
      (1 + wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years * wp$Interest_Rate /
      ((1+wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years - 1)
    well_data[["Maintenance_Cost"]] <- wp$Maintenance_factor * wp$Well_Installation_cost                          # $
    well_data[["Total_Cost"]] <- wp$Annual_Capital_Cost + wp$Maintenance_Cost                                     # $
    well_data[["Cost_of_Energy"]] <- (wp$Electric_Energy * well_data$Energy_cost_rate)                                   # $
    well_data[["Unit_cost"]] <- (wp$Total_Cost + wp$Cost_of_Energy) / (wp$Well_Yield * wp$Annual_Operation_time)
    well_data[["Cost_per_ac_ft"]] <- wp$Unit_cost / conversion_factor                                                   # $/acFt
    #Add the year's output to a list for export to file later
    NumWells <- df[i,"Area"] / wp$Areal_Extent
    TotTime = num_iterations * 2 * t
    if (t == 1)
    {
      outputList <- list()
    }
    outputList[[paste("line",as.character(t))]] <- paste(df[i,"Continent"], ",", df[i,"OBJECTID"], ",", df[i,"CNTRY_NAME"], ",", t, ",",
                                                         well_data$Drawdown, ",", sobs, ",", wp$Volume_Produced, ",", df[i,"Area"], ",", wp$Areal_Extent,
                                                         ",", well_data$Total_Head, ",", wp$Power, ",", wp$Electric_Energy, ",", well_data$Energy_cost_rate,
                                                         ",", wp$Cost_of_Energy, ",", wp$Unit_cost, ",", wp$Cost_per_ac_ft, ",", wp$Interest_Rate,
                                                         ",", wp$Max_Lifetime_in_Years, ",", wp$Maintenance_factor, ",", wp$Well_Yield, ",",
                                                         wp$Annual_Operation_time, ",", well_data$Total_Well_Length, ",", trunc(df[i,"WHYClass"] / 10 * 10),
                                                         ",", wp$Available_volume, ",", NumWells, ",", TotTime, ",", df[i,"GCAM_ID"], ",", df[i,"Basin_Name"], "\n")
    #loop back to next year
    # print(paste(well_data$Exploitable_GW,  wp$Depletion_Limit, well_data$Max_Drawdown, run, TotTime))
    # print(paste((well_data$Exploitable_GW < wp$Depletion_Limit), (well_data$Max_Drawdown >= 1),(run == 0),TotTime <= 200))
  }
  #--- WHILE 5 END
}

#' Main <function to be completely replaced !!!>
#'
#' Function to be completely replaced
#'
#' @param well_param_file Full path with file name and extension to the input well parameters YAML file
#' @param elec_cost_file Full path with file name and extension to the input GCAM electrical rates YAML file
#' @param config_file Full path with file name and extension to the input configuration CSV file
#' @param output_csv Full path with file name and extension to the output CSV file
#' @return Data frame of well parameters
#' @author <name>; <email>
#' @export
main <- function(well_param_file, elec_cost_file, config_file, output_csv)
{

  wp <- load_well_data(well_param_file)

  ec <- load_elec_data(elec_cost_file)

  df <- load_config(config_file)

  # New variable to store non read in parameters
  well_data <- list()

  # -----
  # Output
  # ------
  # Set up the output file. This file will be written to after the completion of every node so the output may be copied
  # and used at any point during the run
  #file_name <- paste(df[1, "CNTRY_NAME"], "_WellResults.csv")

  con <- file(output_csv, "w")

  # Write the headers for the output file
  cat("Continent,ObjID,Country,time,Drawdown,Observed_Drawdown,Volume,Element_Area,Areal_Extent,Total_Head,Power,Electric_Energy,Energy_Cost_Rate,Cost_of_Energy,Unit_Cost,Cost_Per_Ac_Ft,Interest_Rate,Max_Lifetime_in_Years,Maintenance_factor,Well_Yield,Annual_Operation_time,Total_Well_Length,WHYClass,Available_Volume,Number_of_Wells, Total_Time,Basin_ID,Basin_Name\n", file = con)

  # -----
  # Calculations
  # -----
  # Each row in df is a different grid node in the model domain
  for(i in 1:(nrow(df)))
  {
    print(paste("loop ", i))
  	# Calculate and store other node specific attributes
  	if (is.null(ec[[df[i,"CNTRY_NAME"]]]) == FALSE )
  	{
  	  well_data[["Energy_cost_rate"]] <- ec[[df[i,"CNTRY_NAME"]]]
  	}
  	else
  	{
  	  well_data[["Energy_cost_rate"]] <- wp$global_energy_cost_rate
  	}


    # Initalize empty node dependant parameters here. I believe these should be here for reinitialization for each node. Hard to follow.
    #wp[["Well_Yield"]] <- wp$Initial_Well_Yield
    well_data[["Exploitable_GW"]] <- 0
    well_data[["Total_Volume_Produced"]] <- 0
    well_data[["Total_Head"]] <- 0
    well_data[["Drawdown"]] <- 0

    # Set up node dependant variables
    well_data[["Storativity"]] <- df[i,"Porosity"]
    well_data[["Depth_to_Piezometric_Surface"]] <- df[i,"Depth"]
    well_data[["Aqfr_Sat_Thickness"]] <- df[i,"Thickness"]                                                                                     # m
    well_data[["Orig_Aqfr_Sat_Thickness"]] <- df[i,"Thickness"]                                                                                # m
    well_data[["Max_Drawdown"]] <- 0.66 * well_data$Aqfr_Sat_Thickness                                                                                # m
    well_data[["Hydraulic_Conductivity"]] <- (10 ^ df[i,"Permeability"]) * 1e7
    well_data[["roi_boundary"]] <- 1

  	# Initialize
    well_data[["Screen_length"]] <- well_data$Orig_Aqfr_Sat_Thickness * 0.3                                                                           # m
    well_data[["Casing_length"]] <- well_data$Orig_Aqfr_Sat_Thickness + well_data$Depth_to_Piezometric_Surface - well_data$Screen_length                            # m
    well_data[["Total_Well_Length"]] <- well_data$Casing_length + well_data$Screen_length                                                                    # m
    well_data[["Transmissivity"]] <- well_data$Hydraulic_Conductivity * well_data$Screen_length                                                              # m2/s
  	if (df[i,"WHYClass"] == 10)
  	{
  		wp[["Well_Installation_cost"]] <- wp$Well_Install_10 * well_data$Total_Well_Length
  	}
  	else
  	{
  		if (df[i,"WHYClass"] == 20)
  		{
  			wp[["Well_Installation_cost"]] <- wp$Well_Install_20 * well_data$Total_Well_Length
  		}
  	  else
  	  {
  			wp[["Well_Installation_cost"]] <- wp$Well_Install_30 * well_data$Total_Well_Length
  		}
  	}

  	TotTime = 0
  	#wp[["Areal_Extent"]] <- df[i, "Area"]
  	outputList <- list()

    well_data$Max_Drawdown = 0.66 * well_data$Orig_Aqfr_Sat_Thickness
    WT <- well_data$Orig_Aqfr_Sat_Thickness - well_data$Max_Drawdown
    well_data$Total_Thickness = well_data$Depth_to_Piezometric_Surface + well_data$Orig_Aqfr_Sat_Thickness
    run <- 0

#--- WHILE 1 START
    while (run == 0)
    {
      print(paste("while ", i))
    	num_iterations <- 1

  #--- WHILE 2 START
  		while ((well_data$Exploitable_GW < wp$Depletion_Limit) && (well_data$Max_Drawdown >= 1) && (run == 0) && TotTime <= 200)
  		{
  		  print(paste("while2 ", i))
  			#initialize
  			s <- 0
  			t <- wp$Max_Lifetime_in_Years
  			errFactor <- 0.1
  			#k <- 0
  			#Jacob correction for observed drawdown. This is the drawdown to be used with the Theis solution.
  			sadj <- well_data$Max_Drawdown - ((well_data$Max_Drawdown ^ 2) / (2 * well_data$Aqfr_Sat_Thickness))
  			#First compute drawdown with initial Q guess
  			rw <- wp$Well_Diameter * 0.5

  		  well_calculations <- calc_wells(t, rw, wp, well_data)

  			t <- well_calculations$t
  			s <- well_calculations$s
  			W <- well_calculations$W
  			#Second: Iterate on Q.
  			#initialize Q loop
  			inRange <- TRUE

    #--- WHILE 3 START
  			while(inRange == TRUE)
  			{
  			  print(paste("while3 ", i))
  				inRange = (abs(sadj - s) > errFactor)
  				wp[["Well_Yield"]] <- wp$Well_Yield * (abs(sadj / s))
  				s <- (wp$Well_Yield / (4.0 * pi * well_data$Transmissivity) * W)
  			}

  	#--- WHILE 3 END

  			# Guess the radius of influence of Q
  			if (num_iterations < 2)
  			{
  				return_on_investment <- (wp$Well_Yield * t * wp$Annual_Operation_time / (pi * well_data$Orig_Aqfr_Sat_Thickness * df[i,"Porosity"])) ^ 0.5
  				social_roi <- well_data$roi_boundary + errFactor + 1
  				inRange <- TRUE

      #--- WHILE 4 START
  				while(inRange == TRUE)
  				{
  				 # print(paste("while4 ", i))
  				  inRange = (abs(social_roi - well_data$roi_boundary) > errFactor)
  					if (social_roi < 0)
  					{
  						return_on_investment = return_on_investment * 0.75
  					}
  				  else
  				  {
  						return_on_investment = return_on_investment * (social_roi / well_data$roi_boundary) ^ 0.033
  					}
  					well_calculations <- calc_wells(t, return_on_investment, wp, well_data)
  					t <- well_calculations$t
  					social_roi <- well_calculations$s
  				}
      #--- WHILE 4 END

  				wp[["radial_extent"]] <- return_on_investment
  				wp[["Drawdown_roi"]] <- social_roi
  				wp[["Areal_Extent"]] <- pi * (wp$radial_extent ^ 2)                                                                                                                                         #m3
  				if(wp$Areal_Extent > (df[i,"Area"] + errFactor))
  				{
  				  well_data[["Max_Drawdown"]] <- well_data$Max_Drawdown * (abs(df[i,"Area"] / wp$Areal_Extent))
  					WT = well_data$Orig_Aqfr_Sat_Thickness - well_data$Max_Drawdown
  					break
  				}
  			}

  		# Run code while drawdown in pumping well is gt max possible drawdown
  		# iterate through each year of pumping up to the max life time in years

      #--- Previously 'while #5'
  			calculate_drawdown(wp, well_data, rw, df, i, num_iterations)
  		#--- End old 'while 5'

  			#initialize next 20 years
  			wp[["Available_volume"]] <- wp$Areal_Extent * well_data$Orig_Aqfr_Sat_Thickness * well_data$Storativity
  			well_data[["Total_Volume_Produced"]] <- well_data$Total_Volume_Produced + wp$Well_Yield * wp$Max_Lifetime_in_Years * wp$Annual_Operation_time
  			well_data[["Exploitable_GW"]] <- well_data$Total_Volume_Produced / wp$Available_volume
  			wp[["Aqfr_Sat_Thickness"]] <- wp$Total_Thickness - well_data$Total_Volume_Produced / (pi * wp$radial_extent ^ 2 * wp$Storativity)
  			wp[["Depth_to_Piezometric_Surface"]] <- wp$Total_Thickness - well_data$Aqfr_Sat_Thickness
  			well_data[["Max_Drawdown"]] <- wp$Aqfr_Sat_Thickness - WT
  			well_data[["Total_Head"]] <- sobs + well_data$Depth_to_Piezometric_Surface
  			num_iterations <- num_iterations + 1
  			TotTime = num_iterations * 2 * t
  	    #loop back to expl gw
  			#append results to output file
  			for (name in names(outputList))
  			{
  			  cat(outputList[[name]], file = con)
  			}
  		}
    #--- WHILE 2 END

    	run <- 1
    	if (well_data$Max_Drawdown <= 1)
    	{
    	    #cat("Warning: maxDradown less than 1 meter")
    	}
    }
  #--- WHILE 1 END
  }
#--- FOR END
  close(con)
}

