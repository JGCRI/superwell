# Estimate global volume and unit-costs of accessible groundwater production in 235 GCAM water regions

# Modified code originally created by DJ Watson
# File retrieved from file://pnl/projects/JGCRI_YONK/JGCRI/DJ_Share/R%20code/ on July 31, 2019
# Updated the outpus structure as shown in /R Code/MENA_Results_161216/updatedformat/5/superwell.R


# Input files: wellParams.yml, GCAM_Electrical_Rates.yml, Inputs.csv
# Output file: ***_WellResults.csv (as in original code)


options(stringsAsFactors = FALSE)


#' Load well parameters
#'
#' Load well-specific parameters (e.g., costs, etc.) from "wellParams.yml" (total 14 well parameters)
#'
#' @param well_param_file Full path with file name and extension to the input well parameters YAML file
#' @return Data frame of well parameters
#' @import  yaml
#' @author <name>; <email>
#' @export
load_well_data <- function(well_param_file) {

  #well_param_file = "wellParams.yml"
  wp <- yaml.load_file(well_param_file)

  # Store well yield data from the yaml file because we will need to go back to it with every node
  wp[["Initial_Well_Yield"]] <- wp$Well_Yield

  # Store a "generic" electricity cost rate in case of an error
  wp[["global_energy_cost_rate"]] <- wp$Energy_cost_rate

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
load_elec_data <- function(el_cost_file) {

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
load_config <- function(config_file, country = 'Iran') {
  # Node-specific input (permeability, prosity, thickness, etc.) are in "Inputs.csv"
  ## check for one country (Iran)
  #config_file <- 'inputs.csv'
  df <- read.csv(config_file) %>%
      filter(CNTRY_NAME %in% (country))

  return (df)
}


# -----
#Variables
# -----

# Hard-coded variables:
# ---

# Constants
# Euler_constant <- 0.5772156649
# pi <- 3.14159
# conversion_factor <- 0.000810714
# ------

# Parameters in wellParams.yml

# Parameters we likely may change:
# @param Depletion_Limit <- 0.05
# @param Energy_cost_rate <- 0.074
# @param Interest_Rate <- 0.1
# @param Maintenance_factor <- 0.07
# @param Max_lifetime_in_Years <- 20
# @param Pump_efficiency <- 0.5

# Other parameters in wellParams.yml
# @param Annual_Operation_time <- 31536000
# @param Specific_weight <- 9800
# @param Static_head <- 0
# @param Well_Diameter <- 0.28
# @param Well_Install_10 <- 82
# @param Well_Install_10 <- 164
# @param Well_Install_10 <- 50
# @param Initial_Well_Yield <- Well_Yield <- 5e-05

# Other parameters in the script
# @param Max_Drawdown <- 0.66 (commented out in the script)
# @param Screen_length_of_original_Aqfr_Sat_Thickness <- 0.3
# @param WHYClass - 10, 20 or 30
# @param errFactor <- 0.1
# @param Max_Drawdown_to_Orig_Aqfr_Sat_Thickness <- 0.66 (line 139)
# @param radius of influence of Q <- 0.75


calc_wells <- function(t, rw, wp) {

# Pumping well drawdown <-2.3Q/4*pi*T*log(2.25Tt/r2S)
    # transient Theis solution
    # s - the drawdown (m);
    # t - time (years)
    # rw - well radius

  W <- 0.0   # W(u) - the Well Function (exponential integral)
  u <- rw ^ 2 * wp$Storativity / (4 * wp$Transmissivity * t * wp$Annual_Operation_time)
  j <- 1
  term <- -u
  W <- -0.5772156649 - log(u) - term

  while (abs(term) > 0.00000001 && term != Inf) {
    term <- -u * j / (j + 1) ^ 2 * term
    W <- W - term
    j <- j + 1
  }

  s <- (wp$Well_Yield / (4.0 * 3.14159 * wp$Transmissivity) * W)

  result <- list(s = s, t = t, W = W)

  return(result)
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
main <- function(well_param_file, elec_cost_file, config_file, output_csv) {

  wp <- load_well_data(well_param_file)

  ec <- load_elec_data(elec_cost_file)

  df <- load_config(config_file)

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
  for(i in 1:(nrow(df))) {

  	# Calculate and store other node specific attributes
  	wp[["Well_Yield"]] <- wp$Initial_Well_Yield
  	if (is.null(ec[[df[i,"CNTRY_NAME"]]]) == FALSE ) {
  		wp[["Energy_cost_rate"]] <- ec[[df[i,"CNTRY_NAME"]]]
  	} else {
  		wp[["Energy_cost_rate"]] <- wp$global_energy_cost_rate
  	}
  	wp[["Storativity"]] <- df[i,"Porosity"]
  	wp[["Depth_to_Piezometric_Surface"]] <- df[i,"Depth"]
  	wp[["Aqfr_Sat_Thickness"]] <- df[i,"Thickness"]                                                                                     # m
  	wp[["Orig_Aqfr_Sat_Thickness"]] <- df[i,"Thickness"]                                                                                # m
  	wp[["Screen_length"]] <- wp$Orig_Aqfr_Sat_Thickness * 0.3                                                                           # m
  	wp[["Casing_length"]] <- wp$Orig_Aqfr_Sat_Thickness + wp$Depth_to_Piezometric_Surface - wp$Screen_length                            # m
  #	wp[["Max_Drawdown"]] <- 0.66 * wp$Aqfr_Sat_Thickness                                                                                # m
  	wp[["Total_Well_Length"]] <- wp$Casing_length + wp$Screen_length                                                                    # m
  	wp[["Hydraulic_Conductivity"]] <- (10 ^ df[i,"Permeability"]) * 1e7
  	wp[["roi_boundary"]] <- 1
  	wp[["Transmissivity"]] <- wp$Hydraulic_Conductivity * wp$Screen_length                                                              # m2/s
  	if (df[i,"WHYClass"] == 10) {
  		wp[["Well_Installation_cost"]] <- wp$Well_Install_10 * wp$Total_Well_Length
  	} else {
  		if (df[i,"WHYClass"] == 20){
  			wp[["Well_Installation_cost"]] <- wp$Well_Install_20 * wp$Total_Well_Length
  		} else {
  			wp[["Well_Installation_cost"]] <- wp$Well_Install_30 * wp$Total_Well_Length
  		}
  	}
  	wp[["Exploitable_GW"]] <- 0
  	wp[["Total_Volume_Produced"]] <- 0
  	wp[["Total_Head"]] <- 0
  	wp[["Drawdown"]] <- 0
  	TotTime = 0
  	#wp[["Areal_Extent"]] <- df[i, "Area"]
  	outputList <- list()

    wp$Max_Drawdown = 0.66 * wp$Orig_Aqfr_Sat_Thickness
    WT <- wp$Orig_Aqfr_Sat_Thickness - wp$Max_Drawdown
    wp$Total_Thickness = wp$Depth_to_Piezometric_Surface + wp$Orig_Aqfr_Sat_Thickness
    run <- 0
    while (run == 0) {
    	NumIterations <- 1
  		while ((wp$Exploitable_GW < wp$Depletion_Limit) && (wp$Max_Drawdown >= 1) && (run == 0) && TotTime <= 200) {
  			#initialize
  			s <- 0
  			t <- wp$Max_Lifetime_in_Years
  			errFactor <- 0.1
  			#k <- 0
  			#Jacob correction for observed drawdown. This is the drawdown to be used with the Theis solution.
  			sadj <- wp$Max_Drawdown - ((wp$Max_Drawdown ^ 2) / (2 * wp$Aqfr_Sat_Thickness))
  			#First compute drawdown with initial Q guess
  			rw <- wp$Well_Diameter * 0.5
  			calcResults <- calc_wells(t, rw, wp)
  			t <- calcResults$t
  			s <- calcResults$s
  			W <- calcResults$W
  			#Second: Iterate on Q.
  			#initialize Q loop
  			inRange <- TRUE

  			while(inRange == TRUE) {
  				inRange = (abs(sadj - s) > errFactor)
  				wp[["Well_Yield"]] <- wp$Well_Yield * (abs(sadj / s))
  				s <- (wp$Well_Yield / (4.0 * 3.14159 * wp$Transmissivity) * W)
  			}
  				# Guess the radius of influence of Q
  			if (NumIterations < 2) {
  				roi <- (wp$Well_Yield * t * wp$Annual_Operation_time / (3.14159 * wp$Orig_Aqfr_Sat_Thickness * df[i,"Porosity"])) ^ 0.5
  				sroi <- wp$roi_boundary + errFactor + 1
  				inRange <- TRUE
  				while(inRange == TRUE) {
  				  inRange = (abs(sroi - wp$roi_boundary) > errFactor)
  					if (sroi < 0) {
  						roi = roi * 0.75
  					} else {
  						roi = roi * (sroi / wp$roi_boundary) ^ 0.033
  					}
  					calcResults <- calc_wells(t, roi, wp)
  					t <- calcResults$t
  					sroi <- calcResults$s
  				}

  				wp[["radial_extent"]] <- roi
  				wp[["Drawdown_roi"]] <- sroi
  				wp[["Areal_Extent"]] <- 3.14159 * (wp$radial_extent ^ 2)                                                                                                                                         #m3
  				if(wp$Areal_Extent > (df[i,"Area"] + errFactor)) {
  					wp[["Max_Drawdown"]] <- wp$Max_Drawdown * (abs(df[i,"Area"] / wp$Areal_Extent))
  					WT = wp$Orig_Aqfr_Sat_Thickness - wp$Max_Drawdown
  					break
  				}
  			}

  		#Calculate drawdown at the well over time with costs
  		#Initialize drawdown from pumping well and image wells and time
  			s <- 0
  			t <- 0

  		#Run code while drawdown in pumping well is gt max possible drawdown
  		#iterate through each year of pumping up to the max life time in years
  			while ((t < wp$Max_Lifetime_in_Years) && (wp$Well_Yield > 0)) {
  				t <- t + 1
  				calcResults <- calc_wells(t, rw, wp)
  				t <- calcResults$t
  				s <- calcResults$s
  				w <- calcResults$W

  				#Solve quadratic of jacob correction for observed drawdown in well
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
  				if (root1 > wp$Max_Drawdown+errFactor || root1 < 0) {
  				  root1 <- 0
  				}
  				if (root2 > wp$Max_Drawdown+errFactor || root2 < 0) {
  				  root2 <- 0
  				}
  				sobs = root1
  				if (sobs < root2) {
  				  sobs <- root2
  				}
  				#Output (one row per year)
  				wp[["Drawdown"]] <- s                                                                                  # m
  				wp[["Total_Head"]] <- sobs + wp$Depth_to_Piezometric_Surface
  				wp[["Volume_Produced"]] <- t * wp$Annual_Operation_time * wp$Well_Yield                                # m^3
  				wp[["Power"]] <- (wp$Specific_weight * wp$Total_Head * wp$Well_Yield / wp$Pump_Efficiency) / 1000      # KW
  				wp[["Electric_Energy"]] <- wp$Power *(wp$Annual_Operation_time/3600) #(CONVERT(1,"hr","sec")))         # KWh/year
  				wp[["Annual_Capital_Cost"]] <- wp$Well_Installation_cost *
  				    (1 + wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years * wp$Interest_Rate /
  				    ((1+wp$Interest_Rate) ^ wp$Max_Lifetime_in_Years - 1)
  				wp[["Maintenance_Cost"]] <- wp$Maintenance_factor * wp$Well_Installation_cost                          # $
  				wp[["Total_Cost"]] <- wp$Annual_Capital_Cost + wp$Maintenance_Cost                                     # $
  				wp[["Cost_of_Energy"]] <- (wp$Electric_Energy * wp$Energy_cost_rate)                                   # $
  				wp[["Unit_cost"]] <- (wp$Total_Cost + wp$Cost_of_Energy) / (wp$Well_Yield * wp$Annual_Operation_time)
  				wp[["Cost_per_ac_ft"]] <- wp$Unit_cost / 0.000810714                                                   # $/acFt
  				#Add the year's output to a list for export to file later
  				NumWells <- df[i,"Area"] / wp$Areal_Extent
  				TotTime = NumIterations * 2 * t
  				if (t == 1) { outputList <- list() }
  				outputList[[paste("line",as.character(t))]] <- paste(df[i,"Continent"], ",", df[i,"OBJECTID"], ",", df[i,"CNTRY_NAME"], ",", t, ",", wp$Drawdown, ",", sobs, ",", wp$Volume_Produced, ",", df[i,"Area"], ",", wp$Areal_Extent, ",", wp$Total_Head, ",", wp$Power, ",", wp$Electric_Energy, ",", wp$Energy_cost_rate, ",", wp$Cost_of_Energy, ",", wp$Unit_cost, ",", wp$Cost_per_ac_ft, ",", wp$Interest_Rate, ",", wp$Max_Lifetime_in_Years, ",", wp$Maintenance_factor, ",", wp$Well_Yield, ",", wp$Annual_Operation_time, ",", wp$Total_Well_Length, ",", trunc(df[i,"WHYClass"] / 10 * 10), ",", wp$Available_volume, ",", NumWells, ",", TotTime, ",", df[i,"GCAM_ID"], ",", df[i,"Basin_Name"], "\n")
  			#loop back to next year
  			}
  			#initialize next 20 years
  			wp[["Available_volume"]] <- wp$Areal_Extent * wp$Orig_Aqfr_Sat_Thickness * wp$Storativity
  			wp[["Total_Volume_Produced"]] <- wp$Total_Volume_Produced + wp$Well_Yield * wp$Max_Lifetime_in_Years * wp$Annual_Operation_time
  			wp[["Exploitable_GW"]] <- wp$Total_Volume_Produced / wp$Available_volume
  			wp[["Aqfr_Sat_Thickness"]] <- wp$Total_Thickness - wp$Total_Volume_Produced / (3.14159 * wp$radial_extent ^ 2 * wp$Storativity)
  			wp[["Depth_to_Piezometric_Surface"]] <- wp$Total_Thickness - wp$Aqfr_Sat_Thickness
  			wp[["Max_Drawdown"]] <- wp$Aqfr_Sat_Thickness - WT
  			wp[["Total_Head"]] <- sobs + wp$Depth_to_Piezometric_Surface
  			NumIterations <- NumIterations + 1
  			TotTime = NumIterations * 2 * t
  	    #loop back to expl gw
  			#append results to output file
  			for (name in names(outputList)) {
  			  cat(outputList[[name]], file = con)
  			}
  			}
    	  run <- 1
    	  if (wp$Max_Drawdown <= 1) {
    	    #cat("Warning: maxDradown less than 1 meter")
    	  }
    }
  }

  close(con)
}

