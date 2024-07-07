source("utils.R")
#1. Required Input files########################################################
static_files <- list.files(path = here("data", "static"), pattern = "\\.csv$",full.names = TRUE)
bfi_HUC12 <- read_csv("data/bfi/bfi_table_HUC12.txt")
fromto_table <- read_csv("data/fromto_allRegions_12.csv")
daily_files <- list.files(path = here("data","flow"), pattern = "\\.txt$",full.names = TRUE)
nlcd_stations <- read_csv("./data/lu_percentage_by_huc12/NLCD_by_all_stationIDs.csv")
#2. Select HUC02 regions########################################################
huc02_regions <- c("01","02","03")

#3. Prep all data for selected regions##########################################
source("prep_data.R")

#4. Cross validations###########################################################
source("run_loadest_cv.R")
source("run_WRTDS_cv.R")
source("run_XGB_cv.R")

#5. Model evaluations###########################################################
source("evals_cv.R")

#6. Complete models and SHAP ###################################################
source("run_XGB_complete_model.R")

#6. Plots and tables############################################################
variables_names <- c("ln(Q)","CH_L","ln(ΔQ)","ln(Q30)","ln(Q7)","DecYear","DOY","Sed_VHigh","BFI","Sed_Mod","USLE K","Mean PRCP","HydB","Sed_Low","PRCP/SRQ","HydD","HydA","CH_W",
                     "Slope","Sed_High","HydC","CH_S","CH_D","Sed_VLow","Developed","Forest","Wetlands","Grassland/Shrub","Managed Vegetation")
variables_replace <- c("logQ", "Tot_L","logdQ", "logd30Q", "logd7Q", "DecDate","DOY", "sed_Vhigh", "bfi", "sed_mod", "K_factor", "ann_avg_pcp", "fr_hydB", "sed_low", 
                       "ann_avg_srq", "fr_hydD", "fr_hydA", "wCH_W", "wt_slp_sub", "sed_high", "fr_hydC", "wCH_S", "wCH_D", "sed_Vlow","Developed","Forest","Wetlands","Grassland/Shrub","Managed_Vegetation")
names(variables_replace) <- variables_names

#Plot Performance CDF
source("Plot_CDF.R")

#Plot Performance w.r.t. number of samples
source("Plot_accuracy_wrt_sampledDays.R")

#Plot variable importance
source("Plot_variable_importance")

#Save and data##################################################################
#save.image(file = "my_environment.RData")
#load("my_environment.RData")
