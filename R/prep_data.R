all_data <- list()
#A.Watershed characteristics#################################################### 
static_chars <- lapply(static_files,read_csv)
static_chars <- do.call(rbind,static_chars)

#Add BFI values############################
bfi_HUC12 <- bfi_HUC12%>%
                select("HUC12", "MEAN")%>%
                rename("HUC12" = "HUC12",
                       "bfi" = "MEAN")
#add BFI column to static chars
static_chars$bfi <- NA
for(huc in static_chars$HUC12){
  all_hucs <- get_routing_subwatersheds(huc, fromto_table)
  bfi_all_hucs <- bfi_HUC12%>%filter(HUC12 %in% all_hucs)
  static_chars$bfi[static_chars$HUC12 == huc] <- mean(bfi_all_hucs$bfi)
}

#Add NLCD land cover fractions and Drainage Area#####
nlcd_stations <- read_csv("./data/lu_percentage_by_huc12/NLCD_by_all_stationIDs.csv")
nlcd_stations$Developed <- nlcd_stations$`Developed, High Intensity` + nlcd_stations$`Developed, Low Intensity` + nlcd_stations$`Developed, Medium Intensity`
nlcd_stations$Forest <- nlcd_stations$`Deciduous Forest` + nlcd_stations$`Evergreen Forest` + nlcd_stations$`Mixed Forest`
nlcd_stations$Wetlands <- nlcd_stations$`Woody Wetlands` + nlcd_stations$`Emergent Herbaceous Wetlands`
nlcd_stations$'Grassland/Shrub' <- nlcd_stations$`Shrub/Scrub` + nlcd_stations$`Grassland/Herbaceous`
nlcd_stations$Managed_Vegetation <- nlcd_stations$`Cultivated Crops` + nlcd_stations$`Pasture/Hay`

nlcd_stations <- nlcd_stations%>%
                    select("StationID","HUC12","Developed","Forest","Wetlands","Grassland/Shrub","Managed_Vegetation")

static_chars <- static_chars%>%
                        left_join(nlcd_stations%>%select(-"HUC12"), by = "StationID")

static_chars$StationID <- as.character(static_chars$StationID)
static_chars <- static_chars%>%filter(substr(HUC12,1,2) %in% huc02_regions)
all_data[["static"]] <- static_chars

rm(bfi_HUC12,huc,static_files,fromto_table, static_chars,bfi_all_hucs,all_hucs)
#B. Daily flows#################################################################
daily_files <- daily_files[sub("\\.txt$", "", basename(daily_files)) %in% all_data[["static"]]$StationID]
daily_flows <- lapply(daily_files, 
                      function(file) {
                        data <- read.csv(file, sep = " ")
                        data$StationID <- sub("\\.txt$", "", basename(file)) 
                        data})
daily_flows <- do.call(rbind,daily_flows)
daily_flows$Date <- format(ymd(daily_flows$Date),"%m/%d/%Y")
daily_flows <- daily_flows%>%
  select(c("StationID","Date","Flow_cfs"))%>%
  filter(as.numeric(StationID) %in% unique(all_data[["static"]]$StationID))
names(daily_flows) <- c("StationID","date","Q")

all_data[["flows"]] <- daily_flows

rm(daily_flows,daily_files)
#C. Download Samples############################################################
parameter_codes <- c("TN" = "00600", "TP" = "00665", "TSS" = "80154")
sites <- all_data[["static"]] %>%
  filter(substr(HUC12, 1, 2) %in% c("01", "02", "03")) %>%
  mutate(StationID = case_when(
    nchar(StationID) == 7 ~ paste0("USGS-", str_pad(StationID, width = 8, side = "left", pad = "0")),
    nchar(StationID) == 8 ~ paste0("USGS-", str_pad(StationID, width = 8, side = "left", pad = "0")),
    TRUE ~ paste0("USGS-", str_pad(StationID, width = 10, side = "left", pad = "0"))
  ))
pData <- readWQPqw(sites$StationID, parameterCd = c("00665"))
nData <- readWQPqw(sites$StationID, parameterCd = c("00600"))
sData <- readWQPqw(sites$StationID, parameterCd = c("80154"))

sample_data_WQP <- rbind(pData, nData, sData)

sample_data <- sample_data_WQP%>%
  filter(year(ActivityStartDate) > 1995 & year(ActivityStartDate) < 2021)%>%
  filter(!is.na(ResultMeasureValue))%>%
  select(CharacteristicName,ActivityStartDate,MonitoringLocationIdentifier,ResultMeasureValue)%>%
  rename("Par" = "CharacteristicName",
         "date" = "ActivityStartDate",
         "StationID" = "MonitoringLocationIdentifier",
         "ConcAve" = "ResultMeasureValue")%>%
  mutate(Par = case_when(
    Par == "Phosphorus" ~ "TP",
    Par == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)" ~ "TN",
    Par == "Suspended Sediment Concentration (SSC)" ~ "TSS"))%>%
  filter(ConcAve > 0)%>%
  group_by(Par,date,StationID)%>%
  summarise(ConcAve = mean(ConcAve))%>%
  select("StationID","Par","date","ConcAve")%>%
  mutate(StationID = gsub("[^0-9]", "", StationID))%>%
  mutate(StationID = as.character(as.numeric(StationID)))
all_data[["Sample"]] <- sample_data 

rm(parameter_codes,sample_data,sData,nData,pData,sites)
#D. Get antecedent flows########################################################
add_antecedent_flows <- function(flows){
  flows$date <- as.Date(flows$date,format = "%m/%d/%Y")
  flows <- flows%>%
              arrange(StationID, date)%>%
              group_by(StationID) %>%
              complete(date = seq(min(date), max(date), by = "day"))%>%
              #fill(Q, .direction = "downup")%>%
              mutate(dQ = lag(Q, 1),
                     d7Q = rollapply(Q, 7, mean, partial = TRUE, fill = NA, align = "right"),
                     d30Q = rollapply(Q, 30, mean, partial = TRUE, fill = NA, align = "right"))
  return(flows)            
}
all_data[["flows"]] <- add_antecedent_flows(all_data[["flows"]])
all_data[["flows"]] <- all_data[["flows"]]%>%filter(!is.na(Q))
#E. Combine dataframes##########################################################
combine_data <- function(regions_data){
  Sample <- regions_data[["Sample"]]
  Sample <- Sample%>%
    left_join(regions_data[["flows"]], by = c("date","StationID"))%>%
    left_join(regions_data[["static"]], by = "StationID")
  Sample <- Sample%>%
    filter(Q > 0)%>%
    mutate(DecDate = decimalDate(date),
           DOY = yday(date),
           logQ = log(Q),
           logdQ = log(dQ+0.001),
           logd7Q = log(d7Q+0.001),
           logd30Q = log(d30Q+0.001))%>%
    select("StationID","HUC12","Par","date","ConcAve","Q","logQ","logdQ","logd7Q","logd30Q","DecDate","DOY",
           "ann_avg_pcp","ann_avg_srq","bfi", "fr_hydA",
           "fr_hydB","fr_hydC","fr_hydD","Tot_L","wCH_W","wCH_D","wCH_S",
           "wt_slp_sub","sed_Vlow","sed_low","sed_mod","sed_high","sed_Vhigh","K_factor","Developed","Forest","Wetlands","Grassland/Shrub","Managed_Vegetation")
  
  Daily <- regions_data[["flows"]]
  Daily <- Daily%>%
    filter(Q > 0)%>%
    filter(year(date) > 1995 & year(date) < 2021)%>%
    left_join(regions_data[["static"]], by = "StationID")%>%
    mutate(DecDate = decimalDate(date),
           DOY = yday(date),
           logQ = log(Q),
           logdQ = log(dQ+0.001),
           logd7Q = log(d7Q+0.001),
           logd30Q = log(d30Q+0.001))%>%
    select("StationID","HUC12","date","Q","logQ","logdQ","logd7Q","logd30Q","DecDate","DOY",
           "ann_avg_pcp","ann_avg_srq","bfi", "fr_hydA",
           "fr_hydB","fr_hydC","fr_hydD","Tot_L","wCH_W","wCH_D","wCH_S",
           "wt_slp_sub","sed_Vlow","sed_low","sed_mod","sed_high","sed_Vhigh","K_factor","Developed","Forest","Wetlands","Grassland/Shrub","Managed_Vegetation")
  return(list(Sample = Sample,Daily = Daily))
}
combined_data <- combine_data(all_data)
#F.Clean data###################################################################
combined_data[["Sample"]] <- rm_outliers(combined_data[["Sample"]])
combined_data[["Sample"]] <- combined_data[["Sample"]]%>%   #Remove less than 20 samples
                                  group_by(StationID, Par)%>%
                                  filter(n() >= 20)%>%
                                  ungroup()
combined_data[["Daily"]] <- combined_data[["Daily"]]%>%   #filter flow data based on selected stations
                                  filter(StationID %in% combined_data[["Sample"]]$StationID)
combined_data_splits <- map(1:20, ~split_data(combined_data[["Sample"]])) #Create train - test splits


