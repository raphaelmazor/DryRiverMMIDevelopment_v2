library(tidyverse)
library(lubridate)
library(stringi)

## read in PHAB data - combined for region 4 and 9
PHAB_All_Sites <- #read_csv("FilesFromRB4Report/PHAB_All_Sites.csv")
  # read_csv("Data/NonBioData/Habitat/phab_raw_ceden_202412413252_subset.csv")
  read_csv("NotForGit/1_Data import and assembly/all_hab_data.csv")


# Metric Calculations 
#################################################
##  Calculating Percent wetted habitat (Pct_WetHab)
Pct_WetHab_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Wetted habitat") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0, # we assumed that NR meant no wetted hab present 
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date #Not in most recent CEDEN download 
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_WetHab = sum(VariableResult2, na.rm = TRUE)) %>% # I don't think we needed to sum, seems like there's only 1 measurement per site
  ungroup()

#################################################
## Percent fast-water geomorphic habitats (Pct_FastGHab)
Pct_FastGHab_df <- PHAB_All_Sites %>% 
  filter(Analyte %in% c("Run", "Riffle", "Cascade/Falls")) %>% 
  # filter(Analyte == "Run" | Analyte == "Riffle" | Analyte == "Cascade/Falls") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0, # we assumed that NR meant no wetted hab present 
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date 
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_FastGHab = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#################################################
## Percent fast-water geomorphic habitats (Pct_PoolGHab)
Pct_PoolGHab_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Pool") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_PoolGHab = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup() # I don't think we needed to sum, seems like there's only 1 measurement per site

#################################################
## Percent vegetation instream cover (Pct_VgInstream)
Pct_VgInstream_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Vegetation Cover Instream") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "Full Cover" ~ 5,
                                     VariableResult == "Heavy Cover" ~ 4,
                                     VariableResult == "Medium Cover" ~ 3,
                                     VariableResult == "Light Cover" ~ 2,
                                     VariableResult == "Open" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgInstream = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup() 

#################################################
## Percent woody veg in channel (Pct_VgWdChan)
Pct_VgWdChan_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Channel Woody Veg") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgWdChan = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()  # I don't think we needed to sum, seems like there's only 1 measurement per site


#################################################
## Percent nonwoody veg in channel (Pct_VgNwdChan)
Pct_VgNwdChan_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Channel NonWoody Veg") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgNwdChan = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup() 


#################################################
## Percent grass veg in channel (Pct_VgGrsChan)
Pct_VgGrsChan_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Channel Grasses") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgGrsChan = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()  # I don't think we needed to sum, seems like there's only 1 measurement per site


#################################################
## Percent woody veg in riparian (Pct_VgWdRip)
Pct_VgWdRip_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Riparian Woody Veg") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgWdRip = sum(VariableResult2, na.rm = TRUE))  %>%
  ungroup() # I don't think we needed to sum, seems like there's only 1 measurement per site


#################################################
## Percent nonwoody veg in riparian (Pct_VgNwdRip)
Pct_VgNwdRip_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Riparian NonWoody Veg") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgNwdRip = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup() 


#################################################
## Percent grass veg in riparian (Pct_VgGrsRip)
Pct_VgGrsRip_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Riparian Grasses") %>% 
  # there are only 5 possible % ranges for this variable 
  # so I took the midpoints outside of the script since it only needs to be done once 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0, 
                                     VariableResult == "<5" ~ 2.5,
                                     VariableResult == "5-25" ~ 15,
                                     VariableResult == "25-May" ~ 15,## excel made this a date bc of course it did...
                                     VariableResult == "25-50" ~ 37.5,
                                     VariableResult == "50-75" ~ 62.5,
                                     VariableResult == ">75" ~ 87.5,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_VgGrsRip = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup() 


#################################################
## Total veg in channel (Pct_VgChan)
Pct_VgChan <- Pct_VgWdChan_df %>% 
  left_join(Pct_VgNwdChan_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgGrsChan_df, by = c("StationCode", "SampleDate")) %>% 
  mutate(Pct_VgChan = Pct_VgWdChan + Pct_VgNwdChan + Pct_VgGrsChan) %>%  ## this might need to be edited when we make the actual function/clean code
  select(-c(Pct_VgWdChan, Pct_VgNwdChan, Pct_VgGrsChan))

#################################################
## Total veg in riparian (Pct_VgRip)
Pct_VgRip_df <- Pct_VgWdRip_df %>% 
  left_join(Pct_VgNwdRip_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgGrsRip_df, by = c("StationCode", "SampleDate")) %>% 
  mutate(Pct_VgRip = Pct_VgWdRip + Pct_VgNwdRip + Pct_VgGrsRip) %>%   ## this might need to be edited when we make the actual function/clean code
  select(-c(Pct_VgWdRip, Pct_VgNwdRip, Pct_VgGrsRip))

#################################################
## Total woody veg (Pct_VgWdTotal)
Pct_VgWdTotal_df <- Pct_VgWdChan_df %>% 
  left_join(Pct_VgWdRip_df, by = c("StationCode", "SampleDate")) %>% 
  mutate(Pct_VgWdTotal = Pct_VgWdChan + Pct_VgWdRip) %>%   ## this might need to be edited when we make the actual function/clean code
  select(-c(Pct_VgWdChan, Pct_VgWdRip))

#################################################
## Total nonwoody veg (Pct_VgNonWdTotal)
Pct_VgNonWdTotal_df <- Pct_VgNwdChan_df %>% 
  left_join(Pct_VgNwdRip_df, by = c("StationCode", "SampleDate")) %>% 
  mutate(Pct_VgNonWdTotal = Pct_VgNwdChan + Pct_VgNwdRip) %>%   ## this might need to be edited when we make the actual function/clean code
  select(-c(Pct_VgNwdChan, Pct_VgNwdRip))

#################################################
## Total grass veg (Pct_VgGrsTotal)
Pct_VgGrsTotal_df <- Pct_VgGrsChan_df %>% 
  left_join(Pct_VgGrsRip_df, by = c("StationCode", "SampleDate")) %>% 
  mutate(Pct_VgGrsTotal = Pct_VgGrsChan + Pct_VgGrsRip) %>%   ## this might need to be edited when we make the actual function/clean code
  select(-c(Pct_VgGrsChan, Pct_VgGrsRip))

###########################################################
#################################################
############## Percent sands and fines (PCT_SAFN) ###############
# PHAB_particles_df<-PHAB_All_Sites %>% 
#   filter(Analyte == "Substrate Size Class") %>%
#   mutate(NumericResult=as.numeric(VariableResult),
#          VariableResult2=
#            case_when(!is.na(NumericResult)~NumericResult,
#                      is.na(VariableResult)~NA_real_,
#                      VariableResult %in% c("NOT RECORDED","Not Recorded","Not recorded","not recorded")~NA_real_,
#                      VariableResult=="<2"~1,
#                      VariableResult %in% c("Boulder","BOULDER","boulder")~2125,
#                      VariableResult %in% c("Bedrock","bedrock","BEDROCK")~5660,
#                      T~NA_real_
#                      )
#          )




PCT_SAFN_df <- PHAB_All_Sites %>% 
  select(StationCode, SampleDate, Analyte, VariableResult) %>%
  filter(Analyte == "Substrate Size Class") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "<2" ~ 1, 
                                     VariableResult %in% c("Boulder","boulder") ~ 2125, 
                                     VariableResult  %in%  c("Bedrock","bedrock") ~ 5660, 
                                     is.na(VariableResult) ~ 0,
                                     VariableResult == "Not Recorded" ~ NA_real_,
                                     T ~ as.numeric(VariableResult))) %>% 
  filter(VariableResult2 > 0) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(PCT_SAFN = round(sum(VariableResult2 <= 2, na.rm = TRUE)/sum(VariableResult2 > 0, na.rm = TRUE) * 100, 3)) %>%
  ungroup()


############## Median particle size (SB_PT_D50) ###############
SB_PT_D50_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Substrate Size Class") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "<2" ~ 1, 
                                     VariableResult == "Boulder" ~ 2125, 
                                     VariableResult == "boulder" ~ 2125,
                                     VariableResult == "Bedrock" ~ 5660, 
                                     VariableResult == "bedrock" ~ 5660, 
                                     is.na(VariableResult) ~ 0,
                                     VariableResult == "Not Recorded" ~ NA_real_,
                                     T ~ as.numeric(VariableResult))) %>% 
  filter(VariableResult2 > 0) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(SB_PT_D50 = median(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

############## Percent cobbles and larger (Pct_CbBlBr) ###############
Pct_CbBlBr_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Substrate Size Class") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "<2" ~ 1, 
                                     VariableResult == "Boulder" ~ 2125, 
                                     VariableResult == "boulder" ~ 2125,
                                     VariableResult == "Bedrock" ~ 5660, 
                                     VariableResult == "bedrock" ~ 5660, 
                                     is.na(VariableResult) ~ 0,
                                     VariableResult == "Not Recorded" ~ NA_real_,
                                     T ~ as.numeric(VariableResult))) %>% 
  filter(VariableResult2 > 0) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Pct_CbBlBr = round(sum(VariableResult2 >= 64, na.rm = TRUE)/sum(VariableResult2 > 0, na.rm = TRUE) * 100, 3)) %>%
  ungroup()


#########################################################################################################################
## Calculating the overall human activity metrics:
# Sum of human activity extent scores (HumanActivity_Ext), Sum of human activity intensity scores (HumanActivity_Int), 
# Sum of human activity proximity scores (HumanActivity_Prox), Sum of human activity proximity SWAMP scores (HumanActivityProx_SWAMP)
clean_df <- PHAB_All_Sites %>%
  ## add a year column
  # mutate(SampleDate = as.Date(SampleDate, format = "%m/%d/%Y"), 
  #        Year = lubridate::year(SampleDate)) %>%
  ## make a column that extracts the words I want - can filter on this column later 
  mutate(analyte_code = case_when(
    grepl("Extent", Analyte, fixed = TRUE) | grepl("Proximity", Analyte, fixed =TRUE) | 
      grepl("Intensity", Analyte, fixed = TRUE) ~ stri_extract_last_words(Analyte),
    TRUE ~ "Other")) 
## filter to only 2021 data - since thats all we have bug data for
#filter(Year == 2021) # comment out if need to do all years 

## group by site and sample date
# calculate total extent etc for each 
## when you get to SWAMP proximity, use case when 


# filter for extent    
HumanActivity_Ext_df <- clean_df %>% 
  filter(analyte_code == "Extent") %>% 
  # dropping NR bc NR = 0 and thus does not affect the metric
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2, 
                                     VariableResult == "3" ~ 3,
                                     T ~ NA_real_)) %>%
  group_by(StationCode, SampleDate) %>% 
  summarise(HumanActivity_Ext = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

# filter for intensity  
HumanActivity_Int_df <- clean_df %>% 
  filter(analyte_code == "Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2, 
                                     VariableResult == "3" ~ 3,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(HumanActivity_Int = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

# filter for proximity 
HumanActivity_Prox_df <- clean_df %>% 
  filter(analyte_code == "Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VAResult_OrdRank = case_when(VariableResult == "Not Recorded" ~ 0,
                                      VariableResult == "Not Present" ~ 0,
                                      VariableResult == "In Channel" ~ 5, 
                                      VariableResult == "1-5m" ~ 4,
                                      VariableResult == "5-50m" ~ 3,
                                      VariableResult == "50-100m" ~ 2,
                                      VariableResult == "100-200m" ~ 1,
                                      T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(HumanActivity_Prox = sum(VAResult_OrdRank, na.rm = TRUE)) %>%
  ungroup()


# filter proximity for SWAMP metric
HumanActivity_Prox_SWAMP_df <- clean_df %>% 
  filter(analyte_code == "Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VARes_OrdRank_SWAMP = case_when(VariableResult == "Not Recorded" ~ 0,
                                         VariableResult == "Not Present" ~ 0,
                                         VariableResult == "In Channel" ~ 1.5, 
                                         VariableResult == "1-5m" ~ 1,
                                         VariableResult == "5-50m" ~ 0.667,
                                         VariableResult == "50-100m" ~ 0,
                                         VariableResult == "100-200m" ~ 0,
                                         T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(HumanActivity_Prox_SWAMP = sum(VARes_OrdRank_SWAMP, na.rm = TRUE)) %>%
  ungroup()

#######################################################################################################
################### Animal Burrows ###################
#### Animal burrows extent (AnimalBurrow_Ext) ####
AnimalBurrows_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Animal Burrows Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(AnimalBurrow_Ext = VariableResult2, na.rm = TRUE) %>%
  ungroup()

#### Animal Burrows Intensity (AnimalBurrow_Int) ####
AnimalBurrows_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Animal Burrows Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(AnimalBurrow_Int = VariableResult2, na.rm = TRUE) %>%
  ungroup()

#### Animal Burrows Proximity (AnimalBurrow_Prox) ####
AnimalBurrow_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Animal Burrows Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(AnimalBurrow_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Animal Burrows Proximity (AnimalBurrow_Prox_SWAMP) ####
AnimalBurrow_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Animal Burrows Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(AnimalBurrow_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

################### Algal/Surface Mats/Benthic Algae ###################
#### Algal/Surface Mats/Benthic Algal Growth Extent (AlgalMats_Ext) ####
AlgalMats_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Algal/Surface Mats/Benthic Algal Growth Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(AlgalMats_Ext = VariableResult2, na.rm = TRUE) %>%
  ungroup()

#### Algal/Surface Mats/Benthic Algal Growth Intensity (AlgalMats_Int) ####
AlgalMats_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Algal/Surface Mats/Benthic Algal Growth Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(AlgalMats_Int = VariableResult2, na.rm = TRUE) %>%
  ungroup()

#### Algal/Surface Mats/Benthic Algal Growth Proximity (AlgalMats_Prox) ####
AlgalMats_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Algal/Surface Mats/Benthic Algal Growth Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(AlgalMats_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Algal/Surface Mats/Benthic Algal Growth Proximity SWAMP (AlgalMats_Prox_SWAMP) ####
AlgalMats_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Algal/Surface Mats/Benthic Algal Growth Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(AlgalMats_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

################### Burns ###################
#### Burns Extent (Burns_Ext) ####
Burns_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Burns Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(Burns_Ext = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Burns Intensity (Burns_Int) ####
Burns_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Burns Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(Burns_Int = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Burns Proximity (Burns_Prox) ####
Burns_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Burns Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Burns_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Burns Proximity SWAMP (Burns_Prox_SWAMP) ####
Burns_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Burns Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Burns_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

################### Debris Lines/Silt-Laden Vegetation ###################
#### Debris Lines/Silt-Laden Vegetation Extent (DebrisSilt_Ext) ####
DebrisSilt_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Debris Lines/Silt-Laden Vegetation Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(DebrisSilt_Ext = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Debris Lines/Silt-Laden Vegetation Intensity (DebrisSilt_Int) ####
DebrisSilt_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Debris Lines/Silt-Laden Vegetation Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(DebrisSilt_Int = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Debris Lines/Silt-Laden Vegetation Proximity (DebrisSilt_Prox) ####
DebrisSilt_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Debris Lines/Silt-Laden Vegetation Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(DebrisSilt_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Debris Lines/Silt-Laden Vegetation Proximity SWAMP (DebrisSilt_Prox_SWAMP) ####
DebrisSilt_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Debris Lines/Silt-Laden Vegetation Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(DebrisSilt_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

################### High Concentration of Salts ###################
#### High Concentration of Salts Extent (Salts_Ext) ####
Salts_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "High Concentration of Salts Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(Salts_Ext = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### High Concentration of Salts Intensity (Salts_Int) ####
Salts_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "High Concentration of Salts Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(Salts_Int = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### High Concentration of Salts Proximity (Salts_Prox) ####
Salts_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "High Concentration of Salts Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Salts_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### High Concentration of Salts Proximity SWAMP (DebrisSilt_Prox_SWAMP) ####
Salts_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "High Concentration of Salts Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Salts_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

################### Noxious Chemical Odors ###################
#### Noxious Chemical Odors Extent (NoxiousOdors_Ext) ####
NoxiousOdors_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Noxious Chemical Odors Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(NoxiousOdors_Ext = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Noxious Chemical Odors Intensity (NoxiousOdors_Int) ####
NoxiousOdors_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Noxious Chemical Odors Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(NoxiousOdors_Int = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Noxious Chemical Odors Proximity (NoxiousOdors_Prox) ####
NoxiousOdors_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Noxious Chemical Odors Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(NoxiousOdors_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Noxious Chemical Odors Proximity SWAMP (NoxiousOdors_Prox_SWAMP) ####
NoxiousOdors_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Noxious Chemical Odors Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(NoxiousOdors_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

################### Nutrient Related Water Other ###################
#### Nutrient Related Water Other Extent (NutrientWater_Ext) ####
NutrientWater_Ext_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Nutrient Related Water Other Extent") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(NutrientWater_Ext = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Nutrient Related Water Other Intensity (NutrientWater_Int) ####
NutrientWater_Int_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Nutrient Related Water Other Intensity") %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "1" ~ 1,
                                     VariableResult == "2" ~ 2,
                                     VariableResult == "3" ~ 3, 
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>%  
  summarise(NutrientWater_Int = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Nutrient Related Water Other Proximity (NutrientWater_Prox) ####
NutrientWater_Prox_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Nutrient Related Water Other Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 5, 
                                     VariableResult == "1-5m" ~ 4,
                                     VariableResult == "5-50m" ~ 3,
                                     VariableResult == "50-100m" ~ 2,
                                     VariableResult == "100-200m" ~ 1,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(NutrientWater_Prox = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

#### Nutrient Related Water Other Proximity SWAMP (NutrientWater_Prox_SWAMP) ####
NutrientWater_Prox_SWAMP_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Nutrient Related Water Other Proximity") %>% 
  mutate(VariableResult = trimws(VariableResult)) %>% 
  mutate(VariableResult2 = case_when(VariableResult == "Not Recorded" ~ 0,
                                     VariableResult == "Not Present" ~ 0,
                                     VariableResult == "In Channel" ~ 1.5, 
                                     VariableResult == "1-5m" ~ 1,
                                     VariableResult == "5-50m" ~ 0.667,
                                     VariableResult == "50-100m" ~ 0,
                                     VariableResult == "100-200m" ~ 0,
                                     T ~ NA_real_)) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(NutrientWater_Prox_SWAMP = sum(VariableResult2, na.rm = TRUE)) %>%
  ungroup()

##########################################################
################ Mean bankfull width (XBKF_W) ################


XBKF_W_df <- PHAB_All_Sites %>% 
  filter(Analyte %in% c("Bankfull Width", "StreamWidth")) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(XBKF_W = mean(Result, na.rm = TRUE))%>%
  ungroup()

################ Mean hydraulic height (Mn_HydHght) ################


Mn_HydHght_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Hydraulic Height") %>% 
  group_by(StationCode, SampleDate) %>% 
  # summarise(Mn_HydHght = mean(c(Result, 0, 0), na.rm = TRUE)) %>%
  summarise(Mn_HydHght = mean(Result, na.rm = TRUE)) %>% #This is the mean max depth
  ungroup()

################ Mean cross sectional area (Mn_CrossSect) ################
Mn_CrossSect_df1 <- PHAB_All_Sites %>% 
  filter(Analyte == "Hydraulic Height") %>% 
  separate(LocationCode, c("Transect", "Location_in_trans"), sep = ", ") %>%
  group_by(StationCode, SampleDate, Transect) %>% 
  # summarise(HH_trans_mean = mean(Result)) %>% 
  summarise(HH_trans_mean = mean(c(Result, 0, 0), na.rm = TRUE)) %>% 
  # mutate(HH_trans_mean = round(HH_trans_mean, 4)) %>%  #(why round?)
  ungroup()

Mn_CrossSect_df2 <- PHAB_All_Sites %>% 
  mutate(Analyte = trimws(Analyte)) %>% 
  filter(Analyte %in% c("Bankfull Width", "StreamWidth")) %>% 
  mutate(Analyte2 = if_else(Analyte == "StreamWidth", "Bankfull Width", "Bankfull Width")) %>% 
  select(StationCode, SampleDate, LocationCode, Analyte, Result) %>% 
  rename(Transect = LocationCode)

Mn_CrossSect_df_combined <- Mn_CrossSect_df1 %>% 
  left_join(Mn_CrossSect_df2, by = c("StationCode", "SampleDate", "Transect")) %>% 
  mutate(heightxwidth = HH_trans_mean * Result) %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Mn_CrossSect = mean(heightxwidth, na.rm = TRUE)) %>%
  ungroup()



################ Mean highest hydraulic height across transects (thalweg) (Mn_MaxHydHght) ################

Mn_MaxHydHght_df <-
  PHAB_All_Sites %>% 
  filter(Analyte == "Hydraulic Height") %>% 
  separate(LocationCode, c("Transect", "Location_in_trans"), sep = ", ") %>%
  group_by(StationCode, SampleDate, Transect) %>% 
  summarise(Max = max(Result, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, ~ replace(., is.infinite(.), NA)) %>%
  ungroup() %>% 
  group_by(StationCode, SampleDate) %>% 
  summarise(Mn_MaxHydHght = mean(Max, na.rm = TRUE)) %>%
  ungroup()

PHAB_All_Sites %>%
  select(StationCode, SampleDate) %>%
  distinct() %>%
  left_join(Mn_MaxHydHght_df) %>%
  filter(is.na(Mn_MaxHydHght))
PHAB_All_Sites %>%
  filter(StationCode == "901AUDFOX") %>%
  filter(Analyte=="Hydraulic Height") %>%
  select(LocationCode, Result)


################ Mean Slope (XSLOPE) ################
XSLOPE_df <- PHAB_All_Sites %>% 
  filter(Analyte == "Slope, Main") %>% 
  group_by(StationCode, SampleDate) %>% 
  # we are assuming that there is only one slope measurement per transect (for this data)
  # unlike wadeable streams SOP
  summarise(XSLOPE = mean(Result, na.rm = TRUE))


######## Final DF of everything together 
PHAB_METRICS_CALCULATED <- Pct_WetHab_df %>% 
  left_join(Pct_FastGHab_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_PoolGHab_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgInstream_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgWdChan_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgNwdChan_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgGrsChan_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgWdRip_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgNwdRip_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgGrsRip_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgChan, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgRip_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgWdTotal_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgNonWdTotal_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_VgGrsTotal_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(PCT_SAFN_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(SB_PT_D50_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Pct_CbBlBr_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(HumanActivity_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(HumanActivity_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(HumanActivity_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(HumanActivity_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AnimalBurrows_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AnimalBurrows_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AnimalBurrow_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AnimalBurrow_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AlgalMats_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AlgalMats_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AlgalMats_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(AlgalMats_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Burns_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Burns_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Burns_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Burns_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(DebrisSilt_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(DebrisSilt_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(DebrisSilt_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(DebrisSilt_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Salts_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Salts_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Salts_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Salts_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NoxiousOdors_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NoxiousOdors_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NoxiousOdors_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NoxiousOdors_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NutrientWater_Ext_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NutrientWater_Int_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NutrientWater_Prox_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(NutrientWater_Prox_SWAMP_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(XBKF_W_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Mn_HydHght_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(Mn_CrossSect_df_combined, by = c("StationCode", "SampleDate")) %>% 
  left_join(Mn_MaxHydHght_df, by = c("StationCode", "SampleDate")) %>% 
  left_join(XSLOPE_df, by = c("StationCode", "SampleDate"))

# write to csv
write_csv(PHAB_METRICS_CALCULATED, "Data/NonBioData/Habitat/PHAB_Metrics_04092024.csv") 
# skimr::skim(HumanActivity_Ext_df)  


