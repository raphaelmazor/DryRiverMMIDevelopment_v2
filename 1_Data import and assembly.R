library(tidyverse)
library(sf)
library(clipr)


#########Import data#########
library(readxl)
library(clipr)


#Import data from the SMC data portal
library(DBI) # needed to connect to data.dfbase
library(dbplyr) # needed to connect to data.dfbase
library(RPostgreSQL) # needed to connect to our data.dfbase
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(lubridate)

#Make the connection
con <- dbConnect(
  PostgreSQL(),
  host = "geobiology.cottkh4djef2.us-west-2.rds.amazonaws.com",
  dbname = 'smc',
  user='smcread',
  password='1969$Harbor'
)

#Stations lookups
lustations_df<-dbGetQuery(con, sql(
  "SELECT *
FROM
sde.lu_stations"
)) %>% as_tibble()

#BMI taxonomy data
bmi_df<-dbGetQuery(con, sql(
  "SELECT *
FROM
sde.unified_taxonomy"
)) %>% as_tibble()

#Benthic algal taxonomy data
algae_df<-dbGetQuery(con, sql(
  "SELECT *
FROM
sde.unified_algae"
)) %>% as_tibble()

#GIS metrics 
gis_df<-dbGetQuery(con, sql(
  "SELECT *
FROM
sde.gismetrics"
)) %>% as_tibble()

lustations_df %>%
  # filter(masterid=="911GSCAPV")# %>%
  filter(masterid=="911M25305")
  filter(gismetric=="mines")

# setdiff(sites_dates$masterid, gis_df$masterid[gis_df$gismetric=="mines"]) %>%
  # sort()

catchments_df<-dbGetQuery(con, sql(
  "SELECT *
FROM
sde.giscatchments"
)) %>% as_tibble()

setdiff(gis_df$masterid, catchments_df$masterid)
setdiff(catchments_df$masterid, gis_df$masterid)
#Import biological data from a static CEDEN download
meta_arth_df<-  read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Arthropod STE/STE_Arthropods_Feb2024.xlsx",
                          sheet="Sheet1") %>%
  filter(Phylum=="Arthropoda")

meta_bryo_df<-  read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Bryo STE/California Bryophyte STE_v3.xlsx",
            sheet="FULL LIST ADDED")


all_bio_data<-read_csv("Data/BioData/bug_and_bry_data_ceden_01262024.csv") %>%
  filter(CollectionMethodName %in% c("Bryo_DryStreams", "TerrInvert_Trap_DryStreams","TerrInvert_Veg_DryStreams")) %>%
  filter(FinalID %in% c(meta_arth_df$FinalID, meta_bryo_df$FinalID)  ) %>% #Get rid of non-arthropods, non-bryophytes
  mutate(CollectionMethodName = case_when(FinalID=="Eucladium"~"Bryo_DryStreams",
                                          T~CollectionMethodName)) %>%
  left_join(lustations_df %>%
              select(StationCode=stationid, masterid) %>% unique()) %>%
  mutate(RB=str_sub(masterid, 1,1),
         RB = case_when(StationCode=="403S00028"~"4",
                        RB %in% LETTERS~"AZ",
                        T~RB),
         SampleDate=str_sub(SampleDate, 1, nchar(SampleDate)-5) %>% mdy(),
         Year=year(SampleDate))


all_bio_data_usable<-all_bio_data %>%
  filter((CollectionMethodName=="Bryo_DryStreams" & Year>2016) |
           (CollectionMethodName %in% c("TerrInvert_Veg_DryStreams", "TerrInvert_Trap_DryStreams") & Year==2020 & RB =="9")|
           (CollectionMethodName %in% c("TerrInvert_Veg_DryStreams", "TerrInvert_Trap_DryStreams")& Year==2021 & RB  %in% c("4", "9")))

#Drop non RB4, RB8, and RB9 data entirely
all_bio_data_usable2<-all_bio_data_usable %>%
  filter(RB %in% c("4","8","9"))



sites_dates<- all_bio_data_usable2 %>%
  select(StationCode, SampleDate, RB, Latitude, Longitude) %>%
  distinct() %>%
  left_join(lustations_df %>%
              select(StationCode=stationid, masterid))
  # group_by(StationCode,  RB, Latitude, Longitude) %>%
  # tally(name = "Visits")

sites_dates %>% write_clip()


#Import PHAB data from a static CEDEN download
all_hab_data<-read_tsv("Data/NonBioData/Habitat/RB4_CEDEN_PHAB/SWAMP RWB4 Episodic Stream Assessment Tools_Habitat.tsv") %>%
  mutate(rb_number=as.character(rb_number)) %>%
  bind_rows(
    read_tsv("Data/NonBioData/Habitat/RB9_CEDEN_PHAB/SWAMP Dry Stream_Habitat.tsv")
  ) %>%
  mutate(
    SampleDate=mdy_hms(SampleDate)
  ) %>%
  left_join(lustations_df %>%
              select(StationCode=stationid, masterid) %>% unique())  %>%
  mutate(RB=str_sub(masterid, 1,1),
         RB = case_when(StationCode=="403S00028"~"4",
                        RB %in% LETTERS~"AZ",
                        T~RB),
         # SampleDate=str_sub(SampleDate, 1, nchar(SampleDate)-5) %>% mdy(),
         Year=year(SampleDate)) %>%
  filter(StationCode %in% sites_dates$StationCode)

#bug_mets
bug_mets<-read_csv("Data/BioData/Metrics/arthropod_metrics_04242024.csv") %>%
  mutate(SampleDate = SampleDate %>% mdy_hm() %>% as.Date() ) %>%
  inner_join(sites_dates)


bryo_mets<-read_csv("Data/BioData/Metrics/bryo_metrics_04122024.csv") %>%
  mutate(SampleDate = SampleDate %>% mdy_hm() %>% as.Date() ) %>%
  inner_join(sites_dates)

bryo_mets %>%
  # filter(stationcode=="911GSCAPV")# %>%
  filter(masterid=="911M25305")

setdiff(bug_mets$StationCode %>% paste(bug_mets$SampleDate),
        all_bio_data_usable2$StationCode %>% paste(all_bio_data_usable2$SampleDate))

setdiff(bryo_mets$StationCode %>% paste(bryo_mets$SampleDate),
        all_bio_data_usable2$StationCode %>% paste(all_bio_data_usable2$SampleDate))

#Winnow other datasets to the project sites
lu_stations_df_socal<-lustations_df %>%
  filter(stationid %in% sites_dates$StationCode)
bmi_df_socal<-bmi_df %>%  filter(stationcode %in% sites_dates$StationCode)
algae_df_socal<-algae_df %>%  filter(stationcode %in% sites_dates$StationCode)

gis_df_socal<-gis_df %>%  
    # mutate(StationCode = case_when(masterid=="SMC00028"~"403S00028",
    #                              # masterid=="911M25305"~"911GSCAPV",
                                 # T~masterid) ) %>%
  filter(masterid %in% sites_dates$masterid) #This is missing metrics for SMC00028/403S00028




setdiff(gis_df$masterid, sites_dates$masterid)
setdiff(sites_dates$masterid, gis_df$masterid )
"SMC00028" %in%  gis_df$masterid
"403S00028" %in%  gis_df$masterid


setdiff(sites_dates$StationCode, gis_df$masterid )
setdiff(sites_dates$StationCode, gis_df$stationcode)
setdiff(sites_dates$StationCode, gis_df$masterid)




write_csv(all_bio_data_usable2, "NotForGit/1_Data import and assembly/all_bio_data_usable2.csv")
write_csv(all_hab_data, "NotForGit/1_Data import and assembly/all_hab_data.csv")
write_csv(gis_df_socal, "NotForGit/1_Data import and assembly/gis_df_socal.csv")
write_csv(lu_stations_df_socal, "NotForGit/1_Data import and assembly/lu_stations_df_socal.csv")
write_csv(bmi_df_socal, "NotForGit/1_Data import and assembly/bmi_df_socal.csv")
write_csv(algae_df_socal, "NotForGit/1_Data import and assembly/algae_df_socal.csv")
write_csv(sites_dates,"NotForGit/1_Data import and assembly/sites_dates.csv")
write_csv(bug_mets,"NotForGit/1_Data import and assembly/bug_mets.csv")
write_csv(bryo_mets,"NotForGit/1_Data import and assembly/bryo_mets.csv")
