#Prep data for export to appendix

#List of sites

library(tidyverse)

sites_dates<-read_csv("NotForGit/1_Data import and assembly/sites_dates.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  inner_join(
    read_csv("NotForGit/3_Ref screening/disturbance_screening_df.csv") %>%
      select(masterid, StressLevel)
  )

lu_stations_df_socal<-read_csv( "NotForGit/1_Data import and assembly/lu_stations_df_socal.csv")  %>%
  filter(masterid %in% sites_dates$masterid)


all_bio_data_usable2<-read_csv( "NotForGit/1_Data import and assembly/all_bio_data_usable2.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
all_hab_data<-read_csv( "NotForGit/1_Data import and assembly/all_hab_data.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)

gis_df_socal<-read_csv( "NotForGit/1_Data import and assembly/gis_df_socal.csv") %>%
  filter(processed_by=="automated_tool") %>%
  filter(masterid %in% sites_dates$masterid)


bmi_df_socal<-read_csv( "NotForGit/1_Data import and assembly/bmi_df_socal.csv") %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, stationcode=stationid)) %>%
  # mutate(MID_Date=paste(masterid, sampledate, sep="-")) %>%
  filter(masterid %in% sites_dates$masterid)
algae_df_socal<-read_csv( "NotForGit/1_Data import and assembly/algae_df_socal.csv")  %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, stationcode=stationid)) %>%
  # mutate(MID_Date=paste(masterid, sampledate, sep="-")) %>%
  filter(masterid %in% sites_dates$masterid)



#
sites_dates$Streambed_Arthropods <-sites_dates$MID_Date %in% all_bio_data_usable2$MID_Date[all_bio_data_usable2$CollectionMethodName=="TerrInvert_Trap_DryStreams"]
sites_dates$Vegetation_Arthropods <-sites_dates$MID_Date %in% all_bio_data_usable2$MID_Date[all_bio_data_usable2$CollectionMethodName=="TerrInvert_Veg_DryStreams"]
sites_dates$Bryophytes <-sites_dates$MID_Date %in% all_bio_data_usable2$MID_Date[all_bio_data_usable2$CollectionMethodName=="Bryo_DryStreams"]
sites_dates$PHAB <-sites_dates$MID_Date %in% all_hab_data$MID_Date
sites_dates$BMI <-sites_dates$masterid %in% bmi_df_socal$masterid
sites_dates$Diatoms <-sites_dates$masterid %in% algae_df_socal$masterid[algae_df_socal$sampletypecode=="Integrated"]
sites_dates$SBA <-sites_dates$masterid %in% algae_df_socal$masterid[algae_df_socal$sampletypecode!="Integrated"]

sites_dates_appendix<-sites_dates %>%
  arrange(RB, StationCode, SampleDate) %>%
  select(-masterid, -MID_Date) 


write_csv(sites_dates_appendix, "NotForGit/sites_dates_appendix.csv")

write_clip(sites_dates_appendix)
