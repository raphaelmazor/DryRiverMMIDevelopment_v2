library(tidyverse)
library(sf)
library(clipr)

########Import data from previous step########
sites_dates<-read_csv("NotForGit/1_Data import and assembly/sites_dates.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-"))


all_bio_data_usable2<-read_csv( "NotForGit/1_Data import and assembly/all_bio_data_usable2.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
all_hab_data<-read_csv( "NotForGit/1_Data import and assembly/all_hab_data.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
gis_df_socal<-read_csv( "NotForGit/1_Data import and assembly/gis_df_socal.csv") %>%
  filter(processed_by=="automated_tool") %>%
  filter(masterid %in% sites_dates$masterid)
# select(-masterid)
lu_stations_df_socal<-read_csv( "NotForGit/1_Data import and assembly/lu_stations_df_socal.csv")  %>%
  filter(masterid %in% sites_dates$masterid)

bmi_df_socal<-read_csv( "NotForGit/1_Data import and assembly/bmi_df_socal.csv") %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, stationcode=stationid)) %>%
  mutate(MID_Date=paste(masterid, sampledate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
algae_df_socal<-read_csv( "NotForGit/1_Data import and assembly/algae_df_socal.csv")  %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, stationcode=stationid)) %>%
  mutate(MID_Date=paste(masterid, sampledate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)

bug_mets<-read_csv("NotForGit/1_Data import and assembly/bug_mets.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)
bryo_mets<-read_csv("NotForGit/1_Data import and assembly/bryo_mets.csv") %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)

phab_metrics__df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_04092024.csv") %>%
  mutate(StationCode = case_when(StationCode=="403R4SSCT"~"402R4SSCT",
                                 T~StationCode)) %>%
  filter(StationCode %in% sites_dates$StationCode) %>%
  select(-starts_with("na.rm")) %>%
  inner_join(lu_stations_df_socal %>%
               select(masterid, StationCode=stationid)) %>%
  mutate(MID_Date=paste(masterid, SampleDate, sep="-")) %>%
  filter(MID_Date %in% sites_dates$MID_Date)

############
#Screen Ode et al. (2016) ref sites
phab_metrics__df$HumanActivity_Prox_SWAMP
gis_df_socal %>%names(
  
)

ref_screens<-c("ag_1k_16","ag_5k_16", "ag_ws_16",
               "urban_1k_16","urban_5k_16","urban_ws_16",
               "agur_1k_16","agur_5k_16","agur_ws_16",
               "code_21_1k_16","code_21_5k_16","code_21_ws_16",
               "roaddens_1k","roaddens_5k","roaddens_ws",
               "paved_int_1k","paved_int_5k","paved_int_ws",
               "nrst_dam",
               "cnl_pi_pct","mines",
               "Max_HumanActivity_prox")

disturbance_screening_df<-gis_df_socal %>% 
  select(masterid, new_lat, new_long, gismetric, gismetricresult) %>%
  filter(!gismetric %in% c("psa6c","","psa8","xermtn")) %>%
  mutate(gismetricresult=as.numeric(gismetricresult)) %>%
  # filter(is.na(gismetricresult))
  pivot_wider(names_from=gismetric, values_from = gismetricresult) %>%
  left_join(
    phab_metrics__df %>%
      group_by(masterid) %>%
      summarise(Max_HumanActivity_prox = max(HumanActivity_Prox_SWAMP, na.rm=T)) %>%
      ungroup()
  ) %>%
  select(masterid, all_of(ref_screens)) %>%
  mutate(developed_1k_16 = ag_1k_16 + urban_1k_16 + code_21_1k_16,
         developed_5k_16 = ag_5k_16 + urban_5k_16 + code_21_5k_16,
         developed_ws_16 = ag_ws_16 + urban_ws_16 + code_21_ws_16,
         nrst_dam = case_when(nrst_dam<0~Inf,T~nrst_dam),
         Max_HumanActivity_prox = case_when(is.na(Max_HumanActivity_prox)~-Inf,
                                            T~Max_HumanActivity_prox))

disturbance_screening_df %>%
  filter(is.na(mines))
gis_df %>%
  filter(masterid %in% c("402R4SSCT","801SANT1x")) %>%
  # filter(gismetric=="mines")
  select(gismetric) %>% distinct() %>% arrange(gismetric) %>% print(n=91) 

disturbance_screening_df %>%
  mutate(ag_1k_PASS = ag_1k_16<3, ag_5k_PASS = ag_5k_16<3, ag_ws_PASS = ag_ws_16<3, 
         urban_1k_PASS = urban_1k_16<3, urban_5k_PASS = urban_5k_16<3, urban_ws_PASS = urban_ws_16<3, 
         agur_1k_PASS = agur_1k_16<5, agur_5k_PASS = agur_5k_16<5, agur_ws_PASS = agur_ws_16<5,
         code_21_1k_PASS = code_21_1k_16<7, code_21_5k_PASS = code_21_5k_16<7, code_21_ws_PASS = code_21_ws_16<10,
         roaddens_1k_PASS = roaddens_1k<2, roaddens_5k_PASS = roaddens_5k<2, roaddens_ws_PASS = roaddens_ws<2,
         paved_int_1k_PASS = paved_int_1k<5, paved_int_5k_PASS = paved_int_5k<10, paved_int_ws_PASS = paved_int_ws<50,
         nrst_dam_PASS = nrst_dam>10,
         cnl_pi_pct_PASS= cnl_pi_pct<10,
         mines_PASS = mines == 0,
         Max_HumanActivity_prox_PASS=Max_HumanActivity_prox<1.5  ) %>%
  
  
  select(masterid, ends_with("PASS")) %>%
  pivot_longer(cols = ends_with("PASS")) %>%
  # This summarizes the SITE
  # {group_by(masterid) %>%
  # mutate(TotalPass = sum(value),
  #        TotalScreen=length(name)) %>%
  # ungroup() %>%
  # pivot_wider(names_from=name, values_from = value)}
  
  #This summarizes the METRIC
  group_by(name) %>%
  summarise(TotalPass = sum(value),
            TotalScreen=length(name)) %>%
  ungroup() %>%
  mutate(PctPass=TotalPass/TotalScreen) %>%
  arrange(-TotalPass)# %>%write_clip()


disturbance_screening_df<-disturbance_screening_df %>%
  mutate(RefStatus_GIS = 
           case_when(ag_1k_16<3 & ag_5k_16<3 & ag_ws_16<3 &
                       urban_1k_16<3 & urban_5k_16<3 & urban_ws_16<3 &
                       agur_1k_16<5 & agur_5k_16<5 & agur_ws_16<5 &
                       code_21_1k_16<7 & code_21_5k_16<7 & code_21_ws_16<10 &
                       roaddens_1k<2 & roaddens_5k<2 & roaddens_ws<2 &
                       paved_int_1k<5 & paved_int_5k<10 & paved_int_ws<50 &
                       nrst_dam>10 &
                       cnl_pi_pct<10 &
                       mines==0~ "Reference",
                     T~"Nonreference"),
         RefStatus = case_when(RefStatus_GIS=="Nonreference"~"Nonreference",
                               RefStatus_GIS=="Reference" & Max_HumanActivity_prox>=1.5~"Nonreference",
                               RefStatus_GIS=="Reference" & Max_HumanActivity_prox<1.5~"Reference",
                               T~"OTHER"),
         StressLevel = case_when(RefStatus=="Reference"~"Low",
                                 developed_1k_16>=50 | developed_5k_16>=50 | developed_ws_16>=50 |
                                   roaddens_1k>=5 | roaddens_5k>=5 | roaddens_ws>=5 |
                                   Max_HumanActivity_prox >=5 ~"High",
                                 T~"Intermediate"
         )
  ) #%>%
  # filter(RefStatus=="OTHER") 
  # write_clip()
  # %>% skimr::skim()
  # select(Max_HumanActivity_prox)
disturbance_screening_df %>%
  group_by(RefStatus_GIS, RefStatus) %>% 
  tally()


write_csv(disturbance_screening_df, "NotForGit/3_Ref screening/disturbance_screening_df.csv")
