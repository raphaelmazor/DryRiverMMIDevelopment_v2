phab_mets_SWAMP<-dbGetQuery(con, sql(
  "SELECT *
FROM
sde.analysis_phabmetrics"
)) %>% as_tibble()



phab_mets_SWAMP<- phab_mets_SWAMP %>%
  inner_join(lustations_df %>%
               select(stationcode=stationid, masterid))
phab_mets_SWAMP$variable %>% unique() %>% sort()

newdf<-
  phab_metrics__df %>%
  select(masterid, starts_with("Human") ) %>%
  pivot_longer(cols=starts_with("Human"), names_to = "Metric", values_to = "MetricResult") %>%
  mutate(MetricResult=as.numeric(MetricResult)) %>%
  bind_rows(
    phab_mets_SWAMP %>%
      filter(variable== "W1_HALL_SWAMP") %>%
      filter(masterid %in% phab_metrics__df$masterid) %>%
      select(masterid, Metric=variable, MetricResult=result) %>%
      mutate(MetricResult=as.numeric(MetricResult)) 
  )


newdf<-
  phab_metrics__df %>%
  select(masterid, starts_with("Human") ) %>%
  pivot_longer(cols=starts_with("Human"), names_to = "DryMetric", values_to = "DryMetricResult") %>%
  mutate(DryMetricResult=as.numeric(DryMetricResult)) %>%
  group_by(masterid, DryMetric) %>%
  summarize(DryMetricResult=mean(DryMetricResult)) %>%
  ungroup() %>%
  inner_join(
    phab_mets_SWAMP %>%
      filter(variable== "W1_HALL_SWAMP") %>%
      filter(masterid %in% phab_metrics__df$masterid) %>%
      select(masterid, W1_HALL_SWAMP=result) %>%
      mutate(W1_HALL_SWAMP=as.numeric(W1_HALL_SWAMP)) %>%
      na.omit() %>%
      group_by(masterid) %>%
      summarize(W1_HALL_SWAMP=mean(W1_HALL_SWAMP, na.rm=T))
  )

ggplot(data=newdf, aes(x=W1_HALL_SWAMP, y=DryMetricResult))+
  geom_point(position=position_jitter(width=.05, height=0.5), alpha=0.5)+
  facet_wrap(~DryMetric, scales="free_y")+
  geom_smooth(method=lm)
