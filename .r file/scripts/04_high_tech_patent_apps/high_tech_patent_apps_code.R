# reading in files
proj.path <- getwd()
high_tech <- read.csv(file.path(proj.path, "data", "High tech patent apps per nuts3", "high_tech_patent.csv"))
urban_rural <- read.csv(file.path(proj.path, "outputs", "data", "NUTS_names", "NUTS3_names_urban_rural.csv"))

names(high_tech)[6] <- "NUTS3"
names(high_tech)[8] <- "apps"                     

high_tech <- high_tech %>%
  select(5:8) %>%
  filter(unit == "NR", TIME_PERIOD == 2010) %>%
  select(2, 4) %>%
  mutate(NUTS3 = ifelse(NUTS3 == "DE915", "DE91C", NUTS3), 
         NUTS3 = ifelse(NUTS3 == "DE80F", "DE80N", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DE801", "DE80N", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DE807", "DE80K", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DE80B", "DE80J", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DE80C", "DE80J", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DE80D", "DE80L", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DE80E", "DE80M", NUTS3),
         NUTS3 = ifelse(NUTS3 == "DEB19", "DEB1D", NUTS3)) %>%
  group_by(NUTS3) %>%
  summarise(apps = sum(apps, na.rm = TRUE))

urban_rural <- urban_rural %>% select(1)
high_tech <- left_join(urban_rural, high_tech, by = c("NUTS3"))
  
high_tech <- high_tech %>%
  mutate(apps = ifelse(is.na(apps), 0, apps)) %>%
  mutate(tech = case_when(
    apps < 0.75 ~ "lowest",
    apps >= 0.75 & apps <= 2.49 ~ "low",
    apps > 2.49 & apps <= 6.94 ~ "high",
    apps > 6.94 ~ "highest"
  )) %>%
  arrange(apps)

write.csv(high_tech, file.path(proj.path, "outputs", "data", "NUTS_names","NUTS3_names_tech.csv"), row.names = FALSE)
