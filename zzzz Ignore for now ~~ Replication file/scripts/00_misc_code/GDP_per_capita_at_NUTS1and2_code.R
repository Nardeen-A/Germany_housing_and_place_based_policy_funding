# reading 
proj.path <- getwd()
GDP_per_capita <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_NUTS3.csv"))
GDP_per_capita_Thuringia <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_Thuringia_NUTS3.csv"))
population_NUTS3 <- read.csv(file.path(proj.path, "data", "population_NUTS3", "OECD_population_NUTS3.csv"))

# merging thuringia with the rest
GDP_per_capita <- rbind(GDP_per_capita, GDP_per_capita_Thuringia)

# 2016 population 
population_NUTS3_2016 <- population_NUTS3 %>%
  select(3, 5, 8, 12, 19) %>%
  filter(VAR == "T", Gender == "Total", Year == 2016)

names(population_NUTS3_2016)[5] <- "population"
names(population_NUTS3_2016)[1] <- "NUTS3"

# 2010 population 
population_NUTS3_2010 <- population_NUTS3 %>%
  select(3, 5, 8, 12, 19) %>%
  filter(VAR == "T", Gender == "Total", Year == 2010)

names(population_NUTS3_2010)[5] <- "population"
names(population_NUTS3_2010)[1] <- "NUTS3"

# GDP per Capita NUTS1 level 2016 ####
GDP_per_capita_NUTS1 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2016) %>%
  select(1, 4)

names(GDP_per_capita_NUTS1)[1] <- "NUTS1"
names(GDP_per_capita_NUTS1)[2] <- "GDP_per_capita"

GDP_per_capita_NUTS1$population <- population_NUTS3_2016$population[match(GDP_per_capita_NUTS1$NUTS1, population_NUTS3_2016$NUTS3)]

GDP_per_capita_NUTS1 <- GDP_per_capita_NUTS1 %>%
  mutate(GDP_per_capita = GDP_per_capita * (population / 1000000))

names(GDP_per_capita_NUTS1)[2] <- "GDP"

GDP_per_capita_NUTS1 <- GDP_per_capita_NUTS1 %>%
  mutate(NUTS1 = substr(NUTS1, 1, 3)) %>%
  group_by(NUTS1) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>%
  mutate(GDP_per_cap = GDP / (population / 1000000)) %>%
  arrange(GDP_per_cap)

write.csv(GDP_per_capita_NUTS1, file.path(proj.path, 'outputs', 'data', "GDP_per_capita_at_different_NUTS_levels", "2016_GDP_per_capita_NUTS1.csv"), row.names = FALSE)

# GDP per Capita NUTS2 level 2016 ####
GDP_per_capita_NUTS2 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2016) %>%
  select(1, 4)

names(GDP_per_capita_NUTS2)[1] <- "NUTS2"
names(GDP_per_capita_NUTS2)[2] <- "GDP_per_capita"

GDP_per_capita_NUTS2$population <- population_NUTS3_2016$population[match(GDP_per_capita_NUTS2$NUTS2, population_NUTS3_2016$NUTS3)]

GDP_per_capita_NUTS2 <- GDP_per_capita_NUTS2 %>%
  mutate(GDP_per_capita = GDP_per_capita * (population / 1000000))

names(GDP_per_capita_NUTS2)[2] <- "GDP"

GDP_per_capita_NUTS2 <- GDP_per_capita_NUTS2 %>%
  mutate(NUTS2 = substr(NUTS2, 1, 4)) %>%
  group_by(NUTS2) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>%
  mutate(GDP_per_cap = GDP / (population / 1000000)) %>%
  arrange(GDP_per_cap)

write.csv(GDP_per_capita_NUTS2, file.path(proj.path, 'outputs', 'data', "GDP_per_capita_at_different_NUTS_levels", "2016_GDP_per_capita_NUTS2.csv"), row.names = FALSE)



# GDP per Capita NUTS1 level 2010 ####
GDP_per_capita_NUTS1 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2010) %>%
  select(1, 4)

names(GDP_per_capita_NUTS1)[1] <- "NUTS1"
names(GDP_per_capita_NUTS1)[2] <- "GDP_per_capita"

GDP_per_capita_NUTS1$population <- population_NUTS3_2010$population[match(GDP_per_capita_NUTS1$NUTS1, population_NUTS3_2010$NUTS3)]

GDP_per_capita_NUTS1 <- GDP_per_capita_NUTS1 %>%
  mutate(GDP_per_capita = GDP_per_capita * (population / 1000000))

names(GDP_per_capita_NUTS1)[2] <- "GDP"

GDP_per_capita_NUTS1 <- GDP_per_capita_NUTS1 %>%
  mutate(NUTS1 = substr(NUTS1, 1, 3)) %>%
  group_by(NUTS1) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>%
  mutate(GDP_per_cap = GDP / (population / 1000000)) %>%
  arrange(GDP_per_cap)

write.csv(GDP_per_capita_NUTS1, file.path(proj.path, 'outputs', 'data', "GDP_per_capita_at_different_NUTS_levels", "2010_GDP_per_capita_NUTS1.csv"), row.names = FALSE)

# GDP per Capita NUTS2 level 2016 ####
GDP_per_capita_NUTS2 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2010) %>%
  select(1, 4)

names(GDP_per_capita_NUTS2)[1] <- "NUTS2"
names(GDP_per_capita_NUTS2)[2] <- "GDP_per_capita"

GDP_per_capita_NUTS2$population <- population_NUTS3_2010$population[match(GDP_per_capita_NUTS2$NUTS2, population_NUTS3_2010$NUTS3)]

GDP_per_capita_NUTS2 <- GDP_per_capita_NUTS2 %>%
  mutate(GDP_per_capita = GDP_per_capita * (population / 1000000))

names(GDP_per_capita_NUTS2)[2] <- "GDP"

GDP_per_capita_NUTS2 <- GDP_per_capita_NUTS2 %>%
  mutate(NUTS2 = substr(NUTS2, 1, 4)) %>%
  group_by(NUTS2) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE),
            population = sum(population, na.rm = TRUE)) %>%
  mutate(GDP_per_cap = GDP / (population / 1000000)) %>%
  arrange(GDP_per_cap)

write.csv(GDP_per_capita_NUTS2, file.path(proj.path, 'outputs', 'data', "GDP_per_capita_at_different_NUTS_levels", "2010_GDP_per_capita_NUTS2.csv"), row.names = FALSE)


