# reading files
proj.path <- getwd()
Germany_erdf <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'ERDF.csv'))
NUTS2 <- read.csv(file.path(proj.path, 'outputs', 'data', 'NUTS_names', 'NUTS2_names.csv'))
GDP_per_capita <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_NUTS3.csv"))
GDP_per_capita_Thuringia <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_Thuringia_NUTS3.csv"))

# year, policy, state, funding
Germany_erdf <- Germany_erdf %>%
  select(2, 13, 9, 5) 

names(Germany_erdf)[1] <- "year"
names(Germany_erdf)[2] <- "state"
names(Germany_erdf)[3] <- "policy"
names(Germany_erdf)[4] <- "funding"

Germany_erdf$year <- substr(Germany_erdf$year, nchar(Germany_erdf$year) - 3, nchar(Germany_erdf$year))
Germany_erdf$state <- substr(Germany_erdf$state, 1, nchar(Germany_erdf$state) - 2)

Germany_erdf <- Germany_erdf %>%
  group_by(year, state, policy) %>%
  summarise(funding = sum(funding, na.rm = TRUE)) %>%
  arrange(year, state, policy) %>%
  ungroup() %>%
  filter(year >= 2016 & year <= 2019,
         policy != "")

# GDP per capita
GDP_per_capita <- rbind(GDP_per_capita, GDP_per_capita_Thuringia)

GDP_per_capita <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2020) %>%
  select(1, 4)

names(GDP_per_capita_NUTS1)[1] <- "NUTS1"
names(GDP_per_capita_NUTS1)[2] <- "GDP_per_capita"