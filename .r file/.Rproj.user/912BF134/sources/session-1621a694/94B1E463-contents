# reading files
proj.path <- getwd()
Germany_erdf <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'ERDF.csv'))
years_per_project <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'years_per_project.csv'))
NUTS2 <- read.csv(file.path(proj.path, 'outputs', 'data', 'NUTS_names', 'NUTS2_names.csv'))
GDP_per_capita <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_NUTS3.csv"))
GDP_per_capita_Thuringia <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_Thuringia_NUTS3.csv"))

# year, policy, NUTS3, funding
Germany_erdf <- Germany_erdf %>%
  mutate(X = row_number()) %>%
  select(17, 2, 13, 9, 5)

names(Germany_erdf)[2] <- "year"
names(Germany_erdf)[3] <- "NUTS3"
names(Germany_erdf)[4] <- "policy"
names(Germany_erdf)[5] <- "funding"

Germany_erdf$year <- substr(Germany_erdf$year, nchar(Germany_erdf$year) - 3, nchar(Germany_erdf$year))
Germany_erdf$year_per_project <- years_per_project$years_between[match(Germany_erdf$X, years_per_project$X)]

Germany_erdf$year <- as.numeric(Germany_erdf$year)

Germany_erdf <- Germany_erdf %>%
  filter(policy != "") %>%
  rowwise() %>%
  mutate(years = list(year + 0:(year_per_project - 1)),
         allocated_funding = funding / year_per_project) %>%
  unnest(years) %>%
  select(-X, -funding, -year_per_project, -year) %>%
  ungroup()

Germany_erdf <- Germany_erdf %>%
  group_by(years, NUTS3, policy) %>%
  summarise(allocated_funding = sum(allocated_funding, na.rm = TRUE)) %>%
  arrange(years, NUTS3) %>%
  ungroup() %>%
  filter(years >= 2014, 
         years <= 2019)

# GDP per capita
GDP_per_capita <- rbind(GDP_per_capita, GDP_per_capita_Thuringia)

GDP_per_capita <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2020) %>%
  select(1, 4)

names(GDP_per_capita_NUTS1)[1] <- "NUTS1"
names(GDP_per_capita_NUTS1)[2] <- "GDP_per_capita"