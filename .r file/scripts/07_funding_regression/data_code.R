# reading files
proj.path <- getwd()
Germany_erdf <- read.csv(file.path(proj.path, "outputs", "data", "ERDF_data", "ERDF.csv"))
years_per_project <- read.csv(file.path(proj.path, "outputs", "data", "ERDF_data", "years_per_project.csv"))
NUTS2 <- read.csv(file.path(proj.path, "outputs", "data", "NUTS_names", "NUTS2_names.csv"))
GDP_per_capita <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_NUTS3.csv"))
GDP_per_capita_Thuringia <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_Thuringia_NUTS3.csv"))

# year, NUTS3, funding
Germany_erdf <- Germany_erdf %>%
  mutate(X = row_number()) %>%
  select(17, 2, 13, 5)

names(Germany_erdf)[2] <- "year"
names(Germany_erdf)[3] <- "NUTS3"
names(Germany_erdf)[4] <- "funding"

Germany_erdf <- Germany_erdf[nchar(Germany_erdf$year) >= 1, ]
Germany_erdf$year <- substr(Germany_erdf$year, nchar(Germany_erdf$year) - 3, nchar(Germany_erdf$year))
Germany_erdf$year_per_project <- years_per_project$years_between[match(Germany_erdf$X, years_per_project$X)]

Germany_erdf$year <- as.numeric(Germany_erdf$year)

Germany_erdf <- Germany_erdf %>%
  rowwise() %>%
  mutate(
    years = list(year + 0:(year_per_project - 1)),
    allocated_funding = funding / year_per_project
  ) %>%
  unnest(years) %>%
  select(-X, -funding, -year_per_project, -year) %>%
  ungroup()

Germany_erdf_by_year_region_by2years <- Germany_erdf %>%
  filter(
    years >= 2014,
    years <= 2019
  ) %>%
  mutate(year = floor(years / 2) * 2) %>%
  group_by(year, NUTS3) %>%
  summarise(allocated_funding = sum(allocated_funding, na.rm = TRUE)) %>%
  mutate(
    year = as.character(year),
    year = case_when(
      year == "2014" ~ "years_2014_15",
      year == "2016" ~ "years_2016_17",
      year == "2018" ~ "years_2018_19",
      TRUE ~ year
    )
  )

Germany_erdf_by_year_region <- Germany_erdf %>%
  filter(
    years >= 2014,
    years <= 2019
  ) %>%
  group_by(years, NUTS3) %>%
  summarise(allocated_funding = sum(allocated_funding, na.rm = TRUE))
names(Germany_erdf_by_year_region)[1] <- "year"

# logs of funding
Germany_erdf_by_year_region_by2years$allocated_funding <- log(Germany_erdf_by_year_region_by2years$allocated_funding)
Germany_erdf_by_year_region$allocated_funding <- log(Germany_erdf_by_year_region$allocated_funding)

# squaring off missing regions and policies and pivoting wider
join_1 <- expand.grid(
  year = unique(Germany_erdf_by_year_region_by2years$year),
  NUTS3 = unique(Germany_erdf$NUTS3)
)
join_2 <- expand.grid(
  year = unique(Germany_erdf_by_year_region$year),
  NUTS3 = unique(Germany_erdf$NUTS3)
)

Germany_erdf_by_year_region_by2years <- left_join(join_1, Germany_erdf_by_year_region_by2years, by = c("year", "NUTS3"))
Germany_erdf_by_year_region <- left_join(join_2, Germany_erdf_by_year_region, by = c("year", "NUTS3"))

# zeros for missing funding
Germany_erdf_by_year_region_by2years[is.na(Germany_erdf_by_year_region_by2years)] <- 0
Germany_erdf_by_year_region[is.na(Germany_erdf_by_year_region)] <- 0

Germany_erdf_by_year_region_by2years <- Germany_erdf_by_year_region_by2years %>%
  mutate(year = as.factor(year),
         NUTS3 = as.factor(NUTS3)) %>%
  pivot_wider(names_from = year, values_from = c(allocated_funding))

Germany_erdf_by_year_region <- Germany_erdf_by_year_region %>%
  mutate(year = as.factor(year),
         NUTS3 = as.factor(NUTS3)) %>%
  pivot_wider(names_from = year, values_from = c(allocated_funding))

# GDP per capita
GDP_per_capita <- rbind(GDP_per_capita, GDP_per_capita_Thuringia)

GDP_per_capita_2020 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2020) %>%
  select(1, 4)

names(GDP_per_capita_2020)[1] <- "NUTS3"
names(GDP_per_capita_2020)[2] <- "GDP_per_capita"

Germany_erdf_by_year_region_by2years$GDPpercap_2020 <- GDP_per_capita_2020$GDP_per_capita[match(Germany_erdf_by_year_region_by2years$NUTS3, GDP_per_capita_2020$NUTS3)]
Germany_erdf_by_year_region$GDPpercap_2020 <- GDP_per_capita_2020$GDP_per_capita[match(Germany_erdf_by_year_region$NUTS3, GDP_per_capita_2020$NUTS3)]

### 2018
GDP_per_capita_2018 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2018) %>%
  select(1, 4)

names(GDP_per_capita_2018)[1] <- "NUTS3"
names(GDP_per_capita_2018)[2] <- "GDP_per_capita"

Germany_erdf_by_year_region_by2years$GDPpercap_2018 <- GDP_per_capita_2018$GDP_per_capita[match(Germany_erdf_by_year_region_by2years$NUTS3, GDP_per_capita_2018$NUTS3)]
Germany_erdf_by_year_region$GDPpercap_2018 <- GDP_per_capita_2018$GDP_per_capita[match(Germany_erdf_by_year_region$NUTS3, GDP_per_capita_2018$NUTS3)]

Germany_erdf_by_year_region_by2years$GDPpercap_2020 <- log(Germany_erdf_by_year_region_by2years$GDPpercap_2020)
Germany_erdf_by_year_region$GDPpercap_2020 <- log(Germany_erdf_by_year_region$GDPpercap_2020)

Germany_erdf_by_year_region_by2years$GDPpercap_2018 <- log(Germany_erdf_by_year_region_by2years$GDPpercap_2018)
Germany_erdf_by_year_region$GDPpercap_2018 <- log(Germany_erdf_by_year_region$GDPpercap_2018)

# saving files
write.csv(Germany_erdf_by_year_region, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'funding_year_region_18_20_GDP.csv'), row.names = FALSE)


