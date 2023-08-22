# reading files in
proj.path <- getwd()
Germany_aaw <- read.csv("E:\\School - work\\RA\\Germany WIP\\latest_DE.csv")

# removing, German operation name, country, postal codes, currency (EUR), benefactors names (they are links), coordinates, category labels,
## thematic objective labels, policy objectives labels, fund names(labels), programme codes, German language summary to hopefully reduce size
Germany_aaw <- Germany_aaw[, -c(1, 3, 4, 5, 10, 12, 13, 15, 17, 19, 21, 22, 28)]

# Data with only ERDF fund projects
Germany_aaw <- subset(Germany_aaw, Fund_Code == "ERDF")
Germany_aaw <- subset(Germany_aaw, NUTS3_Code != "")
Germany_aaw$NUTS3_Code <- ifelse(substr(Germany_aaw$NUTS3_Code, 1, 3) == "DE4", substr(Germany_aaw$NUTS3_Code, 1, 5), Germany_aaw$NUTS3_Code)
Germany_aaw$NUTS3_Code <- ifelse(substr(Germany_aaw$NUTS3_Code, 1, 4) == "DEE0", substr(Germany_aaw$NUTS3_Code, 1, 5), Germany_aaw$NUTS3_Code)
Germany_aaw <- Germany_aaw[nchar(Germany_aaw$NUTS3_Code) == 5, ]

Germany_aaw <- Germany_aaw %>%
  select(2, 8, 13)
Germany_aaw$Operation_Start_Date <- substr(Germany_aaw$Operation_Start_Date, nchar(Germany_aaw$Operation_Start_Date) - 3, nchar(Germany_aaw$Operation_Start_Date))

names(Germany_aaw)[1] <- "year"
names(Germany_aaw)[2] <- "theme"
names(Germany_aaw)[3] <- "region"

Germany_aaw$year <- as.factor(Germany_aaw$year)
Germany_aaw$theme <- as.factor(Germany_aaw$theme)
Germany_aaw$region <- as.factor(Germany_aaw$region)

year_region <- Germany_aaw %>%
  select(1, 3) %>%
  group_by(year, region) %>%
  arrange(year, region) %>%
  summarise(count = n())

Germany_aaw <- Germany_aaw %>%
  group_by(year, region, theme) %>%
  arrange(year, region, theme) %>%
  summarise(count = n())