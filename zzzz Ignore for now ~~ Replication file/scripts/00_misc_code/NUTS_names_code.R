# reading files
proj.path <- getwd()
nuts_naming <- read.csv(file.path(proj.path, "data", "NUTS_codes.csv"))

# NUTS1 state names ####
NUTS1_nuts_naming <- nuts_naming %>%
  select(1, 3)
NUTS1_nuts_naming <- subset(NUTS1_nuts_naming, X != "")

names(NUTS1_nuts_naming)[1] <- "NUTS1"
names(NUTS1_nuts_naming)[2] <- "State"

NUTS1_nuts_naming$NUTS1 <- as.factor(NUTS1_nuts_naming$NUTS1)

write.csv(NUTS1_nuts_naming, file.path(proj.path, 'outputs', 'data', 'NUTS_names', "NUTS1_names.csv"), row.names = FALSE)

# NUTS2 state names ####
NUTS2_nuts_naming <- nuts_naming %>%
  select(1, 4)
NUTS2_nuts_naming <- subset(NUTS2_nuts_naming, X.1 != "")

names(NUTS2_nuts_naming)[1] <- "NUTS2"
names(NUTS2_nuts_naming)[2] <- "Region"

NUTS2_nuts_naming <- NUTS2_nuts_naming %>% filter(row_number() <= n()-1)

NUTS2_nuts_naming$NUTS2 <- as.factor(NUTS2_nuts_naming$NUTS2)

write.csv(NUTS2_nuts_naming, file.path(proj.path, 'outputs', 'data', 'NUTS_names', "NUTS2_names.csv"), row.names = FALSE)

# NUTS3 state names ####
NUTS3_nuts_naming <- nuts_naming %>%
  select(1, 5)
NUTS3_nuts_naming <- subset(NUTS3_nuts_naming, X.2 != "")

names(NUTS3_nuts_naming)[1] <- "NUTS3"
names(NUTS3_nuts_naming)[2] <- "Region"

NUTS3_nuts_naming <- NUTS3_nuts_naming %>% filter(row_number() <= n()-1)
NUTS3_nuts_naming$NUTS3 <- as.factor(NUTS3_nuts_naming$NUTS3)

write.csv(NUTS3_nuts_naming, file.path(proj.path, 'outputs', 'data', 'NUTS_names', "NUTS3_names.csv"), row.names = FALSE)

