# reading in files
proj.path <- getwd()
NUTS3_names <- read.csv(file.path(proj.path, 'outputs', 'data', 'NUTS_names', 'NUTS3_names.csv'))

NUTS3_names$adminstration <- ifelse(grepl(", Stadtkreis$", NUTS3_names$Region) | grepl(", Kreisfreie Stadt$", NUTS3_names$Region), "urban", "rural")
NUTS3_names$adminstration[NUTS3_names$Region == "Berlin"] <- "urban"
NUTS3_names$adminstration[NUTS3_names$Region == "Hamburg"] <- "urban"

NUTS3_names$Region[NUTS3_names$NUTS3 == "DEC01"] <- "Regionalverband Saarbrücken"
NUTS3_names$adminstration[NUTS3_names$NUTS3 == "DEC01"] <- "rural"

NUTS3_names$Region[NUTS3_names$NUTS3 == "DEC02"] <- "Merzig-Wadern"
NUTS3_names$adminstration[NUTS3_names$NUTS3 == "DEC02"] <- "rural"

NUTS3_names$Region[NUTS3_names$NUTS3 == "DEC03"] <- "Neunkirchen"
NUTS3_names$adminstration[NUTS3_names$NUTS3 == "DEC03"] <- "rural"

NUTS3_names$Region[NUTS3_names$NUTS3 == "DEC04"] <- "Saarlouis"
NUTS3_names$adminstration[NUTS3_names$NUTS3 == "DEC04"] <- "rural"

NUTS3_names$Region[NUTS3_names$NUTS3 == "DEC05"] <- "Saarpfalz-Kreis"
NUTS3_names$adminstration[NUTS3_names$NUTS3 == "DEC05"] <- "rural"

NUTS3_names$Region[NUTS3_names$NUTS3 == "DEC06"] <- "St. Wendel"
NUTS3_names$adminstration[NUTS3_names$NUTS3 == "DEC06"] <- "rural"

write.csv(NUTS3_names, file.path(proj.path, 'outputs', 'data', 'NUTS_names', 'NUTS3_names_urban_rural.csv'), row.names = FALSE)



