proj.path <- getwd()
Germany_aaw <- read.csv("E:\\School - work\\RA\\Germany WIP\\latest_DE.csv")

Germany_aaw <- Germany_aaw[, -c(1, 3, 4, 5, 10, 12, 13, 15, 17, 19, 21, 22, 28)]
# ESF and ERDF, removing all rows with no region code
Germany_aaw <- subset(Germany_aaw, Fund_Code == "ERDF")
Germany_aaw <- subset(Germany_aaw, NUTS3_Code != "")
Germany_aaw$NUTS3_Code <- ifelse(substr(Germany_aaw$NUTS3_Code, 1, 3) == "DE4", substr(Germany_aaw$NUTS3_Code, 1, 5), Germany_aaw$NUTS3_Code)
Germany_aaw$NUTS3_Code <- ifelse(substr(Germany_aaw$NUTS3_Code, 1, 4) == "DEE0", substr(Germany_aaw$NUTS3_Code, 1, 5), Germany_aaw$NUTS3_Code)
Germany_aaw <- Germany_aaw[nchar(Germany_aaw$NUTS3_Code) == 5, ]
Germany_aaw <- subset(Germany_aaw, grepl("^DE", NUTS3_Code))

Germany_erdf <- subset(Germany_aaw, Fund_Code == "ERDF")

Germany_erdf <- Germany_erdf %>%
  filter(!grepl("^DEF", NUTS3_Code)) 

write.csv(Germany_erdf, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'ERDF.csv'), row.names = FALSE)
