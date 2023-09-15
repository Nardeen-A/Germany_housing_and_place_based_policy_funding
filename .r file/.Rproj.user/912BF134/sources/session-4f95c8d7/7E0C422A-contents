# Libraries #####
libs <- c(
    "tidyverse", "sf", "classInt", 
    "cartogram", "rayshader",
    "giscoR", "eurostat"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries 
invisible(lapply(libs, library, character.only = T))

# Reading files ######
proj.path <- getwd()
Germany_aaw <- read.csv("E:\\School - work\\RA\\Germany WIP\\latest_DE.csv")
GDP_per_capita <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_NUTS3.csv"))
GDP_per_capita_Thuringia <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_Thuringia_NUTS3.csv"))
OECD_GDP_per_state <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_state_NUTS1.csv"))

population_NUTS3 <- read.csv(file.path(proj.path, "data", "population_NUTS3", "OECD_population_NUTS3.csv"))

funding_per_gdp_erdf <- read.csv(file.path(proj.path, 'data', 'funding~GDP', 'No_interreg', 'Germany_erdf_funding_GDP_per_capita_region.csv'))
funding_per_gdp_esf <- read.csv(file.path(proj.path, 'data', 'funding~GDP', 'No_interreg','Germany_esf_funding_GDP_per_capita_region.csv'))

# production_manuf_shares <- read.csv(file.path(proj.path, "outputs", "data", "IOT_Data","NUTS2_production_manuf_shares_for_map.csv"))
# labour_manuf_shares <- read.csv(file.path(proj.path, "outputs", "data", "IOT_Data","NUTS2_labour_manuf_shares_for_map.csv"))
# NUTS3_manufacturing_shares <- read.csv(file.path(proj.path, "outputs", "data", "IOT_Data","NUTS3_manufacturing_shares.csv"))

# CONSTANTS #####
# define projections
# longlat
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# Lambert
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# ggplot2 theme #######
theme_for_the_win <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Montserrat"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, 0.99),
      legend.text = element_text(size = 10, color = "grey20"),
      legend.title = element_text(size = 10, color = "grey20"),
      legend.spacing.y = unit(0.25, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(
        c(t = 0, r = 0, b = 0, l = 0), "lines"
      ),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = NA, color = NA), 
      panel.border = element_blank(),
    )
}

# colors ############
cols <- rev(c(
  "#140e26", "#451a40",
  "#7d1d53", "#b32957", "#ccaead",
  "#eb804e", "#ffdc58"
))

# get NUTS3 shape file ##################
deu_nuts3 <- giscoR::gisco_get_nuts(
  year = "2021",
  epsg = "4326",
  resolution = "3",
  nuts_level = "3",
  country = "DE",
  cache = TRUE,
  update_cache = TRUE
)

# variables for the map ############## 
# removing, German operation name, country, postal codes, currency (EUR), benefactors names (they are links), coordinates, category labels,
## thematic objective labels, policy objectives labels, fund names(labels), programme codes, German language summary to hopefully reduce size
Germany_aaw <- Germany_aaw[, -c(1, 3, 4, 5, 10, 12, 13, 15, 17, 19, 21, 22, 28)]

# Data with only ESF and ERDF fund projects
## The number of non ESF AND ERDF data entries are very small in the first place
Germany_aaw <- subset(Germany_aaw, Fund_Code == "ESF" | Fund_Code == "ERDF")
Germany_aaw <- subset(Germany_aaw, NUTS3_Code != "")
Germany_aaw$NUTS3_Code <- ifelse(substr(Germany_aaw$NUTS3_Code, 1, 3) == "DE4", substr(Germany_aaw$NUTS3_Code, 1, 5), Germany_aaw$NUTS3_Code)
Germany_aaw$NUTS3_Code <- ifelse(substr(Germany_aaw$NUTS3_Code, 1, 4) == "DEE0", substr(Germany_aaw$NUTS3_Code, 1, 5), Germany_aaw$NUTS3_Code)
Germany_aaw <- Germany_aaw[nchar(Germany_aaw$NUTS3_Code) == 5, ]

# splitting the data into two data frames; one for ESF and the other for ERDF
Germany_esf <- subset(Germany_aaw, Fund_Code == "ESF")
Germany_erdf <- subset(Germany_aaw, Fund_Code == "ERDF")

Germany_erdf <- Germany_erdf %>%
  group_by(NUTS3_Code) %>%
  mutate(count = n()) %>%
  select(13, 17)

Germany_erdf <- subset(Germany_erdf, grepl("^DE", NUTS3_Code))
Germany_erdf <- Germany_erdf[!duplicated(Germany_erdf), ]
names(Germany_erdf)[1] <- "NUTS_ID"
  
df <- deu_nuts3 |>
  left_join(Germany_erdf, by = "NUTS_ID")

Germany_esf <- Germany_esf %>%
  group_by(NUTS3_Code) %>%
  mutate(count2 = n()) %>%
  select(13, 17)

Germany_esf <- subset(Germany_esf, grepl("^DE", NUTS3_Code))
Germany_esf <- Germany_esf[!duplicated(Germany_esf), ]
names(Germany_esf)[1] <- "NUTS_ID"

df2 <- deu_nuts3 |>
  left_join(Germany_esf, by = "NUTS_ID")
  
# plotting erdf main map ##############

vmin <- min(df$count, na.rm = T)
vmax <- max(df$count, na.rm = T)

# bins
brk <- round(classIntervals(df$count,
                            n = 6,
                            style = "equal"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)
  
make_polygon_map_ERDF <- function() {
  p1 <-
    ggplot(df) +
    geom_sf(aes(fill = count),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "F", alpha = 0.85)(7),
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p1)
}

map_ERDF <- make_polygon_map_ERDF()

# plotting esf main map ##############

vmin <- min(df2$count2, na.rm = T)
vmax <- max(df2$count2, na.rm = T)

# bins
brk <- round(classIntervals(df2$count2,
                            n = 6,
                            style = "kmeans"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_ESF <- function() {
  p2 <-
    ggplot(df2) +
    geom_sf(aes(fill = count2),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "F", alpha = 0.85)(7),
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax),
      trans = "sqrt"
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p2)
}

map_ESF <- make_polygon_map_ESF()


# dot density ######
df$count[is.na(df$count)] <- 0


get_dot_density <- function() {
  num_dots <- ceiling(dplyr::select(as.data.frame(df), count))
  deu_dots <- map_df(
    names(num_dots),
    ~ sf::st_sample(df, size = num_dots[, .x], type = "random") |>
      sf::st_cast("POINT") |>
      sf::st_coordinates() |>
      as_tibble() |>
      setNames(c("long", "lat"))
  )
  return(deu_dots)
}

deu_dots <- get_dot_density()

make_dot_density_map_ERDF <- function() {
  p3 <-
    ggplot(deu_dots) +
    geom_sf(
      data = deu_nuts3, fill = "transparent",
      color = "grey20", size = .1
    ) +
    geom_point(
      data = deu_dots, aes(x = long, y = lat),
      color = cols[5], size = .1, shape = 19, alpha = .2
    ) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p3)
}

map2_ERDF <- make_dot_density_map_ERDF()


df2$count2[is.na(df2$count2)] <- 0


get_dot_density <- function() {
  num_dots <- ceiling(dplyr::select(as.data.frame(df2), count2))
  deu_dots <- map_df(
    names(num_dots),
    ~ sf::st_sample(df, size = num_dots[, .x], type = "random") |>
      sf::st_cast("POINT") |>
      sf::st_coordinates() |>
      as_tibble() |>
      setNames(c("long", "lat"))
  )
  return(deu_dots)
}

deu_dots <- get_dot_density()

make_dot_density_map_ESF <- function() {
  p4 <-
    ggplot(deu_dots) +
    geom_sf(
      data = deu_nuts3, fill = "transparent",
      color = "grey20", size = .1
    ) +
    geom_point(
      data = deu_dots, aes(x = long, y = lat),
      color = cols[5], size = .1, shape = 19, alpha = .2
    ) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p4)
}

map2_ESF <- make_dot_density_map_ESF()



# project per capita (per 1000) ############
population_NUTS3 <- population_NUTS3 %>%
  select(3, 5, 8, 12, 19) %>%
  filter(VAR == "T", Gender == "Total", Year == 2016)

names(population_NUTS3)[5] <- "population"
names(population_NUTS3)[1] <- "NUTS3"

erdf_pojects_per_capita <- Germany_erdf 
erdf_pojects_per_capita$population <- population_NUTS3$population[match(erdf_pojects_per_capita$NUTS_ID, population_NUTS3$NUTS3)]
erdf_pojects_per_capita <- erdf_pojects_per_capita %>%
  mutate(projects_per_capita = count / (population / 1000)) %>%
  select(1, 4)

esf_pojects_per_capita <- Germany_esf
esf_pojects_per_capita$population <- population_NUTS3$population[match(esf_pojects_per_capita$NUTS_ID, population_NUTS3$NUTS3)]
esf_pojects_per_capita <- esf_pojects_per_capita %>%
  mutate(projects_per_capita = count2 / (population / 1000)) %>%
  select(1, 4)

df3 <- deu_nuts3 |>
  left_join(erdf_pojects_per_capita, by = "NUTS_ID")

df4 <- deu_nuts3 |>
  left_join(esf_pojects_per_capita, by = "NUTS_ID")

# plotting erdf projects per capita ##############

vmin <- min(df3$projects_per_capita, na.rm = T)
vmax <- max(df3$projects_per_capita, na.rm = T)

# bins
brk <- round(classIntervals(df3$projects_per_capita,
                            n = 6,
                            style = "equal"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_ERDF_proj_per_cap <- function() {
  p5 <-
    ggplot(df3) +
    geom_sf(aes(fill = projects_per_capita),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 0.85)(7),
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p5)
}

erdf_proj_per_cap_plot <- make_polygon_map_ERDF_proj_per_cap()

# plotting esf projects per capita ##############

vmin <- min(df4$projects_per_capita, na.rm = T)
vmax <- max(df4$projects_per_capita, na.rm = T)

# bins
brk <- round(classIntervals(df4$projects_per_capita,
                            n = 6,
                            style = "kmeans"
)$brks, 0) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)


make_polygon_map_ESF_proj_per_cap <- function() {
  p6 <-
    ggplot(df4) +
    geom_sf(aes(fill = projects_per_capita),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 0.85)(7),
      breaks = breaks,
      labels = round(breaks, 0),
      limits = c(vmin, vmax), 
      trans = "sqrt"
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p6)
}

esf_proj_per_cap_plot <- make_polygon_map_ESF_proj_per_cap()




# funding per capita #########

funding_per_gdp_erdf <- funding_per_gdp_erdf %>%
  mutate(funding_per_gdp = funding_per_capita_germany_erdf / GDP_per_capita) %>%
  select(NUTS3, funding_per_gdp)
names(funding_per_gdp_erdf)[1] <- "NUTS_ID"

funding_per_gdp_esf <- funding_per_gdp_esf %>%
  mutate(funding_per_gdp = funding_per_capita_germany_esf / GDP_per_capita) %>%
  select(NUTS3, funding_per_gdp)
names(funding_per_gdp_esf)[1] <- "NUTS_ID"

df5 <- deu_nuts3 |>
  left_join(funding_per_gdp_erdf, by = "NUTS_ID")

df6 <- deu_nuts3 |>
  left_join(funding_per_gdp_esf, by = "NUTS_ID")

# plotting erdf funding per GDP #############
vmin <- min(df5$funding_per_gdp, na.rm = T)
vmax <- max(df5$funding_per_gdp, na.rm = T)

# bins
brk <- classIntervals(df5$funding_per_gdp,
                            n = 6,
                            style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_ERDF_funding_per_gdp <- function() {
  p6 <-
    ggplot(df5) +
    geom_sf(aes(fill = funding_per_gdp),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "F", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%.3f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p6)
}

erdf_funding_per_gdp_plot <- make_polygon_map_ERDF_funding_per_gdp()


# plotting esf funding per GDP #############

vmin <- min(df6$funding_per_gdp, na.rm = T)
vmax <- max(df6$funding_per_gdp, na.rm = T)

# bins
brk <- classIntervals(df6$funding_per_gdp,
                      n = 6,
                      style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_ESF_funding_per_gdp <- function() {
  p6 <-
    ggplot(df6) +
    geom_sf(aes(fill = funding_per_gdp),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "F", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%.3f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p6)
}

esf_funding_per_gdp_plot <- make_polygon_map_ESF_funding_per_gdp()



# manufacturing shares #########

names(production_manuf_shares)[1] <- "NUTS_ID"
production_manuf_shares$shares <- production_manuf_shares$shares * 100
names(labour_manuf_shares)[1] <- "NUTS_ID"
labour_manuf_shares$shares <- labour_manuf_shares$shares * 100


df7 <- deu_nuts3 |>
  left_join(production_manuf_shares, by = "NUTS_ID")

df8 <- deu_nuts3 |>
  left_join(labour_manuf_shares, by = "NUTS_ID")

# plotting production_manuf_shares #############
vmin <- min(df7$shares, na.rm = T)
vmax <- max(df7$shares, na.rm = T)

# bins
brk <- classIntervals(df7$shares,
                      n = 6,
                      style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_production_manuf_shares <- function() {
  p7 <-
    ggplot(df7) +
    geom_sf(aes(fill = shares),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%.3f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p7)
}

production_manuf_shares_plot <- make_polygon_map_production_manuf_shares()

# plotting labour_manuf_shares #############
vmin <- min(df8$shares, na.rm = T)
vmax <- max(df8$shares, na.rm = T)

# bins
brk <- classIntervals(df8$shares,
                      n = 6,
                      style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_labour_manuf_shares <- function() {
  p7 <-
    ggplot(df8) +
    geom_sf(aes(fill = shares),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%.3f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p7)
}

labour_manuf_shares_plot <- make_polygon_map_labour_manuf_shares()

# GDP per Capita ####
## adding Thuringia GDP data
GDP_per_capita <- rbind(GDP_per_capita, GDP_per_capita_Thuringia)

GDP_per_capita_2016 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2016) %>%
  select(1, 4)

GDP_per_capita_2010 <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2010) %>%
  select(1, 4)

names(GDP_per_capita_2016)[1] <- "NUTS_ID"
names(GDP_per_capita_2016)[2] <- "GDP_per_capita"
names(GDP_per_capita_2010)[1] <- "NUTS_ID"
names(GDP_per_capita_2010)[2] <- "GDP_per_capita"

GDP_per_capita_2016$GDP_per_capita <- log(GDP_per_capita_2016$GDP_per_capita)
GDP_per_capita_2010$GDP_per_capita <- log(GDP_per_capita_2010$GDP_per_capita)

df9 <- deu_nuts3 |>
  left_join(GDP_per_capita_2016, by = "NUTS_ID")

df10 <- deu_nuts3 |>
  left_join(GDP_per_capita_2010, by = "NUTS_ID")
# plotting 2016 GDP per Capita ####
vmin <- min(df10$GDP_per_capita, na.rm = T)
vmax <- max(df10$GDP_per_capita, na.rm = T)

# bins
brk <- classIntervals(df10$GDP_per_capita,
                      n = 6,
                      style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_2016_GDP <- function() {
  p8 <-
    ggplot(df9) +
    geom_sf(aes(fill = GDP_per_capita),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "B", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%0.1f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p8)
}

GDP_per_cap_2016_plot <- make_polygon_map_2016_GDP()

# plotting 2010 GDP per Capita ####
vmin <- min(df10$GDP_per_capita, na.rm = T)
vmax <- max(df10$GDP_per_capita, na.rm = T)

# bins
brk <- classIntervals(df10$GDP_per_capita,
                      n = 6,
                      style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_2010_GDP <- function() {
  p8 <-
    ggplot(df10) +
    geom_sf(aes(fill = GDP_per_capita),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "B", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%.1f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p8)
}

GDP_per_cap_2010_plot <- make_polygon_map_2010_GDP()


# NUTS3 manufacturing shares ####
names(NUTS3_manufacturing_shares)[1] <- "NUTS_ID"
NUTS3_manufacturing_shares$manufacturing_shares <- NUTS3_manufacturing_shares$manufacturing_shares * 100

NUTS3_manufacturing_shares <- NUTS3_manufacturing_shares %>%
  select(1, 4)

df11 <- deu_nuts3 |>
  left_join(NUTS3_manufacturing_shares, by = "NUTS_ID")

# plotting NUTS3 manufacturing shares #############
vmin <- min(df11$manufacturing_shares, na.rm = T)
vmax <- max(df11$manufacturing_shares, na.rm = T)

# bins
brk <- classIntervals(df11$manufacturing_shares,
                      n = 6,
                      style = "equal"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_NUTS3_manufacturing_shares <- function() {
  p9 <-
    ggplot(df11) +
    geom_sf(aes(fill = manufacturing_shares),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 1)(7),
      breaks = breaks,
      labels = sprintf("%.3f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p9)
}

NUTS3_manufacturing_shares <- make_polygon_map_NUTS3_manufacturing_shares()

# population ####
population_2016 <- population_NUTS3 %>%
  select(3, 5, 8, 12, 19) %>%
  filter(VAR == "T", Gender == "Total", Year == 2016) %>%
  select(1,5)

population_2010 <- population_NUTS3 %>%
  select(3, 5, 8, 12, 19) %>%
  filter(VAR == "T", Gender == "Total", Year == 2010)%>%
  select(1,5)

names(population_2016)[1] <- "NUTS_ID"
names(population_2010)[1] <- "NUTS_ID"
names(population_2016)[2] <- "Population"
names(population_2010)[2] <- "Population"

df12 <- deu_nuts3 |>
  left_join(population_2016, by = "NUTS_ID")

df13 <- deu_nuts3 |>
  left_join(population_2010, by = "NUTS_ID")
# plotting population ####
# 2016
vmin <- min(df12$Population, na.rm = T)
vmax <- max(df12$Population, na.rm = T)
# bins
brk <- classIntervals(df12$Population,
                      n = 6,
                      style = "pretty"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
# breaks
breaks <- c(vmin, brk)

make_polygon_map_population_2016 <- function() {
  p10 <-
    ggplot(df12) +
    geom_sf(aes(fill = Population),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%1.0f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p10)
}

population_plot_2016 <- make_polygon_map_population_2016()
# 2010
vmin <- min(df13$Population, na.rm = T)
vmax <- max(df13$Population, na.rm = T)

# bins
brk <- classIntervals(df13$Population,
                      n = 6,
                      style = "pretty"
)$brks |>
  head(-1) |>
  tail(-1) |>
  append(vmax)


# breaks
breaks <- c(vmin, brk)

make_polygon_map_population_2010 <- function() {
  p10 <-
    ggplot(df13) +
    geom_sf(aes(fill = Population),
            color = "grey20",
            size = .1
    ) +
    scale_fill_gradientn(
      name = "",
      colours = viridis_pal(option = "G", alpha = 0.85)(7),
      breaks = breaks,
      labels = sprintf("%1.0f", breaks),
      limits = c(vmin, vmax)
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_for_the_win() +
    labs(
      y = "",
      subtitle = "",
      x = "",
      title = "",
      caption = ""
    )
  return(p10)
}

population_plot_2010 <- make_polygon_map_population_2010()

