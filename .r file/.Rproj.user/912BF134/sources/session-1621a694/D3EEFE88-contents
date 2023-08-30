# reading files in ####
proj.path <- getwd()
Germany_erdf <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'ERDF.csv'))
GDP_per_capita <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_NUTS3.csv"))
GDP_per_capita_Thuringia <- read.csv(file.path(proj.path, "data", "GDP_per_capita_NUTS3", "OECD_GDP_per_capita_Thuringia_NUTS3.csv"))
NUTS1_names <- read.csv(file.path(proj.path, "outputs", "data", "NUTS_names" , "NUTS1_names.csv"))

# GDP per Capita
GDP_per_capita <- rbind(GDP_per_capita, GDP_per_capita_Thuringia)
GDP_per_capita <- GDP_per_capita %>%
  select(3, 9, 14, 21) %>%
  filter(MEAS == "PC_CURR_PR", Year == 2016) %>%
  select(1, 4)

names(GDP_per_capita)[1] <- "NUTS3"
names(GDP_per_capita)[2] <- "GDP_per_capita"

# expenditure
Germany_erdf <- Germany_erdf %>%
  select(13, 5) %>%
  group_by(NUTS3_Code) %>%
  summarise(Total_Eligible_Expenditure_amount = sum(Total_Eligible_Expenditure_amount, na.rm = TRUE)) %>%
  mutate(NUTS3_Code_State = substr(NUTS3_Code, 1, 3))

Germany_erdf$log_expenditure <- log(Germany_erdf$Total_Eligible_Expenditure_amount, 10)

Germany_erdf$GDP_per_cap <- GDP_per_capita$GDP_per_capita[match(Germany_erdf$NUTS3_Code, GDP_per_capita$NUTS3)]
Germany_erdf$log_GDP_per_cap <- log(Germany_erdf$GDP_per_cap, 10)
Germany_erdf$log_funding_over_log_gdp <- Germany_erdf$log_expenditure / Germany_erdf$log_GDP_per_cap

Germany_erdf$state_names <- NUTS1_names$State[match(Germany_erdf$NUTS3_Code_State, NUTS1_names$NUTS1)]

Germany_erdf$NUTS3_Code_State <- factor(Germany_erdf$NUTS3_Code_State, levels = c("DE8", "DEE", "DE4", "DEG", "DED", "DEF", "DEB", "DEC", "DE9", "DEA", "DE3", "DE1", "DE7", "DE2", "DE5", "DE6"))
Germany_erdf$state_names <- factor(Germany_erdf$state_names, levels = c("Mecklenburg-Vorpommern", "Sachsen-Anhalt", "Brandenburg", "Thüringen", "Sachsen", "Schleswig-Holstein", "Rheinland-Pfalz", "Saarland", "Niedersachsen", "Nordrhein-Westfalen", "Berlin", "Baden-Württemberg", "Hessen", "Bayern", "Bremen", "Hamburg"))

# plotting
lo = min(Germany_erdf$log_expenditure, na.rm = TRUE) %>% trunc
hi = max(Germany_erdf$log_expenditure, na.rm = TRUE) %>% trunc
ticks = seq(lo, hi, 1)

theme_set(theme_bw())
theme_update(text = element_text(size = 12.5))

palette <- pals::kelly()
h_selected_colors <- palette[2:20]

h1 = ggplot(Germany_erdf, aes(x = log_expenditure, y = after_stat(density))) + 
  geom_histogram(position = "identity", alpha = 1, colour = 'black', fill = "skyblue") +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = 10 ^ ticks) 
  
ggsave(file = 'funding_histogram_density.svg', plot = h1, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 3.5 * (16 / 9), height = 3.5)
ggsave(file = 'funding_histogram_density.png', plot = h1, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 3.5 * (16 / 9), height = 3.5)

h2 = ggplot(Germany_erdf, aes(x = log_expenditure)) + 
  geom_histogram(position = "identity", alpha = 1, colour = 'black', fill = "skyblue") +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = 10 ^ ticks) 

ggsave(file = 'funding_histogram_count.svg', plot = h2, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 3.5 * (16 / 9), height = 3.5)
ggsave(file = 'funding_histogram_count.png', plot = h2, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 3.5 * (16 / 9), height = 3.5)

h3 = ggplot(Germany_erdf, aes(x = log_expenditure, fill = factor(state_names))) + 
  geom_histogram(position = "stack", alpha = 1, colour = 'black') +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = 10 ^ ticks) +
  scale_fill_manual(
    values = h_selected_colors,
    name = "States") 

ggsave(file = 'funding_histogram_count_filled_by_state.svg', plot = h3, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)
ggsave(file = 'funding_histogram_count_filled_by_state.png', plot = h3, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)

h4 = ggplot(Germany_erdf, aes(x = log_expenditure, y = after_stat(density), fill = factor(state_names))) + 
  geom_histogram(position = "stack", alpha = 1, colour = 'black') +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = 10 ^ ticks) +
  scale_fill_manual(
    values = h_selected_colors,
    name = "States") 

ggsave(file = 'funding_histogram_density_filled_by_state.svg', plot = h4, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)
ggsave(file = 'funding_histogram_density_filled_by_state.png', plot = h4, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)

# funding over gdp per capita
lo = min(Germany_erdf$log_funding_over_log_gdp * 100, na.rm = TRUE) %>% trunc
hi = max(Germany_erdf$log_funding_over_log_gdp* 100, na.rm = TRUE) %>% trunc
step = round((hi - lo)/4)
ticks = seq(lo, hi, step)

h5 = ggplot(Germany_erdf, aes(x = log_funding_over_log_gdp * 100)) + 
  geom_histogram(position = "identity", fill = "skyblue", alpha = 1, colour = 'black') +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = ticks) 

ggsave(file = 'funding_over_GDPperCapita_count_histogram_xaxis_times_100.svg', plot = h5, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 3.5 * (16 / 9), height = 3.5)
ggsave(file = 'funding_over_GDPperCapita_count_histogram_xaxis_times_100.png', plot = h5, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 3.5 * (16 / 9), height = 3.5)

h6 = ggplot(Germany_erdf, aes(x = log_funding_over_log_gdp * 100, fill = factor(state_names))) + 
  geom_histogram(position = "stack", alpha = 1, colour = 'black') +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = ticks) +
  scale_fill_manual(
    values = h_selected_colors,
    name = "States") 

ggsave(file = 'funding_over_GDPperCapita_count_histogram_xaxis_times_100_filled.svg', plot = h6, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)
ggsave(file = 'funding_over_GDPperCapita_count_histogram_xaxis_times_100_filled.png', plot = h6, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)

h7 = ggplot(Germany_erdf, aes(x = log_funding_over_log_gdp * 100, y = after_stat(density), fill = factor(state_names))) + 
  geom_histogram(position = "stack", alpha = 1, colour = 'black') +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = ticks, labels = ticks) +
  scale_fill_manual(
    values = h_selected_colors,
    name = "States") 

ggsave(file = 'funding_over_GDPperCapita_density_histogram_xaxis_times_100_filled.svg', plot = h7, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)
ggsave(file = 'funding_over_GDPperCapita_density_histogram_xaxis_times_100_filled.png', plot = h7, path = file.path(proj.path, "outputs", "plots", "Plots", "Funding histograms"), width = 4.5 * (16 / 9), height = 4.5)




