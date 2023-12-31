# reading files in ####
proj.path <- getwd()
Germany_erdf <- read.csv(file.path(proj.path, "outputs", "data", "ERDF_data", "ERDF.csv"))
NUTS1_names <- read.csv(file.path(proj.path, "outputs", "data", "NUTS_names" , "NUTS1_names.csv"))
GDP_per_capita <- read.csv(file.path(proj.path, "outputs", "data", "GDP_per_capita_at_different_NUTS_levels", "2016_GDP_per_capita_NUTS1.csv"))

# states level data
Germany_erdf_policy_region_count <- Germany_erdf %>%
  select(13, 9) %>%
  group_by(NUTS3_Code) %>%
  mutate(NUTS3_Code = substr(NUTS3_Code, 1, 3))
Germany_erdf_policy_region_count <- subset(Germany_erdf_policy_region_count, grepl("^DE", NUTS3_Code))
Germany_erdf_policy_region_count <- count(Germany_erdf_policy_region_count, NUTS3_Code, Policy_Objective_ID)
Germany_erdf_policy_region_count$Policy_Objective_ID[Germany_erdf_policy_region_count$Policy_Objective_ID == ""] <- "Blank"

# state names ####
Germany_erdf_policy_region_count$NUTS3_Code <- NUTS1_names$State[match(Germany_erdf_policy_region_count$NUTS3_Code, NUTS1_names$NUTS1)]
names(Germany_erdf_policy_region_count)[1] <- "states"

Germany_erdf_policy_region_count$states <- as.factor(Germany_erdf_policy_region_count$states)

# expenditure by theme ####
germany_expenditure <- Germany_erdf 
germany_expenditure$Policy_Objective_ID[germany_expenditure$Policy_Objective_ID == ""] <- "Blank"

germany_expenditure <- germany_expenditure %>%
  group_by(NUTS3_Code, Policy_Objective_ID) %>%
  mutate(NUTS3_Code = substr(NUTS3_Code, 1, 3)) %>%
  summarise(Total_Eligible_Expenditure_amount = sum(Total_Eligible_Expenditure_amount, na.rm = TRUE))
germany_expenditure$NUTS3_Code <- NUTS1_names$State[match(germany_expenditure$NUTS3_Code, NUTS1_names$NUTS1)]
names(germany_expenditure)[1] <- "states"

germany_expenditure$states <- as.factor(germany_expenditure$states)

# composition or share of expenditure ####
germany_expenditure_share <- germany_expenditure 

germany_expenditure_share <- germany_expenditure_share %>%
  ungroup() %>%
  group_by(states) %>%
  mutate(expenditure_per_state = sum(Total_Eligible_Expenditure_amount, na.rm = TRUE))

germany_expenditure_share$share <- germany_expenditure_share$Total_Eligible_Expenditure_amount / germany_expenditure_share$expenditure_per_state

germany_expenditure_share <- germany_expenditure_share %>%
  group_by(states) %>%
  mutate(sums_check = sum(share, na.rm = TRUE))

# ranking
Germany_erdf_policy_region_count$states <- factor(Germany_erdf_policy_region_count$states, levels = c("Mecklenburg-Vorpommern", "Sachsen-Anhalt", "Brandenburg", "Thüringen", "Sachsen", "Schleswig-Holstein", "Rheinland-Pfalz", "Saarland", "Niedersachsen", "Nordrhein-Westfalen", "Berlin", "Baden-Württemberg", "Hessen", "Bayern", "Bremen", "Hamburg"))
germany_expenditure$states <- factor(germany_expenditure$states, levels = c("Mecklenburg-Vorpommern", "Sachsen-Anhalt", "Brandenburg", "Thüringen", "Sachsen", "Schleswig-Holstein", "Rheinland-Pfalz", "Saarland", "Niedersachsen", "Nordrhein-Westfalen", "Berlin", "Baden-Württemberg", "Hessen", "Bayern", "Bremen", "Hamburg"))

germany_expenditure_share_ranked <- germany_expenditure
germany_expenditure_share_ranked <- germany_expenditure_share_ranked %>%
  ungroup() %>%
  group_by(states) %>%
  mutate(expenditure_per_state = sum(Total_Eligible_Expenditure_amount, na.rm = TRUE))

germany_expenditure_share_ranked$share <- germany_expenditure_share_ranked$Total_Eligible_Expenditure_amount / germany_expenditure_share_ranked$expenditure_per_state

germany_expenditure_share_ranked <- germany_expenditure_share_ranked %>%
  group_by(states) %>%
  mutate(sums_check = sum(share, na.rm = TRUE))

# plotting ####
theme_set(theme_bw())
theme_update(text = element_text(size = 12.5))

palette <- pals::kelly()
selected_colors <- palette[2:12]

erdf_theme_count <- ggplot(data = Germany_erdf_policy_region_count, mapping = aes(x = states, y = n, fill = factor(Policy_Objective_ID))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(
    values = selected_colors,
    name = "Theme",
    labels = c("Blank", "Smarter Europe", "Greener, carbon-free Europe", "Connected Europe", "Social Europe")
    ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(file = "erdf_project_count_by_policy.svg", plot = erdf_theme_count, path = file.path(proj.path, "outputs", "plots", "Plots", "policy plots"), width = 5 * (16 / 9), height = 5)
ggsave(file = "erdf_project_count_by_policy.png", plot = erdf_theme_count, path = file.path(proj.path, "outputs", "plots", "Plots", "policy plots"), width = 5 * (16 / 9), height = 5)


erdf_theme_expenditure <- ggplot(data = germany_expenditure, mapping = aes(x = states, y = Total_Eligible_Expenditure_amount, fill = factor(Policy_Objective_ID))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(
    values = selected_colors,
    name = "Theme",
    labels = c("Blank", "Smarter Europe", "Greener, carbon-free Europe", "Connected Europe", "Social Europe")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(file = "erdf_policy_region_expenditure.svg", plot = erdf_theme_expenditure, path = file.path(proj.path, "outputs", "plots", "Plots", "policy plots"), width = 5 * (16 / 9), height = 5)
ggsave(file = "erdf_policy_region_expenditure.png", plot = erdf_theme_expenditure, path = file.path(proj.path, "outputs", "plots", "Plots", "policy plots"), width = 5 * (16 / 9), height = 5)

erdf_theme_expenditure_shares_ranked <- ggplot(data = germany_expenditure_share_ranked, mapping = aes(x = states, y = share, fill = factor(Policy_Objective_ID))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(
    values = selected_colors,
    name = "Theme",
    labels = c("Blank", "Smarter Europe", "Greener, carbon-free Europe", "Connected Europe", "Social Europe")
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(file = "erdf_policy_region_expenditure_comp_by_state_ranked.svg", plot = erdf_theme_expenditure_shares_ranked, path = file.path(proj.path, "outputs", "plots", "Plots", "policy plots"), width = 5 * (16 / 9), height = 5)
ggsave(file = "erdf_policy_region_expenditure_comp_by_state_ranked.png", plot = erdf_theme_expenditure_shares_ranked, path = file.path(proj.path, "outputs", "plots", "Plots", "policy plots"), width = 5 * (16 / 9), height = 5)

