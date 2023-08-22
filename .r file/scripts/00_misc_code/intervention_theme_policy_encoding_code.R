# reading in files
proj.path <- getwd()
Germany_erdf <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'ERDF.csv'))
theme_labels <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'themetic_objective_encoding.csv'))
policy_labels <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'policy_encoding.csv'))
intervention_labels <- read.csv(file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'intervention_field_encoding.csv'))

# selecting columns
Germany_erdf <- Germany_erdf[, c(7:9)] %>%
  arrange(Category_Of_Intervention)

# counting
Germany_erdf_category_of_intervention_theme_counts <- Germany_erdf %>%
  select(Category_Of_Intervention, Thematic_Objective_ID) %>%
  group_by(Category_Of_Intervention) %>%
  count(Thematic_Objective_ID) %>%
  arrange(Category_Of_Intervention)

Germany_erdf_category_of_intervention_policy_counts <- Germany_erdf %>%
  select(Category_Of_Intervention, Policy_Objective_ID) %>%
  group_by(Category_Of_Intervention) %>%
  count(Policy_Objective_ID) %>%
  arrange(Category_Of_Intervention)

Germany_erdf_theme_policy_counts <- Germany_erdf %>%
  select(Thematic_Objective_ID, Policy_Objective_ID) %>%
  group_by(Thematic_Objective_ID) %>%
  count(Policy_Objective_ID) %>%
  arrange(Thematic_Objective_ID)

intervention_theme_policy <- Germany_erdf_category_of_intervention_theme_counts
intervention_theme_policy$policy <- Germany_erdf_theme_policy_counts$Policy_Objective_ID[match(intervention_theme_policy$Thematic_Objective_ID, Germany_erdf_theme_policy_counts$Thematic_Objective_ID)]

names(intervention_theme_policy)[1] <- "intervention"
names(intervention_theme_policy)[2] <- "theme"

intervention_theme_policy <- intervention_theme_policy %>%
  select(intervention, theme, policy, n) 
intervention_theme_policy[intervention_theme_policy == ""] <- NA

intervention_theme_policy$intervention <- as_factor(intervention_theme_policy$intervention)
intervention_theme_policy$theme <- as_factor(intervention_theme_policy$theme)
intervention_theme_policy$policy <- as_factor(intervention_theme_policy$policy)

intervention_theme_policy$intervention_label <- intervention_labels$Category_Label[match(intervention_theme_policy$intervention, intervention_labels$Category_Of_Intervention)]
intervention_theme_policy$theme_label <- theme_labels$Thematic_Objective_Label[match(intervention_theme_policy$theme, theme_labels$Thematic_Objective_ID)]
intervention_theme_policy$policy_label <- policy_labels$Policy_Objective_Label[match(intervention_theme_policy$policy, policy_labels$Policy_Objective_ID)]

intervention_theme_policy <- intervention_theme_policy %>%
  select(1, 5, 2, 6, 3, 7, 4)

write.csv(intervention_theme_policy, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'erdf_intervention_theme_policy_encoding.csv'), row.names = FALSE)

