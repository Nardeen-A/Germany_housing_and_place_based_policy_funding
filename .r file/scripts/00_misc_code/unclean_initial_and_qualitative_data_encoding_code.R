proj.path <- getwd()
Germany_aaw <- read.csv("E:\\School - work\\RA\\Germany WIP\\latest_DE.csv")

# unique intervention fields
category_of_intervention_encoding <- Germany_aaw[, c(14, 15)] %>%
  group_by(Category_Of_Intervention, Category_Label) %>%
  unique() %>%
  arrange(Category_Of_Intervention)

category_of_intervention_encoding <- category_of_intervention_encoding[1:(nrow(category_of_intervention_encoding) - 1), ]

# theme
themetic_objective_encoding <- Germany_aaw[, c(16, 17)] %>%
  group_by(Thematic_Objective_ID, Thematic_Objective_Label) %>%
  unique() %>%
  arrange(Thematic_Objective_ID)

themetic_objective_encoding <- themetic_objective_encoding[-1,]

# policy
policy_encoding <- Germany_aaw[, c(18, 19)] %>%
  group_by(Policy_Objective_ID, Policy_Objective_Label) %>%
  unique() %>%
  arrange(Policy_Objective_ID)
policy_encoding <- policy_encoding[-1,]

# fund
funds_encoding <- Germany_aaw[, c(20, 21)] %>%
  group_by(Fund_Code, Fund_Name) %>%
  unique() %>%
  arrange(Fund_Code)

# regions
programme_encoding <- Germany_aaw[, c(22, 23)] %>%
  group_by(Programme_Code, Programme_Name) %>%
  unique() %>%
  arrange(Programme_Code)

# regions
regions_encoding <- Germany_aaw[, c(25, 24)] %>%
  group_by(NUTS3_Code, Region) %>%
  unique() %>%
  arrange(NUTS3_Code)
regions_encoding <- regions_encoding[-1,]

#saving
write.csv(category_of_intervention_encoding, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'intervention_field_encoding.csv'), row.names = FALSE)
write.csv(themetic_objective_encoding, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'themetic_objective_encoding.csv'), row.names = FALSE)
write.csv(policy_encoding, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'policy_encoding.csv'), row.names = FALSE)
write.csv(funds_encoding, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'funds_encoding.csv'), row.names = FALSE)
write.csv(programme_encoding, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'programme_encoding.csv'), row.names = FALSE)
write.csv(regions_encoding, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'encoding_and_labels_for_qualitative_variables', 'regions_encoding.csv'), row.names = FALSE)
