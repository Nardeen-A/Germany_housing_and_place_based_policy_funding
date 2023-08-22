# reading files in
proj.path <- getwd()
Germany_aaw <- read.csv("E:\\School - work\\RA\\Germany WIP\\latest_DE.csv")

# subsets
Germany_aaw <- subset(Germany_aaw, Fund_Code == "ERDF")
Germany_aaw <- subset(Germany_aaw, NUTS3_Code != "")

interreg_projects <- Germany_aaw[grepl("^(Interreg|Urbact|ESPON)", Germany_aaw$Programme_Name), ] %>%
  select(23, 25, 20, 16, 9)

# totals for each interreg programme type
interreg_va_projects <- interreg_projects[grepl("^(Interreg V-A)", interreg_projects$Programme_Name), ] 
interreg_va_projects <- interreg_va_projects %>%
  add_row(Programme_Name = "Interreg_va_Total", 
          Total_Eligible_Expenditure_amount = sum(interreg_va_projects$Total_Eligible_Expenditure_amount, na.rm = TRUE))

interreg_vb_projects <- interreg_projects[grepl("^(Interreg V-B)", interreg_projects$Programme_Name), ]
interreg_vb_projects <- interreg_vb_projects %>%
  add_row(Programme_Name = "Interreg_vb_Total", 
          Total_Eligible_Expenditure_amount = sum(interreg_vb_projects$Total_Eligible_Expenditure_amount, na.rm = TRUE))

interreg_eu_projects <- subset(interreg_projects, Programme_Name == "Interreg Europe")
interreg_eu_projects <- interreg_eu_projects %>%
  add_row(Programme_Name = "Interreg_eu_Total", 
          Total_Eligible_Expenditure_amount = sum(interreg_eu_projects$Total_Eligible_Expenditure_amount, na.rm = TRUE))
  
Urbact_projects <- subset(interreg_projects, Programme_Name == "Urbact")
Urbact_projects <- Urbact_projects %>%
  add_row(Programme_Name = "Urbact_Total", 
          Total_Eligible_Expenditure_amount = sum(Urbact_projects$Total_Eligible_Expenditure_amount, na.rm = TRUE))

ESPON_projects <- subset(interreg_projects, Programme_Name == "ESPON")
ESPON_projects <- ESPON_projects %>%
  add_row(Programme_Name = "ESPON_Total", 
          Total_Eligible_Expenditure_amount = sum(ESPON_projects$Total_Eligible_Expenditure_amount, na.rm = TRUE))

interreg_projects <- interreg_projects %>%
add_row(Programme_Name = "Interreg_Total", 
        Total_Eligible_Expenditure_amount = sum(interreg_projects$Total_Eligible_Expenditure_amount, na.rm = TRUE))

Germany_aaw_total <- Germany_aaw %>%
  select(23, 25, 20, 16, 9)
Germany_aaw_total <- Germany_aaw_total %>%
  add_row(Programme_Name = "Germany_Total", 
          Total_Eligible_Expenditure_amount = sum(Germany_aaw_total$Total_Eligible_Expenditure_amount, na.rm = TRUE))
# combinging totals
Interreg_totals <- rbind(tail(interreg_va_projects, 1), tail(interreg_vb_projects, 1), tail(interreg_eu_projects, 1),
                         tail(Urbact_projects, 1), tail(ESPON_projects, 1), tail(interreg_projects, 1),
                         tail(Germany_aaw_total, 1)) %>%
  select(1, 5)
# checking total value
sum(Interreg_totals$Total_Eligible_Expenditure_amount) - 3300449553 - 31011306503  # value is equal to the total of 3300449553 so we move forward

Interreg_totals$share_of_interreg_by_total_interreg_expenditure <- Interreg_totals$Total_Eligible_Expenditure_amount / Interreg_totals$Total_Eligible_Expenditure_amount[Interreg_totals$Programme_Name == "Interreg_Total"]
Interreg_totals$share_of_interreg_by_total_germany_expenditure <- Interreg_totals$Total_Eligible_Expenditure_amount / Interreg_totals$Total_Eligible_Expenditure_amount[Interreg_totals$Programme_Name == "Germany_Total"]

write.csv(Interreg_totals, file.path(proj.path, 'outputs', 'data', 'ERDF_data', 'interreg_totals_and_shares.csv'), row.names = FALSE)
