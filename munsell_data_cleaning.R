#merging V3 and V4.1 munsell data

v3_munsell_data_10142026 <- read_csv("v3_munsell_data_10142026.csv")
v4_1_munsell_data_10132026 <- read_csv("v4.1_munsell_data_10132026.csv")

identical(names(v3_munsell_data_10142026), names(v4_1_munsell_data_10132026))
#TRUE

merged_munsell <- rbind(v3_munsell_data_10142026, v4_1_munsell_data_10132026)
 

#checking for duplicates

!any(duplicated(merged_munsell[, c("ID", "date")]))
#TRUE 

#checking notes again
which(!is.na(merged_munsell$notes))
View(merged_munsell[!is.na(merged_munsell$notes), ])
#some not germinated remove from dataframe?
#222, 237, 301, 317, 386, 501, 603, 677, 727	

#removing not germinated from dataframe
merged_munsell %>%
  filter(ObjectID == "727") %>%
  pull(notes)
merged_munsell$Value[merged_munsell$ObjectID == 727] <- 0

merged_munsell <- merged_munsell[merged_munsell$Value != 0, ]

View(merged_munsell[!is.na(merged_munsell$notes), ])
#removed ones with no germination

#graphing?
merged_munsell$date <- as.POSIXct(merged_munsell$date)

ggplot(subset(merged_munsell, ID == "P_B_NV_H_1"), 
       aes(x = date, y = Color)) +
  geom_point() +
  geom_line() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = merged_munsell$date[merged_munsell$ID == "P_B_NV_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Color over Time (ID: P_B_NV_H_1)",
       x = "Date",
       y = "Color") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#saving
write.csv(merged_munsell, "merged_munsell_10192026.csv", row.names = FALSE)


