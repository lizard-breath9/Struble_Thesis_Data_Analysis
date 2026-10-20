#merging V2,V3,V4,V4.1 data
identical(names(v3_stress_clean_10142026), names(v4_1_stress_clean_lostleafadded_10142026))
#FALSE


merged_data <- bind_rows(v4_1_stress_clean_lostleafadded_10142026, v3_stress_clean_10142026)
merged_data <- bind_rows(merged_data, v4_stress_clean_addedlostleaf_10142026) 

merged_data$CreationDate <- as.POSIXct(
  merged_data$CreationDate, 
  format = "%m/%d/%Y %H:%M:%S"
)
merged_data$CreationDate <- as.POSIXct(merged_data$CreationDate)


#creating averages for height and leaf counts for comparison? not great to compare avgs to non avgs though...
v2_avg_data<-v2_clean_data_10152026

v2_avg_data <- v2_avg_data %>% 
  mutate(avg_height = rowMeans(select(., height_one, height_two, height_three), na.rm = TRUE))

v2_avg_data <- v2_avg_data %>% 
  mutate(avg_leaf_count = rowMeans(select(., leaf_count_one, `Leaf count 2`, leaf_count_three), na.rm = TRUE))

#changing Nas to zero
v2_avg_data$avg_height[is.na(v2_avg_data$avg_height)] <- 0
v2_avg_data$avg_leaf_count[is.na(v2_avg_data$avg_leaf_count)] <- 0

#matching these column names to the other dataframes
colnames(v2_avg_data)
colnames(v2_avg_data)[19]<-"height"
colnames(v2_avg_data)[20]<-"leaf_count"

merged_data <- bind_rows(merged_data, v2_avg_data) 


#okay


merged_data$date <- as.Date(merged_data$date)
hist(merged_data$height[merged_data$date=="2026-08-21"]) 
hist(merged_data$height)

merged_data$date <- as.POSIXct(merged_data$date)

ggplot(subset(merged_data, ID == "P_B_NV_H_1"), 
       aes(x = date, y = height)) +
  geom_point() +
  geom_line() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = merged_data$date[merged_data$ID == "P_B_NV_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Height over Time (ID: P_B_NV_H_1)",
       x = "Date",
       y = "Color") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

id_list <- unique(merged_data$ID)

plot_list <- lapply(id_list, function(id) {
  
  ggplot(
    subset(merged_data, ID == id),
    aes(x = date, y = height)
  ) +
    geom_point() +
    geom_line() +
    geom_text_repel(
      aes(label = format(date, "%Y-%m-%d")),
      size = 3,
      box.padding = 0.5,
      max.overlaps = Inf
    ) +
    theme_minimal() +
    labs(
      title = paste("Height over Time (ID:", id, ")"),
      x = "Date and Time",
      y = "Height"
    ) +
    theme(axis.text.x = element_blank())
})

names(plot_list) <- id_list

plot_list[["E_B_S_D_3"]]


pdf("height_plots.pdf", width = 8, height = 5)

for (id in unique(merged_data$ID)) {
  
  print(
    ggplot(
      subset(merged_data, ID == id),
      aes(date, height)
    ) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(title = id)
  )
}

dev.off()


hist(merged_data$height[merged_data$date=="2026-09-30" | merged_data$date=="2026-09-29"])

subset_testdata <- subset(merged_data, date %in% c("2026-09-30", "2026-09-29"))

summary.lm(aov(leaf_count[subset_testdata$species=="P"]~ seed_coating[subset_testdata$species=="P"], data = subset_testdata))
#ELEL leaf count marginally with ID and WY, height sig with WY + marginally with ON, 

summary.lm(aov(leaf_count[subset_testdata$species=="E"]~ weather_treatment[subset_testdata$species=="E"], data = subset_testdata))
#POSE weathertreatment with leaves dessicated, height, leaf count turgor,`discoloration/health`, wilting sig

p1<-ggplot(subset(subset_testdata, species == "P"), aes(x= weather_treatment, y=height,fill=weather_treatment)) + 
  geom_boxplot() +scale_fill_manual(values=c("#4daf4a","#377eb8","#984ea3","brown"))
p1

summary.lm(aov(leaves_dessicated[subset_testdata$species=="P"]~ weather_treatment[subset_testdata$species=="P"]*
                 soil_treatment[subset_testdata$species=="P"], 
               data = subset_testdata))

summary(lm(height[subset_testdata$species=="P"]~
                 soil_treatment[subset_testdata$species=="P"], 
               data = subset_testdata))
#POSE leaves_dessicated, height, turgor, `discoloration/health` sig with soil treatment
#ELEL height super sig with soil treatment

p1<-ggplot(subset(subset_testdata, species == "E"), aes(x= soil_treatment,
                                y=height,
                                fill=soil_treatment)) + 
  geom_boxplot() +scale_fill_manual(values=c("#4daf4a","#377eb8"))
p1


p1<-ggplot(subset(subset_testdata, species == "E"), aes(x= soil_treatment,
                                                        y=height,
                                                        fill=weather_treatment)) + 
  geom_boxplot()
p1



summary.lm(aov(leaf_count[subset_testdata$species=="P"]~ weather_treatment[subset_testdata$species=="P"]*
      soil_treatment[subset_testdata$species=="P"]*seed_coating[subset_testdata$species=="P"], 
    data = subset_testdata))
#interaction of all three had effect on leaf count for pose


p1<-ggplot(subset(subset_testdata, species == "P"), aes(x= seed_coating,
                                                        y=leaf_count,
                                                        fill=weather_treatment)) + 
  geom_boxplot() +
  facet_wrap(~ paste("soil_treatment:", soil_treatment))
p1





#combining all versions with combined survey, tray, and cone data
v2_combined
v3_combined
v4_combined
v4_1_combined

merged_combined_data <- bind_rows(v4_1_combined, v4_combined)
merged_combined_data <- bind_rows(merged_combined_data, v3_combined) 

#creating averages for height and leaf counts for comparison? not great to compare avgs to non avgs though...
v2_avg_data<-v2_combined

v2_avg_data <- v2_avg_data %>% 
  mutate(avg_height = rowMeans(select(., height_one, height_two, height_three), na.rm = TRUE))

v2_avg_data <- v2_avg_data %>% 
  mutate(avg_leaf_count = rowMeans(select(., leaf_count_one, leaf_count_two, leaf_count_three), na.rm = TRUE))

#changing Nas to zero
v2_avg_data$avg_height[is.na(v2_avg_data$avg_height)] <- 0
v2_avg_data$avg_leaf_count[is.na(v2_avg_data$avg_leaf_count)] <- 0

#matching these column names to the other dataframes
colnames(v2_avg_data)
colnames(v2_avg_data)[42]<-"height"
colnames(v2_avg_data)[43]<-"leaf_count"

merged_combined_data <- bind_rows(merged_combined_data, v2_avg_data) 

#saving
write.csv(merged_combined_data, "messy_merged_data_10202026.csv", row.names = FALSE)



#height plots
merged_combined_data$survey_creationdate <- as.POSIXct(
  merged_combined_data$survey_creationdate, 
  format = "%m/%d/%Y %H:%M:%S"
)
merged_combined_data$survey_creationdate <- as.POSIXct(merged_combined_data$survey_creationdate)

ggplot(subset(merged_combined_data, ID == "P_B_NV_H_1"), 
       aes(x = survey_creationdate, y = height, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = merged_data$survey_creationdate[merged_data$ID == "P_B_NV_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Height over Time (ID: P_B_NV_H_1)",
       x = "Date",
       y = "Height") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

id_list <- unique(merged_combined_data$ID)

plot_list <- lapply(id_list, function(id) {
  
  ggplot(
    subset(merged_combined_data, ID == id),
    aes(x=survey_date_time, y=height, color=collector_name)
  ) +
    geom_point() +
    theme_minimal() +
    labs(title = id)

})

names(plot_list) <- id_list

plot_list[["E_B_S_D_3"]]

#pdf with all height plots
pdf("height_plots_withnames.pdf", width = 8, height = 5)

for (id in unique(merged_combined_data$ID)) {
  
  print(
    ggplot(
      subset(merged_combined_data, ID == id),
      aes(x=survey_creationdate, y=height, color=collector_name)
    ) +
      geom_point() +
      theme_minimal() +
      labs(title = id)
  )
}

dev.off()

#duplicates
!any(duplicated(merged_combined_data[, c("ID", "survey_date_time")]))
#false

View(merged_combined_data %>%
       add_count(ID, survey_date_time) %>%
       filter(n > 1))
#E_B_ON_A_3, P_B_X_A_1, E_B_X_A_5
#date 9/8/2026 7:05:00 PM

plot_list[["E_B_ON_A_3"]] #both by Alex, either could work
plot_list[["P_B_X_A_1"]] #both by Alex, one with height over 100 likely wrong (probably wrong ID)
plot_list[["E_B_X_A_5"]] #both by Alex, either could work

table<-table(merged_combined_data$ID) #should all have 30
#some have 29

table[table < 30]
#all B

table[table > 30]
#same dups greater than 30

#date stuff
merged_combined_data$date <- as.Date(merged_combined_data$survey_date_time, format = "%Y/%m/%d %H:%M:%S")
merged_combined_data <- merged_combined_data[order(merged_combined_data$survey_date_time), ]


merged_combined_data$month <- format(merged_combined_data$date, "%m") 
merged_combined_data$day <- format(merged_combined_data$date, "%d")
merged_combined_data$year <- format(merged_combined_data$date, "%Y")


#notes eek
which(!is.na(merged_combined_data$notes))
View(merged_combined_data[!is.na(merged_combined_data$notes), ])
#1,485 notes 

#maybe separate by date? or ID?
unique(merged_combined_data$date)
#"2026-07-21" "2026-07-22" "2026-07-23" "2026-07-24" "2026-07-25" "2026-07-26" "2026-07-27" "2026-07-28" "2026-07-29" "2026-07-30"
#"2026-07-31" "2026-08-01" "2026-08-02" "2026-08-03" "2026-08-04" "2026-08-05" "2026-08-07" "2026-08-11" "2026-08-14" "2026-08-18"
#"2026-08-21" "2026-08-25" "2026-08-28" "2026-09-01" "2026-09-04" "2026-09-08" "2026-09-15" "2026-09-18" "2026-09-22" "2026-09-29"
#"2026-09-30"


june21 <- v2_test_clean_data %>%
  filter(date == "2026-07-29")
View(june29[!is.na(june29$notes), ])