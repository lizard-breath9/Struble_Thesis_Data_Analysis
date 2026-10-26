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
merged_combined_data$survey_date_time <- as.POSIXct(
  merged_combined_data$survey_date_time, 
  format = "%m/%d/%Y %H:%M:%S"
)
merged_combined_data$survey_date_time <- as.POSIXct(merged_combined_data$survey_date_time)

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
merged_combined_data$date <- as.Date(merged_combined_data$survey_date_time, format = "%m/%dY/%
                                     %H:%M:%S")
merged_combined_data <- merged_combined_data[order(merged_combined_data$survey_date_time), ]


merged_combined_data$month <- format(merged_combined_data$date, "%m") 
merged_combined_data$day <- format(merged_combined_data$date, "%d")
merged_combined_data$year <- format(merged_combined_data$date, "%Y")

merged_combined_data <- merged_combined_data[order(merged_combined_data$survey_date_time), ]

#notes eek
which(!is.na(merged_combined_data$notes))
View(merged_combined_data[!is.na(merged_combined_data$notes), ])
#1,485 notes 

#maybe separate by date? or ID?
unique(merged_combined_data$date)

notes <- merged_combined_data %>%
  filter(date == "2026-07-21")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-22")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-23")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-24")
View(notes[!is.na(notes$notes), ])
#nothing to note, unsure germination but will see with germ plots

notes <- merged_combined_data %>%
  filter(date == "2026-07-25")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-26")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-27")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-28")
View(notes[!is.na(notes$notes), ])
#E_B_ID_A_4 thinned early
#E_C_ON_H_5 4 germs

notes <- merged_combined_data %>%
  filter(date == "2026-07-29")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-07-30")
View(notes[!is.na(notes$notes), ])
#E_B_WA_C_4 2 in one coating
#P_C_NV_D_5 2 in one coating
#P_C_ID_C_4 4; 2 in 1 coating
#P_C_S_C_3 4; 2 in 1 coating
#E_C_WA_A_1 4; 3 in one coating

notes <- merged_combined_data %>%
  filter(date == "2026-07-31")
View(notes[!is.na(notes$notes), ])
#E_C_WA_A_1 4 total 2 in one coating, smaller of duo is 21
#P_B_OZ_H_3 4 total 2 in one coating, counted tallest of duo
#P_C_ID_C_4 2 from one coating 4 total, smaller 35mm
#P_C_WY_C_1 2in 1 coating 4 total, smaller one 13
#P_B_ID_D_1 4 total 2 in one coating smaller of duo 19
#E_C_ON_C_2 2 sprouting from one coating
#P_C_S_C_3 2 from one seed 4 total, smallest 18
#P_B_C_C_2 10 & 9 in same coating (don't know if there was 4 germs though)
#P_B_ON_D_3 4 total 2 in one coating smaller of duo 18
#P_B_ON_A_3 Slipped out of my hand and landed upside down, spilling the contents. Was able to salvage 2 plants

notes <- merged_combined_data %>%
  filter(date == "2026-08-01")
View(notes[!is.na(notes$notes), ])
notes$ID[which(!is.na(notes$notes))]
#"P_B_ID_C_3" 2 in 1
#"E_B_ID_C_1" 2 in 1
#"P_B_X_C_5" 2 in 1
#"P_C_ID_C_4" 4; 2 in one
#"P_C_WY_C_1" 4; 2 in one
#"E_C_ID_C_5" 2 in 1
#"P_C_S_C_3 5; 3 in one
#P_B_S_D_2" 2 in 1
#"P_B_S_C_5"  4; 2 in 1
#"E_C_ON_C_2" 4; 2 in 1
# "E_C_WA_A_1" 4; 2 in 1
# "P_B_ID_D_5" 2 in 1
# "P_B_OZ_H_3" 4; 2 in 1
# "P_B_ID_D_1" 4; 2 in 1
# "E_B_WY_D_2" 2 in 1
# "P_B_ID_D_3" 2 in 1
# "E_C_ON_C_1" 2 in 1
# "P_B_X_A_4"  2 in 1
# "E_B_WA_C_4" 2 in 1
# "P_C_NV_D_5" 2 in 1


notes <- merged_combined_data %>%
  filter(date == "2026-08-02")
View(notes[!is.na(notes$notes), ])
# "P_B_X_C_5"  2 in 1
# "P_B_ID_C_3" 2 in 1
# "E_B_ID_C_1" 2 in 1
# "P_B_S_C_5"  4; 2 in 1
# "P_B_C_C_2"  2 in 1
# "E_B_WA_C_4" 2 in 1
# "P_C_S_C_3"  5; 3 in 1
# "P_C_ID_C_4" 4; 2 in 1
# "P_C_WY_C_1"4; 2 in 1
# "E_B_WY_D_2" 2 in 1
# "P_B_ID_D_5" 2 in 1
# "P_B_ID_D_1" 4; 2 in 1
# "P_B_ID_D_3" 2 in 1
# "P_B_S_D_2"  2 in 1
# "P_B_X_A_4"  2 in 1
# "P_B_NV_A_3" 2 in 1
# "P_B_X_H_3" 4; 2 in 1
# "E_C_ON_C_1" 2 in 1
# "E_C_ON_C_2" 4; 2 in 1
# "P_B_S_H_2"  4; 2 in 1
# "E_C_WA_A_1" 4; 3 in 1
# "P_B_OZ_H_3" 4; 2 in 1

notes <- merged_combined_data %>%
  filter(date == "2026-08-03")
View(notes[!is.na(notes$notes), ])
# "E_C_ON_C_1" 2 in 1
# "E_B_WY_D_2"2 in 1
# "P_B_ID_D_5" 2 in 1
# "P_B_ID_C_1" 2 in 1
# "P_B_OZ_H_3" 4; 2 in 1
# "E_C_ON_C_2" 4; 2 in 1
# "E_B_WA_C_4" 2 in 1
# "P_C_WY_C_1" 4; 2 in 1
# "E_C_ID_C_5"4; 2 in 1
# "P_B_X_A_4"  2 in 1
# "P_C_ID_C_4" 4; 2 in 1
# "P_B_ID_C_3" 2 in 1
# "P_B_X_H_3"  4; 2 in 1
# "P_C_S_C_3"  5; 3 in 1
# "P_B_S_C_5"  4; 2 in 1
# "P_B_S_H_2"  4; 2 in 1
# "P_B_S_D_2"  2 in 1
# "P_C_NV_D_5"2 in 1
# "E_C_WA_A_1" 4; 3 in 1
# "E_C_ON_H_5" 2 in 1
# "P_B_X_C_5"  2 in 1
# "E_B_ID_C_1"2 in 1

notes <- merged_combined_data %>%
  filter(date == "2026-08-04")
View(notes[!is.na(notes$notes), ])
#E_C_ID_D_3 Pulled a 92 mm leaf out when grabbing the container, not sure what container it was from?? Removed dicot
#P_B_X_A_4 2 in 1
#E_C_ON_C_1 2 in 1
#P_C_S_C_3 5; 3 in 1, next two are 30 & 8, 8 growing upside down
#P_C_NV_C_2 4; 2 in 1, smallest 1
#P_C_ID_H_5 4; 2 in 1, smallest 6
#P_C_ID_C_4 4; 2 in 1, smallest 42
#P_B_NV_A_3 2 in 1
#P_C_WY_C_1 4; 2 in 1, smallest 26
#E_C_ID_C_5 4; 2 in 1, smallest 38 & leaning
#P_B_X_H_3 4 seeds planted last one 17mm
#P_B_OZ_H_3 4 seeds 29 mm for last one
#P_B_ID_D_1 One extra 25mm
#P_B_S_H_2 4 seeds germed 30 mm for last one
#E_C_WA_A_1 4 germinates last one 35mm

notes <- merged_combined_data %>%
  filter(date == "2026-08-05")
View(notes[!is.na(notes$notes), ])
#nothing

notes <- merged_combined_data %>%
  filter(date == "2026-08-07")
View(notes[!is.na(notes$notes), ])
#E_B_S_A_1 Fell over and lost 2 germinants
#E_B_OZ_C_2 116 & 96 taller leaf bent over 116 shoot got pulled out In moving
#E_B_NV_A_5 4 germed
#P_B_C_H_2 2 shoots from same root? Other one is with 18mm
#P_B_X_H_3 4 germs last one 23 mm
#P_B_S_H_2 4 germs
#E_B_ID_H_1 4 germs
#E_C_C_A_5 Fourth sprout 12mm 1 leaf
#E_B_S_C_1 One more seeding, 11mm 1 leaf
#P_B_S_C_5 Fourth seed, 19 mm 1 leaf
#P_C_WY_A_3 Fourth sprout 5 mm 1 leaf, removed a dicot
#E_C_WA_A_1 Fourth sprout 34 mm 1 leaf
#E_C_ID_A_3 Fourth sprout 5 mm 1 leaf wrapping back into seed
#P_C_S_C_3 5; 3 in one, next are 29 with one additional leaf and 11 which is growing sideways
#P_C_WY_C_1  	4; 2 in 1, smallest 22 with additional leaf
#P_C_S_D_5 4; 2 in 1, smallest 6
#P_C_ID_C_4 4; 2 in 1, smallest 44 with additional leaf
#E_C_ON_C_1 4; 2 in 1, smallest 22 with additional leaf
#P_C_NV_H_5 4; 2 in 1, smallest 15
#P_C_ID_C_2 4; 2 in 1, smallest 4
#P_B_ID_D_2 4; 2 in 1, smallest 29
#P_B_S_D_2 5; 3 in 1, next are 20 and 19
#E_C_ID_C_5 4; 2 in one, smallest 75 with additional leaf
#P_B_ON_D_3 4; 2 in 1, smallest 29
#P_C_S_D_1 4; 2 in 1, smallest 20
#P_C_ID_D_5 2 in 1
#P_C_NV_D_5 4; 2 in 1, smallest 5 and collapsed
#P_C_X_D_3 4; 2 in 1, smallest 2
#P_B_ID_D_1 4; 2 in 1, smallest 25 with additional leaf
#P_C_X_H_3 4; 2 in 1, smallest 6
#E_C_ON_H_5 2 in 1

notes <- merged_combined_data %>%
  filter(date == "2026-08-11")
View(notes[!is.na(notes$notes), ])
#E_B_WY_A_3 Maybe none got planted?
#E_C_X_A_1 +dead sprout from third seed
#P_B_OZ_H_3 4 grerms 39 mm
#P_B_X_H_3 4 germs 27 mm
#P_B_S_H_2 4 germs 47
#P_C_ID_H_1 4 germs
#E_B_ID_H_1 4 germs 69
#P_C_ID_H_5 4 germs last one 41mm
#P_B_X_A_4 4 germs I think 35mm
#P_B_NV_H_2 4 germs 30 mm
#P_B_X_H_4 4 germs 10 mm
#P_C_NV_H_5 4 germsc
#P_C_X_H_3 4 germs
#P_B_WY_A_1 4 germs 27 mm
#E_C_ID_H_5 4 germs
#P_B_S_A_1 4 germs I think 25 mm
#P_B_ID_A_2 4 germs 26 mm
#E_B_NV_A_5 4 germs 25mm
#P_B_ID_D_2 Fourth spout 29 mm 2 leaf
#P_C_WY_A_3 Fourth sprout 33mm 1 leaf, removed dicot
#E_C_WA_A_1 Fourth sprout 47 mm 1 leaf, 112&123 leaning
#P_C_S_A_2 Fourth sprout 19 mm 1 leaf
#P_C_S_A_4 Fourth sprout 21 mm 1 leaf
#P_C_ID_A_5 Fourth sprout 4 mm 1 leaf
#P_B_S_D_2 Fpurth sprout 30 mm 2 leaf fifth sprout 21  mm 2 leaf
#P_B_ON_D_3 Fourth sprout 34 mm 2 laf
#P_B_ID_D_1 Fourth sprout 35 mm 2 leaves
#E_C_ID_A_3 Fourth sprout 4 mm 1 leaf leaning/fallen over
#E_C_C_A_5 Fourth sprout 61 mm 1 lear
#P_C_WA_C_4 4; 2 in 1, smallest 11
#E_C_ON_C_1 4; 2 in 1, smallest 48 with extra leaf
#P_C_X_D_3 4; 2 in 1, smallest 11
#P_C_S_D_5 4; 2 in 1, smallest 20
#P_C_NV_D_5 4, 2 in 1, smallest 5 and collapsed
#P_B_S_C_5 4; 2 in 1, smallest 21 with additional leaf
#E_B_S_C_1 4; 2 in 1, smallest 67
#P_C_S_C_3 5; 3 in 1, next are 48 and 24 both with 1 additional leaf
#P_C_NV_C_2 4; 2 in 1, smallest 28
#P_C_ID_C_2 4; 2 in 1, smallest 11
#P_B_WY_C_5 4; 2 in 1, smallest 26
#P_C_ID_D_5 4; 2 in 1, smallest 9
#P_C_ID_C_4 4; 2 in 1, smallest 43 with additional leaf
#P_C_S_D_1 4; 2 in 1, smallest 42 with additional leaf
#E_C_ID_C_5 4, 2 in 1, smallest 85 with extra leaf
#P_C_WY_C_1 4; 2 in 1, smallest 39 with additional leaf

notes <- merged_combined_data %>%
  filter(date == "2026-08-14")
View(notes[!is.na(notes$notes), ])
#E_B_WY_A_3 No plant
#E_C_C_C_3 Accidentally clipped when thinning, will show negative growth
#E_C_X_H_3 Longest leaf missing tip, maybe 1 cm

notes <- merged_combined_data %>%
  filter(date == "2026-08-18")
View(notes[!is.na(notes$notes), ])
#E_B_WY_A_3 0 leaves, nothing in there
#E_B_X_A_4 This or previous ELEL had leaf stuck between containers, it came off. 2nd sprout 13 mm 1 leaf
#prev elel=E_B_S_A_3
#E_B_NV_A_1 This or next ELEL had leaf trapped between containers, came off
#prev elel=E_B_S_A_1

notes <- merged_combined_data %>%
  filter(date == "2026-08-21")
View(notes[!is.na(notes$notes), ])
#P_C_X_C_4 Shoot fell out
#P_C_NV_H_4 Plant got fully pulled out when grabbing a container, put it back in the soil
#P_B_C_A_1 Thinned, one leaf of main plant came off. Now 3 leaves :(
#P_B_C_D_4 1 sd rm, leaf accidentally pulled off, collected was 4 now 3

notes <- merged_combined_data %>%
  filter(date == "2026-08-25")
View(notes[!is.na(notes$notes), ])
#nothing to note

notes <- merged_combined_data %>%
  filter(date == "2026-08-28")
View(notes[!is.na(notes$notes), ])
#P_C_X_D_2 Nothing in here
#E_B_WY_A_3 No plant
#P_B_ID_A_3 No plant

notes <- merged_combined_data %>%
  filter(date == "2026-09-01")
View(notes[!is.na(notes$notes), ])
#E_B_S_D_2 black spots
# E_B_NV_D_1 black spots
# P_B_ON_D_3 black spots
# P_B_OZ_D_5 black spots
# P_B_WA_D_3 black spots
# P_B_OZ_D_3 black spots
# P_B_WY_D_1 black spots
# P_B_ID_D_4 black spots
# P_B_S_D_4 black spots
# P_B_C_D_3 black spots
# P_B_WA_D_1 black spots
# P_B_ON_D_5 black spots

notes <- merged_combined_data %>%
  filter(date == "2026-09-04")
View(notes[!is.na(notes$notes), ])
#P_C_ON_H_2 No plant
#E_C_S_H_5 No plant
#P_C_X_C_4 No plant
#E_B_ID_H_2 Leave tip got sniped, lost so couldn’t collect for biomass
#P_C_X_D_2 No plant
#E_B_WY_A_3 No plant

notes <- merged_combined_data %>%
  filter(date == "2026-09-08")
View(notes[!is.na(notes$notes), ])
#E_C_ON_D_3 Unsure about leaf count, bizarre plant
#P_C_ON_D_1 Seed was resprouting with one prominent leaf, resulting in inaccurate leaf count
#P_C_ON_H_2 No plant
#E_C_S_H_5 No plant
#P_B_ID_A_3 No plant

notes <- merged_combined_data %>%
  filter(date == "2026-09-15")
View(notes[!is.na(notes$notes), ])
#P_C_X_C_4 no plant
#P_C_ON_H_2 no plant
#E_C_S_H_5 no plant
#E_B_WY_A_3 no plant
#E_C_ON_D_3 Not sure about leaf count

notes <- merged_combined_data %>%
  filter(date == "2026-09-18")
View(notes[!is.na(notes$notes), ])
#E_C_OZ_A_3 Plant was pulled out, tried to replant
#E_C_ON_D_3 Can't confirm leaf count
#P_C_X_C_4 no plant
#E_C_ON_C_4 no plant
#E_C_S_H_5 no plant
#P_C_ON_H_2 no plant

notes <- merged_combined_data %>%
  filter(date == "2026-09-22")
View(notes[!is.na(notes$notes), ])
#P_C_X_D_2 no plant
#E_B_WY_A_3 no plant
#E_B_ON_C_5 no plant

notes <- merged_combined_data %>%
  filter(date == "2026-09-29")
View(notes[!is.na(notes$notes), ])
#E_B_ON_C_5 no plant
#E_B_WY_A_3 no plant

notes <- merged_combined_data %>%
  filter(date == "2026-09-30")
View(notes[!is.na(notes$notes), ])
#nothing 

#creating column for if there were 2 in 1 coating
#listing ones that have this

raw_text<-"#P_B_OZ_H_3 4 grerms 39 mm
#P_B_X_H_3 4 germs 27 mm
#P_B_S_H_2 4 germs 47
#P_C_ID_H_1 4 germs
#E_B_ID_H_1 4 germs 69
#P_C_ID_H_5 4 germs last one 41mm
#P_B_X_A_4 4 germs I think 35mm
#P_B_NV_H_2 4 germs 30 mm
#P_B_X_H_4 4 germs 10 mm
#P_C_NV_H_5 4 germsc
#P_C_X_H_3 4 germs
#P_B_WY_A_1 4 germs 27 mm
#E_C_ID_H_5 4 germs
#P_B_S_A_1 4 germs I think 25 mm
#P_B_ID_A_2 4 germs 26 mm
#E_B_NV_A_5 4 germs 25mm
#P_B_ID_D_2 Fourth spout 29 mm 2 leaf
#P_C_WY_A_3 Fourth sprout 33mm 1 leaf, removed dicot
#E_C_WA_A_1 Fourth sprout 47 mm 1 leaf, 112&123 leaning
#P_C_S_A_2 Fourth sprout 19 mm 1 leaf
#P_C_S_A_4 Fourth sprout 21 mm 1 leaf
#P_C_ID_A_5 Fourth sprout 4 mm 1 leaf
#P_B_S_D_2 Fpurth sprout 30 mm 2 leaf fifth sprout 21  mm 2 leaf
#P_B_ON_D_3 Fourth sprout 34 mm 2 laf
#P_B_ID_D_1 Fourth sprout 35 mm 2 leaves
#E_C_ID_A_3 Fourth sprout 4 mm 1 leaf leaning/fallen over
#E_C_C_A_5 Fourth sprout 61 mm 1 lear
#P_C_WA_C_4 4; 2 in 1, smallest 11
#E_C_ON_C_1 4; 2 in 1, smallest 48 with extra leaf
#P_C_X_D_3 4; 2 in 1, smallest 11
#P_C_S_D_5 4; 2 in 1, smallest 20
#P_C_NV_D_5 4, 2 in 1, smallest 5 and collapsed
#P_B_S_C_5 4; 2 in 1, smallest 21 with additional leaf
#E_B_S_C_1 4; 2 in 1, smallest 67
#P_C_S_C_3 5; 3 in 1, next are 48 and 24 both with 1 additional leaf
#P_C_NV_C_2 4; 2 in 1, smallest 28
#P_C_ID_C_2 4; 2 in 1, smallest 11
#P_B_WY_C_5 4; 2 in 1, smallest 26
#P_C_ID_D_5 4; 2 in 1, smallest 9
#P_C_ID_C_4 4; 2 in 1, smallest 43 with additional leaf
#P_C_S_D_1 4; 2 in 1, smallest 42 with additional leaf
#E_C_ID_C_5 4, 2 in 1, smallest 85 with extra leaf
#P_C_WY_C_1 4; 2 in 1, smallest 39 with additional leaf
#E_B_NV_A_5 4 germed
#P_B_C_H_2 2 shoots from same root? Other one is with 18mm
#P_B_X_H_3 4 germs last one 23 mm
#P_B_S_H_2 4 germs
#E_B_ID_H_1 4 germs
#E_C_C_A_5 Fourth sprout 12mm 1 leaf
#E_B_S_C_1 One more seeding, 11mm 1 leaf
#P_B_S_C_5 Fourth seed, 19 mm 1 leaf
#P_C_WY_A_3 Fourth sprout 5 mm 1 leaf, removed a dicot
#E_C_WA_A_1 Fourth sprout 34 mm 1 leaf
#E_C_ID_A_3 Fourth sprout 5 mm 1 leaf wrapping back into seed
#P_C_S_C_3 5; 3 in one, next are 29 with one additional leaf and 11 which is growing sideways
#P_C_WY_C_1  	4; 2 in 1, smallest 22 with additional leaf
#P_C_S_D_5 4; 2 in 1, smallest 6
#P_C_ID_C_4 4; 2 in 1, smallest 44 with additional leaf
#E_C_ON_C_1 4; 2 in 1, smallest 22 with additional leaf
#P_C_NV_H_5 4; 2 in 1, smallest 15
#P_C_ID_C_2 4; 2 in 1, smallest 4
#P_B_ID_D_2 4; 2 in 1, smallest 29
#P_B_S_D_2 5; 3 in 1, next are 20 and 19
#E_C_ID_C_5 4; 2 in one, smallest 75 with additional leaf
#P_B_ON_D_3 4; 2 in 1, smallest 29
#P_C_S_D_1 4; 2 in 1, smallest 20
#P_C_ID_D_5 2 in 1
#P_C_NV_D_5 4; 2 in 1, smallest 5 and collapsed
#P_C_X_D_3 4; 2 in 1, smallest 2
#P_B_ID_D_1 4; 2 in 1, smallest 25 with additional leaf
#P_C_X_H_3 4; 2 in 1, smallest 6
#E_C_ON_H_5 2 in 1
#P_B_X_A_4 2 in 1
#E_C_ON_C_1 2 in 1
#P_C_S_C_3 5; 3 in 1, next two are 30 & 8, 8 growing upside down
#P_C_NV_C_2 4; 2 in 1, smallest 1
#P_C_ID_H_5 4; 2 in 1, smallest 6
#P_C_ID_C_4 4; 2 in 1, smallest 42
#P_B_NV_A_3 2 in 1
#P_C_WY_C_1 4; 2 in 1, smallest 26
#E_C_ID_C_5 4; 2 in 1, smallest 38 & leaning
#P_B_X_H_3 4 seeds planted last one 17mm
#P_B_OZ_H_3 4 seeds 29 mm for last one
#P_B_ID_D_1 One extra 25mm
#P_B_S_H_2 4 seeds germed 30 mm for last one
#E_C_WA_A_1 4 germinates last one 35mm
#E_C_ON_C_1 2 in 1
#E_B_WY_D_2 2 in 1
#P_B_ID_D_5 2 in 1
#P_B_ID_C_1 2 in 1
#P_B_OZ_H_3 4; 2 in 1
#E_C_ON_C_2 4; 2 in 1
#E_B_WA_C_4 2 in 1
#P_C_WY_C_1 4; 2 in 1
#E_C_ID_C_5 4; 2 in 1
#P_B_X_A_4 2 in 1
#P_C_ID_C_4 4; 2 in 1
#P_B_ID_C_3 2 in 1
#P_B_X_H_3 4; 2 in 1
#P_C_S_C_3 5; 3 in 1
#P_B_S_C_5 4; 2 in 1
#P_B_S_H_2 4; 2 in 1
#P_B_S_D_2  2 in 1
#P_C_NV_D_5 2 in 1
#E_C_WA_A_1 4; 3 in 1
#E_C_ON_H_5 2 in 1
#P_B_X_C_5 2 in 1
#E_B_ID_C_1 2 in 1
#P_B_X_C_5 2 in 1
#P_B_ID_C_3 2 in 1
#E_B_ID_C_1 2 in 1
#P_B_S_C_5 4; 2 in 1
#P_B_C_C_2 2 in 1
#E_B_WA_C_4 2 in 1
#P_C_S_C_3 5; 3 in 1
#P_C_ID_C_4 4; 2 in 1
#P_C_WY_C_1 4; 2 in 1
#E_B_WY_D_2 2 in 1
#P_B_ID_D_5 2 in 1
#P_B_ID_D_1 4; 2 in 1
#P_B_ID_D_3 2 in 1
#P_B_S_D_2  2 in 1
#P_B_X_A_4  2 in 1
#P_B_NV_A_3 2 in 1
#P_B_X_H_3 4; 2 in 1
#E_C_ON_C_1 2 in 1
#E_C_ON_C_2 4; 2 in 1
#P_B_S_H_2 4; 2 in 1
#E_C_WA_A_1 4; 3 in 1
#P_B_OZ_H_3 4; 2 in 1
#P_B_ID_C_3 2 in 1
#E_B_ID_C_1 2 in 1
#P_B_X_C_5 2 in 1
#P_C_ID_C_4 4; 2 in one
#P_C_WY_C_1 4; 2 in one
#E_C_ID_C_5 2 in 1
#P_C_S_C_3 5; 3 in one
#P_B_S_D_2 2 in 1
#P_B_S_C_5  4; 2 in 1
#E_C_ON_C_2 4; 2 in 1
#E_C_WA_A_1 4; 2 in 1
#P_B_ID_D_5 2 in 1
#P_B_OZ_H_3 4; 2 in 1
#P_B_ID_D_1 4; 2 in 1
#E_B_WY_D_2 2 in 1
#P_B_ID_D_3 2 in 1
#E_C_ON_C_1 2 in 1
#P_B_X_A_4  2 in 1
#E_B_WA_C_4 2 in 1
#P_C_NV_D_5 2 in 1
#E_C_WA_A_1 4 total 2 in one coating, smaller of duo is 21
#P_B_OZ_H_3 4 total 2 in one coating, counted tallest of duo
#P_C_ID_C_4 2 from one coating 4 total, smaller 35mm
#P_C_WY_C_1 2in 1 coating 4 total, smaller one 13
#P_B_ID_D_1 4 total 2 in one coating smaller of duo 19
#E_C_ON_C_2 2 sprouting from one coating
#P_C_S_C_3 2 from one seed 4 total, smallest 18
#P_B_C_C_2 10 & 9 in same coating (don't know if there was 4 germs though)
#P_B_ON_D_3 4 total 2 in one coating smaller of duo 18"

# 2. Read the lines and clean up empty spaces
lines <- readLines(textConnection(trimws(raw_text)))

# 3. Separate the ID from the Notes using regular expressions
# This matches the ID (first word starting with #) and everything after it
two_in_one <- data.frame(
  ID = sub("^#([^ ]+).*$", "\\1", lines),   # Strips '#' and grabs the first word
  Notes = sub("^[^ ]+ (.*)$", "\\1", lines),
  stringsAsFactors = FALSE
)

#checking for duplicates
!any(duplicated(two_in_one[, c("ID")]))
#false

View(two_in_one %>%
       add_count(ID) %>%
       filter(n > 1))

#creating binary column for this
two_in_one$germ_twoinone <- as.integer(!is.na(two_in_one$Notes))

#creating column for # of extra germ
two_in_one<-two_in_one %>% mutate(extra_germ=ifelse(grepl(
  "4 g|4;|one more|fourth|fifth|5;|4 seeds|one extra|4 total|4,", Notes, ignore.case=TRUE),1,0))

two_in_one<-two_in_one %>% mutate(extra_germ_num=case_when(grepl(
  "4 g|4;|one more|fourth|4 seeds|one extra|4 total|4,", Notes, ignore.case=TRUE)~4,
  grepl("fifth|5;", Notes, ignore.case=TRUE)~5, TRUE~0))

two_in_one <- two_in_one %>%
  mutate(
    extra_germ_num = case_when(
      grepl("4 g|4;|one more|fourth|4 seeds|one extra|4 total", Notes, ignore.case = TRUE) ~ 4,
      grepl("fifth|5;", Notes, ignore.case = TRUE) ~ 5,
      TRUE ~ 0
    )
  )

#combining duplicate ID
two_in_one <- two_in_one %>%
  group_by(ID) %>%
  summarise(
    Notes = paste(unique(Notes), collapse = ", "), #keeps each note and strings them in note column by ,
    germ_twoinone = max(germ_twoinone,na.rm=TRUE), #keeps max value in the column
    extra_germ = max(extra_germ, na.rm = TRUE),
    extra_germ_num = max(extra_germ_num, na.rm = TRUE)
  )

#adding two_in_one data to merged_combined_data
data_all<-merge(merged_combined_data, two_in_one, by = "ID", all.x = TRUE)

#cleaning this up adding zeros and removing extra notes column
data_all<-subset(data_all, select=-c(Notes))
data_all$germ_twoinone[is.na(data_all$germ_twoinone)]<-0
data_all$extra_germ[is.na(data_all$extra_germ)]<-0
data_all$extra_germ_num[is.na(data_all$extra_germ_num)]<-0

#adding ratio for number germinated out of 3
data_all$germ_ratio<-data_all$germinated/3 

#adding days until first germination
# 1. Define your experiment start date
start_date <- as.Date("2026-07-20") 

# 2. Calculate days to first germination across any rep
first_germination_by_pot <- data_all %>%
  # Make sure the date column is in the proper Date format
  mutate(date = as.Date(date)) %>%
  
  # Keep only the rows where germination actually happened
  filter(germinated > 0) %>%
  
  # Group purely by the unique Pot ID (ignores individual reps to find the overall first)
  group_by(ID) %>%
  
  # Find the earliest date of germination for that pot, then subtract the start date
  summarise(
    First_Germ_Date = min(date, na.rm = TRUE),
    Days_to_First_Germ = as.numeric(First_Germ_Date - start_date)
  )


data_all<-merge(data_all, first_germination_by_pot, by = "ID", all.x = TRUE)

write.csv(data_all, "data_all_10212026.csv", row.names = FALSE)


#want to calculate something like a growth rate?
test<-data_all

data_all<- data_all %>%
  group_by(ID) %>%                         # Group by individual if tracking multiple subjects
  arrange(date) %>%                        # Ensure chronological order
  mutate(
    diff_height = height - lag(height),   # Change in height
    diff_date = as.numeric(date - lag(date), units = "days"),         # Change in time
    growth_rate = diff_height / diff_date) # Rate of growth)
   %>%
  ungroup()


#figuring out germination and height discrepancies
data_all$date <- as.POSIXct(
  data_all$date, 
  format = "%m/%d/%Y %H:%M:%S"
)
data_all$date <- as.POSIXct(data_all$date)


id_list <- unique(data_all$ID)

germ_data<-data_all <- data_all %>% 
  filter(date <= as.Date("2026-08-11"))

#picking out 1 plot to look at
plot_list <- lapply(id_list, function(id) {
  
  ggplot(
    subset(germ_data, ID == id),
    aes(x = date, y = germinated, color=collector_name)
  ) +
    geom_point() + 
    scale_x_datetime(breaks = germ_data$date[germ_data$ID == id], 
                                    date_labels = "%Y-%m-%d") +
    theme_minimal() +
    labs(
      title = paste("Germination over Time (ID:", id, ")"),
      x = "Date and Time",
      y = "Germination"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

names(plot_list) <- id_list

plot_list[["E_B_S_D_3"]]

pdf("germ_plots6.pdf", width = 8, height = 5)
 
 for (id in unique(germ_data$ID)) {
       
         print(
           ggplot(
                 subset(germ_data, ID == id),
                 aes(x = date, y = germinated, color=collector_name)
              ) +
                geom_point() +  
                scale_x_datetime(breaks = germ_data$date[germ_data$ID == id], 
                                   +                                        date_labels = "%Y-%m-%d") +
                 theme_minimal() +
                labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          )
     }

dev.off()

#fixing weird germ
plot_list[["E_B_C_A_1"]]
germ_data$germinated[germ_data$ID == "E_B_C_A_1" & 
                       germ_data$date == as.Date("2026-07-26")] <- 2
germ_data %>%
  filter(ID == "E_B_C_A_1") %>%
  pull(germinated)

ggplot(subset(germ_data, ID == "E_B_C_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_C_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_C_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#E_B_C_C_2
germ_data %>%
  filter(ID == "E_B_C_C_2") %>%
  pull(notes)

germ_data$germinated[germ_data$ID == "E_B_C_C_2" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

ggplot(subset(germ_data, ID == "E_B_C_C_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_C_C_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_C_C_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#E_B_C_C_5
germ_data %>%
  filter(ID == "E_B_C_C_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_C_C_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_C_C_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_C_C_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_C_C_5" & 
                       germ_data$date == as.Date("2026-07-29")] <- 1

#E_B_C_H_3
germ_data %>%
  filter(ID == "E_B_C_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_C_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_C_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_C_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_C_H_3" & 
                       germ_data$date == as.Date("2026-07-29")] <- 3

#E_B_C_H_5
germ_data %>%
  filter(ID == "E_B_C_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_C_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_C_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_C_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_C_H_5" & 
                       germ_data$date == as.Date("2026-08-04")] <- 2

#E_B_ID_A_1
germ_data %>%
  filter(ID == "E_B_ID_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ID_A_1" & 
                       germ_data$date == as.Date("2026-07-26")] <- 1

#E_B_ID_A_3
germ_data %>%
  filter(ID == "E_B_ID_A_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_A_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_A_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_A_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ID_A_3" & 
                       germ_data$date == as.Date("2026-07-30")] <- 1

#E_B_ID_A_4
germ_data %>%
  filter(ID == "E_B_ID_A_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_A_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_A_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_A_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#actually think this may be correct note says it was thinned early

#E_B_ID_C_2
germ_data %>%
  filter(ID == "E_B_ID_C_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_C_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_C_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_C_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ID_C_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 0

#E_B_ID_D_1
germ_data %>%
  filter(ID == "E_B_ID_D_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_D_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_D_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_D_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ID_D_1" & 
                       germ_data$date == as.Date("2026-07-24")] <- 1

#E_B_ID_H_2
germ_data %>%
  filter(ID == "E_B_ID_H_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_H_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_H_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_H_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ID_H_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2


#E_B_ID_H_4
germ_data %>%
  filter(ID == "E_B_ID_H_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ID_H_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ID_H_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ID_H_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ID_H_4" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2


#E_B_NV_A_1
germ_data %>%
  filter(ID == "E_B_NV_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_NV_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_NV_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_NV_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_NV_A_1" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_B_NV_H_3
germ_data %>%
  filter(ID == "E_B_NV_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_NV_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_NV_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_NV_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_NV_H_3" & 
                       germ_data$date == as.Date("2026-07-31")] <- 2

#E_B_ON_C_1
germ_data %>%
  filter(ID == "E_B_ON_C_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ON_C_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ON_C_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ON_C_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ON_C_1" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_B_ON_C_3
germ_data %>%
  filter(ID == "E_B_ON_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_ON_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_ON_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_ON_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_ON_C_3" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#E_B_OZ_A_1
germ_data %>%
  filter(ID == "E_B_OZ_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_OZ_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_OZ_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_OZ_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_OZ_A_1" & 
                       germ_data$date == as.Date("2026-07-30")] <- 2

#E_B_OZ_C_1
germ_data %>%
  filter(ID == "E_B_OZ_C_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_OZ_C_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_OZ_C_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_OZ_C_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_OZ_C_1" & 
                       germ_data$date == as.Date("2026-07-30")] <- 1

#E_B_OZ_C_4
germ_data %>%
  filter(ID == "E_B_OZ_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_OZ_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_OZ_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_OZ_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_OZ_C_4" & 
                       germ_data$date == as.Date("2026-07-28")] <- 1

#E_B_S_A_1
germ_data %>%
  filter(ID == "E_B_S_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_S_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_S_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_S_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#notes say lost 2 germinants so I think this one is okay

#E_B_S_D_5
germ_data %>%
  filter(ID == "E_B_S_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_S_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_S_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_S_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_S_D_5" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_B_S_H_1
germ_data %>%
  filter(ID == "E_B_S_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_S_H_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_S_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_S_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_S_H_1" & 
                       germ_data$date == as.Date("2026-08-07")] <- 3

#E_B_WA_A_2
germ_data %>%
  filter(ID == "E_B_WA_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_A_2" & 
                       germ_data$date == as.Date("2026-08-02")] <- 3

#E_B_WA_A_3
germ_data %>%
  filter(ID == "E_B_WA_A_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_A_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_A_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_A_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_A_3" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#E_B_WA_C_3
germ_data %>%
  filter(ID == "E_B_WA_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_C_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_B_WA_C_4
germ_data %>%
  filter(ID == "E_B_WA_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_C_4" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#E_B_WA_D_2
germ_data %>%
  filter(ID == "E_B_WA_D_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_D_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_D_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_D_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_D_2" & 
                       germ_data$date == as.Date("2026-07-27")] <- 3

#E_B_WA_D_3
germ_data %>%
  filter(ID == "E_B_WA_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_D_3" & 
                       germ_data$date == as.Date("2026-07-24")] <- 2

#E_B_WA_H_3
germ_data %>%
  filter(ID == "E_B_WA_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_H_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 1

#E_B_WA_H_5
germ_data %>%
  filter(ID == "E_B_WA_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WA_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WA_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WA_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WA_H_5" & 
                       germ_data$date == as.Date("2026-07-27")] <- 1

#E_B_WY_A_5
germ_data %>%
  filter(ID == "E_B_WY_A_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WY_A_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WY_A_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WY_A_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WY_A_5" & 
                       germ_data$date == as.Date("2026-08-03")] <- 1

#E_B_WY_C_3
germ_data %>%
  filter(ID == "E_B_WY_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WY_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WY_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WY_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WY_C_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_B_WY_D_2
germ_data %>%
  filter(ID == "E_B_WY_D_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WY_D_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WY_D_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WY_D_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WY_D_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#E_B_WY_H_1
germ_data %>%
  filter(ID == "E_B_WY_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WY_H_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WY_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WY_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WY_H_1" & 
                       germ_data$date == as.Date("2026-08-02")] <- 1

#E_B_WY_H_2
germ_data %>%
  filter(ID == "E_B_WY_H_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_WY_H_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_WY_H_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WY_H_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_WY_H_2" & 
                       germ_data$date == as.Date("2026-07-30")] <- 1

#E_B_X_A_1
germ_data %>%
  filter(ID == "E_B_X_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_X_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_X_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_X_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_X_A_1" & 
                       germ_data$date == as.Date("2026-07-31")] <- 1

#E_B_X_A_3
germ_data %>%
  filter(ID == "E_B_X_A_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_X_A_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_X_A_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_X_A_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_X_A_3" & 
                       germ_data$date == as.Date("2026-08-01")] <- 2

#E_B_X_C_4
germ_data %>%
  filter(ID == "E_B_X_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_X_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_X_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_X_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_X_C_4" & 
                       germ_data$date == as.Date("2026-07-25")] <- 1

#E_B_X_D_2
germ_data %>%
  filter(ID == "E_B_X_D_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_X_D_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_X_D_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_X_D_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_X_D_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_B_X_D_4
germ_data %>%
  filter(ID == "E_B_X_D_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_B_X_D_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_B_X_D_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_X_D_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_B_X_D_4" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_C_C_C_3
germ_data %>%
  filter(ID == "E_C_C_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_C_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_C_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_C_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_C_C_3" & 
                       germ_data$date == as.Date("2026-08-03")] <- 0

#E_C_C_H_5
germ_data %>%
  filter(ID == "E_C_C_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_C_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_C_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_C_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_C_H_5" & 
                       germ_data$date == as.Date("2026-07-31")] <- 2

#E_C_ID_D_5
germ_data %>%
  filter(ID == "E_C_ID_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_ID_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_ID_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_ID_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_ID_D_5" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#E_C_ID_H_1
germ_data %>%
  filter(ID == "E_C_ID_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_ID_H_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_ID_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_ID_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_ID_H_1" & 
                       germ_data$date == as.Date("2026-07-26")] <- 1

#E_C_ID_H_3
germ_data %>%
  filter(ID == "E_C_ID_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_ID_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_ID_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_ID_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_ID_H_3" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_C_NV_A_5
germ_data %>%
  filter(ID == "E_C_NV_A_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_NV_A_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_NV_A_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_NV_A_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_NV_A_5" & 
                       germ_data$date == as.Date("2026-07-27")] <- 3

#E_C_NV_C_4
germ_data %>%
  filter(ID == "E_C_NV_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_NV_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_NV_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_NV_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_NV_C_4" & 
                       germ_data$date == as.Date("2026-07-23")] <- 0

#E_C_NV_C_5
germ_data %>%
  filter(ID == "E_C_NV_C_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_NV_C_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_NV_C_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_NV_C_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_NV_C_5" & 
                       germ_data$date == as.Date("2026-07-29")] <- 2

#E_C_NV_D_2
germ_data %>%
  filter(ID == "E_C_NV_D_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_NV_D_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_NV_D_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_NV_D_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_NV_D_2" & 
                       germ_data$date == as.Date("2026-08-03")] <- 2

#E_C_ON_A_1
germ_data %>%
  filter(ID == "E_C_ON_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_ON_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_ON_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_ON_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_ON_A_1" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#E_C_ON_A_3
germ_data %>%
  filter(ID == "E_C_ON_A_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_ON_A_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_ON_A_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_ON_A_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_ON_A_3" & 
                       germ_data$date == as.Date("2026-07-25")] <- 1

#E_C_OZ_A_1
germ_data %>%
  filter(ID == "E_C_OZ_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_OZ_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_OZ_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_OZ_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_OZ_A_1" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#E_C_OZ_C_3
germ_data %>%
  filter(ID == "E_C_OZ_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_OZ_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_OZ_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_OZ_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_OZ_C_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#E_C_OZ_D_1
germ_data %>%
  filter(ID == "E_C_OZ_D_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_OZ_D_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_OZ_D_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_OZ_D_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_OZ_D_1" & 
                       germ_data$date == as.Date("2026-07-26")] <- 2

#E_C_OZ_D_3
germ_data %>%
  filter(ID == "E_C_OZ_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_OZ_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_OZ_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_OZ_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_OZ_D_3" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_C_S_A_2
germ_data %>%
  filter(ID == "E_C_S_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_S_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_S_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_S_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_S_A_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#E_C_S_C_2
germ_data %>%
  filter(ID == "E_C_S_C_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_S_C_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_S_C_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_S_C_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_S_C_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_C_S_C_5
germ_data %>%
  filter(ID == "E_C_S_C_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_S_C_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_S_C_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_S_C_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_S_C_5" & 
                       germ_data$date == as.Date("2026-08-04")] <- 1

#E_C_S_H_2
germ_data %>%
  filter(ID == "E_C_S_H_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_S_H_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_S_H_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_S_H_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_S_H_2" & 
                       germ_data$date == as.Date("2026-07-26")] <- 1

#E_C_S_H_3
germ_data %>%
  filter(ID == "E_C_S_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_S_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_S_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_S_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_S_H_3" & 
                       germ_data$date == as.Date("2026-07-30")] <- 2

#E_C_WA_A_2
germ_data %>%
  filter(ID == "E_C_WA_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WA_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WA_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WA_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WA_A_2" & 
                       germ_data$date == as.Date("2026-07-29")] <- 1

#E_C_WA_C_4 working on this one
germ_data %>%
  filter(ID == "E_C_WA_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WA_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WA_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WA_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WA_C_4" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_C_WA_D_2
germ_data %>%
  filter(ID == "E_C_WA_D_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WA_D_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WA_D_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WA_D_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WA_D_2" & 
                       germ_data$date == as.Date("2026-08-11")] <- 2

#E_C_WA_H_3
germ_data %>%
  filter(ID == "E_C_WA_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WA_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WA_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WA_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WA_H_3" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#E_C_WY_A_2
germ_data %>%
  filter(ID == "E_C_WY_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WY_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WY_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WY_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WY_A_2" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_C_WY_D_3
germ_data %>%
  filter(ID == "E_C_WY_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WY_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WY_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WY_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WY_D_3" & 
                       germ_data$date == as.Date("2026-07-25")] <- 2

#E_C_WY_D_5
germ_data %>%
  filter(ID == "E_C_WY_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WY_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WY_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WY_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WY_D_5" & 
                       germ_data$date == as.Date("2026-07-24")] <- 1

#E_C_WY_H_4
germ_data %>%
  filter(ID == "E_C_WY_H_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_WY_H_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_WY_H_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_WY_H_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_WY_H_4" & 
                       germ_data$date == as.Date("2026-07-29")] <- 1

#E_C_X_A_3
germ_data %>%
  filter(ID == "E_C_X_A_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_X_A_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_X_A_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_X_A_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_X_A_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 1

#E_C_X_A_5
germ_data %>%
  filter(ID == "E_C_X_A_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_X_A_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_X_A_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_X_A_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_X_A_5" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#E_C_X_D_5
germ_data %>%
  filter(ID == "E_C_X_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_X_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_X_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_X_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_X_D_5" & 
                       germ_data$date == as.Date("2026-08-04")] <- 1

#E_C_X_H_3
germ_data %>%
  filter(ID == "E_C_X_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "E_C_X_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "E_C_X_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_C_X_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "E_C_X_H_3" & 
                       germ_data$date == as.Date("2026-07-28")] <- 1

#P_B_C_C_2
germ_data %>%
  filter(ID == "P_B_C_C_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_C_C_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_C_C_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_C_C_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_C_C_2" & 
                       germ_data$date == as.Date("2026-07-31")] <- 2

#P_B_C_H_5
germ_data %>%
  filter(ID == "P_B_C_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_C_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_C_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_C_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_C_H_5" & 
                       germ_data$date == as.Date("2026-08-02")] <- 1

#P_B_ID_C_2
germ_data %>%
  filter(ID == "P_B_ID_C_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_ID_C_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_ID_C_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_ID_C_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_ID_C_2" & 
                       germ_data$date == as.Date("2026-08-02")] <- 2

#P_B_ID_C_3
germ_data %>%
  filter(ID == "P_B_ID_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_ID_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_ID_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_ID_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_ID_C_3" & 
                       germ_data$date == as.Date("2026-07-29")] <- 1

#P_B_NV_A_3
germ_data %>%
  filter(ID == "P_B_NV_A_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_A_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_A_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_A_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_A_3" & 
                       germ_data$date == as.Date("2026-08-03")] <- 3

#P_B_NV_C_4
germ_data %>%
  filter(ID == "P_B_NV_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_C_4" & 
                       germ_data$date == as.Date("2026-08-01")] <- 1

#P_B_NV_D_1
germ_data %>%
  filter(ID == "P_B_NV_D_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_D_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_D_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_D_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_D_1" & 
                       germ_data$date == as.Date("2026-07-29")] <- 3

#P_B_NV_H_1
germ_data %>%
  filter(ID == "P_B_NV_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_H_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_H_1" & 
                       germ_data$date == as.Date("2026-08-02")] <- 2

#P_B_NV_H_2
germ_data %>%
  filter(ID == "P_B_NV_H_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_H_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_H_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_H_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_H_2" & 
                       germ_data$date == as.Date("2026-08-01")] <- 2

#P_B_NV_H_3
germ_data %>%
  filter(ID == "P_B_NV_H_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_H_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_H_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_H_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_H_3" & 
                       germ_data$date == as.Date("2026-07-26")] <- 1

#P_B_NV_H_4
germ_data %>%
  filter(ID == "P_B_NV_H_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_NV_H_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_NV_H_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_NV_H_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_NV_H_4" & 
                       germ_data$date == as.Date("2026-07-25")] <- 0

#P_B_ON_A_1
germ_data %>%
  filter(ID == "P_B_ON_A_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_ON_A_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_ON_A_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_ON_A_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_ON_A_1" & 
                       germ_data$date == as.Date("2026-08-11")] <- 3

#P_B_ON_D_3
germ_data %>%
  filter(ID == "P_B_ON_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_ON_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_ON_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_ON_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_ON_D_3" & 
                       germ_data$date == as.Date("2026-07-25")] <- 1

#P_B_ON_H_2
germ_data %>%
  filter(ID == "P_B_ON_H_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_ON_H_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_ON_H_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_ON_H_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_ON_H_2" & 
                       germ_data$date == as.Date("2026-07-23")] <- 0

#P_B_OZ_A_2
germ_data %>%
  filter(ID == "P_B_OZ_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_OZ_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_OZ_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_OZ_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_OZ_A_2" & 
                       germ_data$date == as.Date("2026-07-30")] <- 2

#P_B_OZ_D_5
germ_data %>%
  filter(ID == "P_B_OZ_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_OZ_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_OZ_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_OZ_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_OZ_D_5" & 
                       germ_data$date == as.Date("2026-08-11")] <- 2

#P_B_OZ_H_4
germ_data %>%
  filter(ID == "P_B_OZ_H_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_OZ_H_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_OZ_H_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_OZ_H_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_OZ_H_4" & 
                       germ_data$date == as.Date("2026-07-30")] <- 1

#P_B_OZ_H_5
germ_data %>%
  filter(ID == "P_B_OZ_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_OZ_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_OZ_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_OZ_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_OZ_H_5" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#P_B_S_C_3
germ_data %>%
  filter(ID == "P_B_S_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_S_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_S_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_S_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_S_C_3" & 
                       germ_data$date == as.Date("2026-08-03")] <- 3

#P_B_S_C_5
germ_data %>%
  filter(ID == "P_B_S_C_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_S_C_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_S_C_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_S_C_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_S_C_5" & 
                       germ_data$date == as.Date("2026-07-29")] <- 2

#P_B_WA_D_1
germ_data %>%
  filter(ID == "P_B_WA_D_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_WA_D_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_WA_D_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_WA_D_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_WA_D_1" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#P_B_WY_C_2
germ_data %>%
  filter(ID == "P_B_WY_C_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_WY_C_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_WY_C_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_WY_C_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_WY_C_2" & 
                       germ_data$date == as.Date("2026-07-29")] <- 1

#P_B_WY_C_4
germ_data %>%
  filter(ID == "P_B_WY_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_WY_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_WY_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_WY_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_WY_C_4" & 
                       germ_data$date == as.Date("2026-08-11")] <- 2

#P_B_X_A_2
germ_data %>%
  filter(ID == "P_B_X_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_X_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_X_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_X_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_X_A_2" & 
                       germ_data$date == as.Date("2026-08-11")] <- 2

#P_B_X_C_5
germ_data %>%
  filter(ID == "P_B_X_C_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_X_C_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_X_C_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_X_C_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_X_C_5" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#P_B_X_D_3
germ_data %>%
  filter(ID == "P_B_X_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_X_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_X_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_X_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_X_D_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#P_B_X_H_1
germ_data %>%
  filter(ID == "P_B_X_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_B_X_H_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_B_X_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_X_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_B_X_H_1" & 
                       germ_data$date == as.Date("2026-07-24")] <- 0

#P_C_ID_D_5
germ_data %>%
  filter(ID == "P_C_ID_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_ID_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_ID_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_ID_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_ID_D_5" & 
                       germ_data$date == as.Date("2026-08-05")] <- 1

#P_C_ID_H_1
germ_data %>%
  filter(ID == "P_C_ID_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_ID_H_1"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_ID_H_1"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_ID_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_ID_H_1" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#P_C_NV_D_3
germ_data %>%
  filter(ID == "P_C_NV_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_NV_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_NV_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_NV_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_NV_D_3" & 
                       germ_data$date == as.Date("2026-08-04")] <- 1

#P_C_ON_A_4
germ_data %>%
  filter(ID == "P_C_ON_A_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_ON_A_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_ON_A_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_ON_A_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_ON_A_4" & 
                       germ_data$date == as.Date("2026-07-31")] <- 0

#P_C_ON_D_3
germ_data %>%
  filter(ID == "P_C_ON_D_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_ON_D_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_ON_D_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_ON_D_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_ON_D_3" & 
                       germ_data$date == as.Date("2026-08-03")] <- 1

#P_C_ON_D_5
germ_data %>%
  filter(ID == "P_C_ON_D_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_ON_D_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_ON_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_ON_D_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_ON_D_5" & 
                       germ_data$date == as.Date("2026-08-04")] <- 2

#P_C_OZ_A_2
germ_data %>%
  filter(ID == "P_C_OZ_A_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_OZ_A_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_OZ_A_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_OZ_A_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_OZ_A_2" & 
                       germ_data$date == as.Date("2026-08-03")] <- 0

#P_C_OZ_A_4
germ_data %>%
  filter(ID == "P_C_OZ_A_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_OZ_A_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_OZ_A_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_OZ_A_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_OZ_A_4" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#P_C_OZ_C_4
germ_data %>%
  filter(ID == "P_C_OZ_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_OZ_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_OZ_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_OZ_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_OZ_C_4" & 
                       germ_data$date == as.Date("2026-08-05")] <- 2

#P_C_OZ_H_2
germ_data %>%
  filter(ID == "P_C_OZ_H_2") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_OZ_H_2"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_OZ_H_2"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_OZ_H_2)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_OZ_H_2" & 
                       germ_data$date == as.Date("2026-08-03")] <- 0

#P_C_S_C_4
germ_data %>%
  filter(ID == "P_C_S_C_4") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_S_C_4"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_S_C_4"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_S_C_4)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_S_C_4" & 
                       germ_data$date == as.Date("2026-08-11")] <- 2

#P_C_S_H_5
germ_data %>%
  filter(ID == "P_C_S_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_S_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_S_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_S_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_S_H_5" & 
                       germ_data$date == as.Date("2026-08-03")] <- 1

#P_C_WA_H_5
germ_data %>%
  filter(ID == "P_C_WA_H_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_WA_H_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_WA_H_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_WA_H_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_WA_H_5" & 
                       germ_data$date == as.Date("2026-08-07")] <- 2

#P_C_WY_A_5
germ_data %>%
  filter(ID == "P_C_WY_A_5") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_WY_A_5"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_WY_A_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_WY_A_5)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_WY_A_5" & 
                       germ_data$date == as.Date("2026-07-30")] <- 2

#P_C_WY_C_3
germ_data %>%
  filter(ID == "P_C_WY_C_3") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_WY_C_3"), 
       aes(x = date, y = germinated, color=collector_name)) +
  geom_point() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_WY_C_3"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_WY_C_3)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_WY_C_3" & 
                       germ_data$date == as.Date("2026-08-05")] <- 3

#P_C_X_H_1
germ_data %>%
  filter(ID == "P_C_X_H_1") %>%
  pull(notes)

ggplot(subset(germ_data, ID == "P_C_X_H_1"), 
       aes(x = date, y = germinated, color=collector_name, group=1)) +
  geom_point() + geom_line()+
  scale_x_datetime(breaks = germ_data$date[germ_data$ID == "P_C_X_H_1"], 
                   date_labels = "%Y-%m-%d") +
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_C_X_H_1)",
       x = "Date",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

germ_data$germinated[germ_data$ID == "P_C_X_H_1" & 
                       germ_data$date == as.Date("2026-08-05")] <- 1


pdf("germ_plots7.pdf", width = 8, height = 5)

for (id in unique(germ_data$ID)) {
  
  print(
    ggplot(subset(germ_data, ID == id),
      aes(x = date, y = germinated, color=collector_name, group=1)) +
      geom_point() +  geom_line()+
      scale_x_datetime(breaks = germ_data$date[germ_data$ID == id], 
                    date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()


#fixed all of them

#merging fixed germination into big dataframe
test<-data_all_10212026
test <- test %>%
  left_join(
    germ_data %>% select(ID, date, germinated, !any_of(names(test))), 
    by = c("ID", "date")
  )

colnames(test)[79]<-"corrected_germ"

#adding ratio for number germinated out of 3
test$germ_ratio_correct<-test$corrected_germ/3 

#adding days until first germination
# 1. Define the experiment start date
start_date <- as.Date("2026-07-20") 

# 2. Calculate days to first germination across any rep
first_germination_by_pot <- test %>%
  # Make sure the date column is in the proper Date format
  mutate(date = as.Date(date)) %>%
  
  # Keep only the rows where germination actually happened
  filter(corrected_germ > 0) %>%
  
  # Group purely by the unique Pot ID (ignores individual reps to find the overall first)
  group_by(ID) %>%
  
  # Find the earliest date of germination for that pot, then subtract the start date
  summarise(
    correct_first_germ_date = min(date, na.rm = TRUE),
    correct_days_to_first_germ = as.numeric(correct_first_germ_date - start_date)
  )

test<-merge(test, first_germination_by_pot, by = "ID", all.x = TRUE)
  
  
write.csv(test, "data_all_germ_corrected_10262026.csv", row.names = FALSE)

write.csv(germ_data, "germ_corrections_10262026.csv", row.names = FALSE)
