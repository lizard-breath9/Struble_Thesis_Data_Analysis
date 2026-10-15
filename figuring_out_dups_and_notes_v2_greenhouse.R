#####Figuring out notes and duplicates for V2 data#####
v2_germination_data_10142026 <- read_csv("v2_germination_data_10142026.csv")
v2_clean_data_10152026 <- read_csv("v2_clean_data_10152026.csv")

##germination first
v2_germination_test<-v2_germination_data_10142026

#checking for duplicates
!any(duplicated(v2_germination_test[, c("ID", "date")]))
#FALSE

View(v2_germination_test %>%
       add_count(ID, date) %>%
       filter(n > 1))
#7,740 duplicates out of 15,120 total entries

#15 days collected = 15*720 = 10,800?

#first need to determine if we have all the dates present
unique(v2_germination_test$date)

#"2026-07-22" "2026-07-23" "2026-07-25" "2026-07-26" "2026-07-27" "2026-07-29" "2026-07-30" "2026-08-01" "2026-08-03"
#"2026-08-04" "2026-08-05" "2026-08-07" "2026-08-08" "2026-08-11" "2026-08-12" "2026-08-14" "2026-08-18" "2026-08-21"

#stop daily checks on 5th, but 7/24, 7/28, 7/31, 8/2 missing 

unique(v2_germination_test$CreationDate)
#"7/22/2026 8:11:59 PM"  "7/23/2026 6:52:36 PM"  "7/23/2026 6:52:39 PM"  "7/25/2026 6:51:18 PM"  "7/25/2026 6:51:21 PM" 
#"7/26/2026 6:35:12 PM"  "7/27/2026 11:06:32 PM" "7/29/2026 2:41:23 AM"  "7/29/2026 10:37:29 PM" "7/30/2026 7:00:47 PM" 
#"8/1/2026 3:10:18 AM"   "8/1/2026 7:55:27 PM"   "8/3/2026 7:58:24 PM"   "8/3/2026 7:58:27 PM"   "8/4/2026 8:13:28 PM"  
#"8/4/2026 8:15:28 PM"   "8/4/2026 9:26:36 PM"   "8/4/2026 9:31:37 PM"   "8/5/2026 7:14:32 PM"   "8/7/2026 6:48:10 PM"  
#"8/8/2026 1:37:49 AM"   "8/8/2026 1:43:05 AM"   "8/11/2026 9:44:49 PM"  "8/12/2026 12:25:51 AM" "8/14/2026 12:03:47 AM"
#"8/14/2026 9:54:30 PM"  "8/14/2026 9:54:31 PM"  "8/14/2026 10:50:03 PM" "8/14/2026 10:50:04 PM" "8/14/2026 10:50:29 PM"
#"8/18/2026 6:47:20 PM"  "8/18/2026 9:27:31 PM"  "8/18/2026 9:27:42 PM"  "8/18/2026 9:28:04 PM"  "8/21/2026 5:14:33 PM" 
#"8/21/2026 9:40:27 PM"  "8/21/2026 9:40:47 PM"  "8/21/2026 9:42:23 PM" 

#multiple 7/23, 7/25, 7/29, 8/1, 8/3, 8/4

table(v2_germination_test$date)
#2026-07-22 2026-07-23 2026-07-25 2026-07-26 2026-07-27 2026-07-29 2026-07-30 2026-08-01 2026-08-03 2026-08-04 2026-08-05 
#720       1440       1440        720        720       1440        720       1440       1440        720        720 
#2026-08-07 2026-08-08 2026-08-11 2026-08-12 2026-08-14 2026-08-18 2026-08-21 
#180        540        270        180        990        720        720


#each day should have 720

#looks like 7/23 or 7/25 includes data for 7/24 
##7/29 in AM is likely 7/28
###8/1 in AM is likely 7/31
####8/3 likely has 8/2 stored in it
####8/7 and 8/8 likely same day (should be 8/7)
#####same for 8/11+8/12 (should be 8/11) -> missing 270
######likely 8/14 holds last 270 data points



v2_germination_test <- v2_germination_test %>% 
  filter(date <= as.Date("2026-08-05"))

v2_germination_test$CreationDate <- as.POSIXct(
  v2_germination_test$CreationDate, 
  format = "%m/%d/%Y %H:%M:%S"
)

plot(v2_germination_test$CreationDate[v2_germination_test$ID == "E_B_C_A_1"],
     v2_germination_test$germination[v2_germination_test$ID == "E_B_C_A_1"])



library(ggplot2)
library(scales) # Needed for custom date/time formatting

v2_germination_test$CreationDate <- as.POSIXct(v2_germination_test$CreationDate)


ggplot(subset(v2_germination_test, ID == "E_B_WY_H_4"), 
       aes(x = CreationDate, y = germination)) +
  geom_point() +
  geom_line() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = v2_germination_test$CreationDate[v2_germination_test$ID == "E_B_WY_H_4"], 
                   date_labels = "%Y-%m-%d %H:%M") + 
  theme_minimal() +
  labs(title = "Germination over Time (ID: E_B_WY_H_4)",
       x = "Date and Time",
       y = "Germination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Tilts labels so they don't overlap


#another way to visualize, hard to separate the two 7/23 and 7/25 dates
library(ggrepel)

ggplot(subset(v2_germination_test, ID == "P_B_C_A_1"), 
       aes(x = CreationDate, y = germination)) +
  geom_point() +
  geom_line() + 
  # This adds text labels near the points that automatically push away from each other
  geom_text_repel(aes(label = format(CreationDate, "%Y-%m-%d %H:%M")), 
                  size = 3, 
                  box.padding = 0.5,
                  max.overlaps = Inf) +
  theme_minimal() +
  labs(title = "Germination over Time (ID: P_B_C_A_1)",
       x = "Date and Time",
       y = "Germination") +
  # You can remove the cluttered x-axis labels entirely if using ggrepel
  theme(axis.text.x = element_blank()) 



#need to figure out 7/23 and 7/25
#creating new dataset with everything in it for v2
#splitting normal data collection data from germination data
v2_all_data<-v2_raw_data

#cleaning up unnecessary columns (basically only keeping germination as I want a solo germiniation dataframe)
colnames(v2_all_data)
v2_all_data <- subset(v2_all_data, select = -c(`GlobalID`,
                                                   `Radius measurement`,
                                                   `Radius measurement 2`,`Radius measurement 3`,
                                                   `Is the plant dead or alive?`,
                                                   `What stress is visible?`,
                                                   `Wilting Intensity (1-5)`,
                                                   `Extent of shoot death (1-5)`,
                                                   `Discoloration Intensity (1-5)`,`ParentGlobalID`,
                                                   `Creator`,
                                                   `Editor`))


#renaming columns
colnames(v2_all_data)

colnames(v2_all_data)[2]<-"ID"
colnames(v2_all_data)[3]<-"germinated"
colnames(v2_all_data)[4]<-"height_one"
colnames(v2_all_data)[5]<-"height_two"
colnames(v2_all_data)[6]<-"height_three"
colnames(v2_all_data)[7]<-"notes"
colnames(v2_all_data)[10]<-"leaf_count_one"
colnames(v2_all_data)[12]<-"leaf_count_two"
colnames(v2_all_data)[12]<-"leaf_count_three"

#removing rows with no color (C_D etc)
v2_all_data <- v2_all_data %>% 
  filter(nchar(ID) >= 6)

#fixing dates up
v2_all_data$date <- as.Date(v2_all_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#creating columns for values in ID
v2_all_data <- v2_all_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#saving data frame
write.csv(v2_all_data, "v2_all_data_clean_10152026.csv", row.names = FALSE)



#now removing data before 8/5/2026
v2_testing_germ<-v2_all_data
v2_testing_germ <- v2_testing_germ %>% 
filter(date <= as.Date("2026-08-05"))

#now want to see if any of the 7/23 or 7/25 dates have heights (supposedly collected on 7/24)
unique(v2_testing_germ$CreationDate)

#"7/23/2026 6:52:36 PM"  "7/23/2026 6:52:39 PM"  "7/25/2026 6:51:18 PM" "7/25/2026 6:51:21 PM"

v2_testing_germ$CreationDate <- as.POSIXct(
  v2_testing_germ$CreationDate, 
  format = "%m/%d/%Y %H:%M:%S"
)
v2_testing_germ$CreationDate <- as.POSIXct(v2_testing_germ$CreationDate)


ggplot(subset(v2_testing_germ, ID == "P_B_C_D_5"), 
       aes(x = CreationDate, y = height_one)) +
  geom_point() +
  geom_line() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = v2_testing_germ$CreationDate[v2_testing_germ$ID == "P_B_C_D_5"], 
                   date_labels = "%Y-%m-%d %H:%M") + 
  theme_minimal() +
  labs(title = "Height over Time (ID: P_B_C_D_5)",
       x = "Date and Time",
       y = "Height") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Tilts labels so they don't overlap

#E_B_C_A_1,E_B_C_C_1:heights on 7/29 (AM) actually 7/28, 8/1 (AM) actually 7/31, and 8/4

target_time <- as.POSIXct("2026-07-25 06:51:18", tz = "UTC")

v2_testing_germ %>%
  filter(CreationDate == target_time) %>%
  summarise(has_height = any(!is.na(height_one)))

#no heights on either 7/23 date
#no heights on either 7/25 date = no height data on 24th
#so data frames okay but still don't know which does with 24th and where extra 720 coming from?
#think we started data collection in 7/21, could one of them be this?




#figuring out notes now...
v2_test_clean_data<-v2_clean_data_10152026

#are there any notes?
which(!is.na(v2_clean_data_test$notes))

View(v2_clean_data_test[!is.na(v2_clean_data_test$notes), ])
#there's a lot of notes 1,194 will need to figure out a good way to go through them eek


#maybe separate by date? or ID?
unique(v2_test_clean_data$date)

june29 <- v2_test_clean_data %>%
  filter(date == "2026-07-29")
View(june29[!is.na(june29$notes), ])
#5731 E_B_ID_A_4 Accidentally thinned this one a little early
#5813 E_C_ON_H_5 2 seeds are in one of the coatings, totaling 4 plants (one ~ 6mm)

august01 <- v2_test_clean_data %>%
  filter(date == "2026-08-01")
View(august01[!is.na(august01$notes), ])
#nothing particular to note, just 2 in 1 and some growing upside down/sideways

august04 <- v2_test_clean_data %>%
  filter(date == "2026-08-04")
View(august04[!is.na(august04$notes), ])
#10948 E_C_ID_D_3 Pulled a 92 mm leaf out when grabbing the container, not sure what container it was from??
#nothing else to note, 2 in 1, growing sideways, maybe a few that aren't pose or ELEL? but note wasn't sure

august07 <- v2_test_clean_data %>%
  filter(date == "2026-08-07")
View(august07[!is.na(august07$notes), ])
#12230 E_B_S_A_1 Fell over and lost 2 germinants
#nothing else to note 4 germs, sideways growing

august08 <- v2_test_clean_data %>%
  filter(date == "2026-08-08")
View(august08[!is.na(august08$notes), ])
#12754 E_B_OZ_C_2 116 & 96 taller leaf bent over 116 shoot got pulled out In moving
#nothing else to note, extra germs, sideways growing, etc

august11 <- v2_test_clean_data %>%
  filter(date == "2026-08-11")
View(august11[!is.na(august11$notes), ])
#13047 E_B_WY_A_3 Maybe none got planted?
#nothing else to note, extra germs, sideways growing

august12 <- v2_test_clean_data %>%
  filter(date == "2026-08-12")
View(august12[!is.na(august12$notes), ])
#13282  E_C_X_A_1 +dead sprout from third seed
#nothing else to note, extra germs, sideways growing, etc

august14 <- v2_test_clean_data %>%
  filter(date == "2026-08-14")
View(august14[!is.na(august14$notes), ])
#13712 E_B_WY_A_3 No plant
#13909 E_C_C_C_3 Accidentally clipped when thinning, will show negative growth
#14022 E_C_X_H_3 Longest leaf missing tip, maybe 1 cm

august18 <- v2_test_clean_data %>%
  filter(date == "2026-08-18")
View(august18[!is.na(august18$notes), ])
#15137 E_B_WY_A_3 0 leaves, nothing in there
#15145 E_B_NV_A_1 This or next ELEL had leaf trapped between containers, came off
#15147 E_B_X_A_4 This or previous ELEL had leaf stuck between containers, it came off. 2nd sprout 13 mm 1 leaf
#15152 P_B_ID_A_3 Fallen over (not attached to seed?) still green

august21 <- v2_test_clean_data %>%
  filter(date == "2026-08-21")
View(august21[!is.na(august21$notes), ])
#15491 P_C_X_C_4 Shoot fell out
#15649 	P_B_C_A_1 Thinned, one leaf of main plant came off. Now 3 leaves :(
#15716 P_C_NV_H_4 Plant got fully pulled out when grabbing a container, put it back in the soil
#15875 	P_B_C_D_4 1 sd rm, leaf accidentally pulled off, collected was 4 now 3



#now time for germination notes
v2_germination_data_10142026 <- read_csv("v2_germination_data_10142026.csv")
v2_notes_germ<-v2_germination_data_10142026

View(v2_notes_germ[!is.na(v2_notes_germ$notes), ])
#there's a lot of notes 1,275 will need to figure out a good way to go through them eek


#maybe separate by date? or ID?
unique(v2_notes_germ$date)
#"2026-07-22" "2026-07-23" "2026-07-25" "2026-07-26" "2026-07-27" "2026-07-29" "2026-07-30"
#"2026-08-01" "2026-08-03" "2026-08-04" "2026-08-05"

june22 <- v2_notes_germ %>%
  filter(date == "2026-07-22")
View(june22[!is.na(june22$notes), ])

june23 <- v2_notes_germ %>%
  filter(date == "2026-07-23")
View(june23[!is.na(june23$notes), ])

june25 <- v2_notes_germ %>%
  filter(date == "2026-07-25")
View(june25[!is.na(june25$notes), ])
#2592 P_B_ON_H_2 I think previous emergence of this plant was incorrect :(
#2634	P_B_X_H_1 unclear
#2768 E_C_ON_H_4 	Unsure

june26 <- v2_notes_germ %>%
  filter(date == "2026-07-26")
View(june26[!is.na(june26$notes), ])

june27 <- v2_notes_germ %>%
  filter(date == "2026-07-27")
View(june27[!is.na(june27$notes), ])

june29 <- v2_notes_germ %>%
  filter(date == "2026-07-29")
View(june29[!is.na(june29$notes), ])

june30 <- v2_notes_germ %>%
  filter(date == "2026-07-30")
View(june30[!is.na(june30$notes), ])

august1 <- v2_notes_germ %>%
  filter(date == "2026-08-01")
View(august1[!is.na(august1$notes), ])
#checked already, nothing related to germination

august3 <- v2_notes_germ %>%
  filter(date == "2026-08-03")
View(august3[!is.na(august3$notes), ])

august4 <- v2_notes_germ %>%
  filter(date == "2026-08-04")
View(august4[!is.na(august4$notes), ])
#checked already, nothing related to germination

august5 <- v2_notes_germ %>%
  filter(date == "2026-08-05")
View(august5[!is.na(august5$notes), ])

#nothing to note for germination in the notes it seems