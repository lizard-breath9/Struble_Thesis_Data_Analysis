#packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)



#importing data for V4.1 !!!!*******
v4_1_tray_raw_data <- read_csv("Greenhouse_Survey_v4.1_raw_data_10132026/collect_data_for_each_tray_1.csv")
v4_1_cone_raw_data <- read_csv("Greenhouse_Survey_v4.1_raw_data_10132026/v4_1_greenhouse_raw_data.csv")
v4_1_survey_raw_data <- read_csv("Greenhouse_Survey_v4.1_raw_data_10132026/survey_0.csv")


#other ones looking weird so just going to use this one for now.
v4.1_data<-v4_1_cone_raw_data


#sorting out munsell data from other and cleaning up
v4.1_munsell_data<-v4.1_data
v4.1_munsell_data <- v4.1_munsell_data[!is.na(v4.1_munsell_data$`Hue (First #)`), ]
v4.1_munsell_data <- subset(v4.1_munsell_data, select = -c(`# of seeds germinated`, `Leaf Count`,
                                                           `Height measurement`,`Height measurement 2`,
                                                           `Height measurement 3`,`Overall Plant Turgor (1-5)`,
                                                           `Wilting Intensity (1-5)`,`Discoloration/Health (1-5)`,
                                                           `# leaves showing desiccation`,`% desiccation of leaf...18`,
                                                           `% desiccation of leaf...19`,`% desiccation of leaf...20`,
                                                           `% desiccation of leaf...21`,`% desiccation of leaf...22`,
                                                           `% desiccation of leaf...23`,`% desiccation of leaf...24`,
                                                           `% desiccation of leaf...25`,`% desiccation of leaf...26`,
                                                           `% desiccation of leaf...27`,`% desiccation of leaf...28`,
                                                           `% desiccation of leaf...29`,`Select All That Apply`))
v4.1_munsell_data <- subset(v4.1_munsell_data, select=-c(`ParentGlobalID`, `Editor`,`Creator`))
v4.1_munsell_data <- subset(v4.1_munsell_data, select=-c(`GlobalID`))

#renaming and creating color column
colnames(v4.1_munsell_data)[2]<-"ID"
colnames(v4.1_munsell_data)[3]<-"Common Colors (optional)"
v4.1_munsell_data <- v4.1_munsell_data %>%
  unite("Color", `Hue (First #)`, `Letter Component of Hue`, `Value (Second #)`, `Chroma (Final #)`,
        sep = "_", na.rm = TRUE, remove = FALSE)
v4.1_munsell_data <- subset(v4.1_munsell_data, select=-c(`Common Colors (optional)`))

colnames(v4.1_munsell_data)[4]<-"Hue#"
colnames(v4.1_munsell_data)[5]<-"Hue_letter"
colnames(v4.1_munsell_data)[6]<-"Value"
colnames(v4.1_munsell_data)[7]<-"Chroma"

#are there any notes?
which(!is.na(v4.1_munsell_data$`Any notes for this plant?`))
#2564

#fixing the color for this one
colnames(v4.1_munsell_data)[8]<-"notes"
v4.1_munsell_data[2564, "notes"]
#Pbnvh4 7.5GY5/6 ( not included for some reason)
v4.1_munsell_data[2564, "Color"] <- "7.5_GY_5_6"
v4.1_munsell_data[2564, "Color"]
#7.5_GY_5_6

#removing rows with no color (C_D etc)
v4.1_munsell_data <- v4.1_munsell_data[v4.1_munsell_data$Value != 0, ]


#fixing dates up
v4.1_munsell_data$date <- as.Date(v4.1_munsell_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v4.1_munsell_data[, c("ID", "date")]))
#TRUE!

#creating columns for values in ID
v4.1_munsell_data <- v4.1_munsell_data %>% 
  filter(nchar(ID) >= 6)

v4.1_munsell_data <- v4.1_munsell_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )



####V4.1 sorting out regularly collected data from munsell
v4_1_data <- v4_1_cone_raw_data[is.na(v4_1_cone_raw_data$`Hue (First #)`), ]

#removing unnecessary columns
colnames(v4_1_data)
v4_1_data <- subset(v4_1_data, select=-c(`Common Colors (optional)`,`Hue (First #)`,
                                         `Letter Component of Hue`,`Value (Second #)`,`Chroma (Final #)`,
                                         Creator, Editor, `Height measurement 2`,`Height measurement 3`,`# of seeds germinated`))


#renaming columns
colnames(v4_1_data)
colnames(v4_1_data)[3]<-"ID"
colnames(v4_1_data)[4]<-"leaf_count"
colnames(v4_1_data)[5]<-"height"
colnames(v4_1_data)[6]<-"turgor"
colnames(v4_1_data)[7]<-"wilting"
colnames(v4_1_data)[8]<-"discoloration/health"
colnames(v4_1_data)[9]<-"leaves_desiccated"
colnames(v4_1_data)[22]<-"select_apply"
colnames(v4_1_data)[23]<-"notes"
colnames(v4_1_data)[10]<-"leaf1_dessicated_per"
colnames(v4_1_data)[11]<-"leaf2_dessicated_per"
colnames(v4_1_data)[12]<-"leaf3_dessicated_per"
colnames(v4_1_data)[13]<-"leaf4_dessicated_per"
colnames(v4_1_data)[14]<-"leaf5_dessicated_per"
colnames(v4_1_data)[15]<-"leaf6_dessicated_per"
colnames(v4_1_data)[16]<-"leaf7_dessicated_per"
colnames(v4_1_data)[17]<-"leaf8_dessicated_per"
colnames(v4_1_data)[18]<-"leaf9_dessicated_per"
colnames(v4_1_data)[19]<-"leaf10_dessicated_per"
colnames(v4_1_data)[20]<-"leaf11_dessicated_per"
colnames(v4_1_data)[21]<-"leaf12_dessicated_per"


#changing NA in leaf_dessicated % to 0
v4_1_data$leaf1_dessicated_per[is.na(v4_1_data$leaf1_dessicated_per)] <- 0
v4_1_data$leaf2_dessicated_per[is.na(v4_1_data$leaf2_dessicated_per)] <- 0
v4_1_data$leaf3_dessicated_per[is.na(v4_1_data$leaf3_dessicated_per)] <- 0
v4_1_data$leaf4_dessicated_per[is.na(v4_1_data$leaf4_dessicated_per)] <- 0
v4_1_data$leaf5_dessicated_per[is.na(v4_1_data$leaf5_dessicated_per)] <- 0
v4_1_data$leaf6_dessicated_per[is.na(v4_1_data$leaf6_dessicated_per)] <- 0
v4_1_data$leaf7_dessicated_per[is.na(v4_1_data$leaf7_dessicated_per)] <- 0
v4_1_data$leaf8_dessicated_per[is.na(v4_1_data$leaf8_dessicated_per)] <- 0
v4_1_data$leaf9_dessicated_per[is.na(v4_1_data$leaf9_dessicated_per)] <- 0
v4_1_data$leaf10_dessicated_per[is.na(v4_1_data$leaf10_dessicated_per)] <- 0
v4_1_data$leaf11_dessicated_per[is.na(v4_1_data$leaf11_dessicated_per)] <- 0
v4_1_data$leaf12_dessicated_per[is.na(v4_1_data$leaf12_dessicated_per)] <- 0

#removing emtpy conetainers
v4_1_data <- v4_1_data %>% 
  filter(nchar(ID) >= 6)

#creating columns for values in ID
v4_1_data <- v4_1_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#are there any notes?
which(!is.na(v4_1_data$notes))
v4_1_data$notes[!is.na(v4_1_data$notes)]
View(v4_1_data[!is.na(v4_1_data$notes), ])

#P_B_ON_A_1 14 leaves
v4_1_data %>%
  filter(ObjectID == "3512") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 3512] <- 14

#P_C_OZ_C_3 18 leaves
v4_1_data %>%
  filter(ObjectID == "5752") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 5752] <- 18

#P_B_ID_C_2 18 leaves
v4_1_data %>%
  filter(ObjectID == "6200") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6200] <- 18

#P_C_OZ_C_1 18 leaves
v4_1_data %>%
  filter(ObjectID == "6465") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6465] <- 18

#6473	P_C_OZ_C_4 19 leaves
v4_1_data %>%
  filter(ObjectID == "6473") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6473] <- 19

#6504	P_C_ID_C_4 18 leaves
v4_1_data %>%
  filter(ObjectID == "6504") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6504] <- 18

#6607	P_C_OZ_C_3 17 leaves
v4_1_data %>%
  filter(ObjectID == "6607") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6607] <- 17

#6634	P_C_X_H_3 18 leaves
v4_1_data %>%
  filter(ObjectID == "6634") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6634] <- 18

#6711	P_B_ON_C_4 17 leaves
v4_1_data %>%
  filter(ObjectID == "6711") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6711] <- 17

#6732	P_B_OZ_C_2 17 leaves
v4_1_data %>%
  filter(ObjectID == "6732") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6732] <- 17

#6790	P_B_ON_A_1 20 leaves
v4_1_data %>%
  filter(ObjectID == "6790") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6790] <- 20

#6849	P_B_S_H_1 17 leaves
v4_1_data %>%
  filter(ObjectID == "6849") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 6849] <- 17

#7056	P_B_ID_C_2 20 leaves
v4_1_data %>%
  filter(ObjectID == "7056") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7056] <- 20

#7074	P_B_OZ_C_3 17 leaves
v4_1_data %>%
  filter(ObjectID == "7074") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7074] <- 17

#7109	P_C_ON_A_5 18 leaves
v4_1_data %>%
  filter(ObjectID == "7109") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7109] <- 18

#7183	P_B_S_H_1 20 leaves
v4_1_data %>%
  filter(ObjectID == "7183") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7183] <- 20

#7186	P_B_WY_H_2 17 leaves
v4_1_data %>%
  filter(ObjectID == "7186") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7186] <- 17

#7218	P_B_NV_H_5 17 leaves
v4_1_data %>%
  filter(ObjectID == "7218") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7218] <- 17

#7221	P_B_ON_H_4 17 leaves
v4_1_data %>%
  filter(ObjectID == "7221") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7221] <- 17

#7224	P_B_OZ_H_3 19 leaves
v4_1_data %>%
  filter(ObjectID == "7224") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7224] <- 19

#7234	P_B_ON_C_4 19 leaves
v4_1_data %>%
  filter(ObjectID == "7234") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7234] <- 19

#7252	P_B_C_C_2 18 leaves
v4_1_data %>%
  filter(ObjectID == "7252") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7252] <- 18

#7255	P_B_OZ_C_2 18 leaves
v4_1_data %>%
  filter(ObjectID == "7255") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7255] <- 18

#7295	P_B_C_A_4 19 leaves
v4_1_data %>%
  filter(ObjectID == "7295") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7295] <- 19

#7313	P_B_ON_A_1 25 leaves
v4_1_data %>%
  filter(ObjectID == "7313") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7313] <- 25

#7341	P_B_ID_C_2 24 leaves
v4_1_data %>%
  filter(ObjectID == "7341") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7341] <- 24

#7359	P_B_OZ_C_3 17 leaves
v4_1_data %>%
  filter(ObjectID == "7359") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7359] <- 17

#7408	P_C_X_D_3 19 leaves
v4_1_data %>%
  filter(ObjectID == "7408") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7408] <- 19

#7453	P_C_OZ_A_3 17 leaves
v4_1_data %>%
  filter(ObjectID == "7453") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7453] <- 17

#7462	P_C_OZ_C_1 18 leaves
v4_1_data %>%
  filter(ObjectID == "7462") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7462] <- 18

#7470	P_C_OZ_C_4 19 leaves
v4_1_data %>%
  filter(ObjectID == "7470") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7470] <- 19

#7472	P_C_ID_C_5 20 leaves
v4_1_data %>%
  filter(ObjectID == "7472") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7472] <- 20

#7474	P_C_WA_C_4 19 leaves
v4_1_data %>%
  filter(ObjectID == "7474") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7474] <- 19

#7501	P_C_ID_C_4 20 leaves
v4_1_data %>%
  filter(ObjectID == "7501") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7501] <- 20

#7531	P_B_S_H_5 18 leaves
v4_1_data %>%
  filter(ObjectID == "7531") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7531] <- 18

#7569	P_B_X_D_4 17 leaves
v4_1_data %>%
  filter(ObjectID == "7569") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7569] <- 17

#7615	P_B_OZ_A_4 18 leaves
v4_1_data %>%
  filter(ObjectID == "7615") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7615] <- 18

#7681	P_C_NV_C_1 17 leaves
v4_1_data %>%
  filter(ObjectID == "7681") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7681] <- 17

#7700	P_C_OZ_C_3 21 leaves
v4_1_data %>%
  filter(ObjectID == "7700") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7700] <- 21

#7710	P_C_WA_H_3 18 leaves
v4_1_data %>%
  filter(ObjectID == "7710") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7710] <- 18

#7727	P_C_X_H_3 19 leaves
v4_1_data %>%
  filter(ObjectID == "7727") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7727] <- 19

#7805	P_C_ON_H_3 17 leaves
v4_1_data %>%
  filter(ObjectID == "7805") %>%
  pull(notes)
v4_1_data$leaf_count[v4_1_data$ObjectID == 7805] <- 17




#fixing dates up
v4_1_data$date <- as.Date(v4_1_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v4_1_data[, c("ID", "date")]))
#FALSE
View(v4_1_data %>%
  add_count(ID, date) %>%
  filter(n > 1))

#saving dataframes
write.csv(v4.1_munsell_data, "v4.1_munsell_data_10132026.csv", row.names = FALSE)
write.csv(v4_1_data, "v4.1_cleaned_data_10132026.csv", row.names = FALSE)

#duplicates dataframe
duplicates<-(v4_1_data %>%
       add_count(ID, date) %>%
       filter(n > 1))

write.csv(duplicates, "v4.1_duplicates_10132026.csv", row.names = FALSE)


#creating categories for each of the select all options
View(v4_1_data[!is.na(v4_1_data$select_apply), ])

# 1. Ensure your original data has a unique row identifier
v4_1_data <- v4_1_data %>% 
  mutate(row_id = row_number())

# 2. Extract just the values and pivot them (rows with NA will be ignored here)
specific_columns <- v4_1_data %>%
  select(row_id, select_apply) %>% 
  separate_longer_delim(cols = select_apply, delim = ",") %>% 
  mutate(select_apply = str_trim(select_apply)) %>% 
  filter(!is.na(select_apply) & select_apply != "") %>% 
  pivot_wider(
    names_from = select_apply, 
    values_from = select_apply,
    values_fn = ~ 1,     # Use ~ .x instead of ~ 1 if you want the text instead of 1s
    values_fill = 0      # Use NA instead of 0 if you want NAs instead of 0s
  )

# 3. Join the new specific columns back to your original data
# This guarantees that rows with NAs are preserved!
v4_1_data <- v4_1_data %>%
  left_join(specific_columns, by = "row_id") %>% 
  select(-row_id) # Remove the temporary row ID

#changing NAs to zeros for these new columns
v4_1_data$Artificial_Damage[is.na(v4_1_data$Artificial_Damage)] <- 0
v4_1_data$Black_Spots[is.na(v4_1_data$Black_Spots)] <- 0
v4_1_data$Leaning[is.na(v4_1_data$Leaning)] <- 0
v4_1_data$Bent[is.na(v4_1_data$Bent)] <- 0
v4_1_data$Standing_Water[is.na(v4_1_data$Standing_Water)] <- 0

#saving dataframe
#write.csv(final_data, "v4_1_stress_clean_10132026.csv", row.names = FALSE)

#adding lost leaf column
v4_1_data<-v4_1_data
View(v4_1_data[!is.na(v4_1_data$notes), ])

v4_1_data <- v4_1_data %>%
  mutate(
    leaf_lost = ifelse(grepl("leaf", notes, ignore.case = TRUE), 1, 0)
  )

###going through to check if it was an actual leaf lost or something else
#459 E_C_WY_D_1
v4_1_data %>%
  filter(ObjectID == "459") %>%
  pull(leaf_lost) #good

#1823 P_C_WA_C_1
v4_1_data %>%
  filter(ObjectID == "1823") %>%
  pull(notes) #Plant just one desiccated leaf not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "1823") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 1823] <- 0

#2810 P_C_ON_D_1
v4_1_data %>%
  filter(ObjectID == "2810") %>%
  pull(notes) #Seed was resprouting with one prominent leaf, resulting in inaccurate leaf count not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "2810") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 2810] <- 0

#2815 E_C_ON_D_3
v4_1_data %>%
  filter(ObjectID == "2815") %>%
  pull(notes) #Unsure about leaf count, bizarre plant not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "2815") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 2815] <- 0

#4096 E_C_ON_D_3
v4_1_data %>%
  filter(ObjectID == "4096") %>%
  pull(notes) #Not sure about leaf count, bizarre plant not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "4096") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 4096] <- 0

#4194 E_C_C_H_5
v4_1_data %>%
  filter(ObjectID == "4194") %>%
  pull(notes) #Leading leaf bent not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "4194") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 4194] <- 0

#4873 P_C_OZ_H_3
v4_1_data %>%
  filter(ObjectID == "4873") %>%
  pull(notes) #Leaf was pinned between conetainers, desecration past where it was stuck not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "4873") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 4873] <- 0

#5856 E_C_ON_D_3
v4_1_data %>%
  filter(ObjectID == "5856") %>%
  pull(notes) #Can't confirm leaf count not actually lost leaf

v4_1_data %>%
  filter(ObjectID == "5856") %>%
  pull(leaf_lost) #1 incorrect
v4_1_data$leaf_lost[v4_1_data$ObjectID == 5856] <- 0

#saving this new data frame
write.csv(v4_1_data, "v4_1_stress_clean_lostleafadded_10142026.csv", row.names = FALSE)


###combining cone, tray, and survey data for dates
colnames(v4_1_data)
colnames(v4_1_tray_raw_data)

#renaming tray data columns
colnames(v4_1_tray_raw_data)[1]<-"tray_objectID"
colnames(v4_1_tray_raw_data)[2]<-"tray_globalID"
colnames(v4_1_tray_raw_data)[3]<-"tray_label"
colnames(v4_1_tray_raw_data)[4]<-"tray_num"
colnames(v4_1_tray_raw_data)[5]<-"tray_climate"
colnames(v4_1_tray_raw_data)[6]<-"tray_cone_num"
colnames(v4_1_tray_raw_data)[7]<-"tray_ParentGlobalID"
colnames(v4_1_tray_raw_data)[8]<-"tray_creationdate"
colnames(v4_1_tray_raw_data)[9]<-"tray_creator"
colnames(v4_1_tray_raw_data)[10]<-"tray_editdate"
colnames(v4_1_tray_raw_data)[11]<-"tray_editor"

#combining by ParentGlobalID for v4_1_data and tray_globalID
v4_1_combined<-merge(v4_1_data,v4_1_tray_raw_data,by.x="ParentGlobalID",by.y="tray_globalID",all.x=TRUE,all.y=FALSE)
#think its good

#combining with survey data
colnames(v4_1_survey_raw_data)
colnames(v4_1_survey_raw_data)[1]<-"survey_objectID"
colnames(v4_1_survey_raw_data)[2]<-"survey_globalID"
colnames(v4_1_survey_raw_data)[3]<-"survey_date_time"
colnames(v4_1_survey_raw_data)[4]<-"collector_name"
colnames(v4_1_survey_raw_data)[5]<-"survey_greenhouse_notes"
colnames(v4_1_survey_raw_data)[6]<-"data_type"
colnames(v4_1_survey_raw_data)[7]<-"num_trays_collecting"
colnames(v4_1_survey_raw_data)[8]<-"survey_creationdate"
colnames(v4_1_survey_raw_data)[9]<-"survey_creator"
colnames(v4_1_survey_raw_data)[10]<-"survey_editdate"
colnames(v4_1_survey_raw_data)[11]<-"survey_editor"
colnames(v4_1_survey_raw_data)[12]<-"survey_x_cord"
colnames(v4_1_survey_raw_data)[13]<-"survey_y_cord"


#combining by tray_ParentGlobalID and survey_globalID
v4_1_combined<-merge(v4_1_combined,v4_1_survey_raw_data,by.x="tray_ParentGlobalID",by.y="survey_globalID",all.x=TRUE,all.y=FALSE)

#saving data frame
write.csv(v4_1_combined, "v4_1_combined_10202026.csv", row.names = FALSE)

#checking for duplicates
!any(duplicated(v4_1_combined[, c("ID", "survey_creationdate")]))
#FALSE

View(v4_1_combined %>%
       add_count(ID, survey_creationdate) %>%
       filter(n > 1))
#still have issue with the 6 dups


########## STARTING V4 DATA #######################################
v4_cone_raw_data <- read_csv("Greenhouse_Survey_v4_raw_data_10132026/v4_greenhouse_raw_data.csv")
v4_survey_raw_data <- read_csv("Greenhouse_Survey_v4_raw_data_10132026/survey_0.csv")
v4_tray_raw_data <- read_csv("Greenhouse_Survey_v4_raw_data_10132026/collect_data_for_each_tray_1.csv")

#no munsell data
v4_data <- subset(v4_cone_raw_data, select=-c(`Common Colors (optional)`,`Hue (First #)`,
                                     `Letter Component of Hue`,`Value (Second #)`,`Chroma (Final #)`,
                                     `# of seeds germinated`, Creator, Editor, `Height measurement 2`,
                                     `Height measurement 3`))


#renaming columns
colnames(v4_data)
colnames(v4_data)[3]<-"ID"
colnames(v4_data)[4]<-"leaf_count"
colnames(v4_data)[5]<-"height"
colnames(v4_data)[6]<-"turgor"
colnames(v4_data)[7]<-"wilting"
colnames(v4_data)[8]<-"discoloration/health"
colnames(v4_data)[9]<-"leaves_desiccated"
colnames(v4_data)[22]<-"select_apply"
colnames(v4_data)[23]<-"notes"

colnames(v4_data)[10]<-"leaf1_dessicated_per"
colnames(v4_data)[11]<-"leaf2_dessicated_per"
colnames(v4_data)[12]<-"leaf3_dessicated_per"
colnames(v4_data)[13]<-"leaf4_dessicated_per"
colnames(v4_data)[14]<-"leaf5_dessicated_per"
colnames(v4_data)[15]<-"leaf6_dessicated_per"
colnames(v4_data)[16]<-"leaf7_dessicated_per"
colnames(v4_data)[17]<-"leaf8_dessicated_per"
colnames(v4_data)[18]<-"leaf9_dessicated_per"
colnames(v4_data)[19]<-"leaf10_dessicated_per"
colnames(v4_data)[20]<-"leaf11_dessicated_per"
colnames(v4_data)[21]<-"leaf12_dessicated_per"

#changing NA in leaf_dessicated % to 0
v4_data$leaf1_dessicated_per[is.na(v4_data$leaf1_dessicated_per)] <- 0
v4_data$leaf2_dessicated_per[is.na(v4_data$leaf2_dessicated_per)] <- 0
v4_data$leaf3_dessicated_per[is.na(v4_data$leaf3_dessicated_per)] <- 0
v4_data$leaf4_dessicated_per[is.na(v4_data$leaf4_dessicated_per)] <- 0
v4_data$leaf5_dessicated_per[is.na(v4_data$leaf5_dessicated_per)] <- 0
v4_data$leaf6_dessicated_per[is.na(v4_data$leaf6_dessicated_per)] <- 0
v4_data$leaf7_dessicated_per[is.na(v4_data$leaf7_dessicated_per)] <- 0
v4_data$leaf8_dessicated_per[is.na(v4_data$leaf8_dessicated_per)] <- 0
v4_data$leaf9_dessicated_per[is.na(v4_data$leaf9_dessicated_per)] <- 0
v4_data$leaf10_dessicated_per[is.na(v4_data$leaf10_dessicated_per)] <- 0
v4_data$leaf11_dessicated_per[is.na(v4_data$leaf11_dessicated_per)] <- 0
v4_data$leaf12_dessicated_per[is.na(v4_data$leaf12_dessicated_per)] <- 0

#are there any notes?
which(!is.na(v4_data$notes))
v4_data$notes[!is.na(v4_data$notes)]
#just noting black spots?

#fixing dates up
v4_data$date <- as.Date(v4_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v4_data[, c("ID", "date")]))
#TRUE
View(v4_data %>%
       add_count(ID, date) %>%
       filter(n > 1))


#creating columns for values in ID
v4_data <- v4_data %>% 
  filter(nchar(ID) >= 6)

v4_data <- v4_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )


#creating categories for each of the select all options
View(v4_data[!is.na(v4_data$select_apply), ])

# 1. Ensure your original data has a unique row identifier
v4_data <- v4_data %>% 
  mutate(row_id = row_number())

# 2. Extract just the values and pivot them (rows with NA will be ignored here)
specific_columns <- v4_data %>%
  select(row_id, select_apply) %>% 
  separate_longer_delim(cols = select_apply, delim = ",") %>% 
  mutate(select_apply = str_trim(select_apply)) %>% 
  filter(!is.na(select_apply) & select_apply != "") %>% 
  pivot_wider(
    names_from = select_apply, 
    values_from = select_apply,
    values_fn = ~ 1,     # Use ~ .x instead of ~ 1 if you want the text instead of 1s
    values_fill = 0      # Use NA instead of 0 if you want NAs instead of 0s
  )

# 3. Join the new specific columns back to your original data
# This guarantees that rows with NAs are preserved!
v4_data <- v4_data %>%
  left_join(specific_columns, by = "row_id") %>% 
  select(-row_id) # Remove the temporary row ID

#changing NAs to zeros for these new columns
v4_data$Artificial_Damage[is.na(v4_data$Artificial_Damage)] <- 0
v4_data$Bent[is.na(v4_data$Bent)] <- 0


#creating column for black spots
v4_data$Black_spots <- as.integer(!is.na(v4_data$notes))

#creating column for lost leaf
v4_data <- v4_data %>%
  mutate(
    leaf_lost = ifelse(grepl("leaf", notes, ignore.case = TRUE), 1, 0)
  )

#saving dataframe
write.csv(v4_data, "v4_stress_clean_addedlostleaf_10142026.csv", row.names = FALSE)





###combining cone, tray, and survey data for dates
colnames(v4_data)
colnames(v4_tray_raw_data)

#renaming tray data columns
colnames(v4_tray_raw_data)[1]<-"tray_objectID"
colnames(v4_tray_raw_data)[2]<-"tray_globalID"
colnames(v4_tray_raw_data)[3]<-"tray_label"
colnames(v4_tray_raw_data)[4]<-"tray_num"
colnames(v4_tray_raw_data)[5]<-"tray_climate"
colnames(v4_tray_raw_data)[6]<-"tray_cone_num"
colnames(v4_tray_raw_data)[7]<-"tray_ParentGlobalID"
colnames(v4_tray_raw_data)[8]<-"tray_creationdate"
colnames(v4_tray_raw_data)[9]<-"tray_creator"
colnames(v4_tray_raw_data)[10]<-"tray_editdate"
colnames(v4_tray_raw_data)[11]<-"tray_editor"

#combining by ParentGlobalID for v4_1_data and tray_globalID
v4_combined<-merge(v4_data,v4_tray_raw_data,by.x="ParentGlobalID",by.y="tray_globalID",all.x=TRUE,all.y=FALSE)
#think its good

#combining with survey data
colnames(v4_survey_raw_data)
colnames(v4_survey_raw_data)[1]<-"survey_objectID"
colnames(v4_survey_raw_data)[2]<-"survey_globalID"
colnames(v4_survey_raw_data)[3]<-"survey_date_time"
colnames(v4_survey_raw_data)[4]<-"collector_name"
colnames(v4_survey_raw_data)[5]<-"survey_greenhouse_notes"
colnames(v4_survey_raw_data)[6]<-"data_type"
colnames(v4_survey_raw_data)[7]<-"num_trays_collecting"
colnames(v4_survey_raw_data)[8]<-"survey_creationdate"
colnames(v4_survey_raw_data)[9]<-"survey_creator"
colnames(v4_survey_raw_data)[10]<-"survey_editdate"
colnames(v4_survey_raw_data)[11]<-"survey_editor"
colnames(v4_survey_raw_data)[12]<-"survey_x_cord"
colnames(v4_survey_raw_data)[13]<-"survey_y_cord"


#combining by tray_ParentGlobalID and survey_globalID
v4_combined<-merge(v4_combined,v4_survey_raw_data,by.x="tray_ParentGlobalID",by.y="survey_globalID",all.x=TRUE,all.y=FALSE)

#saving data frame
write.csv(v4_combined, "v4_combined_10202026.csv", row.names = FALSE)

#checking for duplicates
!any(duplicated(v4_combined[, c("ID", "survey_creationdate")]))
#TRUE

##########DONE WITH V4.1 and V4#####################






################# V3 ###########################
#importing data
v3_cone_raw_data <- read_csv("Greenhouse_Survey_V3_raw_data_10142026/v3_greenhouse_raw_data.csv")
v3_tray_raw_data <- read_csv("Greenhouse_Survey_V3_raw_data_10142026/collect_data_for_each_tray_1.csv")
v3_survey_raw_data <- read_csv("Greenhouse_Survey_V3_raw_data_10142026/survey_0.csv")

#checking out data
colnames(v3_data_raw)


#separating munsell from regular data

#sorting out munsell data from other and cleaning up
v3_munsell_data<-v3_cone_raw_data
v3_munsell_data <- v3_munsell_data[!is.na(v3_munsell_data$`Hue (First #)`), ]

#cleaning up unnecessary columns
v3_munsell_data <- subset(v3_munsell_data, select = -c(`# of seeds germinated`, `Leaf Count`,
                                                       `Height measurement`,`Height measurement 2`,
                                                       `Height measurement 3`,`Radius measurement`,
                                                       `Radius measurement 2`,`Radius measurement 3`,
                                                       `Wilting Intensity (1-5)`,`Extent of Desiccation (1-5)`,
                                                       `Discoloration Intensity (1-5)`,`Select All That Apply`,
                                                       `Creator`,`Editor`,
                                                       `Common Colors (optional)`,`GlobalID`,
                                                       `ParentGlobalID`))


#renaming columns
colnames(v3_munsell_data)

colnames(v3_munsell_data)[2]<-"ID"
colnames(v3_munsell_data)[3]<-"Hue#"
colnames(v3_munsell_data)[4]<-"Hue_letter"
colnames(v3_munsell_data)[5]<-"Value"
colnames(v3_munsell_data)[6]<-"Chroma"
colnames(v3_munsell_data)[7]<-"notes"

#removing rows with no color (C_D etc)
v3_munsell_data <- v3_munsell_data %>% 
  filter(nchar(ID) >= 6)

#creating Color column
v3_munsell_data <- v3_munsell_data %>%
  unite("Color", `Hue#`, Hue_letter, Value, Chroma,sep = "_", na.rm = TRUE, remove = FALSE)

#are there any notes?
which(!is.na(v3_munsell_data$notes))

View(v3_munsell_data[!is.na(v3_munsell_data$notes), ])
#about condition of plant, which is not relevant for munsell

#fixing dates up
v3_munsell_data$date <- as.Date(v3_munsell_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v3_munsell_data[, c("ID", "date")]))
#TRUE!

#creating columns for values in ID
v3_munsell_data <- v3_munsell_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#saving data frame
write.csv(v3_munsell_data, "v3_munsell_data_10142026.csv", row.names = FALSE)


#now to v3 stress/regular plant data
v3_data<-v3_cone_raw_data

#separating out munsell data from stress data
v3_data <- v3_data[is.na(v3_data$`Hue (First #)`), ]


#removing unnecessary columns
colnames(v3_data)
v3_data <- subset(v3_data, select=-c(`# of seeds germinated`,
                                                     `Height measurement 2`,
                                                      `Height measurement 3`,`Radius measurement`,
                                                     `Radius measurement 2`,
                                                     `Radius measurement 3`,`Hue (First #)`,
                                                     `Letter Component of Hue`,
                                                     `Value (Second #)`,`Chroma (Final #)`,
                                                     Creator, Editor,
                                                     `Common Colors (optional)`))


#renaming columns
colnames(v3_data)

colnames(v3_data)[3]<-"ID"
colnames(v3_data)[4]<-"height"
colnames(v3_data)[5]<-"wilting"
colnames(v3_data)[6]<-"dessication_extent"
colnames(v3_data)[7]<-"discoloration/health"
colnames(v3_data)[8]<-"select_apply"
colnames(v3_data)[9]<-"notes"
colnames(v3_data)[13]<-"leaf_count"

#removing rows with no plants (C_D etc)
v3_data <- v3_data %>% 
  filter(nchar(ID) >= 6)

#fixing dates up
v3_data$date <- as.Date(v3_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v3_data[, c("ID", "date")]))
#TRUE

#creating columns for values in ID
v3_data <- v3_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#creating categories for each of the select all options
View(v3_data[!is.na(v3_data$select_apply), ])

# 1. Ensure your original data has a unique row identifier
v3_data <- v3_data %>% 
  mutate(row_id = row_number())

# 2. Extract just the values and pivot them (rows with NA will be ignored here)
specific_columns <- v3_data %>%
  select(row_id, select_apply) %>% 
  separate_longer_delim(cols = select_apply, delim = ",") %>% 
  mutate(select_apply = str_trim(select_apply)) %>% 
  filter(!is.na(select_apply) & select_apply != "") %>% 
  pivot_wider(
    names_from = select_apply, 
    values_from = select_apply,
    values_fn = ~ 1,     # Use ~ .x instead of ~ 1 if you want the text instead of 1s
    values_fill = 0      # Use NA instead of 0 if you want NAs instead of 0s
  )

# 3. Join the new specific columns back to your original data
# This guarantees that rows with NAs are preserved!
v3_data <- v3_data %>%
  left_join(specific_columns, by = "row_id") %>% 
  select(-row_id) # Remove the temporary row ID

#changing NAs to zeros for these new columns
v3_data$Artificial_Damage[is.na(v3_data$Artificial_Damage)] <- 0
v3_data$Bent[is.na(v3_data$Bent)] <- 0
v3_data$Leaning[is.na(v3_data$Leaning)] <- 0
v3_data$Standing_Water[is.na(v3_data$Standing_Water)] <- 0

#checking notes
#are there any notes?
which(!is.na(v3_data$notes))
View(v3_data[!is.na(v3_data$notes), ])

#creating column for lost leaf pulled, broken, missing
v3_data <- v3_data %>%
  mutate(
    leaf_lost = ifelse(grepl("pulled|broken|missing", notes, ignore.case = TRUE), 1, 0)
  )

#creating column for black spots
v3_data <- v3_data %>%
  mutate(
    Black_Spots = ifelse(grepl("spots|disease", notes, ignore.case = TRUE), 1, 0)
  )

#saving dataframe
write.csv(v3_data, "v3_stress_clean_10142026.csv", row.names = FALSE)




###combining cone, tray, and survey data for dates
colnames(v3_tray_raw_data)

#renaming tray data columns
colnames(v3_tray_raw_data)[1]<-"tray_objectID"
colnames(v3_tray_raw_data)[2]<-"tray_globalID"
colnames(v3_tray_raw_data)[3]<-"tray_label"
colnames(v3_tray_raw_data)[4]<-"tray_climate"
colnames(v3_tray_raw_data)[5]<-"tray_cone_num"
colnames(v3_tray_raw_data)[6]<-"tray_ParentGlobalID"
colnames(v3_tray_raw_data)[7]<-"tray_creationdate"
colnames(v3_tray_raw_data)[8]<-"tray_creator"
colnames(v3_tray_raw_data)[9]<-"tray_editdate"
colnames(v3_tray_raw_data)[10]<-"tray_editor"
colnames(v3_tray_raw_data)[11]<-"tray_num"

#combining by ParentGlobalID for v4_1_data and tray_globalID
v3_combined<-merge(v3_data,v3_tray_raw_data,by.x="ParentGlobalID",by.y="tray_globalID",all.x=TRUE,all.y=FALSE)

#combining with survey data
colnames(v3_survey_raw_data)
colnames(v3_survey_raw_data)[1]<-"survey_objectID"
colnames(v3_survey_raw_data)[2]<-"survey_globalID"
colnames(v3_survey_raw_data)[3]<-"survey_date_time"
colnames(v3_survey_raw_data)[4]<-"collector_name"
colnames(v3_survey_raw_data)[5]<-"survey_greenhouse_notes"
colnames(v3_survey_raw_data)[6]<-"data_type"
colnames(v3_survey_raw_data)[7]<-"num_trays_collecting"
colnames(v3_survey_raw_data)[8]<-"survey_creationdate"
colnames(v3_survey_raw_data)[9]<-"survey_creator"
colnames(v3_survey_raw_data)[10]<-"survey_editdate"
colnames(v3_survey_raw_data)[11]<-"survey_editor"
colnames(v3_survey_raw_data)[12]<-"survey_x_cord"
colnames(v3_survey_raw_data)[13]<-"survey_y_cord"


#combining by tray_ParentGlobalID and survey_globalID
v3_combined<-merge(v3_combined,v3_survey_raw_data,by.x="tray_ParentGlobalID",by.y="survey_globalID",all.x=TRUE,all.y=FALSE)

#saving data frame
write.csv(v3_combined, "v3_combined_10202026.csv", row.names = FALSE)


#checking for duplicates
!any(duplicated(v3_combined[, c("ID", "survey_creationdate")]))
#TRUE

##### DONE WITH V3 DATA ################





###### STARTING V2 DATA #######################################
#August 4th first normal data, before this just germiniation

#importing data
v2_cone_raw_data <- read_csv("Greenhouse_Survey_V2_raw_data_10142026/v2_greenhouse_raw_data.csv")
v2_tray_raw_data <- read_csv("Greenhouse_Survey_V2_raw_data_10142026/collect_data_for_each_tray_1.csv")
v2_survey_raw_data <- read_csv("Greenhouse_Survey_V2_raw_data_10142026/survey_0.csv")

#splitting germination data from normal data collection
v2_germination_data<-v2_cone_raw_data

#cleaning up unnecessary columns (basically only keeping germination as I want a solo germiniation dataframe)
colnames(v2_germination_data)
v2_germination_data <- subset(v2_germination_data, select = -c(`Height measurement`,`Height measurement 2`,
                                                       `Height measurement 3`,`Radius measurement`,
                                                       `Radius measurement 2`,`Radius measurement 3`,
                                                       `Is the plant dead or alive?`,
                                                       `What stress is visible?`,
                                                       `Wilting Intensity (1-5)`,
                                                       `Extent of shoot death (1-5)`,
                                                       `Discoloration Intensity (1-5)`,
                                                       `Creator`,
                                                       `Editor`, `Leaf count`,`Leaf count 2`, `Leaf count 3`))


#renaming columns
colnames(v2_germination_data)

colnames(v2_germination_data)[3]<-"ID"
colnames(v2_germination_data)[4]<-"germination"
colnames(v2_germination_data)[5]<-"notes"

#removing rows with no color (C_D etc)
v2_germination_data <- v2_germination_data %>% 
  filter(nchar(ID) >= 6)

#are there any notes?
which(!is.na(v2_germination_data$notes))

View(v2_germination_data[!is.na(v2_germination_data$notes), ])
#there's a lot of notes 1,275 will need to figure out a good way to go through them eek

#fixing dates up
v2_germination_data$date <- as.Date(v2_germination_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v2_germination_data[, c("ID", "date")]))
#FALSE

View(v2_germination_data %>%
       add_count(ID, date) %>%
       filter(n > 1))

#believe duplicates are because the time is very off, so thinks 
#some were done on the same day when they are not
#solved with survey creation date

#creating columns for values in ID
v2_germination_data <- v2_germination_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#saving data frame
write.csv(v2_germination_data, "v2_germination_data_10142026.csv", row.names = FALSE)


###combining cone, tray, and survey data for dates
colnames(v2_tray_raw_data)

#renaming tray data columns
colnames(v2_tray_raw_data)[1]<-"tray_objectID"
colnames(v2_tray_raw_data)[2]<-"tray_globalID"
colnames(v2_tray_raw_data)[3]<-"tray_label"
colnames(v2_tray_raw_data)[4]<-"tray_climate"
colnames(v2_tray_raw_data)[5]<-"tray_cone_num"
colnames(v2_tray_raw_data)[6]<-"tray_ParentGlobalID"
colnames(v2_tray_raw_data)[7]<-"tray_creationdate"
colnames(v2_tray_raw_data)[8]<-"tray_creator"
colnames(v2_tray_raw_data)[9]<-"tray_editdate"
colnames(v2_tray_raw_data)[10]<-"tray_editor"


#combining by ParentGlobalID for v4_1_data and tray_globalID
v2_germ_combined<-merge(v2_germination_data,v2_tray_raw_data,by.x="ParentGlobalID",by.y="tray_globalID",all.x=TRUE,all.y=FALSE)

#combining with survey data
colnames(v2_survey_raw_data)
colnames(v2_survey_raw_data)[1]<-"survey_objectID"
colnames(v2_survey_raw_data)[2]<-"survey_globalID"
colnames(v2_survey_raw_data)[3]<-"survey_date_time"
colnames(v2_survey_raw_data)[4]<-"collector_name"
colnames(v2_survey_raw_data)[5]<-"survey_greenhouse_notes"
colnames(v2_survey_raw_data)[6]<-"data_type"
colnames(v2_survey_raw_data)[7]<-"num_trays_collecting"
colnames(v2_survey_raw_data)[8]<-"survey_creationdate"
colnames(v2_survey_raw_data)[9]<-"survey_creator"
colnames(v2_survey_raw_data)[10]<-"survey_editdate"
colnames(v2_survey_raw_data)[11]<-"survey_editor"
colnames(v2_survey_raw_data)[12]<-"survey_x_cord"
colnames(v2_survey_raw_data)[13]<-"survey_y_cord"

#combining by tray_ParentGlobalID and survey_globalID
v2_germ_combined<-merge(v2_germ_combined,v2_survey_raw_data,by.x="tray_ParentGlobalID",by.y="survey_globalID",all.x=TRUE,all.y=FALSE)

#saving data frame
write.csv(v2_germ_combined, "v2_germ_combined_10202026.csv", row.names = FALSE)





#splitting normal data collection data from germination data
v2_data<-v2_cone_raw_data

#cleaning up unnecessary columns (basically only keeping germination as I want a solo germiniation dataframe)
colnames(v2_data)
v2_data <- subset(v2_data, select = -c(`Radius measurement`,`Radius measurement 2`,`Radius measurement 3`,
                                                               `Is the plant dead or alive?`,
                                                               `What stress is visible?`,
                                                               `Wilting Intensity (1-5)`,
                                                               `Extent of shoot death (1-5)`,
                                                               `Discoloration Intensity (1-5)`,
                                                               `Creator`,
                                                               `Editor`))


#renaming columns
colnames(v2_data)

colnames(v2_data)[3]<-"ID"
colnames(v2_data)[4]<-"germinated"
colnames(v2_data)[5]<-"height_one"
colnames(v2_data)[6]<-"height_two"
colnames(v2_data)[7]<-"height_three"
colnames(v2_data)[8]<-"notes"
colnames(v2_data)[12]<-"leaf_count_one"
colnames(v2_data)[13]<-"leaf_count_two"
colnames(v2_data)[14]<-"leaf_count_three"

#removing rows with no color (C_D etc)
v2_data <- v2_data %>% 
  filter(nchar(ID) >= 6)

#fixing dates up
v2_data$date <- as.Date(v2_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#removing data with only germination 
#heights start on the 29th (on the data sheet, but I think this is supposed to be the 28th and the dates are weird)
#height on 29th in AM heights (acutally 7/28), but 29th in PM is not (is actualy 7/29)
#heights also on 8/1 (in the AM) (which should be 7/31), but some do not have this data, since the dates are all mess up (8/1 PM)
#heights on 8/1 AM = 7/31, germination data in PM = 8/1
#8/2 missing?
#heights on 8/4 (correct day) - emergence stops this day
#8/5 has only germination data - think I did this to check after fertilizer application
#heights on 8/7 (correct day) - some on 8/8 but i think they are the same day 8/7 since is in the AM
#heights on 8/11 (correct day) - also has some on 8/12 but in AM so think its same day
#8/12 - thinned
#8/14 heights (correct day)
#heights on 8/18 (correct day)
#heights on 8/21 (correct day)
#dates until 8/21

v2_clean_data_test<-v2_clean_data

#can remove all before 7/29 because no height data collected before this
v2_clean_data_test <- v2_clean_data_test %>% 
  filter(date >= as.Date("2026-07-29"))

#data collection on 8/1 (AM) and 8/4 with germination data in between that needs to be removed?

#removing 7/29 PM data^, 7/30/2026 7:00:47 PM^, 8/1/2026 7:55:27 PM^, 8/3/2026 7:58:24 PM^, 
#8/3/2026 7:58:27 PM^, 8/5/2026 7:14:32 PM
unique(v2_clean_data_test$CreationDate)
#7/29/2026 10:37:29 PM

# 1. Convert your column to a proper date-time object (MDY + HMS format)
# Replace 'df' with your actual dataset name, and 'your_column' with your actual column name
v2_clean_data_test <- v2_clean_data_test %>%
  mutate(CreationDate = mdy_hms(CreationDate))

# 2. Define the exact date and time you want to remove
target_time <- mdy_hms("8/5/2026 7:14:32 PM")

# 3. Filter out that specific timestamp
v2_clean_data_test <- v2_clean_data_test %>%
  filter(CreationDate != target_time)

#should have 8*720 left = 5760 (correct)

#creating columns for values in ID
v2_data <- v2_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#are there any notes?
which(!is.na(v2_data$notes))

View(v2_data[!is.na(v2_data$notes), ])
#there's a lot of notes 1,194 will need to figure out a good way to go through them eek

#checking for duplicates
!any(duplicated(v2_data[, c("ID", "date")]))
#FALSE

View(v2_data %>%
       add_count(ID, date) %>%
       filter(n > 1))

#there are 540 duplicates, could be same issue where because times are weird, could be thinking 2 collected on same day
#solved with survey creation date!

#saving data frame
write.csv(v2_data, "v2_clean_data_10152026.csv", row.names = FALSE)




###combining cone, tray, and survey data for dates
colnames(v2_tray_raw_data)
v2_combined<-merge(v2_data,v2_tray_raw_data,by.x="ParentGlobalID",by.y="tray_globalID",all.x=TRUE,all.y=FALSE)

#combining with survey data
colnames(v2_survey_raw_data)
v2_combined<-merge(v2_combined,v2_survey_raw_data,by.x="tray_ParentGlobalID",by.y="survey_globalID",all.x=TRUE,all.y=FALSE)

#saving data frame
write.csv(v2_combined, "v2_combined_10202026.csv", row.names = FALSE)


#checking for duplicates
!any(duplicated(v2_combined[, c("ID", "survey_creationdate")]))
#TRUE

#yay solved duplicates issues!!!