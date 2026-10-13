#packages
library(dplyr)
library(tidyr)




#importing data

collect_data_for_each_conetaine_2 <- read_csv("C:/Users/strublee/Desktop/Oregon State University/Thesis/Greenhouse/Data/Greenhouse_Survey_v4.1_10012026/collect_data_for_each_conetaine_2.csv")

#other ones looking weird so just going to use this one for now.

v4.1_data<-collect_data_for_each_conetaine_2


#sorting out munsell data from other and cleaning up
v4.1_munsell_data<-v4.1_data

v4.1_munsell_data <- v4.1_munsell_data[!is.na(v4.1_munsell_data$`Hue (First #)`), ]

v4.1_munsell_data <- subset(v4.1_munsell_data, select = -c(`# of seeds germinated`, `Leaf Count`,`Height measurement`,`Height measurement 2`,`Height measurement 3`,`Overall Plant Turgor (1-5)`,`Wilting Intensity (1-5)`,`Discoloration/Health (1-5)`,`# leaves showing desiccation`,`% desiccation of leaf...18`,`% desiccation of leaf...19`,`% desiccation of leaf...20`,`% desiccation of leaf...21`,`% desiccation of leaf...22`,`% desiccation of leaf...23`,`% desiccation of leaf...24`,`% desiccation of leaf...25`,`% desiccation of leaf...26`,`% desiccation of leaf...27`,`% desiccation of leaf...28`,`% desiccation of leaf...29`,`Select All That Apply`))

v4.1_munsell_data <- subset(v4.1_munsell_data, select=-c(`ParentGlobalID`, `Editor`,`Creator`))

v4.1_munsell_data <- subset(v4.1_munsell_data, select=-c(`GlobalID`))

colnames(v4.1_munsell_data)[2]<-"ID"

colnames(v4.1_munsell_data)[3]<-"Common Colors (optional)"

v4.1_munsell_data <- v4.1_munsell_data %>%
  unite("Color", `Hue (First #)`, `Letter Component of Hue`, `Value (Second #)`, `Chroma (Final #)`,sep = "_", na.rm = TRUE, remove = FALSE)

v4.1_munsell_data <- subset(v4.1_munsell_data, select=-c(`Common Colors (optional)`))

#are there any notes?
which(!is.na(v4.1_munsell_data$`Any notes for this plant?`))
#2564

colnames(v4.1_munsell_data)[8]<-"notes"

v4.1_munsell_data[2564, "notes"]
#Pbnvh4 7.5GY5/6 ( not included for some reason)
v4.1_munsell_data[2564, "Color"] <- "7.5_GY_5_6"
v4.1_munsell_data[2564, "Color"]
#7.5_GY_5_6

#removing rows with no color (C_D etc)
colnames(v4.1_munsell_data)[4]<-"Hue#"
colnames(v4.1_munsell_data)[5]<-"Hue_letter"
colnames(v4.1_munsell_data)[6]<-"Value"
colnames(v4.1_munsell_data)[7]<-"Chroma"

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


#sorting out regularly collected data from munsell

v4.1_regular_data <- v4.1_data[is.na(v4.1_data$`Hue (First #)`), ]

v4.1_regular_data <- subset(v4.1_regular_data, select=-c(`GlobalID`,`Common Colors (optional)`,`Hue (First #)`,`Letter Component of Hue`,`Value (Second #)`,`Chroma (Final #)`,ParentGlobalID, Creator, Editor))
v4.1_regular_data <- subset(v4.1_regular_data, select=-c(germinated))
v4.1_regular_data <- subset(v4.1_regular_data, select=-c(`Height measurement 2`,`Height measurement 3`))

#renaming columns
colnames(v4.1_regular_data)
colnames(v4.1_regular_data)[2]<-"ID"
colnames(v4.1_regular_data)[3]<-"leaf_count"
colnames(v4.1_regular_data)[4]<-"height"
colnames(v4.1_regular_data)[5]<-"turgor"
colnames(v4.1_regular_data)[6]<-"wilting"
colnames(v4.1_regular_data)[7]<-"discoloration/health"
colnames(v4.1_regular_data)[8]<-"leaves_dessicated"
colnames(v4.1_regular_data)[21]<-"select_apply"
colnames(v4.1_regular_data)[22]<-"notes"

max(v4.1_regular_data$leaves_dessicated, na.rm = TRUE)
#12

colnames(v4.1_regular_data)[9]<-"leaf1_dessicated_per"
colnames(v4.1_regular_data)[10]<-"leaf2_dessicated_per"
colnames(v4.1_regular_data)[11]<-"leaf3_dessicated_per"
colnames(v4.1_regular_data)[12]<-"leaf4_dessicated_per"
colnames(v4.1_regular_data)[13]<-"leaf5_dessicated_per"
colnames(v4.1_regular_data)[14]<-"leaf6_dessicated_per"
colnames(v4.1_regular_data)[15]<-"leaf7_dessicated_per"
colnames(v4.1_regular_data)[16]<-"leaf8_dessicated_per"
colnames(v4.1_regular_data)[17]<-"leaf9_dessicated_per"
colnames(v4.1_regular_data)[18]<-"leaf10_dessicated_per"
colnames(v4.1_regular_data)[19]<-"leaf11_dessicated_per"
colnames(v4.1_regular_data)[20]<-"leaf12_dessicated_per"

#changing NA in leaf_dessicated % to 0

v4.1_regular_data$leaf1_dessicated_per[is.na(v4.1_regular_data$leaf1_dessicated_per)] <- 0
v4.1_regular_data$leaf2_dessicated_per[is.na(v4.1_regular_data$leaf2_dessicated_per)] <- 0
v4.1_regular_data$leaf3_dessicated_per[is.na(v4.1_regular_data$leaf3_dessicated_per)] <- 0
v4.1_regular_data$leaf4_dessicated_per[is.na(v4.1_regular_data$leaf4_dessicated_per)] <- 0
v4.1_regular_data$leaf5_dessicated_per[is.na(v4.1_regular_data$leaf5_dessicated_per)] <- 0
v4.1_regular_data$leaf6_dessicated_per[is.na(v4.1_regular_data$leaf6_dessicated_per)] <- 0
v4.1_regular_data$leaf7_dessicated_per[is.na(v4.1_regular_data$leaf7_dessicated_per)] <- 0
v4.1_regular_data$leaf8_dessicated_per[is.na(v4.1_regular_data$leaf8_dessicated_per)] <- 0
v4.1_regular_data$leaf9_dessicated_per[is.na(v4.1_regular_data$leaf9_dessicated_per)] <- 0
v4.1_regular_data$leaf10_dessicated_per[is.na(v4.1_regular_data$leaf10_dessicated_per)] <- 0
v4.1_regular_data$leaf11_dessicated_per[is.na(v4.1_regular_data$leaf11_dessicated_per)] <- 0
v4.1_regular_data$leaf12_dessicated_per[is.na(v4.1_regular_data$leaf12_dessicated_per)] <- 0

#are there any notes?
which(!is.na(v4.1_regular_data$notes))
v4.1_regular_data$notes[!is.na(v4.1_regular_data$notes)]
View(v4.1_regular_data[!is.na(v4.1_regular_data$notes), ])

v4.1_regular_data %>%
  filter(ObjectID == "3512") %>%
  pull(notes)

v4.1_regular_data_test<-v4.1_regular_data
#P_B_ON_A_1 14 leaves
v4.1_regular_data$leaf_count[v4.1_regular_data$ObjectID == 3512] <- 14
v4.1_regular_data %>% filter(ObjectID == 3512) #correctly changed

v4.1_regular_data %>%
  filter(ObjectID == "3512") %>%
  pull(notes)

#P_C_OZ_C_3 18 leaves
v4.1_regular_data %>%
  filter(ObjectID == "5752") %>%
  pull(notes)
v4.1_regular_data$leaf_count[v4.1_regular_data$ObjectID == 5752] <- 18
v4.1_regular_data %>% filter(ObjectID == 5752) #correctly changed

#P_B_ID_C_2 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6200") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6200] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 6200) #correctly changed

#P_C_OZ_C_1 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6465") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6465] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 6465) #correctly changed

#6473	P_C_OZ_C_4 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6473") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6473] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 6473) #correctly changed

#6504	P_C_ID_C_4 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6504") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6504] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 6504) #correctly changed

#6607	P_C_OZ_C_3 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6607") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6607] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 6607) #correctly changed

#6634	P_C_X_H_3 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6634") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6634] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 6634) #correctly changed

#6711	P_B_ON_C_4 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6711") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6711] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 6711) #correctly changed

#6732	P_B_OZ_C_2 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6732") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6732] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 6732) #correctly changed

#6790	P_B_ON_A_1 20 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6790") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6790] <- 20
v4.1_regular_data_test %>% filter(ObjectID == 6790) #correctly changed

#6849	P_B_S_H_1 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "6849") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 6849] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 6849) #correctly changed

#7056	P_B_ID_C_2 20 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7056") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7056] <- 20
v4.1_regular_data_test %>% filter(ObjectID == 7056) #correctly changed

#7074	P_B_OZ_C_3 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7074") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7074] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7074) #correctly changed

#7109	P_C_ON_A_5 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7109") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7109] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7109) #correctly changed

#7183	P_B_S_H_1 20 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7183") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7183] <- 20
v4.1_regular_data_test %>% filter(ObjectID == 7183) #correctly changed

#7186	P_B_WY_H_2 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7186") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7186] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7186) #correctly changed

#7218	P_B_NV_H_5 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7218") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7218] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7218) #correctly changed

#7221	P_B_ON_H_4 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7221") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7221] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7221) #correctly changed

#7224	P_B_OZ_H_3 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7224") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7224] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7224) #correctly changed

#7234	P_B_ON_C_4 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7234") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7234] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7234) #correctly changed

#7252	P_B_C_C_2 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7252") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7252] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7252) #correctly changed

#7255	P_B_OZ_C_2 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7255") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7255] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7255) #correctly changed

#7295	P_B_C_A_4 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7295") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7295] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7295) #correctly changed

#7313	P_B_ON_A_1 25 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7313") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7313] <- 25
v4.1_regular_data_test %>% filter(ObjectID == 7313) #correctly changed

#7341	P_B_ID_C_2 24 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7341") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7341] <- 24
v4.1_regular_data_test %>% filter(ObjectID == 7341) #correctly changed

#7359	P_B_OZ_C_3 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7359") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7359] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7359) #correctly changed

#7408	P_C_X_D_3 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7408") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7408] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7408) #correctly changed

#7453	P_C_OZ_A_3 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7453") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7453] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7453) #correctly changed

#7462	P_C_OZ_C_1 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7462") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7462] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7462) #correctly changed

#7470	P_C_OZ_C_4 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7470") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7470] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7470) #correctly changed

#7472	P_C_ID_C_5 20 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7472") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7472] <- 20
v4.1_regular_data_test %>% filter(ObjectID == 7472) #correctly changed

#7474	P_C_WA_C_4 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7474") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7474] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7474) #correctly changed

#7501	P_C_ID_C_4 20 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7501") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7501] <- 20
v4.1_regular_data_test %>% filter(ObjectID == 7501) #correctly changed

#7531	P_B_S_H_5 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7531") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7531] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7531) #correctly changed

#7569	P_B_X_D_4 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7569") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7569] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7569) #correctly changed

#7615	P_B_OZ_A_4 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7615") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7615] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7615) #correctly changed

#7681	P_C_NV_C_1 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7681") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7681] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7681) #correctly changed

#7700	P_C_OZ_C_3 21 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7700") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7700] <- 21
v4.1_regular_data_test %>% filter(ObjectID == 7700) #correctly changed

#7710	P_C_WA_H_3 18 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7710") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7710] <- 18
v4.1_regular_data_test %>% filter(ObjectID == 7710) #correctly changed

#7727	P_C_X_H_3 19 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7727") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7727] <- 19
v4.1_regular_data_test %>% filter(ObjectID == 7727) #correctly changed

#7805	P_C_ON_H_3 17 leaves
v4.1_regular_data_test %>%
  filter(ObjectID == "7805") %>%
  pull(notes)
v4.1_regular_data_test$leaf_count[v4.1_regular_data_test$ObjectID == 7805] <- 17
v4.1_regular_data_test %>% filter(ObjectID == 7805) #correctly changed




#fixing dates up
v4.1_regular_data_test$date <- as.Date(v4.1_regular_data_test$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates

!any(duplicated(v4.1_regular_data_test[, c("ID", "date")]))
#FALSE

View(v4.1_regular_data_test %>%
  add_count(ID, date) %>%
  filter(n > 1))

#creating columns for values in ID

v4.1_regular_data_test <- v4.1_regular_data_test %>% 
  filter(nchar(ID) >= 6)

v4.1_regular_data_test <- v4.1_regular_data_test %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )


#saving dataframes
write.csv(v4.1_munsell_data, "v4.1_munsell_data_10132026.csv", row.names = FALSE)
write.csv(v4.1_regular_data_test, "v4.1_cleaned_data_10132026.csv", row.names = FALSE)


duplicates<-(v4.1_regular_data_test %>%
       add_count(ID, date) %>%
       filter(n > 1))

write.csv(duplicates, "v4.1_duplicates_10132026.csv", row.names = FALSE)



#creating categories for each of the select all options
#blah
