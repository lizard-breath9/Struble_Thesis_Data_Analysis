#packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(tidyverse)



#importing data for V4.1 !!!!*******

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
cleaned_data<-v4_1_cleaned_data_10132026

View(cleaned_data[!is.na(cleaned_data$select_apply), ])

#cleaned_data <- cleaned_data %>% separate_longer_delim(cols = select_apply, delim = ",")

library(tidyverse)
library(stringr)
library(tidyverse)

# 1. Ensure your original data has a unique row identifier
cleaned_data <- cleaned_data %>% 
  mutate(row_id = row_number())

# 2. Extract just the values and pivot them (rows with NA will be ignored here)
specific_columns <- cleaned_data %>%
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
final_data <- cleaned_data %>%
  left_join(specific_columns, by = "row_id") %>% 
  select(-row_id) # Remove the temporary row ID


#changing NAs to zeros for these new columns
final_data$Artificial_Damage[is.na(final_data$Artificial_Damage)] <- 0
final_data$Black_Spots[is.na(final_data$Black_Spots)] <- 0
final_data$Leaning[is.na(final_data$Leaning)] <- 0
final_data$Bent[is.na(final_data$Bent)] <- 0
final_data$Standing_Water[is.na(final_data$Standing_Water)] <- 0


#saving dataframe
write.csv(final_data, "v4_1_stress_clean_10132026.csv", row.names = FALSE)

##

#adding lost leaf column

v4_1_clean<-final_data
View(v4_1_clean[!is.na(v4_1_clean$notes), ])

v4_1_clean <- v4_1_clean %>%
  mutate(
    leaf_lost = ifelse(grepl("leaf", notes, ignore.case = TRUE), 1, 0)
  )

###going through to check if it was an actual leaf lost or something else
#459 E_C_WY_D_1
v4_1_clean %>%
  filter(ObjectID == "459") %>%
  pull(leaf_lost) #good

#1823 P_C_WA_C_1
v4_1_clean %>%
  filter(ObjectID == "1823") %>%
  pull(notes) #Plant just one desiccated leaf not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "1823") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 1823] <- 0

#2810 P_C_ON_D_1
v4_1_clean %>%
  filter(ObjectID == "2810") %>%
  pull(notes) #Seed was resprouting with one prominent leaf, resulting in inaccurate leaf count not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "2810") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 2810] <- 0

#2815 E_C_ON_D_3
v4_1_clean %>%
  filter(ObjectID == "2815") %>%
  pull(notes) #Unsure about leaf count, bizarre plant not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "2815") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 2815] <- 0

#4096 E_C_ON_D_3
v4_1_clean %>%
  filter(ObjectID == "4096") %>%
  pull(notes) #Not sure about leaf count, bizarre plant not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "4096") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 4096] <- 0

#4194 E_C_C_H_5
v4_1_clean %>%
  filter(ObjectID == "4194") %>%
  pull(notes) #Leading leaf bent not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "4194") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 4194] <- 0

#4873 P_C_OZ_H_3
v4_1_clean %>%
  filter(ObjectID == "4873") %>%
  pull(notes) #Leaf was pinned between conetainers, desecration past where it was stuck not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "4873") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 4873] <- 0

#5856 E_C_ON_D_3
v4_1_clean %>%
  filter(ObjectID == "5856") %>%
  pull(notes) #Can't confirm leaf count not actually lost leaf

v4_1_clean %>%
  filter(ObjectID == "5856") %>%
  pull(leaf_lost) #1 incorrect
v4_1_clean$leaf_lost[v4_1_clean$ObjectID == 5856] <- 0

#saving this new data frame
write.csv(v4_1_clean, "v4_1_stress_clean_lostleafadded_10142026.csv", row.names = FALSE)






########## STARTING V4 DATA #######################################
v4_data<-collect_data_for_each_conetaine_2

#no munsell data
v4_data <- subset(v4_data, select=-c(`GlobalID`,`Common Colors (optional)`,`Hue (First #)`,`Letter Component of Hue`,`Value (Second #)`,`Chroma (Final #)`,ParentGlobalID, Creator, Editor))
v4_data <- subset(v4_data, select=-c(`# of seeds germinated`))
v4_data <- subset(v4_data, select=-c(`Height measurement 2`,`Height measurement 3`))

#renaming columns
colnames(v4_data)
colnames(v4_data)[2]<-"ID"
colnames(v4_data)[3]<-"leaf_count"
colnames(v4_data)[4]<-"height"
colnames(v4_data)[5]<-"turgor"
colnames(v4_data)[6]<-"wilting"
colnames(v4_data)[7]<-"discoloration/health"
colnames(v4_data)[8]<-"leaves_dessicated"
colnames(v4_data)[21]<-"select_apply"
colnames(v4_data)[22]<-"notes"

colnames(v4_data)[9]<-"leaf1_dessicated_per"
colnames(v4_data)[10]<-"leaf2_dessicated_per"
colnames(v4_data)[11]<-"leaf3_dessicated_per"
colnames(v4_data)[12]<-"leaf4_dessicated_per"
colnames(v4_data)[13]<-"leaf5_dessicated_per"
colnames(v4_data)[14]<-"leaf6_dessicated_per"
colnames(v4_data)[15]<-"leaf7_dessicated_per"
colnames(v4_data)[16]<-"leaf8_dessicated_per"
colnames(v4_data)[17]<-"leaf9_dessicated_per"
colnames(v4_data)[18]<-"leaf10_dessicated_per"
colnames(v4_data)[19]<-"leaf11_dessicated_per"
colnames(v4_data)[20]<-"leaf12_dessicated_per"

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
v4_cleaned_data<-v4_data

View(v4_cleaned_data[!is.na(v4_cleaned_data$select_apply), ])

# 1. Ensure your original data has a unique row identifier
v4_cleaned_data <- v4_cleaned_data %>% 
  mutate(row_id = row_number())

# 2. Extract just the values and pivot them (rows with NA will be ignored here)
specific_columns <- v4_cleaned_data %>%
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
v4_final_data <- v4_cleaned_data %>%
  left_join(specific_columns, by = "row_id") %>% 
  select(-row_id) # Remove the temporary row ID

#changing NAs to zeros for these new columns
v4_final_data$Artificial_Damage[is.na(v4_final_data$Artificial_Damage)] <- 0
v4_final_data$Bent[is.na(v4_final_data$Bent)] <- 0


#creating column for black spots
v4_final_data$Black_spots <- as.integer(!is.na(v4_final_data$notes))

#creating column for lost leaf
v4_final_data <- v4_final_data %>%
  mutate(
    leaf_lost = ifelse(grepl("leaf", notes, ignore.case = TRUE), 1, 0)
  )

#saving dataframe
write.csv(v4_final_data, "v4_stress_clean_addedlostleaf_10142026.csv", row.names = FALSE)





##########DONE WITH V4.1 anf V4#####################






################# V3 ###########################
#importing data
v3_data_raw <- read_csv("Greenhouse_Survey_V3_10142026/collect_data_for_each_conetaine_2.csv")


#checking out data
colnames(v3_data_raw)


#separating munsell from regular data

#sorting out munsell data from other and cleaning up
v3_munsell_data<-v3_data_raw

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
v3_cleaned_data<-v3_data_raw




#separating out munsell data from stress data
v3_cleaned_data <- v3_cleaned_data[is.na(v3_cleaned_data$`Hue (First #)`), ]


#removing unnecessary columns
colnames(v3_cleaned_data)
v3_cleaned_data <- subset(v3_cleaned_data, select=-c(`GlobalID`,`# of seeds germinated`,
                                                     `Height measurement 2`,
                                                      `Height measurement 3`,`Hue (First #)`,
                                                     `Letter Component of Hue`,
                                                     `Value (Second #)`,`Chroma (Final #)`,
                                                     ParentGlobalID, Creator, Editor,
                                                     `Common Colors (optional)`))
v3_cleaned_data <- subset(v3_cleaned_data, select=-c(`Radius measurement`,`Radius measurement 2`,
                                                     `Radius measurement 3`))

#renaming columns
colnames(v3_cleaned_data)

colnames(v3_cleaned_data)[2]<-"ID"
colnames(v3_cleaned_data)[3]<-"height"
colnames(v3_cleaned_data)[4]<-"wilting"
colnames(v3_cleaned_data)[5]<-"dessication_extent"
colnames(v3_cleaned_data)[6]<-"discoloration/health"
colnames(v3_cleaned_data)[7]<-"select_apply"
colnames(v3_cleaned_data)[8]<-"notes"
colnames(v3_cleaned_data)[11]<-"leaf_count"

#removing rows with no plants (C_D etc)
v3_cleaned_data <- v3_cleaned_data %>% 
  filter(nchar(ID) >= 6)

#fixing dates up
v3_cleaned_data$date <- as.Date(v3_cleaned_data$CreationDate, format = "%m/%d/%Y %H:%M:%S")

#checking for duplicates
!any(duplicated(v3_cleaned_data[, c("ID", "date")]))
#TRUE

#creating columns for values in ID
v3_cleaned_data <- v3_cleaned_data %>%
  separate_wider_delim(
    cols = ID, 
    delim = "_", 
    names = c("species", "soil_treatment", "seed_coating","weather_treatment","rep"),
    cols_remove = FALSE # Keeps the original column
  )

#creating categories for each of the select all options
View(v3_cleaned_data[!is.na(v3_cleaned_data$select_apply), ])

# 1. Ensure your original data has a unique row identifier
v3_test_data<-v3_cleaned_data

v3_test_data <- v3_test_data %>% 
  mutate(row_id = row_number())

# 2. Extract just the values and pivot them (rows with NA will be ignored here)
specific_columns <- v3_test_data %>%
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
v3_test_data <- v3_test_data %>%
  left_join(specific_columns, by = "row_id") %>% 
  select(-row_id) # Remove the temporary row ID

#changing NAs to zeros for these new columns
v3_test_data$Artificial_Damage[is.na(v3_test_data$Artificial_Damage)] <- 0
v3_test_data$Bent[is.na(v3_test_data$Bent)] <- 0
v3_test_data$Leaning[is.na(v3_test_data$Leaning)] <- 0
v3_test_data$Standing_Water[is.na(v3_test_data$Standing_Water)] <- 0

#checking notes
#are there any notes?
which(!is.na(v3_test_data$notes))
View(v3_test_data[!is.na(v3_test_data$notes), ])

#creating column for lost leaf pulled, broken, missing
v3_test_data <- v3_test_data %>%
  mutate(
    leaf_lost = ifelse(grepl("pulled|broken|missing", notes, ignore.case = TRUE), 1, 0)
  )

#creating column for black spots
v3_test_data <- v3_test_data %>%
  mutate(
    Black_Spots = ifelse(grepl("spots|disease", notes, ignore.case = TRUE), 1, 0)
  )

#saving dataframe
write.csv(v3_test_data, "v3_stress_clean_10142026.csv", row.names = FALSE)


##### DONE WITH V3 DATA ################
