data_adding <- read_csv("data_added_plant_desiccation_10272026.csv")

#leaf counts
pdf("leaf_count_plots.pdf", width = 8, height = 5)

for (id in unique(data$ID)) {
  
  print(
    ggplot(subset(data, ID == id),
           aes(x = date, y = leaf_count, color=collector_name, group=1)) +
      geom_point() +  geom_line()+
      scale_x_datetime(breaks = data$date[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()

#weird ELEL
#E_B_C_A_3,E_B_C_C_1,E_B_C_C_3, E_B_C_D_1, E_B_C_H_2, E_B_C_H_5
#E_B_ID_A_3,E_B_ID_A_5,E_B_ID_C_1,E_B_ID_C_4, E_B_ID_D_2, E_B_ID_D_4,E_B_ID_H_2
#E_B_ID_H_3, E_B_NV_C_4,E_B_NV_H_2,E_B_ON_A_3, E_B_ON_C_2, E_B_ON_C_4
#E_B_S_A_1,E_B_S_A_4,E_B_S_C_1, E_B_S_C_3,E_B_S_D_3, E_B_S_D_5,
#E_B_S_H_3,E_B_WA_A_1,E_B_WA_A_5,E_B_WA_C_1, E_B_WA_H_2, E_B_WA_H_3,E_B_WA_H_5
#E_B_WY_D_1, E_B_WY_D_3,E_B_WY_H_1, E_B_WY_H_4,E_B_X_A_1, E_B_X_A_3,E_B_X_A_4
#E_B_X_A_5,E_B_X_C_4,E_B_X_H_3, E_C_C_A_1,E_C_C_A_3,E_C_C_A_4,E_C_C_C_2,
#E_C_C_C_3, E_C_C_D_2,E_C_C_H_2,E_C_ID_A_1,E_C_ID_A_2,E_C_ID_D_2,E_C_ID_H_3
#E_C_ID_H_4,E_C_NV_C_2,E_C_NV_C_5, E_C_NV_D_5,E_C_NV_H_2, 
#E_C_ON_C_1, E_C_ON_D_1,E_C_ON_D_3, E_C_ON_H_5,E_C_OZ_A_1,
#E_C_OZ_A_3,E_C_OZ_C_1, E_C_OZ_C_2,E_C_OZ_D_2, E_C_OZ_D_4, 
#E_C_OZ_H_4,E_C_S_C_3,E_C_S_D_2, E_C_S_H_1, E_C_S_H_2,E_C_S_H_3, E_C_S_H_4
#E_C_WA_A_2, E_C_WA_C_1, E_C_WA_C_2, E_C_WA_C_4, E_C_WA_D_4, E_C_WA_D_5,
#E_C_WA_H_3,E_C_WA_H_4, E_C_WY_A_1, E_C_WY_A_3, E_C_WY_A_4, E_C_WY_C_2,
#E_C_WY_C_3, E_C_WY_D_4, E_C_WY_H_5, E_C_X_A_1, E_C_X_A_4,E_C_X_A_5, E_C_X_C_2, E_C_X_H_5

#weird POSE
#P_B_C_D_1, P_B_C_D_2, P_B_C_D_3, P_B_C_D_5, P_B_ID_A_3, P_B_ID_C_5, P_B_ID_D_2
#P_B_ID_H_4, P_B_NV_A_2, P_B_NV_A_3, P_B_NV_C_3, P_B_NV_C_5, P_B_NV_D_3,
#P_B_ON_A_3, P_B_ON_H_1, P_B_ON_H_2, P_B_ON_H_3, P_B_ON_H_5, P_B_OZ_C_5,
#P_B_S_C_1,P_B_S_C_5, P_B_S_D_4, P_B_WA_A_4, P_B_WA_D_1, P_B_WA_H_2,
#P_B_WY_A_2, P_B_WY_A_3, P_B_WY_D_5, P_B_WY_H_4, P_B_X_C_1, P_B_X_D_3, 
#P_B_X_H_2, P_C_C_C_1, P_C_C_D_2, P_C_C_D_4, P_C_C_H_4, P_C_ID_C_4, 
#P_C_NV_C_1, P_C_NV_D_3, P_C_NV_D_5, P_C_NV_H_3, P_C_NV_H_4, P_C_ON_A_2, 
#P_C_ON_A_5, P_C_ON_C_1, P_C_ON_D_2, P_C_OZ_A_1, P_C_OZ_A_2, P_C_OZ_A_4, 
#P_C_OZ_C_3, P_C_OZ_H_2, P_C_OZ_H_5, P_C_S_A_2, P_C_S_D_1, P_C_S_D_2, 
#P_C_S_D_4, P_C_S_D_5, P_C_S_H_5, P_C_WA_H_3, P_C_WY_C_3, P_C_WY_D_2, 
#P_C_X_C_5, P_C_X_H_1, P_C_X_H_3


#before changing anything need to subtract 1 from all leaf counts
data_adding$leaf_count <- ifelse(data_adding$leaf_count > 0, 
                                 data_adding$leaf_count - 1, data_adding$leaf_count)

#leaf count messed up for few data points after thinning before changing surveys
#8/14, 8/18/, 8/21

data_adding$leaf_count <- ifelse(as.character(data_adding$date) %in% c("2026-08-14", "2026-08-18", "2026-08-21"),
                                 data_adding$leaf_count_one -1,
                                 data_adding$leaf_count)
#fixed yay
data_adding$date <- as.POSIXct(
  data_adding$date, 
  format = "%m/%d/%Y %H:%M:%S")

data_adding <- data_adding[order(data_adding$date), ]


#went through each above using this code, there are a lot so didn't want to keep each
#thinned after 8/11 so if leaf counts are odd directly after this date, that is okay
#before 8/11 was an average, but the one plant kept may of had less leaves (chose the 
#tallest plant, did not account for leaf #) 
#also if there are some zero up downs before this date that is okay too some days only
#collected germination data

data_adding %>%
  filter(ID == "P_C_X_H_3") %>%
  pull(notes) #check notes to see if any where cut off or things like this (maybe a real decrease in leaf count)

ggplot(subset(data_adding, ID == "P_C_X_H_3"), 
       aes(x = date, y = leaf_count, color=collector_name, group=1)) +
  geom_point() + geom_line()+
  scale_x_datetime(breaks = data_adding$date[data_adding$ID == "P_C_X_H_3"], 
                   date_labels = "%Y-%m-%d") +
  theme_minimal() +
  labs(title = "Leaf Count over Time (ID: P_C_X_H_3)",
       x = "Date",
       y = "Leaf Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #graphing to view 

data_adding$leaf_count[data_adding$ID == "P_C_X_H_3" & 
                         data_adding$date == as.Date("2026-08-21")] <- 3 #changing leaf count if necessary



#E_B_ON_A_3 has duplicate?
!any(duplicated(data_adding[, c("ID", "date")]))
View(data_adding %>%
       add_count(ID, date) %>%
       filter(n > 1))

#one with 4 leaves makes more sense, so renaming other one it may go with another day/data point
data_adding$ID[data_adding$ID == "E_B_ON_A_3" & 
                 data_adding$date == as.Date("2026-09-08") & 
                 data_adding$ObjectID == 3474] <- "unknown_E_B_ON_A_3"

#fixed plots
pdf("fixed_leaf_count_plots.pdf", width = 8, height = 5)

for (id in unique(data_adding$ID)) {
  
  print(
    ggplot(subset(data_adding, ID == id),
           aes(x = date, y = leaf_count, color=collector_name, group=1)) +
      geom_point() +  geom_line()+
      scale_x_datetime(breaks = data_adding$data_adding[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()

write.csv(data_adding, "corrected_leaf_counts_data_10302026.csv", row.names=FALSE)


#height

pdf("height_plots.pdf", width = 8, height = 5)

for (id in unique(data_adding$ID)) {
  
  print(
    ggplot(subset(data_adding, ID == id),
           aes(x = date, y = height, color=collector_name, group=1)) +
      geom_point() +  geom_line()+
      scale_x_datetime(breaks = data_adding$data_adding[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()

#E_B_ON_A_4, E_C_ON_C_5, E_C_ON_H_1, E_C_X_A_2, P_B_ID_A_3, P_B_NV_A_5, P_B_OZ_D_1
#P_B_S_A_3, P_B_X_C_3, P_C_ID_A_4, P_C_NV_D_5, P_C_ON_D_4, P_C_ON_H_2, P_C_X_C_4
#heights are all over the place but i'm going to leave them alone

#turgor
pdf("turgor_plots2.pdf", width = 8, height = 5)

for (id in unique(data_adding$ID)) {
  
  print(
    ggplot(subset(data_adding, ID == id),
           aes(x = date, y = turgor, color=collector_name, group=1)) +
      geom_point() + ylim(1, 5)+ geom_line()+
      scale_x_datetime(breaks = data_adding$data_adding[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()

#go up and down but don't think there are any worth changing

#wilting
pdf("wilting_plots.pdf", width = 8, height = 5)

for (id in unique(data_adding$ID)) {
  
  print(
    ggplot(subset(data_adding, ID == id),
           aes(x = date, y = wilting, color=collector_name, group=1)) +
      geom_point() + ylim(1, 5)+ geom_line()+
      scale_x_datetime(breaks = data_adding$data_adding[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()
#also goes up/down a lot, but don't think I should change any


#discoloration/health
pdf("discoloration_health_plots.pdf", width = 8, height = 5)

for (id in unique(data_adding$ID)) {
  
  print(
    ggplot(subset(data_adding, ID == id),
           aes(x = date, y = `discoloration/health`, color=collector_name, group=1)) +
      geom_point() + ylim(1, 5)+ geom_line()+
      scale_x_datetime(breaks = data_adding$data_adding[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()
#mostly ones



#discoloration/health
pdf("leaves_desiccated_plots.pdf", width = 8, height = 5)

for (id in unique(data_adding$ID)) {
  
  print(
    ggplot(subset(data_adding, ID == id),
           aes(x = date, y = leaves_desiccated, color=collector_name, group=1)) +
      geom_point()+ geom_line()+
      scale_x_datetime(breaks = data_adding$data_adding[data$ID == id], 
                       date_labels = "%Y-%m-%d") +
      theme_minimal() +
      labs(title = id) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

dev.off()
#going to leave this alone as well

#going to recalculate growth rate?
data_adding<- data_adding %>%
  group_by(ID) %>%                         # Group by individual if tracking multiple subjects
  arrange(date) %>%                        # Ensure chronological order
  mutate(
    diff_height = height - lag(height),   # Change in height
    diff_date = as.numeric(date - lag(date), units = "days"),         # Change in time
    growth_rate = diff_height / diff_date) # Rate of growth)
%>%
  ungroup()

#calculate early growth rate
data_adding <- subset(data_adding, ID != "unknown_E_B_ON_A_3") %>%
  group_by(ID) %>%
  mutate(e_diff_height = height[date == as.Date("2026-08-25")] -  
           height[date == as.Date("2026-07-21")],
    e_diff_date = as.numeric(as.Date("2026-08-25") - as.Date("2026-07-21")),
    early_growth = e_diff_height / e_diff_date) %>%
  ungroup()

#calculate growth rate during stress
data_adding <- subset(data_adding, ID != "unknown_E_B_ON_A_3") %>%
  group_by(ID) %>%
  mutate(s_diff_height = height[date == as.Date("2026-09-15")] -  
           height[date == as.Date("2026-08-25")],
         s_diff_date = as.numeric(as.Date("2026-09-15") - as.Date("2026-08-25")),
         stress_growth = s_diff_height / s_diff_date) %>%
  ungroup()

#calculate growth rate during recovery
data_adding <- subset(data_adding, ID != "unknown_E_B_ON_A_3") %>%
  group_by(ID) %>%
  mutate(r_diff_height = height[date == as.Date("2026-09-29")] -  
           height[date == as.Date("2026-09-15")],
         r_diff_date = as.numeric(as.Date("2026-09-29") - as.Date("2026-09-15")),
         recovery_growth = r_diff_height / r_diff_date) %>%
  ungroup()

#saving 
write.csv(data_adding, "added_growth_rates_data_10302026.csv", row.names=FALSE)
