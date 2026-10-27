#creating column for plant_desiccation
test<-data
test$leaf12_dessicated_per[is.na(test$leaf12_dessicated_per)] <- 0 #change NAs to 0
test <- test %>%
  mutate(plant_desiccation = rowSums(pick(leaf1_dessicated_per, leaf2_dessicated_per,
                                          leaf3_dessicated_per,leaf4_dessicated_per,
                                          leaf5_dessicated_per,leaf6_dessicated_per,
                                          leaf7_dessicated_per,leaf8_dessicated_per,
                                          leaf9_dessicated_per,leaf10_dessicated_per,
                                          leaf11_dessicated_per,leaf12_dessicated_per)/leaf_count, 
                                     na.rm = TRUE))

#creating data sets for before weather treatment #date 8/21
before_weather <- subset(test, date == as.Date("2026-08-21"))

#creating data sets for final weather treatment #date 9/15
after_weather <- subset(test, date == as.Date("2026-09-15"))

#final data
final_data <- subset(test, date == as.Date("2026-09-29"))

#saving these data frames
write.csv(test, "data_added_plant_desiccation_10272026.csv", row.names=FALSE)
write.csv(before_weather, "before_weather_10272026.csv", row.names=FALSE)
write.csv(after_weather, "after_weather_10272026.csv", row.names=FALSE)
write.csv(final_data, "final_data_10272026.csv", row.names=FALSE)


#starting with before weather
summary(before_weather$height[before_weather$species == "P"]) #E+P look fine
hist(before_weather$height[before_weather$species == "E"]) #p looks pretty normal, E a bit left skewed

m0<-glm(height ~ 1, family = gaussian, 
        data = subset(before_weather, species == "P"))
m1<-glm(height ~ seed_coating*weather_treatment*soil_treatment, family = gaussian, 
        data = subset(before_weather, species == "P"))
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#height ~ soil_treatment
step.backward <- step(m1, 
                      scope = formula(m0), 
                      direction = "backward")
#height ~ weather_treatment + soil_treatment + weather_treatment:soil_treatment

model<-glm(height ~ soil_treatment*weather_treatment+weather_treatment+soil_treatment, family = gaussian, 
           data = subset(before_weather, species == "P"))
summary(model)


##############after_weather
summary(after_weather$height[after_weather$species == "E"]) #E+P look fine
hist(after_weather$height[after_weather$species == "P"]) #both look pretty normal

m0<-glm(height ~ 1, family = gaussian, 
        data = subset(after_weather, species == "P"))
m1<-glm(height ~ seed_coating*weather_treatment*soil_treatment, family = gaussian, 
        data = subset(after_weather, species == "P"))
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#height ~ weather_treatment
step.backward <- step(m1, 
                     scope = formula(m0), 
                     direction = "backward")
#height ~ weather_treatment

model<-glm(height ~ weather_treatment, family = gaussian, 
           data = subset(after_weather, species == "P"))
summary(model)


#######final data
summary(final_data$height[final_data$species == "P"]) #E+P look fine
hist(final_data$height[final_data$species == "E"]) #both look pretty normal, maybe a bit left skew

m0<-glm(height ~ 1, family = gaussian, 
        data = subset(final_data, species == "P"))
m1<-glm(height ~ seed_coating*weather_treatment*soil_treatment, family = gaussian, 
        data = subset(final_data, species == "P"))
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#height ~ weather_treatment + soil_treatment + weather_treatment:soil_treatment
step.backward <- step(m1, 
                      scope = formula(m0), 
                      direction = "backward")
#height ~ weather_treatment + soil_treatment + weather_treatment:soil_treatment

model<-glm(height ~ soil_treatment*weather_treatment+weather_treatment+soil_treatment, family = gaussian, 
           data = subset(final_data, species == "P"))
summary(model)








#leaf_count
#starting with before weather
summary(before_weather$leaf_count[before_weather$species == "E"]) #E+P look fine
hist(before_weather$leaf_count[before_weather$species == "P"]) #mostly 3&4

m0<-glm(leaf_count ~ 1, family = quasipoisson, 
        data = subset(before_weather, species == "P"))
m1<-glm(leaf_count ~ seed_coating*weather_treatment*soil_treatment, family = quasipoisson, 
        data = subset(before_weather, species == "P"))
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
add1(m0, scope = formula(m1), test = "F")
m2<-glm(leaf_count ~ soil_treatment, family = quasipoisson, 
        data = subset(before_weather, species == "P"))
add1(m2, scope = formula(m1), test = "F")
m3<-glm(leaf_count ~ soil_treatment+weather_treatment, family = quasipoisson, 
        data = subset(before_weather, species == "P"))
add1(m3, scope = formula(m1), test = "F")
m4<-glm(leaf_count ~ soil_treatment+weather_treatment+soil_treatment*weather_treatment, 
        family = quasipoisson, 
        data = subset(before_weather, species == "P"))
add1(m4, scope = formula(m1), test = "F") #good
#leaf_count ~ soil_treatment+weather_treatment+soil_treatment*weather_treatment,



##############after_weather
summary(after_weather$leaf_count[after_weather$species == "E"]) #E+P look fine
hist(after_weather$leaf_count[after_weather$species == "E"]) 

m0<-glm(leaf_count ~ 1, family = poisson, 
        data = subset(after_weather, species == "P"))
m1<-glm(leaf_count ~ seed_coating*weather_treatment*soil_treatment, family = poisson, 
        data = subset(after_weather, species == "P"))
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#leaf_count ~ weather_treatment
step.backward <- step(m1, 
                      scope = formula(m0), 
                      direction = "backward")
#leaf_count ~ weather_treatment
model<-glm(leaf_count ~ weather_treatment, family = poisson, 
           data = subset(after_weather, species == "P"))
summary(model)


#######final data
summary(final_data$leaf_count[final_data$species == "P"]) #E+P look fine
hist(final_data$leaf_count[final_data$species == "P"]) #both look pretty normal, maybe a bit left skew

m0<-glm(leaf_count ~ 1, family = poisson, 
        data = subset(final_data, species == "P"))
m1<-glm(leaf_count ~ seed_coating*weather_treatment*soil_treatment, family = poisson, 
        data = subset(final_data, species == "P"))
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#leaf_count ~ weather_treatment
step.backward <- step(m1, 
                      scope = formula(m0), 
                      direction = "backward")
#leaf_count ~ weather_treatment + soil_treatment + weather_treatment:soil_treatment

model<-glm(leaf_count ~ soil_treatment*weather_treatment+weather_treatment+soil_treatment, family = poisson, 
           data = subset(final_data, species == "P"))
summary(model)




#plant desiccation
#no data before weather :(

##############after_weather
summary(after_weather$plant_desiccation[after_weather$species == "P"]) #E+P look fine
hist(after_weather$plant_desiccation[after_weather$species == "E"]) #right skew

m0<- glmmTMB(
  plant_desiccation ~ 1,
  family = tweedie(),
  data = subset(after_weather, species == "P")
)
m1 <- glmmTMB(
  plant_desiccation ~ seed_coating * weather_treatment * soil_treatment,
  family = tweedie(),
  data = subset(after_weather, species == "P")
)
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#plant_desiccation ~ weather_treatment + soil_treatment + seed_coating + 
#weather_treatment:soil_treatment + soil_treatment:seed_coating

step.backward <- step(m1, 
                      scope = formula(m0), 
                      direction = "backward")
#plant_desiccation ~ seed_coating * weather_treatment * soil_treatment

model<-glmmTMB(
  plant_desiccation ~ seed_coating + weather_treatment + soil_treatment + 
    weather_treatment*soil_treatment + soil_treatment*seed_coating,
  family = tweedie(),
  data = subset(after_weather, species == "P")
)
summary(model)

p1<-ggplot(subset(after_weather, species == "P"), aes(x= seed_coating, 
                                                      y=plant_desiccation,
                                                      fill=weather_treatment)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("brown","blue","orange", "purple")) +
  facet_wrap(~ paste("soil_treatment:", soil_treatment))
p1

#######final data
summary(final_data$plant_desiccation[final_data$species == "E"]) #E+P look fine
hist(final_data$plant_desiccation[final_data$species == "P"]) #both look pretty normal, maybe a bit left skew


m0<- glmmTMB(
  plant_desiccation ~ 1,
  family = tweedie(),
  data = subset(final_data, species == "P")
)
m1 <- glmmTMB(
  plant_desiccation ~ seed_coating * weather_treatment * soil_treatment,
  family = tweedie(),
  data = subset(final_data, species == "P")
)
step.forward <- step(m0, 
                     scope = formula(m1), 
                     direction = "forward")
#plant_desiccation ~ weather_treatment + soil_treatment + weather_treatment:soil_treatment

step.backward <- step(m1, 
                      scope = formula(m0), 
                      direction = "backward")
#plant_desiccation ~ seed_coating + weather_treatment + soil_treatment + 
#seed_coating:soil_treatment + weather_treatment:soil_treatment

model<-glmmTMB(
  plant_desiccation ~ seed_coating + weather_treatment + soil_treatment + 
    weather_treatment*soil_treatment + soil_treatment*seed_coating,
  family = tweedie(),
  data = subset(final_data, species == "P")
)
summary(model)

p1<-ggplot(subset(final_data, species == "P"), aes(x= seed_coating, 
                                                      y=plant_desiccation,
                                                      fill=weather_treatment)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("brown","blue","orange", "purple")) +
  facet_wrap(~ paste("soil_treatment:", soil_treatment))
p1
