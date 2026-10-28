#loading dataset
data<-test

#checking out the data
library(vegan)
library(pastecs)
library(corrplot)
library(ggplot2)
library(ggpubr)

#POSE data first
summary(data$height[data$species == "P"])
#max height is 886 no way

data %>%
  filter(species == "P") %>%
  slice_max(height, n = 1, with_ties = TRUE) %>%
  pull(ID)
#"P_B_NV_D_5" 1909
#gonna guess this was supposed to be 86

data %>%
  filter(ID == "P_B_NV_D_5") %>%
  pull(height)
#205,886,0

ggplot(subset(data, ID == "P_B_NV_D_5"), 
       aes(x = date, y = height)) +
  geom_point() +
  geom_line() + # Optional: connects the points chronologically
  scale_x_datetime(breaks = data$date[data$ID == "P_B_NV_D_5"], 
                   date_labels = "%Y-%m-%d") + 
  theme_minimal() +
  labs(title = "Height over Time (ID: P_B_NV_D_5)",
       x = "Date",
       y = "Color") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data$height[data$ID == "P_B_NV_D_5" & data$date == as.Date("2026-09-04")] <- 86
summary(data$height[data$species == "P"])

#now looking at ELEL data
summary(data$height[data$species == "E"])

#checking something with dates
table(data$date)

#missing 44 entries on 9/1

#correcting 9/30 to 9/29
data <- data %>%
  mutate(date = if_else(date == as.Date("2026-09-30"), as.Date("2026-09-29"), date))

#final data set
final_data <- subset(data, date == as.Date("2026-09-29"))










#Objective 3: how coatings affect germ/emergence, growth, and survival 



######growth
#height
summary(data$height[data$species == "E"])
summary(data$height[data$species == "P"])
summary.lm(aov(height[final_data$species=="P"]~ seed_coating[final_data$species=="P"], data = final_data))
#overall not sig
#ID  marginally sig 0.0693
#X  marginally sig 0.0798 
summary.lm(aov(height[final_data$species=="E"]~ seed_coating[final_data$species=="E"], data = final_data))
#overall not sig
#ON marginally sig 0.0925  
#WA marginally sig 0.0922 
#X marginally sig 0.0914 
#WY sig 0.0407 


#graphing
#weather colors = control = blue, heatwave = green, drought = dark orange, D+H = light orange
#colors control=blue, xan, sterile, OZ, ON, ID, WY, WA, NV
p1<-ggplot(subset(data, species == "E"), aes(x= seed_coating, y=height,fill=seed_coating)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1


#leaf count
summary(data$leaf_count[data$species == "E"])
summary(data$leaf_count[data$species == "P"])
lc_model<-glm(leaves_desiccated ~ seed_coating, family = poisson, data = subset(final_data, species == "P"))
check_overdispersion(lc_model) #yep
summary(lc_model)
lc_model<-glm(leaves_desiccated ~ seed_coating, family = poisson, data = subset(final_data, species == "E"))
check_overdispersion(lc_model) #no
summary(lc_model)


#growth rate?
#create avg growth rate first?
data_avg<-data
data_avg<- data %>%
  group_by(ID) %>%
  summarise(
    weighted_growth_rate =
      sum(diff_height, na.rm = TRUE) /
      sum(diff_date, na.rm = TRUE)
  )

#based on seed coating?
data %>%
  group_by(seed_coating) %>%
  summarise(
    mean_growth_rate = mean(growth_rate, na.rm = TRUE),
    sd_growth_rate = sd(growth_rate, na.rm = TRUE),
    n = n()
  )

test<-data
test<-merge(test, data_avg, by = "ID", all.x = TRUE)

POSE_growth_data <- test %>%
  filter(species == "P") %>%
  distinct(ID, .keep_all = TRUE)

POSE_growth_data <- subset(POSE_growth_data, seed_coating != "A")

summary.lm(aov(weighted_growth_rate
               ~ seed_coating, data = POSE_growth_data)) #not sig


ELEL_growth_data <- test %>%
  filter(species == "E") %>%
  distinct(ID, .keep_all = TRUE)

summary.lm(aov(weighted_growth_rate
               ~ seed_coating, data = ELEL_growth_data)) #not sig



#########germ
hist(data$correct_first_germ_date[data$species=="E"], breaks="day")
data %>%
  filter(species == "E") %>%
  distinct(ID, .keep_all = TRUE) %>%
  pull(correct_days_to_first_germ) %>%
  log()%>%
  hist(main = "Histogram for Species E (Unique IDs)", 
       xlab = "Correct Days to First Germination")

ELEL_germ_data <- data %>%
  filter(species == "E") %>%
  distinct(ID, .keep_all = TRUE)

summary.lm(aov(log(correct_days_to_first_germ)
               ~ seed_coating, data = ELEL_germ_data))
#NV sig 0.03670 
#WA sig 0.00783 
#WY sig 0.01531 

#graphing
p1<-ggplot(ELEL_germ_data, aes(x= seed_coating, y=correct_days_to_first_germ,fill=seed_coating)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1

hist(ELEL_germ_data$germ_ratio_correct)

library(betareg)
library(lmtest)
library(emmeans)

# Transform exact 0s and 1s so the Beta distribution can read them
ELEL_test<-ELEL_germ_data
n <- nrow(ELEL_test)
ELEL_test$trans_ratio <- (ELEL_test$germ_ratio_correct * (n - 1) + 0.5) / n

#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ seed_coating, data = ELEL_test)
null_model <- betareg(trans_ratio ~ 1, data = ELEL_test)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model)

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ seed_coating)

glm_model <- glm(
  germ_ratio_correct ~ seed_coating,
  family = quasibinomial,
  data = ELEL_test
)
summary(glm_model)


#germ POSE
hist(data$correct_first_germ_date[data$species=="P"], breaks="day")
data %>%
  filter(species == "P") %>%
  distinct(ID, .keep_all = TRUE) %>%
  pull(correct_days_to_first_germ) %>%
  hist(main = "Histogram for Species E (Unique IDs)", 
       xlab = "Correct Days to First Germination")

POSE_germ_data <- data %>%
  filter(species == "P") %>%
  distinct(ID, .keep_all = TRUE)

POSE_test<-POSE_germ_data
POSE_test2 <- subset(POSE_test, seed_coating != "A")

summary.lm(aov(log(correct_days_to_first_germ)
               ~ seed_coating, data = POSE_test2))
#marginally sig 0.06436
#OZ sig 0.0437 
#graphing
p1<-ggplot(POSE_test2, aes(x= seed_coating, y=correct_days_to_first_germ,fill=seed_coating)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1

hist(POSE_test2$germ_ratio_correct)

library(betareg)
library(lmtest)
library(emmeans)

# Transform exact 0s and 1s so the Beta distribution can read them
n <- nrow(POSE_test2)
POSE_test2$trans_ratio <- (POSE_test2$germ_ratio_correct * (n - 1) + 0.5) / n

#Fit Beta Regression
POSE_test2$seed_coating <- as.factor(POSE_test2$seed_coating)
beta_model <- betareg(trans_ratio ~ seed_coating, data = POSE_test2)
null_model <- betareg(trans_ratio ~ 1, data = POSE_test2)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model)

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ seed_coating)

#glm model?
glm_model <- glm(
  germ_ratio_correct ~ seed_coating,
  family = quasibinomial,
  data = POSE_test2
)
summary(glm_model)


#survival? 

#does color fit here? i have no clue if anything is right lmao
munsell<-merged_munsell_10192026

POSE_munsell<- munsell %>%
  filter(species == "P")

ELEL_munsell<- munsell %>%
  filter(species == "E")

#POSE
summary.lm(aov(Value ~ seed_coating, data = POSE_munsell))
#X sig 0.0317 
##graphing
summary.lm(aov(Chroma ~ seed_coating, data = POSE_munsell))
#WY sig 0.0484     
#OZ marginally sig 0.0948


color_table <- table(
  POSE_munsell$seed_coating,
  POSE_munsell$Color
)
chisq.test(color_table)

#ELEL
summary.lm(aov(Value ~ seed_coating, data = ELEL_munsell))
#WY sig 0.0277  
summary.lm(aov(Chroma ~ seed_coating, data = ELEL_munsell))
#WY sig 0.00615     
#OZ marginally sig 0.06770 
#WA sig 0.00349 

color_table <- table(
  ELEL_munsell$seed_coating,
  ELEL_munsell$Color
)
chisq.test(color_table)







#Objective 4: effectiveness of coatings into invasive soils
#height
summary(lm(height[final_data$species=="P"]~ soil_treatment[final_data$species=="P"], data = final_data))
#sig 0.009621
summary(lm(height[final_data$species=="E"]~ soil_treatment[final_data$species=="E"], data = final_data))
#sig 0.000507 

#graphing
p1<-ggplot(subset(final_data, species == "P"), aes(x= soil_treatment,
                                                        y=height,
                                                        fill=soil_treatment)) + 
  geom_boxplot() +scale_fill_manual(values=c("brown","#377eb8"))+
  stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1


#interaction?
summary(lm(height[final_data$species=="P"]~ 
             soil_treatment[final_data$species=="P"]*seed_coating[final_data$species=="P"], 
           data = final_data))
#not sig
summary(lm(height[final_data$species=="E"]~ 
             soil_treatment[final_data$species=="P"]*seed_coating[final_data$species=="E"], 
           data = final_data))
#not sig


#leaf count
summary(data$leaf_count[data$species == "E"])
summary(data$leaf_count[data$species == "P"])
hist(final_data$leaf_count[final_data$species == "P"])
hist(final_data$leaf_count[final_data$species == "E"])

lc_model<-glm(leaf_count ~ soil_treatment, family = quasipoisson, data = subset(final_data, species == "P"))
check_overdispersion(lc_model) #yep
summary(lc_model) #not sig
lc_model<-glm(leaf_count ~ soil_treatment, family = poisson, data = subset(final_data, species == "E"))
check_overdispersion(lc_model) #no
summary(lc_model) #not sig

#interaction
lc_model<-glm(leaf_count ~ soil_treatment*seed_coating, 
              family = quasipoisson, data = subset(final_data, species == "P"))
check_overdispersion(lc_model) #yep
summary(lc_model) #not sig 
lc_model<-glm(leaf_count ~ soil_treatment*seed_coating, 
              family = poisson, data = subset(final_data, species == "E"))
check_overdispersion(lc_model) #no
summary(lc_model) #not sig



#########germ
#ELEL
hist(ELEL_germ_data$correct_first_germ_date, breaks="day")

lc_model<-glm(correct_days_to_first_germ ~ soil_treatment, 
              family = gaussian, data = ELEL_germ_data)
summary(lc_model) #sig

#graphing
p1<-ggplot(ELEL_germ_data, aes(x= soil_treatment, y=correct_days_to_first_germ,fill=soil_treatment)) + 
  geom_boxplot()+ 
  scale_fill_manual(values=c("brown","#377eb8"))+ 
  stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1

#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ soil_treatment, data = ELEL_germ_data)
null_model <- betareg(trans_ratio ~ 1, data = ELEL_germ_data)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model) #sig

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ soil_treatment) #sig

glm_model <- glm(
  germ_ratio_correct ~ soil_treatment,
  family = quasibinomial,
  data = ELEL_germ_data
)
summary(glm_model)

#graphing
p1<-ggplot(ELEL_germ_data, aes(x= soil_treatment, y=germ_ratio_correct,fill=soil_treatment)) + 
  geom_boxplot()+ 
  scale_fill_manual(values=c("brown","#377eb8"))+ 
  stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1



#germ POSE
hist(POSE_test2$correct_first_germ_date, breaks="day")

lc_model<-glm(correct_days_to_first_germ ~ soil_treatment, 
              family = gaussian(), data = POSE_test2)

summary(lc_model) #sig

#graphing
p1<-ggplot(POSE_test2, aes(x= soil_treatment, y=correct_days_to_first_germ,fill=soil_treatment)) + 
  geom_boxplot()+ 
  scale_fill_manual(values=c("brown","#377eb8"))+ 
  stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1

#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ soil_treatment, data = POSE_test2)
null_model <- betareg(trans_ratio ~ 1, data = POSE_test2)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model) #sig

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ soil_treatment) #sig

glm_model <- glm(
  germ_ratio_correct ~ soil_treatment,
  family = quasibinomial,
  data = POSE_test2
)
summary(glm_model) #sig

#graphing
p1<-ggplot(POSE_test2, aes(x= soil_treatment, y=germ_ratio_correct,fill=soil_treatment)) + 
  geom_boxplot()+ 
  scale_fill_manual(values=c("brown","#377eb8"))+ 
  stat_summary(fun=mean, geom="point", shape=17, size=5,color="red", fill="red")
p1
#looks weird...


#interactions? ELEL
lc_model<-glm(correct_days_to_first_germ ~ soil_treatment*seed_coating, 
              family = gaussian, data = ELEL_germ_data)
summary(lc_model) #not sig

#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ soil_treatment*seed_coating, data = ELEL_test)
null_model <- betareg(trans_ratio ~ 1, data = ELEL_test)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model) #sig?

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ soil_treatment*seed_coating) #sig

glm_model <- glm(
  germ_ratio_correct ~ soil_treatment*seed_coating,
  family = quasibinomial,
  data = ELEL_test
)
summary(glm_model) #not sig

#interaction POSE
lc_model<-glm(correct_days_to_first_germ ~ soil_treatment*seed_coating, 
              family = gaussian, data = POSE_test2)
summary(lc_model) #not sig

#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ soil_treatment*seed_coating, data = POSE_test2)
null_model <- betareg(trans_ratio ~ 1, data = POSE_test2)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model) #sig?

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ soil_treatment*seed_coating) #sig

glm_model <- glm(
  germ_ratio_correct ~ soil_treatment*seed_coating,
  family = quasibinomial,
  data = POSE_test2
)
summary(glm_model)






#Objective 5: coatings to ameliorate stress from weather
#height
summary.lm(aov(height[final_data$species=="P"]~ 
                 weather_treatment[final_data$species=="P"], data = final_data))
#sig 2.972e-07
summary.lm(aov(height[final_data$species=="E"]~ 
                 weather_treatment[final_data$species=="E"], data = final_data))
#sig 1.241e-14


#graphing
#weather colors = control = blue, heatwave = green, drought = dark orange, D+H = light orange
p1<-ggplot(subset(final_data, species == "P"), aes(x= weather_treatment, y=height,fill=weather_treatment)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="black")+
  scale_fill_manual(values=c("brown","blue","orange", "purple"))
p1

#interactions?
summary.lm(aov(height[final_data$species=="P"]~ 
                 weather_treatment[final_data$species=="P"]*seed_coating[final_data$species=="P"],
               data = final_data))
#not sig
summary.lm(aov(height[final_data$species=="E"]~ 
                 weather_treatment[final_data$species=="E"]*seed_coating[final_data$species=="E"],
               data = final_data))
#a few sig interactions, OZ+H,S+H, 

p1<-ggplot(subset(final_data, species == "P"), aes(x= weather_treatment, y=height,fill=seed_coating)) + 
  geom_boxplot()
p1


#leaf count
hist(final_data$leaf_count[data$species == "E"])
hist(final_data$leaf_count[data$species == "P"])
lc_model<-glm(leaf_count ~ weather_treatment, family = quasipoisson, 
              data = subset(final_data, species == "P"))
check_overdispersion(lc_model) #sig
summary(lc_model)
lc_model<-glm(leaf_count ~ weather_treatment, family = poisson, 
              data = subset(final_data, species == "E"))
check_overdispersion(lc_model) #no
summary(lc_model) #sig

#graphing
p1<-ggplot(subset(final_data, species == "E"), aes(x= weather_treatment, y=leaf_count,fill=weather_treatment)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="black")+
  scale_fill_manual(values=c("brown","blue","orange", "purple"))
p1

#interaction
lc_model<-glm(leaf_count ~ weather_treatment*seed_coating, family = quasipoisson, 
              data = subset(final_data, species == "P"))
check_overdispersion(lc_model) 
summary(lc_model) #some sig values, ID+C, ID+H, WY+C, WY+H, X+H (comparing to D+C)
lc_model<-glm(leaf_count ~ weather_treatment*seed_coating, family = poisson, 
              data = subset(final_data, species == "E"))
check_overdispersion(lc_model) #no
summary(lc_model) #one sig ON+C

p1<-ggplot(subset(final_data, species == "P"), aes(x= weather_treatment, y=leaf_count,fill=seed_coating)) + 
  geom_boxplot()
p1

#growth rate?
#create avg growth rate first?
test<-data
test<-merge(test, data_avg, by = "ID", all.x = TRUE)

summary.lm(aov(weighted_growth_rate
               ~ weather_treatment, data = POSE_growth_data)) #sig 0.03303
summary.lm(aov(weighted_growth_rate
               ~ weather_treatment, data = ELEL_growth_data)) #marginally sig 0.05225

#graphing
p1<-ggplot(ELEL_growth_data, aes(x= weather_treatment, y=weighted_growth_rate,fill=weather_treatment)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="black")+
  scale_fill_manual(values=c("brown","blue","orange", "purple"))
p1

#interaction?
summary.lm(aov(weighted_growth_rate
               ~ weather_treatment*seed_coating, data = POSE_growth_data)) #not sig
summary.lm(aov(weighted_growth_rate
               ~ weather_treatment*seed_coating, data = ELEL_growth_data)) #some sig
p1<-ggplot(ELEL_growth_data, aes(x= weather_treatment, y=weighted_growth_rate,fill=seed_coating)) + 
  geom_boxplot()
p1


#########germ
#ELEL
summary.lm(aov(log(correct_days_to_first_germ)
               ~ weather_treatment, data = ELEL_germ_data))
#sig 0.005357, although didn't expect it to be? weirrrrd

#graphing
p1<-ggplot(ELEL_germ_data, aes(x= weather_treatment, y=correct_days_to_first_germ,fill=weather_treatment)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="black", fill="black")+
  scale_fill_manual(values=c("brown","blue","orange", "purple"))
p1

#interaction?
summary.lm(aov(correct_days_to_first_germ
               ~ weather_treatment*seed_coating, data = ELEL_germ_data)) #some interactions

p1<-ggplot(ELEL_germ_data, aes(x= weather_treatment, y=correct_days_to_first_germ,fill=seed_coating)) + 
  geom_boxplot()
p1

#germ ratio
#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ weather_treatment, data = ELEL_test)
null_model <- betareg(trans_ratio ~ 1, data = ELEL_test)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model)

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ weather_treatment)

glm_model <- glm(
  germ_ratio_correct ~ weather_treatment,
  family = quasibinomial,
  data = ELEL_test
)
summary(glm_model) #not sig


#germ POSE
summary.lm(aov(log(correct_days_to_first_germ)
               ~ weather_treatment, data = POSE_test2))
#sig 3.505e-05
#graphing
p1<-ggplot(POSE_test2, aes(x= weather_treatment, y=correct_days_to_first_germ,fill=weather_treatment)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=17, size=5,color="black", fill="black")+
  scale_fill_manual(values=c("brown","blue","orange", "purple"))
p1

#interaction?
summary.lm(aov(log(correct_days_to_first_germ)
               ~ weather_treatment*seed_coating, data = POSE_test2)) #not sig

#germ ratio
#Fit Beta Regression
beta_model <- betareg(trans_ratio ~ weather_treatment, data = POSE_test2)
null_model <- betareg(trans_ratio ~ 1, data = POSE_test2)

#Check for overall group effect (Likelihood Ratio Test)
lrtest(null_model, beta_model)

#Pairwise Comparisons
emmeans(beta_model, specs = pairwise ~ weather_treatment)

#glm model?
glm_model <- glm(
  germ_ratio_correct ~ weather_treatment,
  family = quasibinomial,
  data = POSE_test2
)
summary(glm_model) #not sig




p1<-ggplot(subset(subset_testdata, species == "P"), aes(x= seed_coating,
                                                        y=leaf_count,
                                                        fill=weather_treatment)) + 
  geom_boxplot() +
  facet_wrap(~ paste("soil_treatment:", soil_treatment))
p1
