#SETTING WORK DIRECTORY
setwd("P:\\Spring 2020\\ML\\Mid-term")

# READING DATA FROM CSV FILE IN WORK DIRECTORY
hatecrimedata=read.csv(file='hatecrime.csv') # READING AND CREATING DATA 
#hatecrimedata

# Determining the relation between income inequality and the number of hatecrimes and hate incidents
#Pearson correlation
correlation1 = cor.test(hatecrimedata$gini_index, hatecrimedata$hate_crimes_per_100k_splc,method="pearson")
correlation2 = cor.test(hatecrimedata$gini_index, hatecrimedata$avg_hatecrimes_per_100k_fbi,method="pearson")
correlation1
correlation2

#Plotting the relationship between income inequality(gini_index) and hatecrime and hate incidents
library(ggplot2)

ggplot(hatecrimedata, aes(gini_index,hate_crimes_per_100k_splc,color=gini_index)) + geom_point()
ggplot(hatecrimedata, aes(gini_index,avg_hatecrimes_per_100k_fbi,color=gini_index)) + geom_point()
  

##Problem 2

#Creating linear regression models for both FBI and SPLC using all the features in the dataset
mdlsplc = lm(hate_crimes_per_100k_splc ~  median_household_income + share_unemployed_seasonal + share_population_in_metro_areas + share_non_citizen +
               share_white_poverty + gini_index + share_non_white + share_voters_voted_trump, data=hatecrimedata)
summary(mdlsplc)
mdlfbi = lm(avg_hatecrimes_per_100k_fbi ~  median_household_income + share_unemployed_seasonal + share_population_in_metro_areas + share_non_citizen
            + share_white_poverty + gini_index + share_non_white + share_voters_voted_trump, data=hatecrimedata)
summary(mdlfbi)

#Creating the best models for SPLC and FBI by choosing only the significant variables from the previoud model
mdlsplc = lm(hate_crimes_per_100k_splc ~  share_population_in_metro_areas + gini_index + share_non_white + share_voters_voted_trump, 
             data=hatecrimedata)
mdlfbi = lm(avg_hatecrimes_per_100k_fbi ~  share_voters_voted_trump + share_population_in_metro_areas + gini_index 
            + share_non_white, data=hatecrimedata)

summary(mdlsplc)
summary(mdlfbi)



#Problem 4
#Plotting Horizontal Bar graphs for Hatecrime numbers for both SPLC and FBI features across all the states
#Color of the graph indicates the range of the hatecrime values

library(ggplot2)

ggplot(hatecrimedata, aes(ï..state,hate_crimes_per_100k_splc)) + 
      geom_bar(aes(fill=hate_crimes_per_100k_splc),position=position_dodge(),stat = "identity") + coord_flip()
ggplot(hatecrimedata, aes(ï..state,avg_hatecrimes_per_100k_fbi)) + 
      geom_bar(aes(fill=avg_hatecrimes_per_100k_fbi),position=position_dodge(),stat = "identity") + coord_flip()

##Problem 5
#Creating baseline model for SPLC feature using share_voters_voted_trump as the independent variable as that was the most significant variable in the best model
mdlbasesplc = lm(hate_crimes_per_100k_splc ~ share_voters_voted_trump,data=hatecrimedata)
summary(mdlbasesplc)

#Creating baseline model for FBI feature using share_voters_voted_trump as the independent variable as that was the most significant variable in the best model
mdlbasefbi = lm(avg_hatecrimes_per_100k_fbi ~ share_voters_voted_trump, data=hatecrimedata)
summary(mdlbasefbi)

#Comparing the baseline models to the best models for both FBI and SPLC features using ANOVA tables
anova(mdlbasesplc,mdlsplc)
anova(mdlbasefbi,mdlfbi)


