
#Two way Analysis of variance for balanced designs (equal group size)
# install.packages("car")
library(car)



country <- c("oil_import","oil_import","oil_import", "oil_export","oil_export","oil_export",
             "oil_import","oil_import","oil_import", "oil_export","oil_export","oil_export",
             "oil_import","oil_import","oil_import", "oil_export","oil_export","oil_export")
country <- as.factor(country)

HDI <- c(4,6,8,4,8,9,6,6,9,7,10,13,8,9,13,12,14,16)

climate_risk <- c("HIGH","HIGH","HIGH","HIGH","HIGH","HIGH",
                  "MEDIUM","MEDIUM","MEDIUM","MEDIUM","MEDIUM","MEDIUM",
                  "LOW","LOW","LOW","LOW","LOW","LOW")

climate_risk <- as.factor(climate_risk)

my_data <- data.frame(country,HDI,climate_risk)

str(my_data)

library(ggpubr)

plot <- ggline(my_data, x = "climate_risk", y = "HDI", color = "country",
               add = c("mean_se", "jitter"),
               ylab="HDI Progress", xlab="climate Risk",
               legend.title="Country",legend="right")
plot

#####Two-way ANOVA with interaction#####
#Interaction = the effect of one factor on the dependent variable depends on the level of another factor
#Type does not matter in a balanced design#

model1 <- aov(HDI ~ climate_risk+country+climate_risk:country, data = my_data)
summary(model1)

####Check model assumptions####

#Assumption 1: Normal distribution of the data and the model residuals

plot(model1,2)
qqPlot(model1$residuals,
       id = FALSE) # id = FALSE to remove point identification)

aov_residuals <- residuals(object = model1)
shapiro.test(aov_residuals)



#Assumption 2: Homogeneity of variance assumption of the groups
plot(model1,1)
leveneTest(HDI ~ climate_risk * country, data = my_data)



#Post-hoc test: In case of a significant interaction effect, 
#we investigate all separate group combinations

library("lsmeans")

library("multcomp")

library("multcompView")
TukeyHSD(model1)

# posthoc <- lsmeans(model1,
#                    pairwise ~ climate_risk*country, 
#                    adjust="tukey")
# posthoc
# 
# cld(posthoc[[1]],  alpha=.05,  Letters=letters)


