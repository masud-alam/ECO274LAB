


## Example

dat <- iris

dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
                   "small", "big")

##We now create a contingency table of the two variables Species and size with the table() function:


table(dat$Species, dat$size)

#draw a barplot to visually represent the data

library(ggplot2)

ggplot(dat) +
  aes(x = Species, fill = size) +
  geom_bar()


#visualize in terms of proportions 

ggplot(dat) +
  aes(x = Species, fill = size) +
  geom_bar(position = "fill")



#If we prefer to have the bars next to each other:

ggplot(dat) +
  aes(x = Species, fill = size) +
  geom_bar(position = "dodge")


##we are going to test if there is a relationship between the variables Species and size


test <- chisq.test(table(dat$Species, dat$size))
test
test$statistic
test$p.value
test$expected


#if the smallest expected frequencies is lower than 5, then use the Fisher’s exact test


# load packages
library(ggstatsplot)
library(ggplot2)

# plot
ggbarstats(
  data = dat,
  x = size,
  y = Species
) +
  labs(caption = NULL) # remove caption



###One way ANOVA----


#install.packages("palmerpenguins")
library(palmerpenguins)


library(tidyverse)

dat <- penguins %>%
  select(species, flipper_length_mm)
summary(dat)

library(ggplot2)

ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")




res_aov <- aov(flipper_length_mm ~ species,
               data = dat)

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE) # id = FALSE to remove point identification)
shapiro.test(res_aov$residuals)
boxplot(flipper_length_mm ~ species,
        data = dat)

# Dotplot
library("lattice")

dotplot(flipper_length_mm ~ species,
        data = dat)

#the Levene’s test can be performed thanks to the leveneTest() function from the {car} package:

library(car)

leveneTest(flipper_length_mm ~ species,
           data = dat)


##Check outliers

library(ggplot2)

ggplot(dat) +
  aes(x = species, y = flipper_length_mm) +
  geom_boxplot()


#ANOVA in R

# 1st method:
oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = TRUE) # assuming equal variances


# 2nd method:
res_aov <- aov(flipper_length_mm ~ species,
               data = dat
)

summary(res_aov)


oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = FALSE # assuming unequal variances
)


##To report ANOVA

library("report") # Load the package every time you start R

report(res_aov)

