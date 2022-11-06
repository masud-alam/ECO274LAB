##One sample t-test----
# Define given dataset
dataset <- c(546, 551, 548, 556, 549, 554)
#Create qqplot for the dataset
qqnorm(dataset)
qqline(dataset)
# Perform one-sample t-test
t.test( x= dataset,mu=553, alternative = "less",conf.level = 0.95)
## For the two-tailed test, alt = "two.sided"


## Paired t-test----

#case 1

dat1 <- data.frame(
  sample1 = c(0.9, -0.8, 0.1, -0.3, 0.2),
  sample2 = c(0.8, -0.9, -0.1, 0.4, 0.1)
)
dat1
dat_ggplot <- data.frame(
  value = c(0.9, -0.8, 0.1, -0.3, 0.2, 0.8, -0.9, -0.1, 0.4, 0.1),
  sample = c(rep("1", 5), rep("2", 5))
)

library(ggplot2)

ggplot(dat_ggplot) +
  aes(x = sample, y = value) +
  geom_boxplot() +
  theme_minimal()

boxplot(value ~ sample,
        data = dat_ggplot)




library(BSDA)

z.test(dat1$sample1,
       dat1$sample2,
       alternative = "two.sided",
       mu = 0,
       sigma.x = 1,
       sigma.y = 1,
       conf.level = 0.95
)

# Case 2


dat2 <- data.frame(
  sample1 = c(1.78, 1.5, 0.9, 0.6, 0.8, 1.9),
  sample2 = c(0.8, -0.7, -0.1, 0.4, 0.1, NA)
)
dat2
dat_ggplot <- data.frame(
  value = c(1.78, 1.5, 0.9, 0.6, 0.8, 1.9, 0.8, -0.7, -0.1, 0.4, 0.1),
  sample = c(rep("1", 6), rep("2", 5))
)

ggplot(dat_ggplot) +
  aes(x = sample, y = value) +
  geom_boxplot() +
  theme_minimal()

test <- t.test(dat2$sample1, dat2$sample2,
               var.equal = TRUE, alternative = "greater"
)
test
test$p.value


#Case 3


dat3 <- data.frame(
  value = c(0.8, 0.7, 0.1, 0.4, 0.1, 1.78, 1.5, 0.9, 0.6, 0.8, 1.9),
  sample = c(rep("1", 5), rep("2", 6))
)
dat3


ggplot(dat3) +
  aes(x = sample, y = value) +
  geom_boxplot() +
  theme_minimal()


test <- t.test(value ~ sample,
               data = dat3,
               var.equal = FALSE,
               alternative = "less"
)
test



## Case 4


dat4 <- data.frame(
  before = c(0.9, -0.8, 0.1, -0.3, 0.2),
  after = c(0.8, -0.9, -0.1, 0.4, 0.1)
)
dat4
dat4$difference <- dat4$after - dat4$before

ggplot(dat4) +
  aes(y = difference) +
  geom_boxplot() +
  theme_minimal()

t.test_pairedknownvar <- function(x, V, m0 = 0, alpha = 0.05, alternative = "two.sided") {
  M <- mean(x)
  n <- length(x)
  sigma <- sqrt(V)
  S <- sqrt(V / n)
  statistic <- (M - m0) / S
  p <- if (alternative == "two.sided") {
    2 * pnorm(abs(statistic), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(statistic, lower.tail = TRUE)
  } else {
    pnorm(statistic, lower.tail = FALSE)
  }
  LCL <- (M - S * qnorm(1 - alpha / 2))
  UCL <- (M + S * qnorm(1 - alpha / 2))
  value <- list(mean = M, m0 = m0, sigma = sigma, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
  # print(sprintf("P-value = %g",p))
  # print(sprintf("Lower %.2f%% Confidence Limit = %g",
  #               alpha, LCL))
  # print(sprintf("Upper %.2f%% Confidence Limit = %g",
  #               alpha, UCL))
  return(value)
}

test <- t.test_pairedknownvar(dat4$after - dat4$before,
                              V = 1
)
test
test$p.value


# Case 5

dat5 <- data.frame(
  before = c(9, 8, 1, 3, 2),
  after = c(16, 11, 15, 12, 9)
)
dat5
dat5$difference <- dat5$after - dat5$before

ggplot(dat5) +
  aes(y = difference) +
  geom_boxplot() +
  theme_minimal()
test <- t.test(dat5$after, dat5$before,
               alternative = "greater",
               paired = TRUE
)
test


### Wincoxon test

dat <- data.frame(
  country = as.factor(c(rep("dev", 12), rep("lowInc", 12))),
  income = c(
    19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18,
    16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14
  )
)

dat
library(ggplot2)

ggplot(dat) +
  aes(x = country, y = income) +
  geom_boxplot(fill = "darkseagreen1") +
  theme_minimal()




hist(subset(dat, country == "dev")$income,
     main = "Income for Dev",
     xlab = "Income"
)
hist(subset(dat, country == "lowInc")$income,
     main = "Income for low Income country",
     xlab = "Income"
)

shapiro.test(subset(dat, country == "dev")$income)
shapiro.test(subset(dat, country == "lowInc")$income)


test <- wilcox.test(dat$income ~ dat$country)
test


## Paired wilcoxon test

dat2 <- data.frame(
  Beginning = c(16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14),
  Middle = c(19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18)
)

dat2

#We transform the dataset to have it in a tidy format:

dat2 <- data.frame(
  Time = c(rep("Before", 12), rep("Middle", 12)),
  Outcome = c(dat2$Beginning, dat2$Middle)
)
dat2


# Reordering dat2$Time
dat2$Time <- factor(dat2$Time,
                    levels = c("Before", "Middle")
)

ggplot(dat2) +
  aes(x = Time, y = Outcome) +
  geom_boxplot(fill = "darkseagreen1") +
  theme_minimal()


test <- wilcox.test(dat2$Outcome ~ dat2$Time,
                    paired = TRUE)
test


## One sample binomial test


#Binomial
#The binomial random numbers are discrete random numbers.
#They have the distribution of the number of successes in
#n independent Bernoulli trials where a Bernoulli trial results 
#in success or failure, success with probability p. 

n=1; p=.5                     # set the probability


# Find 10 random values from a sample of 1 with probability of 0.5.
rbinom(10,n,p)                # 10 different such numbers

n = 10; p=.5
rbinom(1,n,p)                 

rbinom(5,n,p)                 # 5 binomial number

#The following codes will show 100 binomially distributed random numbers 

n=30;p=.25                    # set appropriate prob and number
x=rbinom(100,n,p)            # 100 random numbers
x
hist(x,probability=TRUE,col="darkseagreen1")

dbinom(3, size = 5, prob = 0.8)
dbinom(4, size = 5, prob = 0.8)
dbinom(5, size = 5, prob = 0.8)
# Pr[3 or more] = 0.21 + 0.41 + 0.33 = 0.95. This means that we’d expect to #get 3 or more “No Collateral” borrower 95% of the time with repeated #sampling.

##Alternatively

# calculate the number of hits from 3 to 5
xsuccesses <- 3:5

# do each calculation
probx <- dbinom(xsuccesses, size = 5, prob = 0.8)

# make a table from those two values
probTable <- data.frame(xsuccesses, probx) 

# display the table
show(probTable )

