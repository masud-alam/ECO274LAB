##https://github.com/vincentarelbundock/WDI

install.packages("WDI")
require(WDI)
WDIsearch('gdp')
WDIsearch('gdp.*capita.*constant')
dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('MX','','PK','JP','MY', 'IN','US'), start=1960, end=2022)
library(ggplot2)
ggplot(dat, aes(year, NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')


dat_SA <-  WDI(indicator='NY.GDP.PCAP.KD', country=c('NP','BD','PK', 'IN'), start=1960, end=2022)
ggplot(dat_SA, aes(year, NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita South Asia')



###GDP_Growth_Bar Plot
# Load required library
library(ggplot2)

# Create a sample data frame with GDP growth data
year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
gdp_growth <- c(2.5, 2.8, 1.9, 2.5, 2.9, 3.1, 2.6, 2.2, 2.9, 2.3, -3.5)
gdp_data <- data.frame(year, gdp_growth)

# Create a bar plot of GDP growth over time
ggplot(data = gdp_data, aes(x = year, y = gdp_growth)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label=gdp_growth), vjust=0)+
  ggtitle("GDP Growth Over Time") +
  xlab("Year") +
  ylab("GDP Growth (%)")+
  theme_bw()






### Arranging four plots in one space to compare the macroeconomic performance

require(WDI)
WDIsearch('gdp')
WDIsearch('gdp.*capita.*constant')
dat = WDI(indicator= c('NY.GDP.PCAP.KD',"MS.MIL.XPND.GD.ZS"), country=c('MX','JP'), start=1970, end=2000)

library(ggplot2)
p1 <- ggplot(dat, aes(year, NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')
p1
p2 <- ggplot(dat, aes(year, MS.MIL.XPND.GD.ZS, color=country)) + geom_line() + 
  xlab('Year') + ylab('Military expenditure (% of GDP)')
p2


dat1 = WDI(indicator= c('NY.GDP.PCAP.KD',"MS.MIL.XPND.GD.ZS"), country=c('PK', 'IN'), start=1970, end=2000)


p3 <- ggplot(dat1, aes(year, NY.GDP.PCAP.KD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')
p3
p4 <- ggplot(dat1, aes(year, MS.MIL.XPND.GD.ZS, color=country)) + geom_line() + 
  xlab('Year') + ylab('Military expenditure (% of GDP)')
p4



library("cowplot")
plot_grid(p1,p2,p3, p4 + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)



