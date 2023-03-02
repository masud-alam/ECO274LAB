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
