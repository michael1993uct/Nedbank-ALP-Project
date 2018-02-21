#The purpose of this script is to develop at dataset for the industry relevant variables in the average monthly format to be used
# in the ALP APP.
rm(list = ls())
library("Quandl")
library("dplyr")

Quandl.api_key("z16Bss9ysz1A6Hg_nEqu")
randdollar <- Quandl("FRED/DEXSFUS", collapse = "monthly")
Golddata <- Quandl("LBMA/GOLD", collapse = "monthly")
Oildata <- Quandl("OPEC/ORB", collapse = "monthly")
consumer_confidence <- Quandl("OECD/KEI_CSCICP02_ZAF_ST_Q")
CPI <- Quandl("WGEM/ZAF_CPTOTSAXNZGY")
gdp_deflator <- Quandl("WWDI/ZAF_NY_GDP_DEFL_KD_ZG")
gdp_growth <- Quandl("WWDI/ZAF_NY_GDP_MKTP_KD_ZG")
private_fixed_cap <- Quandl("WWDI/ZAF_NE_GDI_FPRV_CN")
Date = Golddata["Date"]
Industrydata = data.frame(Date)
Industrydata <- Industrydata %>% mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
                mutate(Gold_Price = Golddata$`USD (AM)`) %>%
                 filter(Date > "2010-01-31")
CPI <- filter(CPI, Date > "2009-11-30")
Oildata <- Oildata %>%  filter(Date > "2010-01-31")
randdollar <- randdollar %>%  filter(Date > "2010-01-31")
consumer_confidence <- consumer_confidence %>% filter(Date > "2010-01-31")
gdp_deflator <- gdp_deflator %>% filter(Date > "2010-01-31")
gdp_growth <- gdp_growth %>% filter(Date > "2010-01-31")
private_fixed_cap <- private_fixed_cap %>% filter(Date > "2010-01-31")
Industrydata <- Industrydata %>% left_join(Oildata)
Industrydata <- Industrydata %>% left_join(randdollar, by = "Date") %>%
                                  left_join(consumer_confidence, by = "Date") %>%
                                  left_join(CPI, by = "Date") %>%
                                  left_join(gdp_deflator, by = "Date") %>%
                                  left_join(gdp_growth, by = "Date") %>%
                                  left_join(private_fixed_cap, by = "Date")

names(Industrydata)[3] <- "Oil Price"
names(Industrydata)[4] <- "Dollar Rand"
names(Industrydata)[5] <- "Consumer Confidence"
names(Industrydata)[6] <- "CPI"
names(Industrydata)[7] <- "GDP Deflator"
names(Industrydata)[8] <- "GDP Growth"
names(Industrydata)[9] <- "Private fixed capital formation"



write.csv2(Industrydata, file = "Industry Data for ALP.csv", sep = ";")
