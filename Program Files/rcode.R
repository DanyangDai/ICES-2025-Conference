
###################### R Script    #####################
# R script for Online and Official Inflation Spillover Analysis of the Euro Area
# Name: Danyang Dai 
# Student ID: 985198 

#load in packages 
library(foreign)
library(ggplot2)
library(tidyverse)
library(haven)
library(fable)
library(lubridate)
library(tsibble)
library(feasts)
library(dplyr)
library(ggpubr)
library(xtable)
library(vars)
library(MASS)
library(strucchange)
library(zoo)
library(GGally)


# Importing data
paths <- list.files("data/raw_data", pattern = "*\\.dta$",
                    full.names = TRUE)

names(paths) <- str_match(paths, "raw_([A-Z]+)_CPI\\.dta")[,2]


data <- map_dfr(paths, read_dta, .id = "Country") %>% 
  mutate(date = yearmonth(ymd("1960-01-01") + ifelse(Country == "AUSTRALIA", 3, 1)*months(date))) |> 
  as_tsibble(index = date, key = Country)


data_eu <- data %>% 
  filter(!(Country %in% c("USA", "CANADA", "AUSTRALIA")))

# Truncate data and STL decomposition 

data_eu %>% 
  rename(CPI_Official = i_cpi) |> 
  model(STL(CPI_Official)) %>%
  components(season_adjust) %>% 
  autoplot() +
  theme_minimal()

data_eu %>% 
  rename(CPI_Online = i_online) |> 
  model(STL(CPI_Online)) %>%
  components(season_adjust) %>% 
  autoplot() +
  theme_minimal()

data_eusean <-data_eu %>% 
  model(STL(i_cpi)) %>%
  components() %>% 
  dplyr::select(season_adjust) %>% 
  filter(date >= yearmonth("2010 Sep"))

data_euadj <- data_eu %>% 
  model(STL(i_cpi)) %>%
  components() %>% 
  dplyr::select(season_adjust) %>% 
  filter(date >= yearmonth("2010 Sep"))  %>% 
  pivot_wider(names_from = Country,values_from = season_adjust)


data_onlineadj <- data_eu%>% 
  model(STL(i_online)) %>%
  components() %>% 
  dplyr::select(season_adjust)  %>% 
  filter(date >= yearmonth("2010 Sep"))

data_euadj_online <- data_eu %>% 
  model(STL(i_online)) %>%
  components() %>% 
  dplyr::select(season_adjust) %>%
  filter(date >= yearmonth("2010 Sep")) %>% 
  pivot_wider(names_from = Country,values_from = season_adjust) %>% 
  dplyr::select(date,`.model`,UK,FRANCE,NETHERLANDS,IRELAND,ITALY,GREECE,GERMANY)



# Plot data 

data_timeadj <- data_eu %>% 
  filter(date >= yearmonth("2010 Sep")) 

nonseasonadj <- ggplot(data=data_timeadj, aes(x = date, y = i_cpi)) + 
  geom_line(aes(color = Country))+
  labs(y="CPI Index", x = "Time") + 
  theme_minimal()

seasonadj <- ggplot(data=data_eusean, aes(x = date, y = season_adjust)) +
  geom_line(aes(color = Country))+
  labs(y="Seasonal Adjusted", x = "Time")+ 
  theme_minimal()

ggarrange(nonseasonadj, seasonadj, 
          ncol = 1, nrow = 2)

onlinenon <- ggplot(data=data_timeadj, aes(x = date, y = i_online)) + 
  geom_line(aes(color = Country))+
  labs(y="Online CPI Index", x = "Time")+ 
  theme_minimal()

onlineseasonadj <- ggplot(data=data_onlineadj, aes(x = date, y = season_adjust)) +
  geom_line(aes(color = Country))+
  labs(y="Seasonal Adjusted", x = "Time")+ 
  theme_minimal()

ggarrange(onlinenon, onlineseasonadj, 
          ncol = 1, nrow = 2)

ggarrange(nonseasonadj, onlinenon, 
          ncol = 1, nrow = 2)



# Lag Selection ICs
yt <- data_euadj |>  dplyr::select(UK,FRANCE,NETHERLANDS,IRELAND,ITALY,GREECE,GERMANY)

yt <- yt[,1:7]

vars::VARselect(yt)$criteria

ggpairs(yt) 

ggpairs(yot)

# ADF tests 

adf_1<- data_eusean %>%
  features(season_adjust, features = list(adf = ~ tseries::adf.test(.)$p.value))
print(xtable(adf_1, type = "latex"), file = "adf1.tex")

adf_2<- data_onlineadj %>%
  features(season_adjust, features = list(adf = ~ tseries::adf.test(.)$p.value))
print(xtable(adf_2, type = "latex"), file = "adf2.tex")



# Estimating VAR 1
# ordering: UK, France, Netherlands, Ireland,Italy,Greece, Germany

reducevar <- VAR(yt,p=1,type = "none")

yot <- data_euadj_online |> 
  dplyr::select(UK,FRANCE,NETHERLANDS,IRELAND,ITALY,GREECE,GERMANY)

yot <- yot[,1:7]

#yot <- yot[, c(7,1,6,4,5,3,2)]

reducevaron <- VAR(yot,p=1,type = "none")

# FEVD and Spillover 
fevd_official <-fevd(reducevar, n.ahead = 12)
fevd_online <-fevd(reducevaron, n.ahead = 12)

fevd_off <- array(NA,dim = c(7,7,12))

for (i in 1:12) {
  fevd_off[,,i] <- rbind(fevd_official$UK[i,],fevd_official$FRANCE[i,],fevd_official$NETHERLANDS[i,],fevd_official$IRELAND[i,],fevd_official$ITALY[i,],fevd_official$GREECE[i,],fevd_official$GERMANY[i,])
}


spillover_official <- c(rep(NA,12))

for(i in 1:12) {
  spillover_official[i] <- 1-sum(diag(fevd_off[,,i]))/sum(fevd_off[,,i])
}


fevd_on <- array(NA,dim = c(7,7,12))

for (i in 1:12) {
  fevd_on[,,i] <- rbind(fevd_online$UK[i,],fevd_online$FRANCE[i,],fevd_online$NETHERLANDS[i,],fevd_online$IRELAND[i,],fevd_online$ITALY[i,],fevd_online$GREECE[i,],fevd_online$GERMANY[i,])
}



spillover_online <- c(rep(NA,12))

for(i in 1:12) {
  spillover_online[i] <- 1-sum(diag(fevd_on[,,i]))/sum(fevd_on[,,i])
}

spill <- cbind(spillover_official*100,spillover_online*100)

tibble(
  Month = seq_along(spillover_official),
  Official = round(spillover_official*100,2),
  Online = round(spillover_online*100,2)
) %>% 
  pivot_longer(-Month, names_to = "Type", values_to = "Spillover") %>% 
  ggplot(aes(x = Month, y = Spillover, colour = Type)) + 
  geom_point() +
  geom_text(aes(label=Spillover),hjust=0, vjust=0)+ 
  theme_bw() +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))


saveRDS(fevd_off,file = "data/official_fevd.rds")

saveRDS(fevd_on,file = "data/online_fevd.rds")


# Estimating another set of VAR 1 using different cholesky ordering 
## UK, Ireland, France, Netherlands, Germany, Greece, Italy 
yt_2 <- data_euadj[3:9]
#yt_2 <- yt[, c(7,4,1,6,2,3,5)]
var_2 <- VAR(yt_2,p=1,type = "none")
fevd_var2 <-fevd(var_2, n.ahead = 12)

fevd_off2 <- array(NA,dim = c(7,7,12))

for (i in 1:12) {
  fevd_off2[,,i] <- rbind(fevd_var2$UK[i,],fevd_var2$IRELAND[i,],fevd_var2$FRANCE[i,],fevd_var2$NETHERLANDS[i,],fevd_var2$GERMANY[i,],fevd_var2$GREECE[i,],fevd_var2$ITALY[i,])
}

spill_off2 <- c(rep(NA,12))

for(i in 1:12) {
  spill_off2[i] <- 1-sum(diag(fevd_off2[,,i]))/sum(fevd_off2[,,i])
}


yton_2 <- data_euadj_online[3:9]
#yton_2 <- yton_2[, c(7,4,1,6,2,3,5)]
varon_2 <- VAR(yton_2,p=1,type = "none")
fevd_var2on <-fevd(varon_2, n.ahead = 12)

fevd_on2 <- array(NA,dim = c(7,7,12))

for (i in 1:12) {
  fevd_on2[,,i] <- rbind(fevd_var2on$UK[i,],fevd_var2on$IRELAND[i,],fevd_var2on$FRANCE[i,],fevd_var2on$NETHERLANDS[i,],fevd_var2on$GERMANY[i,],fevd_var2on$GREECE[i,],fevd_var2on$ITALY[i,])
}

spill_on2 <- c(rep(NA,12))

for(i in 1:12) {
  spill_on2[i] <- 1-sum(diag(fevd_on2[,,i]))/sum(fevd_on2[,,i])
}


spill2 <- cbind(spill_off2*100,spill_on2*100)
