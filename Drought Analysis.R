#Packages to be used
library(tidyverse)
library(SPEI)
library(scPDSI)
library(trend)
library(zoo)
library(tsbox)
library(styler)


#Create data and summarise data
sokoto_2 <-  sokoto_1 %>% na.omit() %>% 
    mutate(TAVG = (TMIN + TMAX)/2) %>% 
      mutate(Month = format(Date, "%m"), year = format(Date, "%Y")) %>% 
        select(-Date) %>% 
          group_by(Month, year) %>% 
            summarise(TMIN = mean(TMIN), TMAX = mean(TMAX), 
              TAVG = mean(TAVG), RAIN = sum(RAIN)) %>%  
  arrange(year)

#To obtain the SPEI obtain the PET and Balance
#Get PET (Potential Evapotranspiration)
#Using the thornthwaite
sokoto_2[,"PET"] <- thornthwaite(sokoto_2$TAVG, 13.08)

#GET BAL
sokoto_2[,"BAL"] <- sokoto_2$RAIN - sokoto_2$PET


#create the time series for the data
sokoto_2_ts <- ts(sokoto_2, start = c(1986, 1), frequency = 12)


#Get a three months spei
spei1 <- spei(sokoto_2_ts[,"BAL"], 3)


#Create a dataframe from the fitted values of SPEI
sokoto_spei1 <- ts_data.frame(spei1$fitted) %>% 
  mutate(sign = ifelse(value >= 0, "pos", "neg"), 
         Period = zoo::as.yearmon(time, "%Y/%m"))


#Plot the three months spei using a line plot
ggplot(data = sokoto_spei1, aes(x = Period, y = value)) +
    geom_line(size = 1, col = "#e8647c") +
        geom_hline(yintercept = 0 , linetype = "solid", color = "black", size = 0.8) +
               scale_y_continuous(limits = c(-3, 3), breaks = -3:3) + 
                     scale_x_yearmon(format="%Y", n=20) +
theme(axis.text.x = element_text(angle =  45), 
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5)) +
          ggtitle("Sokoto 3-months SPEI (Thornthwaite)") +
             xlab("Year") + ylab("SPEI") +
                  geom_hline(yintercept = c(-0.99, -1.49, -2.00),
                             linetype = "dashed", size = 0.8)



#Using the hargreaves method
#Get the PET and Balance Values
#Using Hargreaves
sokoto_har[,"Har"] <- hargreaves(sokoto_har$TMIN, sokoto_har$TMAX, lat = 13.08)

#Get the PET values
sokoto_har[,"harP"] <- sokoto_har$RAIN - sokoto_har$Har

#Create a timeseries 
sokoto_har_ts <- ts(data = sokoto_har, start = c(1986,1), frequency = 12)


#3 months hargreaves SPEI
spei1 <- spei(sokoto_har_ts[,"harP"], 3)


#Create the a datafrane from the fitted SPEI values
sokoto_spei1_har <- ts_data.frame(spei1$fitted) %>% 
  mutate(sign = ifelse(value >= 0, "pos", "neg"), Period = zoo::as.yearmon(time, "%Y/%m"))
         
         
         
#Plot the hargreaves Spei values
ggplot(data = sokoto_spei1_har, aes(x = Period, y = value)) +
  geom_line(size = 1, col = "#e8647c") +
   geom_hline(yintercept = 0 , linetype = "solid", color = "black", size = 0.8) +
    scale_y_continuous(limits = c(-3, 3), 
                     breaks = -3:3) + 
         scale_x_yearmon(format="%Y", n=20) +
theme(axis.text.x = element_text(angle = 45), 
        plot.title = element_text(face = "bold", size = 15, hjust = 0.5)) +
         ggtitle("Sokoto 3-months SPEI (Hargreaves)") +
           xlab("Year") + ylab("SPEI") +
             geom_hline(yintercept = c(-0.99, -1.49, -2.00), 
                        linetype = "dashed", size = 0.8)



#Using the self calibrated palmer drought severity Index
#sc-PDSI
library(scPDSI)

#Get the result of  pdsi
pdsi6 <- (pdsi(P = sokoto_2$RAIN,PE = sokoto_2$PET, start = 1986, sc = F))


#Convert fitted values to a dataframe
sokoto_sc <- ts_data.frame(pdsi6$X) %>% na.omit() %>% 
  mutate(sign = ifelse(value >= 0, "pos", "neg"), Period = zoo::as.yearmon(time, "%Y/%m"))


#Plot sc-PDSI values
ggplot(data = sokoto_sc, aes(x = Period, y = value)) +
    geom_line(size = 1, col = "#e8647c") +
      geom_hline(yintercept = 0 , linetype = "solid", color = "black", size = 0.8) +
         scale_y_continuous(limits = c(-6, 8), breaks = -6:8) + 
            scale_x_yearmon(format="%Y", n=20) +
              theme(axis.text.x = element_text(angle = 
                                     45), plot.title = element_text(face = "bold", size = 15, hjust = 0.5)) +
  ggtitle("Sokoto sc-PDSI") +
  xlab("Year") + ylab("SPEI") +
  geom_hline(yintercept = c(-1.0, -2.00, -3.00), linetype = "dashed", size = 0.8)



#Compare the results of drought Indices
cor.test(sokoto_spei1$value, sokoto_spei1_har$value, method = "pearson")
cor.test(sokoto_spei1$value, sokoto_sc$value, method = "pearson")
cor.test(sokoto_spei1_har$value, sokoto_sc$value, method = "pearson")

#seasonal mannkendall and senslope
library(trend)
library(styler)
#This function combines the result of the seasonal mannkendall into a dataframe
mannKendall <- function(x){
                time <- ts(x, start = 1986, frequency = 12)
                manken <- smk.test(time)
                cbind(season ,S = manken$Sg, varSg = round(manken$varSg),
                      tau = round(manken$taug, 4), Z = round(manken$Zg, 4),
                        "Pr(>|z|)" = round(manken$pvalg, 4), 
                      Signifi = ifelse(manken$pvalg = 0, "***",
                      ifelse(manken$pvalg < 0.001 & manken$pvalg > 0, "**",
                      ifelse(manken$pvalg < 0.01  & manken$pvalg > 0.01, "*"))))z
                     
}
                      
                      
                      
                      
library(trend)
mannKendall <- function(x){
  time <- ts(x, start = 1989, frequency = 12)
  manken <- smk.test(time)
  cbind(season ,S = manken$Sg, varSg = round(manken$varSg),
        tau = round(manken$taug, 4), Z = round(manken$Zg, 4),
        "Pr(>|z|)" = round(manken$pvalg, 4), Signifi = ifelse(manken$pvalg < 0.05, "*", " "))
}


#Apply function to the
#Use the monthly data (sokoto_2)
summary(smk.test(sokoto_2_ts[,"RAIN"]))

         