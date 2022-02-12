 # Desmatamento
Meus códigos sobre desmatamento na Am. legal

# Here's a link to get the data that I used in this project
https://www.kaggle.com/mbogernetto/brazilian-amazon-rainforest-degradation?select=inpe_brazilian_amazon_fires_1999_2019.csv

setwd("~/R")
library(readr)
library(ggplot2)
library(lubridate)
library(sf)
library(maps)
library(dplyr)
library(tidyverse)
library(broom)
library(reshape2)


# Deforestation area (Km²) by year and state, from 2004 to 2019.
area_2004_2019 <- read.csv2("def_area_2004_2019.csv", header = T, sep = ",")
View(area_2004_2019)

# Data about start year, end year, and severity of 2 of the most important climatic phenomena.
el_nino_la_nina_1999_2019 <- read.csv2("el_nino_la_nina_1999_2019.csv", header = T, sep = ",")
View(el_nino_la_nina_1999_2019)

# Amount of fire outbreaks in Brazilian Amazon by state, month and year, from 1999 to 2019.
inpe_brazilian_amazon_fires_1999_2019 <- read.csv2("inpe_brazilian_amazon_fires_1999_2019.csv", header = T, sep = ",")
View(inpe_brazilian_amazon_fires_1999_2019)

# Investigate the type of the variable 
str(area_2004_2019)
str(el_nino_la_nina_1999_2019)
str(inpe_brazilian_amazon_fires_1999_2019)

#Summary
summary(area_2004_2019)
summary(el_nino_la_nina_1999_2019)
summary(inpe_brazilian_amazon_fires_1999_2019)

# Month and year together
inpe_brazilian_amazon_fires_1999_2019$date <- with(inpe_brazilian_amazon_fires_1999_2019, ym(sprintf('%04d%02d', year, month)))

ggplot(data=area_2004_2019, aes(x=Ano.Estados, y=AMZ.LEGAL)) + geom_line() + scale_x_binned(breaks = seq(2004, 2019, by =1)) +
  labs(x = "Ano", y = "Área de desmatamento Km2", title = "Desmatamento na Amazônia Legal (2004-2019)" ) + theme_minimal()


# Plotting the firesposts at Amazônia Legal
ggplot(inpe_brazilian_amazon_fires_1999_2019, aes(date, firespots)) +
  geom_bar(stat="identity", na.rm = TRUE) +
  ggtitle("Qtde de incêndios por mês na Amazônia Legal (1990 - 2019) ") +
  xlab("Ano") + ylab("Foco de incêndios") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=18)) + theme_minimal()

# Amount of firespots by state
ggplot(data =inpe_brazilian_amazon_fires_1999_2019) +
  geom_line(aes(x= date, y = firespots), color="gray") + 
  facet_wrap(~state, nrow=3) + theme_minimal()


# Creating seasons and plotting them by year
df_Seasons <- inpe_brazilian_amazon_fires_1999_2019 %>%
  mutate(season =
           ifelse(month %in% c(1, 2, 3), "Verão",
                  ifelse(month %in% c(4, 5, 6), "Outono",
                         ifelse(month %in% c(7, 8, 9), "Inverno", "Primavera"))))
ggplot(data = df_Seasons) + 
  geom_line(aes(x=date, y = firespots), color = "gray") +
       facet_wrap(~season, nrow = 1) +theme_minimal()

# Total deforested area in the Am Legal states and the sum of them per year

area_2004_2019 %>%
  rename(
    Ano.Estados = ï..Ano.Estados)
colnames(area_2004_2019)<- c('Ano.Estados')
colnames(area_2004_2019)

estados<-melt(area_2004_2019, id = "Ano.Estados") 
head(estados)
ggplot(estados, aes(x=Ano.Estados, y= value, colour = variable, group = variable)) + 
  geom_line() + ggtitle("Total de Área desmatada na Amazônia Legal (Km2) (1990 - 2019) ") +
  xlab("Ano") + ylab("Área em Km2")  + theme_minimal()

#Criando dummies para El Nino e La Nina

el_nino_la_nina_1999_2019$Male <- ifelse(el_nino_la_nina_1999_2019$phenomenon == 'El Nino', 1, 0) 
el_nino_la_nina_1999_2019$Female <- ifelse(el_nino_la_nina_1999_2019$phenomenon == 'La Nina', 1, 0)

#Merging two data sets
merge1 <- merge(inpe_brazilian_amazon_fires_1999_2019, el_nino_la_nina_1999_2019, by.x = "year", by.y = "start.year", 
                all = T, no.dups = T)
View(merge1)

#Two simple regressions with summary
ninolm <- lm(firespots ~ Male, data = merge1)
summary(ninolm)

ninalm <- lm(firespots~ Female, data = merge1)
summary(ninalm)


abline(lm(firespots~ Female, data = merge1))


ggplot(data = inpe_brazilian_amazon_fires_1999_2019, aes(geometry = geometry)) +
  geom_sf() + 
  geom_point(data = inpe_brazilian_amazon_fires_1999_2019, aes(x = longitude, y = latitude), size = 4,
             shape = 23, fill = "darkred") +
               coord_sf(xlim = c(-88,-78), ylim = c(24.5, 33), expand = F)

