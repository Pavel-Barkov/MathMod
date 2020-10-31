install.packages("xlsx", dep = T)
install.packages("readr")
library(xlsx)
library(readr)
plot(Trees$'Ht (m)', Trees$'Crown Diameter (m)')
plot(Trees$'Ht (m)', Trees$'Crown Diameter (m)', xlab = "Высота (м)", ylab = "Диаметр кроны (м)")
install.packages("ggplot2")
library(ggplot2)
ggplot(Trees1, aes(x = 'Ht (m)', y = 'Crown Diameter (m)')) + geom_point()
ggplot(Trees1, aes(x = 'Ht (m)', y = 'Crown Diameter (m)', color = Species)) + geom_point()


#Задание. Произвести "очистку" таблицы таким образом, что:
#Все переменные имеют корректный тип данных
#Повторяющиеся переменные убраны
#Из имен переменных убраны размерности
#Всем переменам заданы их реальные размерности
#Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
#Категориальные переменные должны быть факторами
#Категории переменной из имени должны быть убраны
#Коды категориальных переменных заменены их категориями
#Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
#Виды должны быть переименованы на латыне

#Все переменные имеют корректный тип данных - да

#Повторяющиеся переменные убраны

dbh (mm)
HR
% Variation

Trees1 = Trees1 %>% select(-`dbh (mm)`)
Trees1 = Trees1 %>% select(-'HR')
Trees1 = Trees1 %>% select(-'% Variation')

#Из имен переменных убраны размерности

Trees1 = Trees1 %>% rename(dbh = `dbh (m)`)
Trees1 = Trees1 %>% rename(Ht = 'Ht (m)')
Trees1 = Trees1 %>% rename(ClearanceHt = 'Clearance Ht (m)')
Trees1 = Trees1 %>% rename(Clearance_Ht = 'ClearanceHt')
Trees1 = Trees1 %>% rename(Total_N.S.E.W_Radial_Crown_Spread = 'Total N.S.E.W Radial Crown Spread (m)')
Trees1 = Trees1 %>% rename(Average_Radial_Crown_Spread = 'Average Radial Crown spread (m)')
Trees1 = Trees1 %>% rename(Total_Mean_Radial_Crown_Spread = 'Total Mean Radial Crown Spread (m)')
Trees1 = Trees1 %>% rename(Crown_Diameter = 'Crown Diameter (m)')
Trees1 = Trees1 %>% rename(Stem_diameter_Jan_2017 = 'Stem diameter Jan 2017 (mm)')
Trees1 = Trees1 %>% rename(Second_yr_dia_gain = '2yr dia gain (mm)')
Trees1 = Trees1 %>% rename(Annual_Girth_Increment = 'Annual Girth Increment (mm)')
Trees1 = Trees1 %>% rename(Crown_Depth = 'Crown Depth (m)')

#Всем переменам заданы их реальные размерности
library(units)
units(Trees1$Ht) = as_units("m")
Trees1$Ht
Trees1 %>% as.data.frame()
units(Trees1$dbh) = as_units("m")
units(Trees1$Clearance_Ht) = as_units("m")
units(Trees1$Crown_Depth) = as_units("m")
units(Trees1$Total_N.S.E.W_Radial_Crown_Spread) = as_units("m")
units(Trees1$Average_Radial_Crown_Spread) = as_units("m")
units(Trees1$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(Trees1$Crown_Diameter) = as_units("m")
units(Trees1$Stem_diameter_Jan_2017) = as_units("mm")
units(Trees1$Second_yr_dia_gain) = as_units("mm")
units(Trees1$Annual_Girth_Increment) = as_units("mm")
units(Trees1$`Predicted crown diamet using combined formulla`) = as_units("m")


#Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной

Trees1 = Trees1 %>%
  mutate(S = as.numeric(S))%>%
  mutate(error = `Predicted crown diamet using combined formulla`-`Crown_Diameter`)
Trees1 = Trees1 %>% mutate(error = 'Predicted crown diamet using combined formulla' - Crown_Diameter)
Trees1 = Trees1 %>% select(-'Difference')

#Категориальные переменные должны быть факторами

library(forcats)
library(sf)
Trees1$`Data Set      1=Norwich                0= Peterborough`
Trees1 = Trees1 %>%
  mutate(Data_Set = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(Data_Set = fct_recode(Data_Set, Norwich = "1", Peterborough = "0"))

#Виды должны быть переименованы на латыне
# Transform all to latin 
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis

Trees1$Species
Trees1$Species[Trees1$Species == "Oak"] = "Quercus robur"
Trees1$Species[Trees1$Species == "Norway maple"] = "Acer platanoides"
Trees1$Species[Trees1$Species == "Norway Maple"] = "Acer platanoides"
Trees1$Species[Trees1$Species == "Silver Birch"] = "Betula pendula"
Trees1$Species[Trees1$Species == "Sycomore"] = "Platanus occidentalis"

#Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84

library(stringr)
Trees1$`Grid Reference`
coord1 = str_replace_all(Trees1$`Grid Reference`, ' ', '')
coord_n = str_trunc(coord1, 12, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
coord_e = str_trunc(coord1, 7, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
quadr = str_trunc(coord1, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_e), as.integer(coord_n), quadr)

names(table_c)=c("E", "N", "Quadr")
table_c = na.exclude(table_c)

table_c = table_c %>% mutate("Easting_BC" = case_when(
  Quadr == "TF" ~ E +600000, 
))
table_c = table_c %>% mutate("Northing_BC" = case_when(
  Quadr == "TF" ~ N +300000, 
))
table_c = table_c %>% mutate("Easting_BC" = case_when(
  Quadr == "TG" ~ E +700000, 
))
table_c = table_c %>% mutate("Northing_BC" = case_when(
  Quadr == "TG" ~ N +300000, 
))
table_c = table_c %>% mutate("Easting_BC" = case_when(
  Quadr == "TL" ~ E +600000, 
))
table_c = table_c %>% mutate("Northing_BC" = case_when(
  Quadr == "TL" ~ N +200000, 
))
table_c = na.exclude(table_c)

table_WGS =
  table_c %>%
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()

table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head


Trees1$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)
