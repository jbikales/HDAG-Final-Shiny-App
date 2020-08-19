library(tidyverse)
library(readr)

mobility_raw <- read.csv("NEW DATA/Global_Mobility_report.csv") 

# Denver

den_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "Colorado" & sub_region_2 == "Denver County")

den.aqi.2020 <- read.csv("NEW DATA/Denver/aqidaily2020.csv") 
den.aqi.2019 <- read.csv("NEW DATA/Denver/aqidaily2019.csv") 

sub.den.aqi.2019 <- den.aqi.2019[46:213,]
den.baseline <- median(sub.den.aqi.2019[,"Overall.AQI.Value"])

sub.den.aqi <- den.aqi.2020[46:213,]
den.percent.change <- ((sub.den.aqi[, "Overall.AQI.Value"] - den.baseline)/den.baseline)*100

pct_den <- as_tibble(den.percent.change) %>% 
  mutate(id = row_number())

mob_den <- as_tibble(den_mobility) %>% 
  mutate(id = row_number())

combined_den <- right_join(pct_den, mob_den)

write_rds(combined_den, "HDAG-Sustainability/clean-data/denver.rds")

# New York

nyc_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "New York" & sub_region_2 == "New York County")

nyc.aqi.2020 <- read.csv("NEW DATA/New York/aqidaily2020.csv") 
nyc.aqi.2019 <- read.csv("NEW DATA/New York/aqidaily2019.csv") 

sub.nyc.aqi.2019 <- nyc.aqi.2019[46:213,]
nyc.baseline <- median(sub.nyc.aqi.2019[,"Overall.AQI.Value"])

sub.nyc.aqi <- nyc.aqi.2020[46:213,]
nyc.percent.change <- ((sub.nyc.aqi[, "Overall.AQI.Value"] - nyc.baseline)/nyc.baseline)*100

pct_nyc <- as_tibble(nyc.percent.change) %>% 
  mutate(id = row_number())

mob_nyc <- as_tibble(nyc_mobility) %>% 
  mutate(id = row_number())

combined_nyc <- right_join(pct_nyc, mob_nyc)

write_rds(combined_nyc, "HDAG-Sustainability/clean-data/new_york.rds")

# Hennepin

hen_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "Minnesota" & sub_region_2 == "Hennepin County")

hen.aqi.2020 <- read.csv("NEW DATA/Hennepin/aqidaily2020.csv") 
hen.aqi.2019 <- read.csv("NEW DATA/Hennepin/aqidaily2019.csv") 

sub.hen.aqi.2019 <- hen.aqi.2019[46:213,]
hen.baseline <- median(sub.hen.aqi.2019[,"Overall.AQI.Value"])

sub.hen.aqi <- hen.aqi.2020[46:213,]
hen.percent.change <- ((sub.hen.aqi[, "Overall.AQI.Value"] - hen.baseline)/hen.baseline)*100

pct_hen <- as_tibble(hen.percent.change) %>% 
  mutate(id = row_number())

mob_hen <- as_tibble(hen_mobility) %>% 
  mutate(id = row_number())

combined_hen <- right_join(pct_hen, mob_hen)

write_rds(combined_hen, "HDAG-Sustainability/clean-data/hennepin.rds")


# Hawaii

hi_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "Hawaii" & sub_region_2 == "Hawaii County")

hi.aqi.2020 <- read.csv("NEW DATA/Hawaii/aqidaily2020.csv") 
hi.aqi.2019 <- read.csv("NEW DATA/Hawaii/aqidaily2019.csv") 

sub.hi.aqi.2019 <- hi.aqi.2019[46:213,]
hi.baseline <- median(sub.hi.aqi.2019[,"Overall.AQI.Value"])

sub.hi.aqi <- hi.aqi.2020[46:213,]
hi.percent.change <- ((sub.hi.aqi[, "Overall.AQI.Value"] - hi.baseline)/hi.baseline)*100

pct_hi <- as_tibble(hi.percent.change) %>% 
  mutate(id = row_number())

mob_hi <- as_tibble(hi_mobility) %>% 
  mutate(id = row_number())

combined_hi <- right_join(pct_hi, mob_hi)

write_rds(combined_hi, "HDAG-Sustainability/clean-data/hawaii.rds")


# Suffolk

bos_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "Massachusetts" & sub_region_2 == "Suffolk County")

bos.aqi.2020 <- read.csv("NEW DATA/Suffolk/aqidaily2020.csv") 
bos.aqi.2019 <- read.csv("NEW DATA/Suffolk/aqidaily2019.csv") 

sub.bos.aqi.2019 <- bos.aqi.2019[46:213,]
bos.baseline <- median(sub.bos.aqi.2019[,"Overall.AQI.Value"])

sub.bos.aqi <- bos.aqi.2020[46:213,]
bos.percent.change <- ((sub.bos.aqi[, "Overall.AQI.Value"] - bos.baseline)/bos.baseline)*100

pct_bos <- as_tibble(bos.percent.change) %>% 
  mutate(id = row_number())

mob_bos <- as_tibble(bos_mobility) %>% 
  mutate(id = row_number())

combined_bos <- right_join(pct_bos, mob_bos)

write_rds(combined_bos, "HDAG-Sustainability/clean-data/suffolk.rds")

# San Juan County

sj_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "New Mexico" & sub_region_2 == "San Juan County")

sj.aqi.2020 <- read.csv("NEW DATA/San Juan/aqidaily2020.csv") 
sj.aqi.2019 <- read.csv("NEW DATA/San Juan/aqidaily2019.csv") 

sub.sj.aqi.2019 <- sj.aqi.2019[46:213,]
sj.baseline <- median(sub.sj.aqi.2019[,"Overall.AQI.Value"])

sub.sj.aqi <- sj.aqi.2020[46:213,]
sj.percent.change <- ((sub.sj.aqi[, "Overall.AQI.Value"] - sj.baseline)/sj.baseline)*100

pct_sj <- as_tibble(sj.percent.change) %>% 
  mutate(id = row_number())

mob_sj <- as_tibble(sj_mobility) %>% 
  mutate(id = row_number())

combined_sj <- right_join(pct_sj, mob_sj)

write_rds(combined_sj, "HDAG-Sustainability/clean-data/san_juan.rds")


# San Joaquin County

joa_mobility <- mobility_raw %>% 
  filter(sub_region_1 == "California" & sub_region_2 == "San Joaquin County")

joa.aqi.2020 <- read.csv("NEW DATA/San Joaquin/aqidaily2020.csv") 
joa.aqi.2019 <- read.csv("NEW DATA/San Joaquin/aqidaily2019.csv") 

sub.joa.aqi.2019 <- joa.aqi.2019[46:213,]
joa.baseline <- median(sub.joa.aqi.2019[,"Overall.AQI.Value"])

sub.joa.aqi <- joa.aqi.2020[46:213,]
joa.percent.change <- ((sub.joa.aqi[, "Overall.AQI.Value"] - joa.baseline)/joa.baseline)*100

pct_joa <- as_tibble(joa.percent.change) %>% 
  mutate(id = row_number())

mob_joa <- as_tibble(joa_mobility) %>% 
  mutate(id = row_number())

combined_joa <- right_join(pct_joa, mob_joa)

write_rds(combined_joa, "HDAG-Sustainability/clean-data/san_joaquin.rds")