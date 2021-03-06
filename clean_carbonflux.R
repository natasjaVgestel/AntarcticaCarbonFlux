# script to clean Antarctica Carbon Flux data

# load packages
library(dplyr)
library(tidyr)
library(purrr)

# read in data by site
site1 <- list.files(path = "./site 1/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read.csv(., skip = 13, header = FALSE, colClasses = c(rep("numeric", 3),
                                                                rep("character", 2),
                                                                rep("numeric", 8),
                                                                rep("character", 3),
                                                                rep("numeric", 31),
                                                                "character",
                                                                rep("numeric", 13),
                                                                "character",
                                                                rep("numeric", 13),
                                                                rep("character", 5),
                                                                rep("numeric", 21))))

site2 <- list.files(path = "./site 2/",
                    pattern = "*.csv", 
                    full.names = T) %>% 
  map_df(~read.csv(., skip = 13, header = FALSE, colClasses = c(rep("numeric", 3),
                                                               rep("character", 2),
                                                               rep("numeric", 8),
                                                               rep("character", 3),
                                                               rep("numeric", 31),
                                                               "character",
                                                               rep("numeric", 13),
                                                               "character",
                                                               rep("numeric", 13),
                                                               rep("character", 5),
                                                               rep("numeric", 21))))

site3 <- list.files(path = "./site 3/",
                    pattern = "*.csv", 
                    full.names = T) %>% 
  map_df(~read.csv(., skip = 13, header = FALSE, colClasses = c(rep("numeric", 3),
                                                               rep("character", 2),
                                                               rep("numeric", 8),
                                                               rep("character", 3),
                                                               rep("numeric", 31),
                                                               "character",
                                                               rep("numeric", 13),
                                                               "character",
                                                               rep("numeric", 13),
                                                               rep("character", 5),
                                                               rep("numeric", 21))))

site4 <- list.files(path = "./site 4/",
                    pattern = "*.csv", 
                    full.names = T) %>% 
  map_df(~read.csv(., skip = 13, header = FALSE, colClasses = c(rep("numeric", 3),
                                                                rep("character", 2),
                                                                rep("numeric", 8),
                                                                rep("character", 3),
                                                                rep("numeric", 31),
                                                                "character",
                                                                rep("numeric", 13),
                                                                "character",
                                                                rep("numeric", 13),
                                                                rep("character", 5),
                                                                rep("numeric", 21))))

# create list of column names
col_names <- c("obs", "sys_time", "elapsed", "date", "sys_hhmmss", "vtot", "rho", "Flux", "virga", 
               "vchamber", "area", "R", "collar_offset", "plot", "measurement", "notes", 
               "gasex_time", "E", "A", "RHcham", "VPcham", "SVPcham", "Leak", "LeakPct", "CorrFact", 
               "CorrFactPct", "Fan", "Qin", "Qabs", "alpha","convert", "S", "meas_time", "CO2_s", 
               "CO2_r", "H2O_s", "H2O_r", "Flow", "Pa", "ΔPcham", "Tair", "Tleaf", "Tleaf2", 
               "Fan_speed", "Qamb_in", "Qamb_out", "match_time", "match_hhmmss", "count", "co2_adj", 
               "h2o_adj", "co2_at", "h2o_at", "co2_cv", "h2o_cv",
               "CO2_s_d:MN", "CO2_s_d:SLP", "CO2_s_d:SD", "CO2_s_d:OK", "Stable", "Total", 
               "State", "ADC_CH1", "ADC_CH2", "ADC_CH3", "ADC_CH4", "ADC_CH5", "ADC_CH6", 
               "ADC_CH7", "ADC_CH8", "DAC_1", "DAC_2", "DAC_3", "DAC_4", "GPIO", "GPIO_dir",
               "excit_5v", "power_12v", "power_5v", "ch1_pullup", "AuxPower", "MatchValveR", 
               "MatchValveS", "MatchCO2", "MatchH2O", "DIAG", "Flow_s", "Flow_r", "Txchg", 
               "Tirga", "Tchopper", "Ts", "Tr", "CO2_%", "Desiccant_%", "Humidifier_%", 
               "Txchg_sp", "CO2_r_sp", "H2O_r_sp", "SS_s", "SS_r")

# add column names
names(site1) <- col_names
names(site2) <- col_names
names(site3) <- col_names
names(site4) <- col_names

# add site IDs
site1$site <- rep(1)
site2$site <- rep(2)
site3$site <- rep(3)
site4$site <- rep(4)

# combine all sites into one dataset
data_all <- bind_rows(site1, site2, site3, site4)

# remove measurements that were redone
data_all <- subset(data_all, notes != "redone")

# create warming treatment column
data_all <- separate(data = data_all, col = plot, into = c("plot_id", "treatment"), sep = "(?<=[0-9])(?=[A-Za-z])", remove = FALSE)
data_all$treatment[data_all$treatment == "c"] <- "control"
data_all$treatment[data_all$treatment == "w"] <- "warming"

data_all <- dplyr::select(data_all, -plot_id)

# remove obs column and reorder remaining columns
data_all <- dplyr::select(data_all, -obs)
data_all <- dplyr::select(data_all, date, site, plot, treatment, measurement, notes, everything())

# consolidate measurement naming
data_all$measurement[data_all$measurement == "nee" |
                       data_all$measurement == "ne" |
                       data_all$measurement == "NEE " |
                       data_all$measurement == "Nee" |
                       data_all$measurement == " nee"] <- "NEE"
data_all$measurement[data_all$measurement == "er" |
                       data_all$measurement == "ner"] <- "ER"
data_all$measurement[data_all$measurement == "NEE OR ER" |
                       data_all$measurement == "NEE or ER"] <- ""

# consolidate notes
data_all$notes[data_all$notes == "d" |
                           data_all$notes == "dr" |
                           data_all$notes == "black" |
                           data_all$notes == "dark, wrong collar ht" |
                           data_all$notes == "dark, skipped shade2"] <- "dark"
data_all$notes[data_all$notes == "lght" |
                 data_all$notes == "l" |
                 data_all$notes == "high"] <- "light"
data_all$notes[data_all$notes == "shade 1" |
                 data_all$notes == "shade " |
                 data_all$notes == "shade 1 now" |
                 data_all$notes == "shade 1, wrong collar ht.     ade" |
                 data_all$notes == "single shade" |
                 data_all$notes == "shade1 " |
                 data_all$notes == "shade1" |
                 data_all$notes == "shade2" |
                 data_all$notes == "shade 2" |
                 data_all$notes == "shade 2, wrong collar ht. " |
                 data_all$notes == "double shade" |
                 data_all$notes == "sheer" |
                 data_all$notes == "sher" |
                 data_all$notes == "sheer1" |
                 data_all$notes == "sheer cloth" |
                 data_all$notes == "sheer doubled" |
                 data_all$notes == "kelly shadow" |
                 data_all$notes == "shadow" |
                 data_all$notes == "kelly + cloth" |
                 data_all$notes == "dark1" |
                           data_all$notes == "dark2" |
                           data_all$notes == "darklight"] <- "shade"

# remove shade measurements
data_all <- subset(data_all, notes != "shade")

# set missing measurement type for light and dark measurements and remove from notes
data_all$measurement[data_all$notes == "light"] <- "NEE"
data_all$measurement[data_all$notes == "dark"] <- "ER"

data_all$notes[data_all$notes == " " |
                 data_all$notes == "light" |
                 data_all$notes == "dark"] <- ""

data_all$measurement[data_all$measurement == "" & data_all$Qamb_out > 30] <- "NEE"
data_all$measurement[data_all$measurement == "" & data_all$Qamb_out < 30] <- "ER"

write.csv(data_all, "data_all.csv")
