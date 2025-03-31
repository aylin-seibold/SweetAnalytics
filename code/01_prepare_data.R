#----------------------------------#
#### Structure of this code ########
#----------------------------------#
# 1. Load Data
# 2. Pre-processing of data
#----------------------------------#

# License: Creative Commons Attribution 4.0 International License (CC BY 4.0)
# Attribution: FiveThirtyEight - Candy Power Ranking Dataset
# Link: https://github.com/fivethirtyeight/data/tree/master/candy-power-ranking

#----------------------------------#
#### 1. Load Data ####
#----------------------------------#


url <- "https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/candy-power-ranking/candy-data.csv"
data.raw <- fread(url)

#----------------------------------#
#### 2. Pre-processing of data ####
#----------------------------------#

# Conversion of categorical data to factor variables
candy.data <- data.raw[, (names(data.raw)[sapply(data.raw, is.integer)]) := lapply(.SD, as.factor), 
                       .SDcols = sapply(data.raw, is.integer)]


#  Conversion of sugar percentage and price percentage into categorical variables
candy.data[, sugar.category := cut(sugarpercent, 
                                   breaks = c(0, 0.33, 0.66, 1), 
                                   labels = c("Niedrig", "Mittel", "Hoch"))]

candy.data[, price.category := cut(pricepercent, 
                                   breaks = c(0, 0.33, 0.66, 1), 
                                   labels = c("Günstig", "Mittel", "Teuer"))]

# Conversion of win percentage to a proportional value
candy.data[, win.prop := winpercent / 100]
