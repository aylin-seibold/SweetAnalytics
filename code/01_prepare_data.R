#----------------------------------#
#### Structure of this code ########
#----------------------------------#
# 1. Load Packages
# 2. Load Data
# 3. Pre-processing of Data
#----------------------------------#

#----------------------------------#
#### 1. Load Packages ####
#----------------------------------#

packages <- c("ggplot2", "data.table", "randomForest")
if(length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
invisible(lapply(packages, library, character.only = TRUE))

#----------------------------------#
#### 2. Load Data ####
#----------------------------------#

# License: Creative Commons Attribution 4.0 International License (CC BY 4.0)
# Attribution: FiveThirtyEight - Candy Power Ranking Dataset
# Link: https://github.com/fivethirtyeight/data/tree/master/candy-power-ranking

if (file.exists("data/raw/candy.data.raw")){
  candy.data.raw <- readRDS("data/raw/candy.data.raw")
} else {
  url <- "https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/candy-power-ranking/candy-data.csv"
  candy.data.raw <- fread(url)
  # Save raw data
  saveRDS(candy.data.raw, "data/raw/candy.data.raw")
}

#----------------------------------#
#### 3. Pre-processing of Data ####
#----------------------------------#

if (file.exists("data/raw/candy.data.raw")){
  candy.data <- readRDS("data/intermediate/candy.data")
} else {
# Conversion of categorical data to factor variables
candy.data <- candy.data.raw[, (names(candy.data.raw)[sapply(candy.data.raw, is.integer)]) := lapply(.SD, as.factor), 
                       .SDcols = sapply(candy.data.raw, is.integer)]


#  Conversion of sugar percentage and price percentage into categorical variables
candy.data[, sugar.category := cut(sugarpercent, 
                                   breaks = c(0, 0.33, 0.66, 1), 
                                   labels = c("Niedrig", "Mittel", "Hoch"))]

candy.data[, price.category := cut(pricepercent, 
                                   breaks = c(0, 0.33, 0.66, 1), 
                                   labels = c("Günstig", "Mittel", "Teuer"))]

# Conversion of win percentage to a proportional value
candy.data[, win.prop := winpercent / 100]

# Save prepocessed data
saveRDS(candy.data, "data/intermediate/candy.data")
}