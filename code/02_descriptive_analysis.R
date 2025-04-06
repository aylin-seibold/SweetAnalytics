#### Structure of this code ########
#----------------------------------#
# 1. Boxplots of Continuous Variables
# 2. Frequency Plots of the Categorical Variables
# 3. Frequency Plots for the Top and Bottom 30% Most Popular Candies
#----------------------------------#

#----------------------------------#
#### 1. Boxplots of Continuous Variables ####
#----------------------------------#

# Winpercent
boxplot.winprop <- ggplot(candy.data, aes(x = win.prop) ) +
  geom_boxplot() +
  labs(
    title = "Verteilung des Gewinnprozentsatzes basierend auf 269.000 Vergleichen",
    x = "Gewinnprozentsatz"
    ) +
  coord_flip() +
  theme(axis.title.y = element_blank()) 
ggsave("plots/boxplot.winprop.pdf", plot = boxplot.winprop, device = "pdf", width = 8, height = 6)

# Sugarpercent
boxplot.sugarpercent <- ggplot(candy.data, aes(x = sugarpercent) ) +
  geom_boxplot() +
  labs(
    title = "Verteilung der Zuckerperzentile",
    x = "Perzentile"
  ) +
  coord_flip() +
  theme(axis.title.y = element_blank()) 
ggsave("plots/boxplot.sugarpercent.pdf", plot = boxplot.sugarpercent, device = "pdf", width = 8, height = 6)

# Pricepercent
boxplot.pricepercent <- ggplot(candy.data, aes(x = pricepercent) ) +
  geom_boxplot() +
  labs(
    title = "Verteilung der Preisperzentile",
    x = "Perzentile"
  ) +
  coord_flip() +
  theme(axis.title.y = element_blank()) 
ggsave("plots/boxplot.pricepercent.pdf", plot = boxplot.pricepercent, device = "pdf", width = 8, height = 6)

#----------------------------------#
#### 2. Frequency plots of the Categorical Variables ####
#----------------------------------#

# Calculate the frequency of each binary categorical feature (e.g., chocolate, fruity, caramel, etc.)
# The goal is to visualize how often each ingredient/feature appears across all candies in the dataset.

# Total sample size (total number of candies)
n <- nrow(candy.data)

# Specify the binary categorical features (each coded as 0 or 1)
features <- c("chocolate", "fruity", "caramel", "peanutyalmondy", 
              "nougat", "crispedricewafer", "hard", "bar", "pluribus")

# Function to count the frequency of each feature in the dataset
long.counts <- function(dt, group.label) {
  melted <- melt(dt[, ..features], measure.vars = features,
                 variable.name = "feature", value.name = "value")
  melted <- as.data.table(melted)
  counts <- melted[value == 1, .N, by = feature]
  counts[, group := group.label]
  return(counts)
}

# Get counts for all features in the dataset
counts.data <- long.counts(candy.data, features)

# Create bar plot for the frequencies of each categorical feature
barplot.categories <- ggplot(counts.data, aes(x = feature, y = N/n)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Häufigkeit der Kategorien",
    x = "Merkmal",
    y = "Anteil"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/barplot.categories.pdf", plot = barplot.categories, device = "pdf", width = 8, height = 6)

#----------------------------------#
#### 3. Frequency Plots for the Top and Bottom 30% Most Popular Candies ####
#----------------------------------#


top.n <- ceiling(n * 0.30)  # Calculate the number of top 30% candies based on the total sample size

# Select top 30% candies based on popularity
top.30 <- candy.data[order(-win.prop)][1:top.n]

# Select bottom 30% candies
bottom.30 <- candy.data[order(win.prop)][1:top.n]

# Create frequency tables of binary features for top and bottom 30%
counts.top <- long.counts(top.30, "Top 30%")
counts.bottom <- long.counts(bottom.30, "Bottom 30%")
freq.all <- rbind(counts.top, counts.bottom)

# Bar plot comparing feature frequencies in top and bottom 30% groups
barplot.cat.30 <- ggplot(freq.all, aes(x = feature, y = N/n, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Häufigkeit von Merkmalen in Top 30% vs. Bottom 30% Süßigkeiten",
    x = "Merkmal",
    y = "Anzahl",
    fill = "Gruppe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/barplot.cat.30.pdf", plot = barplot.cat.30, device = "pdf", width = 8, height = 6)

# Categorical variables to analyze
categories <- c("sugar.category", "price.category")

# Count frequency of each category level for both top and bottom 30% groups
category.counts <- function(dt, group.label) {
  result.list <- lapply(categories, function(cat) {
    dt[, .N, by = cat][, .(category = get(cat), feature = cat, count = N)]
  })
  result <- rbindlist(result.list)
  result[, group := group.label]
  return(result)
}

# Get category counts for both groups
counts.top.cat <- category.counts(top.30, "Top 30%")
counts.bottom.cat <- category.counts(bottom.30, "Bottom 30%")

# Combine both for plotting
category.data <- rbind(counts.top.cat, counts.bottom.cat)

# Create bar plot comparing sugar and price category frequencies in top and bottom 30% groups
barplot.sugar.price.30 <- ggplot(category.data, aes(x = category, y = count/n, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ feature, scales = "free_x") +
  labs(
    title = "Verteilung von Zucker- und Preiskategorien in Top vs. Bottom 30%",
    x = "Kategorie",
    y = "Anteil",
    fill = "Gruppe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/barplot.sugar.price.30.pdf", plot = barplot.sugar.price.30 , device = "pdf", width = 8, height = 6)

# Create a binary column indicating whether a candy is in the top 30% (1 for top, 0 for not)
candy.data[, top30 := fifelse(competitorname %in% top.30$competitorname, 1, 0)]

# Create a binary column indicating whether a candy is in the bottom 30% (1 for bottom, 0 for not)
candy.data[, bottom30 := fifelse(competitorname %in% top.30$competitorname, 0, 1)]


