#### Structure of this code ########
#----------------------------------#
# 1. Boxplots for Continuous Variables
# 2. Frequency Plots of the Categorical Variables
# 3. Frequency Plots for the Top and Bottom 30% Most Popular Sweets
#----------------------------------#

#----------------------------------#
#### 1. Boxplots for Continuous Variables ####
#----------------------------------#

# Winpercent
boxplot.winprop <- ggplot(candy.data, aes(x = win.prop) )+
  geom_boxplot() +
  labs(
    title = "Verteilung des Gewinnprozentsatzes basierend auf 269.000 Vergleichen",
    x = "Gewinnprozentsatz"
    ) +
  coord_flip() +
  theme(axis.title.y = element_blank()) 
ggsave("plots/boxplot.winprop.pdf", plot = boxplot.winprop, device = "pdf", width = 8, height = 6)

# Sugarpercent
boxplot.sugarpercent <- ggplot(candy.data, aes(x = sugarpercent) )+
  geom_boxplot() +
  labs(
    title = "Verteilung der Zuckerperzentile",
    x = "Perzentile"
  ) +
  coord_flip() +
  theme(axis.title.y = element_blank()) 
ggsave("plots/boxplot.sugarpercent.pdf", plot = boxplot.sugarpercent, device = "pdf", width = 8, height = 6)

# Pricepercent
boxplot.pricepercent <- ggplot(candy.data, aes(x = pricepercent) )+
  geom_boxplot() +
  labs(
    title = "Verteilung der Preisperzentile",
    x = "Percentile"
  ) +
  coord_flip() +
  theme(axis.title.y = element_blank()) 
ggsave("plots/boxplot.pricepercent.pdf", plot = boxplot.pricepercent, device = "pdf", width = 8, height = 6)

#----------------------------------#
#### 2. Frequency plots of the Categorical Variables ####
#----------------------------------#

# Sample size
n <- nrow(candy.data)

# Names of the categorial features
features <- c("chocolate", "fruity", "caramel", "peanutyalmondy", 
              "nougat", "crispedricewafer", "hard", "bar", "pluribus")

# Function for counting frequencies
long.counts <- function(dt, group.label) {
  melted <- melt(dt[, ..features], measure.vars = features,
                 variable.name = "feature", value.name = "value")
  melted <- as.data.table(melted)
  counts <- melted[value == 1, .N, by = feature]
  counts[, group := group.label]
  return(counts)
}

# Data table for frequencies
counts.data <- long.counts(candy.data, features)

# Barplot over all frequencies
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
#### 3. Frequency Plots for the Top and Bottom 30% Most Popular Sweets ####
#----------------------------------#

top.n <- ceiling(n * 0.30)

# Select top 30% of sweets based on popularity
top.30 <- candy.data[order(-win.prop)][1:top.n]

# Select bottom 30% of sweets
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

# Bar plot: comparison of sugar and price category distribution
barplot.sugar.price.30 <- ggplot(category.data, aes(x = category, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ feature, scales = "free_x") +
  labs(
    title = "Verteilung von Zucker- und Preiskategorien in Top vs. Bottom 30%",
    x = "Kategorie",
    y = "Anzahl",
    fill = "Gruppe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/barplot.sugar.price.30 .pdf", plot = barplot.sugar.price.30 , device = "pdf", width = 8, height = 6)

# Add binary indicator column: 1 if candy is in Top 30%, 0 otherwise and vise versa
candy.data[, top30 := fifelse(competitorname %in% top.30$competitorname, 1, 0)]
candy.data[, bottom30 := fifelse(competitorname %in% top.30$competitorname, 0, 1)]


