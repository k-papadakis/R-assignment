library(data.table)
library(ggplot2)
library(corrplot)
library(ggrepel)
library(maps)

Sys.setlocale("LC_ALL", "English")

# TODO: Latest day of the dataset is ...
# Load the data
coronavirus::update_dataset(silence = TRUE)
vax <- as.data.table(coronavirus::covid19_vaccine)
# Drop the provinces
vax <- vax[is.na(vax$province), ]
# Add vaccination ratios percentages
vax$fully_vaccinated_ratio <- round(vax$people_fully_vaccinated / vax$population, digits = 4) * 100
vax$partially_vaccinated_ratio <- round(vax$people_partially_vaccinated / vax$population, digits = 4) * 100

# Our data for each country, but only for the most recent date.
# (The most recent date might differ among countries. It doesn't in our case, but it might happen)
# vax_now<- vax[, .SD[which.max(.SD$date)], by = country_region]
# length(unique(vax_now$date)) == 1  # TRUE
# To avoid futures issues, we will find the most recent day over the entire dataset,
# and pick the relevant entries.
vax_now <- vax[date == max(date), ]

# Plot Partially and Fully Vaccinated rates for the most populous countries
t <- na.omit(vax_now, cols = c("partially_vaccinated_ratio", "fully_vaccinated_ratio"))
t <- t[order(-population)[1:50], .(country_region, population, partially_vaccinated_ratio, fully_vaccinated_ratio)]
t <- melt(t, measure.vars = c("partially_vaccinated_ratio", "fully_vaccinated_ratio"),
          variable.name = "vaccinated_ratio_type", value.name = "vaccinated_ratio")
ggplot(data = t,
       aes(x = reorder(country_region, -population), y = vaccinated_ratio, fill = vaccinated_ratio_type)) +
  geom_bar(stat = "identity", position = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Vaccination Coverage in the Most Populous Countries",
       subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "", y = "Vaccinated Ratio") +
  scale_fill_discrete(name = "Vaccinated Ratio Type",
                      labels = c("Partially Vaccinated Ratio", "Fully Vaccinated Ratio"))

# Plot the Countries with the highest vaccination rates
t <- na.omit(vax_now, cols = c("partially_vaccinated_ratio", "fully_vaccinated_ratio"))
t <- t[order(-fully_vaccinated_ratio)[1:30]]
ggplot(t, aes(reorder(country_region, -fully_vaccinated_ratio), fully_vaccinated_ratio)) +
  geom_segment( aes(x=reorder(country_region, -fully_vaccinated_ratio), xend=reorder(country_region, population),
                    y=0, yend=fully_vaccinated_ratio)) +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Countries with the Highest Vaccination Coverage",
       subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "", y = "Fully Vaccinated Ratio")

# Make a world map data.table
world_map <- as.data.table(map_data("world"))
world_map$iso3 <- iso.alpha(world_map$region, 3)
vax_map <- vax_now[world_map, on = .(iso3)]  # left join

# Map-plot Fully Vaccinated Rates
ggplot(vax_map, aes(i.long, i.lat, group = group)) +
  geom_polygon(aes(fill = fully_vaccinated_ratio)) +
  scale_fill_viridis_c(option = "viridis", name = "%") +
  labs(title = "Vaccination Rates around the World",
       subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "Longitude", y = "Latitude")

# Map-plot Partial-Only Vaccination Rates
ggplot(vax_map, aes(i.long, i.lat, group = group)) +
  geom_polygon(aes(fill = partially_vaccinated_ratio - fully_vaccinated_ratio)) +
  scale_fill_viridis_c(option = "viridis", name = "%") +
  labs(title = "Partial-Only Vaccination Rates around the World",
       subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "Longitude", y = "Latitude")


# Deviation from the World Average Histogram
mean_partial <- vax_now[, mean(fully_vaccinated_ratio, na.rm = TRUE)]
t <- na.omit(vax_now, cols = c("fully_vaccinated_ratio", "fully_vaccinated_ratio"))
t$above_below <- t$fully_vaccinated_ratio > mean_partial
t$deviation <- t$fully_vaccinated_ratio - mean_partial

ggplot(data = t[order(-population)[1:30]], aes(x = reorder(country_region, population), y = deviation, fill = above_below)) +
  geom_bar(stat = 'identity', width = .5) +
  scale_fill_discrete(name = "Vaccination Rates",
                    labels = c("Below World Average", "Above World Average")) +
  labs(title = "Vaccination Rates Deviation from the World Average",
       subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "Countries",
       y = "Deviation"
  ) +
  coord_flip()

# Deviation from the World Average Map
ggplot(t[world_map, on = .(iso3)], aes(i.long, i.lat, group = group)) +
  geom_polygon(aes(fill = deviation)) +
  # scale_fill_distiller(type = "div", name = "Deviation") +
  scale_fill_gradient2(midpoint = 0, limits = c(-55, 55), name = "Deviation") +
  labs(title = "Vaccination Rates Deviation from the World Average",
       subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "Longitude", y = "Latitude")

# Box Plots per continent
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
t <- na.omit(vax_now, cols = c("continent_name", "fully_vaccinated_ratio"))
t[ , outlier := ifelse(is_outlier(fully_vaccinated_ratio), country_region, NA_character_) , by = continent_name]
ggplot(t,
       aes(x = continent_name, y = fully_vaccinated_ratio, fill = continent_name)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = 1, outlier.color = "red") +
  scale_fill_viridis_d(alpha=0.6) +
  geom_jitter(na.rm = TRUE, color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(title = "Boxplots with jitter for the Vaccination Ratio of each Continent",
       subtitle = sprintf("Data from %s", vax_now[1, date]), x = "", y = "Percentage of the Fully Vaccinated") +
  geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = FALSE)

# World-wide and per continent daily time series
t <- na.omit(vax, cols = c("fully_vaccinated_ratio", "date", "country_region", "continent_name"))
continent_avg <- t[ ,
  .(mean_fully_vaccinated_ratio = weighted.mean(fully_vaccinated_ratio, population)),
  by = .(date, continent_name)
]
world_avg <- t[ ,
  .(continent_name = "World", mean_fully_vaccinated_ratio = weighted.mean(fully_vaccinated_ratio, population)),
  by = date
]
continent_avg <- rbind(continent_avg, world_avg)[order(date, continent_name)]

ggplot(continent_avg, aes(date, mean_fully_vaccinated_ratio, colour=continent_name, group = continent_name)) +
  # geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B %Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Vaccination rates over time",
       subtitle = sprintf("Data from %s to %s", min(continent_avg$date), max(continent_avg$date)),
       x = "", y = "Percentage of the Fully Vaccinated") +
  scale_color_discrete(name = "")

# Exploring Greece
vax_gr = vax[country_region == "Greece"]
ggplot(vax_gr, aes(date, fully_vaccinated_ratio)) +
  geom_line()
