library(data.table)
library(ggplot2)
library(maps)

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
       x = "Countries", y = "Vaccinated Ratio") +
  scale_fill_discrete(name = "Vaccinated Ratio Type",
                      labels = c("Partially Vaccinated Ratio", "Fully Vaccinated Ratio"))

world_map <- as.data.table(map_data("world"))
world_map$iso3 <- iso.alpha(world_map$region, 3)
vax_map <- vax_now[world_map, on = .(iso3)]  # left join

ggplot(vax_map, aes(i.long, i.lat, group = group)) +
  geom_polygon(aes(fill = fully_vaccinated_ratio)) +
  scale_fill_viridis_c(option = "viridis", name = "%") +
  labs(title = "Vaccination Rates around the World", subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "Longitude", y = "Latitude")

ggplot(vax_map, aes(i.long, i.lat, group = group)) +
  geom_polygon(aes(fill = partially_vaccinated_ratio - fully_vaccinated_ratio)) +
  scale_fill_viridis_c(option = "viridis", name = "%") +
  labs(title = "Partial-Only Vaccination Rates around the World", subtitle = sprintf("Data from %s", vax_now[1, date]),
       x = "Longitude", y = "Latitude")

# TODO: Deviation plot from world mean
# TODO: Deviation plot from world mean (map)
# TODO: Deviation plot from continent mean
# TODO: Box plot per continent (annotate the outlier countries)
# TODO: Vaccine coverage curves over time for some countries (or may just continent means or continent top).
#  One monthly, one daily.
# TODO: Group by continent
# TODO: Some fully / partially plot?



