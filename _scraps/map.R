
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Load North America
na <- ne_countries(continent = "North America", returnclass = "sf")
na_subset <- na[na$name %in% c("United States of America", "Canada", "Mexico"), ]

# --- Disjoint OR: US ∪ Mexico ---
ggplot() +
  geom_sf(data = na_subset, fill = "white", color = "gray40") +
  geom_sf(data = na_subset[na_subset$name %in% c("United States of America", "Mexico"), ],
          fill = "skyblue", alpha = 0.6) +
  labs(title = "Meteor lands in the US OR Mexico") +
  theme_void()

# --- Disjoint AND: US ∩ Mexico (empty set) ---
# We just show nothing shaded, but title explains
ggplot() +
  geom_sf(data = na_subset, fill = "white", color = "gray40") +
  labs(title = "Meteor lands in the US AND Mexico (∅)") +
  theme_void()

# --- Complement: not US (Canada ∪ Mexico) ---
ggplot() +
  geom_sf(data = na_subset, fill = "white", color = "gray40") +
  geom_sf(data = na_subset[na_subset$name %in% c("Canada","Mexico"), ],
          fill = "skyblue", alpha = 0.6) +
  labs(title = "Meteor does NOT land in the US") +
  theme_void()
