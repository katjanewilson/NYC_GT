
## Map practice TEST

Small test

You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE, message=FALSE, warning=FALSE}
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#"ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
```

```{r, echo=FALSE}
ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                     26.83))
ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-87, -50), ylim = c(24.5, 33), expand = FALSE)
```


different types of map available: “terrain”, “satellite”, “roadmap”, “hybrid”