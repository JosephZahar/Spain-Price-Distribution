library(akima)
library(reshape2)
library(patchwork)

hypothesis_df2 = hypothesis_df %>% filter(price <= 50)
price_distrib <- function(city){
  hypothesis_df_B = hypothesis_df2 %>% filter(neighbourhood == city)
  fld <- with(hypothesis_df_B, akima::interp(x = longitude, y = latitude, z = price, duplicate = "mean"))
  df <- melt(fld$z, na.rm = TRUE)
  names(df) <- c("x", "y", "price")
  df$lon <- fld$x[df$x]
  df$lat <- fld$y[df$y]
  return(df)}

Madridmap <- get_map(location=c(lon=-3.7038,
                                lat=40.4168), zoom=12, maptype='roadmap')
p1 <- ggmap(Madridmap, extent = "device") +
  geom_tile(data = price_distrib('Madrid'), aes(x = lon, y = lat, z = price, fill = price), alpha = 0.8) +
  stat_contour(data = price_distrib('Madrid'), aes(x = lon, y = lat, z = price), color = 'black', alpha = 0.8, bins = 6, size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Price/night $",
                        low = "green", high = "red") +
  labs(title ="Madrid") +theme(legend.position = "None")


Barcelonamap <- get_map(location=c(lon=2.154007,
                                   lat= 41.40205), zoom=13, maptype='roadmap')
p2 <- ggmap(Barcelonamap, extent = "device") +
  geom_tile(data = price_distrib('Barcelona'), aes(x = lon, y = lat, z = price, fill = price), alpha = 0.8) +
  stat_contour(data = price_distrib('Barcelona'), aes(x = lon, y = lat, z = price), color = 'black', alpha = 0.8, bins = 6, size = 0.3)+
  scale_fill_continuous(name = "Price/night $",
                        low = "green", high = "red") +
  labs(title ="Barcelona") 

Sevillamap <- get_map(location=c(lon=-5.974072,
                                 lat= 37.392529), zoom=13, maptype='roadmap')
p3 <- ggmap(Sevillamap, extent = "device") +
  geom_tile(data = price_distrib('Sevilla'), aes(x = lon, y = lat, z = price, fill = price), alpha = 0.8) +
  stat_contour(data = price_distrib('Sevilla'), aes(x = lon, y = lat, z = price), color = 'black', alpha = 0.8, size = 0.3, bins = 6) +
  scale_fill_continuous(name = "Price/night $",
                        low = "green", high = "red") +
  labs(title ="Sevilla") +  theme(legend.position = "None")

Mallorcamap <- get_map(location=c(lon=3.0176,
                                  lat= 39.5953), zoom=10, maptype='roadmap')
p4 <- ggmap(Mallorcamap, extent = "device") +
  geom_tile(data = price_distrib('Mallorca'), aes(x = lon, y = lat, z = price, fill = price), alpha = 0.8) +
  stat_contour(data = price_distrib('Mallorca'), aes(x = lon, y = lat, z = price), color = 'black', alpha = 0.8, bins = 6, size = 0.3) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Price/night $",
                        low = "green", high = "red") +
  labs(title ="Mallorca")  + theme(legend.position = "None")

p1 + p2 + p3 + p4 + plot_layout(widths = 4)
ggsave(dpi = 300, filename = "maps.png")
