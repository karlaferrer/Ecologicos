
# Arquivos ----------------------------------------------------------------

#Shape UF IBGE malha municipal - Rio de Janeiro
library(sf)
library(colorspace)

rj.sf <- read_sf("/Users/karlaferreira/Documents/Ensp 2023/Ecologicos/RJ_Municipios_2022/RJ_Municipios_2022.shp",crs = 31981)
st_crs(rj.sf)

#cobertura vacinal esquema primário em 21/12/23
#Ministério da Saúde - Cobertura Vacinal COVID-19
cobrj <- readxl::read_xlsx("cobertura_RJ.xlsx")
cobrj <- cobrj |> janitor::clean_names()
names(cobrj)

#juntar os dados de cobertura e shape UF
rj.sf2 <- rj.sf |> 
  mutate(municipio_residencia = NM_MUN)  |> 
  inner_join(cobrj, by = "municipio_residencia")

rj.sf2 <- rj.sf2 |> 
  mutate(cobertura = round(cobertura_vacinal_2_doses_percent*100, digits = 1))

g1rj <- ggplot(rj.sf2) + 
  geom_sf(aes(fill = cut_interval(cobertura,5)), size = 0.2) + 
  colorspace::scale_fill_discrete_sequential(palette = "Reds",
name = "cobertura %") +
  #labs(title = "Cobertura vacinal COVID-19 - 2 doses") +
  #annotation_scale(location = "bl", width_hint = 0.4) +
  #annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_minimal) +
  theme(legend.title = element_text(size = 9)) +
  theme(legend.text = element_text(size = 7)) +
  theme_void()

g2rj <- ggplot(rj.sf2, aes(cobertura)) + 
  geom_density(fill = "red", alpha = 0.2) +
  geom_vline(xintercept = quantile(uf.sf$cobertura,
probs = seq(0, 1, by = 0.2)), color = "red", linetype = "dashed") +
  labs(x= "cobertura %", y = "densidade") +
  theme(text = element_text(size = 7)) +
  theme(plot.margin = margin(3, 1, 1, 1, "cm"))

temarj <- ggpubr::ggarrange(g1rj, g2rj, nrow = 1, widths = c(6.5,3.5))
ggsave("tematico.jpg",tema, dpi=300)


# Vizinhanca --------------------------------------------------------------

library(sf)
#Coordenadas dos centroides das UF
centroides <- sf::st_centroid(rj.sf2)
coordenadas <- sf::st_coordinates(centroides)

# Convertendo para Spatial Polygons para usar
# poly2nb
rj.sp <- as_Spatial(rj.sf2)

library(spdep)
# Criando a vizinhança
vizrj <- poly2nb(rj.sp)
vizrj

# Transformando a vizinhança em linhas para
# plotar
viz.sfrj <- nb2lines(vizrj, coords = coordenadas, as_sf = TRUE)
viz.sfrj <- st_set_crs(viz.sfrj, st_crs(rj.sf2))

# Plotando o mapa de conectividade por
# contiguidade
ggplot() + 
  geom_sf(data = rj.sf2, fill = "salmon", size = 0.2,color = "white") + 
  geom_sf(data = centroides, size = 1) +
  geom_sf(data = viz.sfrj, size = 0.2) + 
  ggtitle("Vizinhança por conectividade") +
  ylab("Latitude") + xlab("Longitude") + 
  theme_minimal()

# Autocorrelacao ----------------------------------------------------------

library(tmap)

#correlacao da cobertura vacinal
pesos.viz <- nb2listw(vizrj)
moran.test(rj.sf2$cobertura, pesos.viz)

#correlograma
correl <- sp.correlogram(vizrj, rj.sf2$cobertura, order = 5, method = "I")
correl
#Correlograma
plot(correl)

#uf com p-valores mais significativos no Moran Local
rj.sf2$pval <- localmoran(rj.sf2$cobertura, pesos.viz)[, 5]

plot_pv <- tm_shape(rj.sf2) + 
  tm_polygons(col = "pval", title = "p-valores",
              breaks = c(0, 0.01, 0.05, 0.1, 1), border.col = "white",
              palette = "-Oranges") + tm_scale_bar(width = 0.15) + 
  tm_layout(frame = FALSE) + tmap_mode("plot")
ggsave("plot_pv.jpg",plot_pv, dpi=300)

#Moran Local (Lisa Map) da cobertura:
resI <- localmoran.sad(lm(rj.sf2$cobertura ~ 1), 1:length(vizrj),
                       vizrj, style = "W")
summary(resI)[1:5, ]

rj.sf2$MoranLocal <- summary(resI)[, 1]

ggplot(rj.sf2) + 
  geom_sf(aes(fill = MoranLocal), color = "black",
          size = 0.2) + 
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red 3",
                                              mid = 0) + 
  ggtitle("Moran local") + theme_void()

