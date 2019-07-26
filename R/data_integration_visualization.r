if (!require(pdftools)) {
  install.packages("pdftools", repos = "http://cran.us.r-project.org")
  require(pdftools)
}
if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
  require(dplyr)
}
if (!require(readr)) {
  install.packages("readr", repos = "http://cran.us.r-project.org")
  require(readr)
}
if (!require(stringi)) {
  install.packages("stringi", repos = "http://cran.us.r-project.org")
  require(stringi)
}
if (!require(stringr)) {
  install.packages("stringr", repos = "http://cran.us.r-project.org")
  require(stringr)
}
if (!require(xlsx)) {
  install.packages("xlsx", repos = "http://cran.us.r-project.org")
  require(xlsx)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
  require(ggplot2)
}
if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
}
if(!require(viridis)){
  install.packages("viridis")
  library(viridis)
}
if(!require(gganimate)){
  install.packages("gganimate")
  library(gganimate)
}

brazilian_states <- read.csv("csv/brazilian_states.csv",sep=",", stringsAsFactors=FALSE, encoding = "UTF-8")
brazilian_cities <- read.csv("csv/brazilian_cities.csv",sep=",", stringsAsFactors=FALSE, encoding = "Latin-1")
universities <- read.xlsx("csv/universities_after.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_research <- read.xlsx("csv/university_research.xlsx", sheetIndex = 1, encoding = "UTF-8")
research_names <- read.xlsx("csv/research_names.xlsx", sheetIndex = 1, encoding = "UTF-8")
shape_brazil <- readOGR("shapefile/Brazil.shp", "Brazil",use_iconv=TRUE)
colnames(brazilian_states)[1]<-"codigo_uf"

all_data <- universities %>% left_join(brazilian_cities, by = c("Codigo_Cidade" = "codigo_ibge"))
all_data <- all_data %>% left_join(brazilian_states, by = c("codigo_uf" = "codigo_uf"))
all_data <- all_data %>% left_join(university_research, by = c("id" = "id_universidade"))
all_data <- all_data %>% left_join(research_names, by = c("id_pesquisa" = "id_ordenado"))

shape_brazil@data$id <- rownames(shape_brazil@data)
shp_df <- broom::tidy(shape_brazil, region = 'id')
shape_brazil <- shape_brazil@data %>% inner_join(shp_df, by = c("id" = "id"))

all_data<-all_data[,-9]
all_data<-all_data[,-13]
all_data<-all_data[,-13]
all_data<-all_data[,-15]
colnames(all_data)[7]<-"lat_university"
colnames(all_data)[8]<-"long_university"
all_data$lat_university <- as.numeric(as.character(all_data$lat_university))
all_data$long_university <- as.numeric(as.character(all_data$long_university))
all_data$Nota <- as.numeric(as.character(all_data$Nota))

mybreaks <- as.numeric(c(3, 4, 5, 6, 7))

ggplot() + 
  geom_polygon( data = shape_brazil, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
  geom_path( data = shape_brazil, aes( x = long, y = lat, group = group), color = "black", size = 0.1) +
  geom_point( data = all_data, aes(x = long_university, y = lat_university, size = Nota, color = Nota, alpha = rev(Nota))) +
  scale_color_viridis(name = "Conceito", breaks = mybreaks) +
  scale_size_continuous(name = "Conceito", breaks = mybreaks) +
  coord_map() + 
  guides( colour = guide_legend(), alpha = FALSE) +
  ggtitle("Programas de Pós-Graduação em Computação") +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = c(0.9, 0.15),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#dddddd", color = "black"),
    plot.title = element_text(size= 16, hjust=0.5, color = "#4e4d47"))
  
all_data$id_mestrado_doutorado <- all_data$id_mestrado_doutorado + 14
p <- ggplot( data = all_data, aes(group = Ano)) +
  geom_polygon( data = shape_brazil, aes( x = long, y = lat, group = group), fill = "white") +
  geom_path( data = shape_brazil, aes( x = long, y = lat, group = group), color = "black", size = 0.05) +
  geom_point( data = all_data, aes(x = long_university, y = lat_university, size = 4, color = as.factor(Nota), alpha = 0.05, group = Ano, shape = as.factor(id_mestrado_doutorado))) +
  scale_color_manual(name = "Conceito", breaks = mybreaks, values = c("red","orange","yellow","dark green","light green")) +
  scale_shape_manual(name = "Nível", values = c(15, 17, 18, 19), labels = c("Mestrado Profissional","Mestrado","Doutorado","Mestrado + Doutorado")) +
  coord_map() +
  shadow_mark(past = TRUE) +
  guides( colour = guide_legend(override.aes = list(size = 8)), shape = guide_legend(override.aes = list(size=8)), alpha = FALSE, size = FALSE) +
  ggtitle( "Programas de Pós-Graduação em Computação em {round(frame_time,0)}") +
  labs( x = "Longitude", y = "Latitude") +
  transition_time(Ano) +
  theme(
    legend.title = element_text(size = 18), 
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.0, "cm"),
    legend.key.width = unit(0.4,"cm"),
    legend.position = c(0.15, 0.25),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size = 20, hjust = 0.1, color = "black"))

animate(p, nframes = 52, fps = 1, width = 1800, height = 900, renderer = gifski_renderer("images/gganim.gif", loop = FALSE)) + ease_aes('cubic-in-out')

animate(p, nframes = 52, fps = 1, width = 1800, height = 900, renderer = ffmpeg_renderer()) +  ease_aes('cubic-in-out')

ggplot() + 
  geom_polygon( data = shape_brazil, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
  geom_path( data = shape_brazil, aes( x = long, y = lat, group = group), color = "black", size = 0.1) +
  geom_point( data = all_data[all_data$nome=="Inteligencia Artificial",], aes(x = long_university, y = lat_university, size = as.factor(Nota), color = as.factor(Nota), alpha = 0.7)) +
  scale_colour_manual(name = "Conceito", values = c("red","blue","dark green","green","yellow")) +
  scale_size_manual(name = "Conceito", values = c(3,4,5,6,7)) +
  coord_map() + 
  guides( alpha = FALSE) +
  ggtitle("Programas de Pós-Graduação em Inteligência Artificial") +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = c(0.9, 0.15),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    panel.background = element_rect(fill = "#f5f5f2"), 
    legend.background = element_rect(fill = "#dddddd", color = "black"),
    plot.title = element_text(size= 16, hjust=0.5, color = "#4e4d47"))

ggplot() + 
  geom_polygon( data = shape_brazil, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.3) +
  geom_path( data = shape_brazil, aes( x = long, y = lat, group = group), color = "black", size = 0.1) +
  geom_point( data = all_data[all_data$nome=="Teoria da Computação",], aes(x = long_university, y = lat_university, size = Nota, color = Nota, alpha = 0.3)) +
  scale_color_viridis(name = "Conceito", breaks = mybreaks) +
  scale_size_continuous(name = "Conceito", breaks = mybreaks) +
  coord_map() + 
  guides( colour = guide_legend(), alpha = FALSE) +
  ggtitle("Programas de Pós-Graduação em Teoria da Computação") +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = c(0.9, 0.15),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    panel.background = element_rect(fill = "#f5f5f2"), 
    legend.background = element_rect(fill = "#dddddd", color = "black"),
    plot.title = element_text(size= 16, hjust=0.5, color = "#4e4d47"))

ggplot() + 
  geom_polygon( data = shape_brazil, aes(x = long, y = lat, group = group), fill = "white", alpha = 0.3) +
  geom_path( data = shape_brazil, aes( x = long, y = lat, group = group), color = "grey", size = 0.1) +
  geom_point( data = all_data[all_data$Nivel == "MESTRADO/DOUTORADO",], aes(x = long_university, y = lat_university, size = Nota, color = Nota, alpha = 0.3)) +
  geom_text_repel( data = all_data[!duplicated(all_data$Sigla) & all_data$Nivel == "MESTRADO/DOUTORADO",], aes(x = long_university, y = lat_university, label = Sigla, size = 5), hjust = 0.1, nudge_x = 1, show.legend = FALSE) +
  scale_shape_identity() +
  scale_color_viridis(name = "Conceito", breaks = mybreaks) +
  scale_size_continuous(name = "Conceito", breaks = mybreaks) +
  coord_map() + 
  guides( colour = guide_legend(), size = guide_legend(), alpha = FALSE, label = FALSE, text = FALSE) +
  ggtitle("Programas de Pós-Graduação com Mestrado e Doutorado") +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.position = c(0.9, 0.15),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    panel.background = element_rect(fill = "#f5f5f2"), 
    legend.background = element_rect(fill = "#dddddd", color = "black"),
    plot.title = element_text(size= 16, hjust=0.5, color = "#4e4d47"))









































