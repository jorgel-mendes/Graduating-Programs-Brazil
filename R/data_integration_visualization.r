if (!require(pdftools)) install.packages("pdftools", repos = "http://cran.us.r-project.org")
require(pdftools)

if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
require(dplyr)

if (!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
require(readr)

if (!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
require(stringi)

if (!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
require(stringr)

if (!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")
require(xlsx)

if (!require(rgdal)) install.packages("rgdal")
require(rgdal)

if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
require(ggplot2)

if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
require(RColorBrewer)

if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
require(viridis)

if(!require(gganimate)) install.packages("gganimate", repos = "http://cran.us.r-project.org")
require(gganimate)

if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
require(ggrepel)

if(!require(gpclib)) install.packages("gpclib")
require(gpclib)

if(!require(maptools)) install.packages("maptools")
require(maptools)

gpclibPermit()

if(!require(mapproj)) install.packages("mapproj")
require(mapproj)

if(!require(gifski)) install.packages("gifski")
require(gifski)

if(!require(png)) install.packages("png")
require(png)

if(!require(av)) install.packages("av")
require(av)

if(!require(openssl)) install.packages("openssl")
require(openssl)

universities <- read.xlsx("csv/universities_after.xlsx", sheetIndex = 1, encoding = "UTF-8")
brazilian_cities <- read.csv("csv/brazilian_cities.csv", sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
brazilian_states <- read.csv("csv/brazilian_states.csv",sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
research_names <- read.xlsx("csv/research_names.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_research <- read.xlsx("csv/university_research.xlsx", sheetIndex = 1, encoding = "UTF-8")
graduation_level <- read.xlsx("csv/graduation_level.xlsx", sheetIndex = 1, encoding = "UTF-8")
course_name <- read.xlsx("csv/course_name.xlsx", sheetIndex = 1, encoding = "UTF-8")
concentration_area <- read.xlsx("csv/concentration_area.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_concentration_area <- read.xlsx("csv/university_concentration_area.xlsx", sheetIndex = 1, encoding = "UTF-8")
shape_brazil <- readOGR("shapefile/Brazil.shp", "Brazil", use_iconv = TRUE)

all_data <- universities %>% left_join(brazilian_cities, by = c("city_id" = "ibge_code"))
all_data <- all_data %>% left_join(brazilian_states, by = c("state_id" = "state_id"))
all_data <- all_data %>% left_join(university_research, by = c("id" = "university_id"))
all_data <- all_data %>% left_join(research_names, by = c("research_id" = "id"))
all_data <- all_data %>% left_join(graduation_level, by = c("level" = "id"))
all_data <- all_data %>% left_join(course_name, by = c("course" = "id"))
all_data <- all_data %>% left_join(university_concentration_area, by = c("id" = "university_id"))
all_data <- all_data %>% left_join(concentration_area, by = c("concentration_area_id" = "id"))

all_data <- all_data[,-9]
all_data <- all_data[,-13]
all_data <- all_data[,-13]
all_data <- all_data[,-15]
all_data$latitude <- as.numeric(as.character(all_data$latitude))
all_data$longitude <- as.numeric(as.character(all_data$longitude))
all_data$grade <- as.numeric(as.character(all_data$grade))

mybreaks <- as.numeric(c(3, 4, 5, 6, 7))

theme <- theme(axis.text = element_text(size = 16),
              plot.caption = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 18, face = "bold"),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 14),
              legend.key.size = unit(1.0, "cm"),
              legend.key.width = unit(0.4,"cm"),
              text = element_text(color = "#22211d"), 
              legend.background = element_rect(fill = "#dddddd", color = "black"),
              panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
              plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47"))

labs <- labs(x = "Longitude", y = "Latitude", caption = "Data source: Computer Science Graduating Programs - Sucupira Website") 

ggplot() + 
  geom_polygon( data = shape_brazil, aes(x = long, y = lat, group = group), fill = "grey", size = 0.1, color = "black") +
  geom_point( data = all_data[!duplicated(all_data$id),], aes(x = longitude, y = latitude, size = grade, color = grade, alpha = I(2/(grade)))) +
  scale_color_viridis(name = "Conceito", breaks = mybreaks) +
  scale_size_continuous(name = "Conceito", breaks = mybreaks) +
  coord_map() + 
  guides( colour = guide_legend(), alpha = FALSE) +
  ggtitle("Programas de Pós-Graduação em Computação") +
  labs + theme
  
all_data$shape <- all_data$level + 14

p <- ggplot(data = all_data, aes(group = year)) +
     geom_polygon(data = shape_brazil, aes( x = long, y = lat, group = group), fill = "white", size = 0.1, color = "black") +
     geom_point(data = all_data, aes(x = longitude, y = latitude,  size = 4, color = as.factor(grade), alpha = 0.05,  group = year, shape = as.factor(shape))) +
     scale_color_manual(name = "Conceito", breaks = mybreaks, values = c("red","orange","yellow","dark green","light green")) +
     scale_shape_manual(name = "Nível", values = c(15, 17, 18, 19), labels = c("Mestrado Profissional","Mestrado","Doutorado","Mestrado + Doutorado")) +
     coord_map() +
     shadow_mark(past = TRUE) +
     guides(colour = guide_legend(override.aes = list(size = 8)), shape = guide_legend(override.aes = list(size = 8)), alpha = FALSE, size = FALSE) +
     ggtitle("Programas de Pós-Graduação em Computação em {round(frame_time,0)}") +
     transition_time(year) +
     theme + labs

animate(p, nframes = 52, fps = 2, width = 1800, height = 900, renderer = gifski_renderer("images/gganim.gif", loop = FALSE)) + ease_aes('cubic-in-out')

animate(p, width = 1800, height = 1200, renderer = ffmpeg_renderer())
anim_save("nations.mp4")

animate(p, width = 1800, nframes = 52, fps = 2, height = 1200, renderer = av_renderer('animation.mp4'))

all_data_2 <- subset(all_data, all_data$research_name=="Inteligencia Artificial")

ggplot() + 
geom_polygon(data = shape_brazil, aes(x = long, y = lat, group = group), fill = "gray", size = 0.1, color = "black") +
geom_point(data = all_data_2[!duplicated(all_data_2$id),], aes(x = longitude, y = latitude, size = as.factor(grade), color = as.factor(grade), alpha = I(2/grade))) +
scale_colour_manual(name = "Conceito", values = c("red","blue","dark green","green","yellow")) +
scale_size_manual(name = "Conceito", values = mybreaks) +
geom_text_repel(data = all_data_2[all_data_2$longitude > -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = as.factor(6)), hjust = 0, nudge_x = -34 - subset(all_data_2, all_data_2$longitude > -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) +
geom_text_repel(data = all_data_2[all_data_2$longitude <= -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = as.factor(6)), hjust = 1, nudge_x = -72 - subset(all_data_2, all_data_2$longitude <= -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) + 
coord_map() + 
guides() +
ggtitle("Programas de Pós-Graduação com Linha de Pesquisa em Inteligência Artificial") +
labs + theme

all_data_2 <- subset(all_data, all_data$concentration_area == "Teoria da Computação")

ggplot() + 
geom_polygon(data = shape_brazil, aes(x = long, y = lat, group = group), fill = "gray", size = 0.15, color = "black") +
geom_point(data = all_data_2[!duplicated(all_data_2$id),], aes(x = longitude, y = latitude, size = grade, color = grade)) +
geom_text_repel(data = all_data_2[!duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = 6), hjust = 0, nudge_x = -34 - subset(all_data_2, !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) +
scale_color_viridis(name = "Conceito", breaks = mybreaks) +
scale_size_continuous(name = "Conceito", breaks = mybreaks) +
coord_map() + 
guides(colour = guide_legend(), alpha = FALSE) +
ggtitle("Programas de Pós-Graduação com Área de Concentração em Teoria da Computação") +
labs + theme

all_data_2 <- subset(all_data, all_data$graduation_level == "Mestrado e Doutorado Acadêmicos")

ggplot() + 
geom_polygon(data = shape_brazil, aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.15) +
geom_point(data = all_data_2[!duplicated(all_data_2$id),], aes(x = longitude, y = latitude, size = grade, color = grade, shape = 19, alpha = I(2/grade))) +
geom_text_repel(data = all_data_2[all_data_2$longitude > -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = 6), hjust = 0, nudge_x = -34 - subset(all_data_2, all_data_2$longitude > -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) +
geom_text_repel(data = all_data_2[all_data_2$longitude <= -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = 6), hjust = 1, nudge_x = -72 - subset(all_data_2, all_data_2$longitude <= -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) + 
scale_shape_identity() +
scale_color_viridis(name = "Conceito", breaks = mybreaks) +
scale_size_continuous(name = "Conceito", breaks = mybreaks) +
coord_map() + 
guides(colour = guide_legend(), size = guide_legend()) +
ggtitle("Programas de Pós-Graduação com Mestrado e Doutorado") +
labs + theme


