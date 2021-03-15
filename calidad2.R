library(tidyverse)
library(ggthemes)

calidad <- read.csv("abast_calidad.csv")
calidad <- abast_calidad
calidad <- limni_calidad
calidad_abast <- read.csv("calidad_abast.csv")

calidad[Porcentaje==0] <- NA
calidad <- na.omit(calidad2)
calidad2$Año <- as.factor(calidad2$Año)
calidad2 = calidad[!(calidad$Porcentaje == 0), ]

fill <- c("#0A9740", "#F1DA0C", "#A71E1E")
fill <- c("#0A9740", "#A71E1E", "#F1DA0C")


calidad2$Calidad <- factor(calidad2$Calidad,levels = c("Buena", "Media", "Mala"))

p <-  ggplot(calidad2, aes(x = Año, y = Porcentaje, fill = Calidad)) + 
  geom_bar(stat = 'identity', position = 'stack', width = 0.5) +
  scale_fill_manual(values = fill) +
  labs(y = "Porcentaje de fuentes [%]", x = "Comparación entre años 2019 y 2020") +
  ggtitle(label = "Reporte de calidad fisicoquímica (fuentes limnigráficas)",
          subtitle = "Semana del 21 de febrero al 28 de febrero de 2021") +
  geom_text(aes(label = scales::percent(round(Porcentaje/100, 2))),
            position="stack",vjust=+1.3, size=2.9) +
  facet_wrap(~ Territorial, nrow = 2, scales = "free_x") +
  theme_classic() +
  theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
   

p

ggsave("calidad_limni.png", plot = last_plot(), 
       width = 1024/4, height = 768/4, units = c("mm"),
       dpi = 400)

calidad2 %>% 
  group_by(Año) %>% 
  summarise(n = sum(Número))

calidad[apply(calidad!=0, 1, all),]

calidad[calidad$Porcentaje != 0,]
