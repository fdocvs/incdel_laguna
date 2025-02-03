# Datos SESNSP: Incidencia delictiva en la Zona Metropolitana de la Laguna ----

## Configuración ----

library(pacman)

p_load(readr, readxl, dplyr, stringr, ggplot2, forcats,ggthemes, scales, 
       tidyverse, ggrepel)

options(scipen=999) # Prevenir notación científica

rm(list=ls())

## Bases de datos ----

IDM <- vroom::vroom("/Users/santiago/Documentos OFF/México Evalúa/Visualizaciones Coahuila/IDM_NM_feb22.csv"
                    , skip = 0, locale = vroom::locale(encoding = "CP1252")) ## Código que elimina error al abrir base

CONAPO <- read_excel("/Users/santiago/Documentos OFF/México Evalúa/SESNSP/pob_mun_2015-2030.xls")

CONAPO$pob_estim <- as.numeric(CONAPO$pob_estim)

# IDEFC <- vroom::vroom("/Users/santiago/Documentos OFF/México Evalúa/SESNSP/Original data/IDEFC_NM_sep21.csv"
#                       , skip = 0, locale = vroom::locale(encoding = "CP1252"))

## Homologación de varnames 

names(IDM)[1] <- "ao"
names(IDM)[4] <- "cvemunicipio"
names(IDM)[6] <- "bj_afectado"
names(IDM)[7] <- "delito"
names(IDM)[8] <- "subdelito"
names(IDM)[9] <- "modalidad"
names(IDM)[10] <- "ene"
names(IDM)[11] <- "feb"
names(IDM)[12] <- "mar"
names(IDM)[13] <- "abr"
names(IDM)[14] <- "may"
names(IDM)[15] <- "jun"
names(IDM)[16] <- "jul"
names(IDM)[17] <- "ago"
names(IDM)[18] <- "sep"
names(IDM)[19] <- "oct"
names(IDM)[20] <- "nov"
names(IDM)[21] <- "dic"


## Tasas

### IDM: Datos municipales ----


## Claves municipales LER=10012, GP=10007, TOR=5035


IDM_ZML <- select(IDM, -c(2, 3, 5)) # Elimina columnas que no se usan
IDM_ZML1 <- IDM_ZML %>% 
  filter(cvemunicipio %in% c("10012", "10007", "5035")) %>% 
  left_join(CONAPO, by= c("cvemunicipio", "ao")) %>% 
  select(-c(19, 20, 21, 23, 24))


IDM_ZML2 <- IDM_ZML1 %>% 
  mutate(total_del = rowSums(IDM_ZML1[, c(7:18)], na.rm=TRUE)) %>% 
  mutate(tasa = total_del/pob_estim*100000)

tasas_sub <- IDM_ZML2 %>%
  group_by(ao, delito, subdelito) %>%
  summarise(tasa_subdelito = sum(tasa))

tasas_del <- IDM_ZML2 %>%
  group_by(ao, delito) %>%
  summarise(tasa_delito = sum(tasa))

tasas_mod <- IDM_ZML2 %>%
  group_by(ao, delito, subdelito, modalidad) %>%
  summarise(tasa_delito = sum(tasa))








## Homicidios ----





tasas_homi <- as.data.frame(tasas_sub) %>% 
  filter(subdelito %in% c("Homicidio doloso", "Feminicidio"), ao < 2022) %>% 
  group_by(ao) %>% 
  summarise(tasa_subdelito = sum(tasa_subdelito))


tasa_asesi <- ggplot(tasas_homi, aes(x = ao, y = tasa_subdelito)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución anual de asesinatos en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2021\n",
       caption = "*Tasa de carpetas de investigación por cada 100 mil habs. por homicidios dolosos y feminicidios en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) + 
  scale_x_continuous(breaks = seq(from = 2015, to = 2021, by = 1)) +
  geom_point() + 
  geom_label_repel(label = round(tasas_homi$tasa_subdelito, 1), size = 3)

tasa_asesi

ggsave(tasa_asesi, filename ="tasa_asesinatos.png",
       width = 9, height = 6, units = "in", bg = "white")





IDM_ZML5 <- IDM_ZML %>% 
  filter(cvemunicipio %in% c("10012", "10007", "5035"), 
         subdelito %in% c("Homicidio doloso", "Feminicidio")) %>% 
  select(-c(2, 3, 4, 6))

names(IDM_ZML5)[3] <- "01"
names(IDM_ZML5)[4] <- "02"
names(IDM_ZML5)[5] <- "03"
names(IDM_ZML5)[6] <- "04"
names(IDM_ZML5)[7] <- "05"
names(IDM_ZML5)[8] <- "06"
names(IDM_ZML5)[9] <- "07"
names(IDM_ZML5)[10] <- "08"
names(IDM_ZML5)[11] <- "09"
names(IDM_ZML5)[12] <- "10"
names(IDM_ZML5)[13] <- "11"
names(IDM_ZML5)[14] <- "12"

IDM_ZML6 <- pivot_longer(IDM_ZML5, 3:14, names_to = "Mes", values_to = "CI") %>%
  group_by(ao, Mes) %>%
  summarise(CI = sum(CI)) %>% 
  filter(!is.na(CI))


names(IDM_ZML6)[1] <- "Year"
names(IDM_ZML6)[2] <- "Month"

IDM_ZML6$date <- 
  paste(IDM_ZML6$Year, IDM_ZML6$Month, sep="-")


IDM_ZML6$date <- paste0(IDM_ZML6$date, '-01')
IDM_ZML6$date <- as.Date(IDM_ZML6$date, format = '%Y-%m-%d')



asesi_lala <- ggplot(IDM_ZML6, aes(x = date, y = CI)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución mensual de asesinatos registrados en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2022\n",
       caption = "*Número de carpetas de investigación por homicidio doloso y feminicidio en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  geom_smooth() +
  geom_text_repel(size = 3.5,
                  data = subset(IDM_ZML6, 
                                as.character(IDM_ZML6$date) %in% c("2016-05-01","2017-05-01",
                                                                             "2017-08-01", "2018-01-01",
                                                                             "2018-08-01", "2018-10-01",
                                                                             "2020-07-01", "2021-11-01",
                                                                             "2022-01-01")), aes(label = CI)) +
  geom_point(data = subset(IDM_ZML6, 
                           as.character(IDM_ZML6$date) %in% c("2016-05-01","2017-05-01",
                                                              "2017-08-01", "2018-01-01",
                                                              "2018-08-01", "2018-10-01",
                                                              "2020-07-01", "2021-11-01",
                                                              "2022-01-01")))
  
  
asesi_lala


ggsave(asesi_lala, filename ="mensual_asesinatos_loess.png",
       width = 9, height = 6, units = "in", bg = "white")




## Percepción de inseguridad ----



perc_ins <- read_xlsx("/Users/santiago/Documentos OFF/México Evalúa/Visualizaciones Coahuila/prop_inseg.xlsx")
perc_ins$date <- as.Date(perc_ins$date, format = '%Y-%m-%d')
ex1<- pivot_longer(perc_ins, 2:3, names_to = "porc", values_to = "value") %>% 
  group_by(date, porc)


inseg <- ggplot(ex1, aes(date, value), label = value) +
  geom_line(aes(color = porc)) +
  scale_color_manual(values = c("#206779", "#58b9ac"), labels = c("La Laguna", "Nacional"),
                     guide = guide_legend(reverse = TRUE)) +
  geom_point() +
  theme_minimal() +
  labs(title = "\nPorcentaje de la población mayor de 18 años con percepción de inseguridad",
       subtitle = "2017-2022\n",
       caption = "\nNota: Levantamiento de la encuesta para el segundo trimestre de 2020 cancelada dada la situación COVID-19.\nElaboración de Fernando Cuevas con datos de la Encuesta Nacional de Seguridad Pública Urbana (INEGI).\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months",
               limits = as.Date(c("2017-01-01", "2022-04-01")),
               breaks = as.Date(c("2017-03-01", "2022-03-01"))) +
  geom_text_repel(aes(label = value), size = 2.8, nudge_x = 10, nudge_y = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank(),
        legend.position= "right",
        legend.direction = "vertical",
        legend.box.spacing = unit(0, 'cm'),
        legend.text=element_text(size=10)) +
  scale_fill_discrete(breaks=c("Nacional", "La Laguna")) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), limits = c(30, 90))


inseg

ggsave(inseg, filename ="perc_inseg.png",
       width = 9, height = 6, units = "in", bg = "white")





## Robo a negocio ----




IDM_ZML_rn <- IDM_ZML %>% 
  filter(cvemunicipio %in% c("10012", "10007", "5035"), 
         subdelito == "Robo a negocio") %>% 
  select(-c(2, 3, 4, 6))

names(IDM_ZML_rn)[3] <- "01"
names(IDM_ZML_rn)[4] <- "02"
names(IDM_ZML_rn)[5] <- "03"
names(IDM_ZML_rn)[6] <- "04"
names(IDM_ZML_rn)[7] <- "05"
names(IDM_ZML_rn)[8] <- "06"
names(IDM_ZML_rn)[9] <- "07"
names(IDM_ZML_rn)[10] <- "08"
names(IDM_ZML_rn)[11] <- "09"
names(IDM_ZML_rn)[12] <- "10"
names(IDM_ZML_rn)[13] <- "11"
names(IDM_ZML_rn)[14] <- "12"

IDM_ZML_rn2 <- pivot_longer(IDM_ZML_rn, 3:14, names_to = "Mes", values_to = "CI") %>%
  group_by(ao, Mes) %>%
  summarise(CI = sum(CI)) %>% 
  filter(!is.na(CI))


names(IDM_ZML_rn2)[1] <- "Year"
names(IDM_ZML_rn2)[2] <- "Month"

IDM_ZML_rn2$date <- 
  paste(IDM_ZML_rn2$Year, IDM_ZML_rn2$Month, sep="-")


IDM_ZML_rn2$date <- paste0(IDM_ZML_rn2$date, '-01')
IDM_ZML_rn2$date <- as.Date(IDM_ZML_rn2$date, format = '%Y-%m-%d')



robo_neg <- ggplot(IDM_ZML_rn2, aes(x = date, y = CI)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución mensual de robos a negocios registrados en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2022\n",
       caption = "*Número de carpetas de investigación por robo a negocio en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200))


robo_neg


ggsave(robo_neg, filename ="mensual_roboneg.png",
       width = 9, height = 6, units = "in", bg = "white")







tasas_rn <- as.data.frame(tasas_sub) %>% 
  filter(subdelito == "Robo a negocio",
         ao < 2022) %>% 
  group_by(ao) %>% 
  summarise(tasa_subdelito = sum(tasa_subdelito))


tasa_roboneg <- ggplot(tasas_rn, aes(x = ao, y = tasa_subdelito)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución anual de robos a negocios registrados en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2021\n",
       caption = "*Tasa de carpetas de investigación por cada 100 mil habs. en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) + 
  scale_x_continuous(breaks = seq(from = 2015, to = 2021, by = 1)) +
  geom_point() + 
  geom_label_repel(label = round(tasas_rn$tasa_subdelito, 1), size = 3)

tasa_roboneg

ggsave(tasa_roboneg, filename ="tasa_roboneg.png",
       width = 9, height = 6, units = "in", bg = "white")




## Secuestro ----


IDM_ZML_sec <- IDM_ZML %>% 
  filter(cvemunicipio %in% c("10012", "10007", "5035"), 
         subdelito == "Secuestro") %>% 
  select(-c(2, 3, 4, 6))

names(IDM_ZML_sec)[3] <- "01"
names(IDM_ZML_sec)[4] <- "02"
names(IDM_ZML_sec)[5] <- "03"
names(IDM_ZML_sec)[6] <- "04"
names(IDM_ZML_sec)[7] <- "05"
names(IDM_ZML_sec)[8] <- "06"
names(IDM_ZML_sec)[9] <- "07"
names(IDM_ZML_sec)[10] <- "08"
names(IDM_ZML_sec)[11] <- "09"
names(IDM_ZML_sec)[12] <- "10"
names(IDM_ZML_sec)[13] <- "11"
names(IDM_ZML_sec)[14] <- "12"

IDM_ZML_sec2 <- pivot_longer(IDM_ZML_sec, 3:14, names_to = "Mes", values_to = "CI") %>%
  group_by(ao, Mes) %>%
  summarise(CI = sum(CI)) %>% 
  filter(!is.na(CI))


names(IDM_ZML_sec2)[1] <- "Year"
names(IDM_ZML_sec2)[2] <- "Month"

IDM_ZML_sec2$date <- 
  paste(IDM_ZML_sec2$Year, IDM_ZML_sec2$Month, sep="-")


IDM_ZML_sec2$date <- paste0(IDM_ZML_sec2$date, '-01')
IDM_ZML_sec2$date <- as.Date(IDM_ZML_sec2$date, format = '%Y-%m-%d')



secuestros <- ggplot(IDM_ZML_sec2, aes(x = date, y = CI)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución mensual de secuestros registrados en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2022\n",
       caption = "*Número de carpetas de investigación por secuestro en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5))


secuestros


ggsave(secuestros, filename ="mensual_secuestro.png",
       width = 9, height = 6, units = "in", bg = "white")





tasas_sec <- as.data.frame(tasas_sub) %>% 
  filter(subdelito == "Secuestro",
         ao < 2022) %>% 
  group_by(ao) %>% 
  summarise(tasa_subdelito = sum(tasa_subdelito))


tasa_secuestro <- ggplot(tasas_sec, aes(x = ao, y = tasa_subdelito)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución anual de secuestros registrados en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2021\n",
       caption = "*Tasa de carpetas de investigación por cada 100 mil habs. en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) + 
  scale_x_continuous(breaks = seq(from = 2015, to = 2021, by = 1)) +
  geom_point() + 
  geom_label_repel(label = round(tasas_sec$tasa_subdelito, 1), size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1), limits = c(0, 4))


tasa_secuestro

ggsave(tasa_secuestro, filename ="tasa_secuestro.png",
       width = 9, height = 6, units = "in", bg = "white")




## Extorsión ----


IDM_ZML_ext <- IDM_ZML %>% 
  filter(cvemunicipio %in% c("10012", "10007", "5035"), 
         delito == "Extorsión") %>% 
  select(-c(2, 3, 4, 6))

names(IDM_ZML_ext)[3] <- "01"
names(IDM_ZML_ext)[4] <- "02"
names(IDM_ZML_ext)[5] <- "03"
names(IDM_ZML_ext)[6] <- "04"
names(IDM_ZML_ext)[7] <- "05"
names(IDM_ZML_ext)[8] <- "06"
names(IDM_ZML_ext)[9] <- "07"
names(IDM_ZML_ext)[10] <- "08"
names(IDM_ZML_ext)[11] <- "09"
names(IDM_ZML_ext)[12] <- "10"
names(IDM_ZML_ext)[13] <- "11"
names(IDM_ZML_ext)[14] <- "12"

IDM_ZML_ext2 <- pivot_longer(IDM_ZML_ext, 3:14, names_to = "Mes", values_to = "CI") %>%
  group_by(ao, Mes) %>%
  summarise(CI = sum(CI)) %>% 
  filter(!is.na(CI))


names(IDM_ZML_ext2)[1] <- "Year"
names(IDM_ZML_ext2)[2] <- "Month"

IDM_ZML_ext2$date <- 
  paste(IDM_ZML_ext2$Year, IDM_ZML_ext2$Month, sep="-")


IDM_ZML_ext2$date <- paste0(IDM_ZML_ext2$date, '-01')
IDM_ZML_ext2$date <- as.Date(IDM_ZML_ext2$date, format = '%Y-%m-%d')



extorsion <- ggplot(IDM_ZML_ext2, aes(x = date, y = CI)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución mensual de delitos por extorsión en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2022\n",
       caption = "*Número de carpetas de investigación en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5))


extorsion


ggsave(extorsion, filename ="mensual_extorsion.png",
       width = 9, height = 6, units = "in", bg = "white")





tasas_ext <- as.data.frame(tasas_sub) %>% 
  filter(delito == "Extorsión",
         ao < 2022) %>% 
  group_by(ao) %>% 
  summarise(tasa_subdelito = sum(tasa_subdelito))


tasa_extorsion <- ggplot(tasas_ext, aes(x = ao, y = tasa_subdelito)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución anual de delitos por extorsión registrados en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2021\n",
       caption = "*Tasa de carpetas de investigación por cada 100 mil habs. en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) + 
  scale_x_continuous(breaks = seq(from = 2015, to = 2021, by = 1)) +
  geom_point() + 
  geom_label_repel(label = round(tasas_ext$tasa_subdelito, 1), size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 6, by = 1), limits = c(0, 7))


tasa_extorsion

ggsave(tasa_extorsion, filename ="tasa_extorsion.png",
       width = 9, height = 6, units = "in", bg = "white")





## Violencia de género ----


tasas_viogen <- as.data.frame(tasas_sub) %>% 
  filter(subdelito == "Violencia de género en todas sus modalidades distinta a la violencia familiar",
         ao < 2022) %>% 
  group_by(ao) %>% 
  summarise(tasa_subdelito = sum(tasa_subdelito))


tasa_vg <- ggplot(tasas_viogen, aes(x = ao, y = tasa_subdelito)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución anual de delitos por violencia de género registrados\nen la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2021\n",
       caption = "*Tasa de carpetas de investigación por cada 100 mil habs. en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) + 
  scale_x_continuous(breaks = seq(from = 2015, to = 2021, by = 1)) +
  geom_point() + 
  geom_label_repel(label = round(tasas_viogen$tasa_subdelito, 1), size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.25), limits = c(0, 2))


tasa_vg

ggsave(tasa_vg, filename ="tasa_vg.png",
       width = 9, height = 6, units = "in", bg = "white")




## Violencia familiar ----


IDM_ZML_viofam <- IDM_ZML %>% 
  filter(cvemunicipio %in% c("10012", "10007", "5035"), 
         subdelito == "Violencia familiar") %>% 
  select(-c(2, 3, 4, 6))

names(IDM_ZML_viofam)[3] <- "01"
names(IDM_ZML_viofam)[4] <- "02"
names(IDM_ZML_viofam)[5] <- "03"
names(IDM_ZML_viofam)[6] <- "04"
names(IDM_ZML_viofam)[7] <- "05"
names(IDM_ZML_viofam)[8] <- "06"
names(IDM_ZML_viofam)[9] <- "07"
names(IDM_ZML_viofam)[10] <- "08"
names(IDM_ZML_viofam)[11] <- "09"
names(IDM_ZML_viofam)[12] <- "10"
names(IDM_ZML_viofam)[13] <- "11"
names(IDM_ZML_viofam)[14] <- "12"

IDM_ZML_viofam2 <- pivot_longer(IDM_ZML_viofam, 3:14, names_to = "Mes", values_to = "CI") %>%
  group_by(ao, Mes) %>%
  summarise(CI = sum(CI)) %>% 
  filter(!is.na(CI))


names(IDM_ZML_viofam2)[1] <- "Year"
names(IDM_ZML_viofam2)[2] <- "Month"

IDM_ZML_viofam2$date <- 
  paste(IDM_ZML_viofam2$Year, IDM_ZML_viofam2$Month, sep="-")


IDM_ZML_viofam2$date <- paste0(IDM_ZML_viofam2$date, '-01')
IDM_ZML_viofam2$date <- as.Date(IDM_ZML_viofam2$date, format = '%Y-%m-%d')



viofam <- ggplot(IDM_ZML_viofam2, aes(x = date, y = CI)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución mensual de delitos por violencia familiar en la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2022\n",
       caption = "*Número de carpetas de investigación en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 years") +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600))


viofam


ggsave(viofam, filename ="mensual_viofam.png",
       width = 9, height = 6, units = "in", bg = "white")





tasas_viofam <- as.data.frame(tasas_sub) %>% 
  filter(subdelito == "Violencia familiar",
         ao < 2022) %>% 
  group_by(ao) %>% 
  summarise(tasa_subdelito = sum(tasa_subdelito))


tasa_vf <- ggplot(tasas_viofam, aes(x = ao, y = tasa_subdelito)) + 
  geom_line(color = "#206779") + 
  theme_minimal() +
  labs(title = "\nEvolución anual de delitos por violencia familiar registrados\nen la Zona Metropolitana de La Laguna*",
       subtitle = "2015-2021\n",
       caption = "*Tasa de carpetas de investigación por cada 100 mil habs. en los municipios de Lerdo, Gómez Palacio y Torreón.\nElaboración de Fernando Cuevas con datos del SESNSP.\n",
       x = "",
       y = "") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8), 
        text = element_text(family = "CircularStd-Book")) + 
  scale_x_continuous(breaks = seq(from = 2015, to = 2021, by = 1)) +
  geom_point() + 
  geom_label_repel(label = round(tasas_viofam$tasa_subdelito, 1), size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 1200, by = 300), limits = c(0, 1200))


tasa_vf

ggsave(tasa_vf, filename ="tasa_viofamiliar.png",
       width = 9, height = 6, units = "in", bg = "white")

