#1. Carga de Paquetes ------

pacman::p_load(tidyverse,
               haven, 
               forcats,
               car,
               sjmisc,
               sjPlot,
               survey,
               srvyr,
               kableExtra)


#2. Carga de Datos -----

TS4 <- read_dta("input/data/TS4.dta")


#3. Procesamiento de Datos -----

# Filtracion y Seleccion de Datos -----

sjPlot::view_df(TS4)


TS4_Proc <- TS4 %>% 
  filter(edad >= 18 & edad <= 64)%>% 
  mutate(tramo_edad = case_when(edad <= 19 ~ "Adolescente",
                                edad <= 24 ~  "Adulto Joven",
                                edad > 24 & edad <= 64 ~ "Adulto",
                                TRUE ~ NA_character_)) %>%
  mutate(sexo = as.character(.$sexo)) %>% 
  mutate(sexo = car::recode(.$sexo, c("1 = 'Hombre'; 2 = 'Mujer'"))) %>%
  mutate(c6 = as.character(.$c6)) %>% 
  mutate(c6 = car::recode(.$c6, c("1 = 'Ha Disminuido'; 2 = 'Se Ha Mantenido';
                                    3 = 'Ha Aumentado'; 4 = 'No Consume'"))) %>%
  mutate(c10 = as.character(.$c10)) %>% 
  mutate(c10 = car::recode(.$c10, c("1 = 'Nunca'; 2 = 'Pocas Veces';
                                    3 = 'Algunas Veces'; 4 = 'Muchas Veces';
                                    5 = 'Siempre'"))) %>%
  mutate(c11 = as.character(.$c11)) %>% 
  mutate(c11 = car::recode(.$c11, c("1 = 'Si'; 2 = 'No'"))) %>%
  select(folio, region, sexo, edad, tramo_edad, c1, c2, c3, c6, c10, c11,
         factor_TS4)


nrow(na.omit(TS4_Proc))

TS4_Proc[TS4_Proc == -777] <- NA
TS4_Proc[TS4_Proc == -888] <- NA
TS4_Proc[TS4_Proc == -999] <- NA

nrow(na.omit(TS4_Proc))

TS4_Proc <- na.omit(TS4_Proc)

# Creacion Objeto Encuesta ----

objeto_encuesta<- TS4_Proc %>%
  as_survey_design(ids = folio,
                   strata = region,
                   weights = factor_TS4)


# Analisis Bivariado -----

objeto_encuesta %>% 
  group_by(sexo, c1) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

objeto_encuesta %>% 
  group_by(sexo, c2) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

objeto_encuesta %>% 
  group_by(tramo_edad, c3) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

objeto_encuesta %>% 
  group_by(sexo, c6) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

objeto_encuesta %>% 
  group_by(sexo, c10) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

objeto_encuesta %>% 
  group_by(tramo_edad, c11) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()


# Graficos ----

plot_frq(objeto_encuesta$variables$c1,
         type = c("bar"),
         title = "Satisfaccion con la vida",
         show.ci = T,
         coord.flip = T,
         geom.colors = "violet",
         weight.by = objeto_encuesta$variables$factor_TS4)

plot_frq(objeto_encuesta$variables$c2,
         type = c("bar"),
         title = "Estado de Animo Actual",
         show.ci = T,
         coord.flip = T,
         geom.colors = "violet",
         weight.by = objeto_encuesta$variables$factor_TS4)

plot_frq(objeto_encuesta$variables$c3,
         type = c("bar"),
         title = "Necesidad de Tratamiento en Salud Mental",
         show.ci = T,
         coord.flip = T,
         geom.colors = "violet",
         weight.by = objeto_encuesta$variables$factor_TS4)


# Tablas ----

objeto_encuesta %>%
  group_by(c6) %>%
  summarise(porcentaje = survey_mean(vartype = "ci", na.rm = T)) %>%
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>%
  kable(caption = "Consumo de Medicamentos",
        format = "pipe",
        colnames = c("variable",
                     "prc %",
                     "prc % limite inferior",
                     "prc % limite superior"))

objeto_encuesta %>%
  group_by(c10) %>%
  summarise(porcentaje = survey_mean(vartype = "ci", na.rm = T)) %>%
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>%
  kable(caption = "Frecuencia de Conversacion de Problemas",
        format = "pipe",
        colnames = c("variable",
                     "prc %",
                     "prc % limite inferior",
                     "prc % limite superior"))

objeto_encuesta %>%
  group_by(c11) %>%
  summarise(porcentaje = survey_mean(vartype = "ci", na.rm = T)) %>%
  mutate(porcentaje = porcentaje*100,
         porcentaje_low = porcentaje_low*100,
         porcentaje_upp = porcentaje_upp*100) %>%
  kable(caption = "Padres han recibido Diagnostico de Salud Mental",
        format = "pipe",
        colnames = c("variable",
                     "prc %",
                     "prc % limite inferior",
                     "prc % limite superior"))


#4. Guardado de Datos -------

save(TS4_Proc, file = "output/data/TS4_Proc.RData")

save(TS4_Proc, objeto_encuesta,
     file = "output/data/datos_proc.RData")



