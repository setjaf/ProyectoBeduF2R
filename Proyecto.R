library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(reshape2)
library(gridExtra)

# Establecemos el wd
wd <- "~/Documentos/FCiencias/2021-1/BEDU/Fase_2/Proyecto"
setwd(wd)
rm(wd)

# DATOS DEL PREP 2018 ----
# Cargamos los datos
datos_1 <- read.csv("PREP_2018/base_1.0.csv")
datos_2 <- read.delim("PREP_2018/base_2.0.csv", sep = "|", na.strings = c("-"))

glimpse(datos_1)
colnames(datos_1) <- str_to_title(names(datos_1))

glimpse(datos_2)
datos_2$CLAVE_CASILLA <- str_remove_all(datos_2$CLAVE_CASILLA, "'")
datos_2$CLAVE_ACTA <- str_remove_all(datos_2$CLAVE_ACTA, "'")
colnames(datos_2) <- str_to_title(names(datos_2))
glimpse(datos_2)

# Demos formato a las variables del df
datos_2 <- datos_2 %>% mutate(Clave_casilla = as.factor(Clave_casilla),
                              Clave_acta = as.factor(Clave_acta),
                              Id_estado = as.factor(Id_estado),
                              Id_distrito_federal =
                                  as.factor(Id_distrito_federal),
                              Seccion = as.factor(Seccion),
                              Id_casilla = as.factor(Id_casilla),
                              Ext_contigua = as.factor(Ext_contigua),
                              Ubicacion_casilla = as.factor(Ubicacion_casilla),
                              Tipo_acta = as.factor(Tipo_acta),
                              Total_boletas_sobrantes =
                                  as.integer(Total_boletas_sobrantes),
                              Total_personas_votaron =
                                  as.integer(Total_personas_votaron),
                              Total_rep_partido_ci_votaron =
                                  as.integer(Total_rep_partido_ci_votaron),
                              Total_votos_sacados =
                                  as.integer(Total_votos_sacados),
                              Pan = as.integer(Pan),
                              Pri = as.integer(Pri),
                              Prd = as.integer(Prd),
                              Pvem = as.integer(Pvem),
                              Pt = as.integer(Pt),
                              Mc = as.integer(Mc),
                              Panal = as.integer(Panal),
                              Morena = as.integer(Morena),
                              Pes = as.integer(Pes),
                              C_pan_prd_mc = as.integer(C_pan_prd_mc),
                              C_pan_prd = as.integer(C_pan_prd),
                              C_pan_mc = as.integer(C_pan_mc),
                              C_prd_mc = as.integer(C_prd_mc),
                              C_pri_pvem_panal = as.integer(C_pri_pvem_panal),
                              C_pri_pvem = as.integer(C_pri_pvem),
                              C_pri_panal = as.integer(C_pri_panal),
                              C_pvem_panal = as.integer(C_pvem_panal),
                              C_pt_morena_pes = as.integer(C_pt_morena_pes),
                              C_pt_morena = as.integer(C_pt_morena),
                              C_pt_pes = as.integer(C_pt_pes),
                              C_morena_pes = as.integer(C_morena_pes),
                              Cand_ind_01 = as.integer(Cand_ind_01),
                              Cand_ind_02 = as.integer(Cand_ind_02),
                              No_registrados = as.integer(No_registrados),
                              Nulos = as.integer(Nulos),
                              Total_votos_asentado =
                                  as.integer(Total_votos_asentado),
                              Total_votos_calculado =
                                  as.integer(Total_votos_calculado),
                              Lista_nominal = as.integer(Lista_nominal),
                              Representantes_pp_ci =
                                  as.integer(Representantes_pp_ci),
                              Observaciones = as.factor(Observaciones),
                              Contabilizada = as.factor(Contabilizada),
                              Mecanismos_traslado =
                                  as.factor(Mecanismos_traslado),
                              Sha = as.factor(Sha),
                              Fecha_hora_acopio =
                                  ymd_hms(Fecha_hora_acopio,
                                          tz = "America/Mexico_City"),
                              Fecha_hora_captura =
                                  ymd_hms(Fecha_hora_captura,
                                          tz = "America/Mexico_City"),
                              Fecha_hora_verificacion =
                                  ymd_hms(Fecha_hora_verificacion,
                                          tz = "America/Mexico_City"),
                              Origen = as.factor(Origen),
                              Digitalizacion = as.factor(Digitalizacion),
                              Tipo_documento = as.factor(Tipo_documento),
                              Cotejada = as.factor(Cotejada)) 

glimpse(datos_2)
summary(datos_2)

# Ya con los datos en el formato correcto se pueden obtener muchas cosas

# Distritos electorales por estado
datos_2 %>% group_by(Estado) %>%
    summarise(Distritos_por_estado = n_distinct(Id_distrito_federal)) %>%
    ggplot(aes(x = Estado, y = Distritos_por_estado)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Número de distritos electorales por estado") +
    ylab("") + xlab("")
# Se observa que el Estado de México y Veracruz son los estados con mayor número
# de distritos electorales.

# Numero de electores segun la lista nominal por estado
datos_2 %>% group_by(Estado) %>%
    summarise(Lista_nom = sum(Lista_nominal, na.rm = T)) %>%
    ggplot(aes(x= Estado, y = Lista_nom)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Lista nominal por estado") +
    ylab("") + xlab("")
# Nuevamente el Estado de México y Veracruz contienen la mayor cantidad de
# electores

# Numero de electores total
aux <- datos_2 %>% group_by(Estado) %>%
    summarise(Lista_nom = sum(Lista_nominal, na.rm = T))
e_totales <- sum(aux$Lista_nom)
print(paste("En total", e_totales,
            "personas se encontraban en la lista nominal del 2018."))

# Creamos una función que regrese una grafica por estado de los partidos o los
# candidatos
graficas <- function(partcand, estado){
    estado = str_to_upper(estado)
    if(partcand == "Partido"){
        aux = datos_2 %>% group_by(Estado) %>%
            summarise(PRI = sum(Pri, na.rm = T), PAN = sum(Pan, na.rm = T),
                      PRD = sum(Prd, na.rm = T), PVEM = sum(Pvem, na.rm = T),
                      PANAL = sum(Panal, na.rm = T), MC = sum(Mc, na.rm = T),
                      PT = sum(Pt, na.rm = T), MORENA = sum(Morena, na.rm = T),
                      PES = sum(Pes, na.rm = T),
                      PAN_PRD_MC = sum(C_pan_prd_mc, na.rm = T),
                      PAN_PRD = sum(C_pan_prd, na.rm = T),
                      PAN_MC = sum(C_pan_mc, na.rm = T),
                      PRD_MC = sum(C_prd_mc, na.rm = T),
                      PRI_PVEM_PANAL = sum(C_pri_pvem_panal, na.rm = T),
                      PRI_PVEM = sum(C_pri_pvem, na.rm = T),
                      PRI_PANAL = sum(C_pri_panal, na.rm = T),
                      PVEM_PANAL = sum(C_pri_pvem_panal, na.rm = T),
                      PT_MORENA_PES = sum(C_pt_morena_pes, na.rm = T),
                      PT_MORENA = sum(C_pt_morena, na.rm = T),
                      PT_PES = sum(C_pt_pes, na.rm = T),
                      MORENA_PES = sum(C_morena_pes, na.rm = T),
                      INDEP_1 = sum(Cand_ind_01, na.rm = T),
                      INDEP_2 = sum(Cand_ind_02, na.rm = T),
                      No_registrado = sum(No_registrados, na.rm = T),
                      Nulo = sum(Nulos, na.rm = T)) %>%
            filter(Estado == estado)
        aux = aux[,-1]
        aux = melt(aux, value.name = "Votos")
        final = aux %>% ggplot(aes(x = variable, y = Votos)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por partidos y coaliciones\n",
                           estado)) + xlab("")
        return(final)}
    if(partcand == "Candidato"){
        aux = datos_2 %>% group_by(Estado) %>%
            summarise(J.A.M = sum(Pri, Pvem, Panal, C_pri_pvem_panal, C_pri_pvem,
                                  C_pri_panal, C_pvem_panal, na.rm = T),
                      R.A = sum(Pan, Prd, Mc, C_pan_prd_mc, C_pan_prd, C_pan_mc,
                                C_prd_mc, na.rm = T),
                      A.M.L.O = sum(Pt, Morena, Pes, C_pt_morena_pes, C_pt_morena,
                                    C_pt_pes, C_morena_pes, na.rm = T),
                      M.Z = sum(Cand_ind_01, na.rm = T),
                      J.R.C = sum(Cand_ind_02, na.rm = T),
                      Nulos = sum(Nulos, na.rm = T)) %>%
            filter(Estado == estado)
        aux = aux[,-1]
        aux = melt(aux, value.name = "Votos")
        final = aux %>% ggplot(aes(x = variable, y = Votos)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por candidato\n",
                           estado)) + xlab("")
        return(final)}
    else{stop("Argumento(s) no valido(s). Los argumentos aceptan acentos")}
    }

# Ejemplos de la funcion graficas (LA FUNCION ES SENSIBLE A ACENTOS)
graficas("Partido", "Morelos")
graficas("Candidato", "Yucatán")
graficas("Partido", "Campeche")
graficas("Partido", "Tlaxcala")

# Buscamos el partido que gano en cada estado
e_estados <- datos_2 %>% group_by(Estado) %>%
    summarise(PRI = sum(Pri, na.rm = T), PAN = sum(Pan, na.rm = T),
              PRD = sum(Prd, na.rm = T), PVEM = sum(Pvem, na.rm = T),
              PANAL = sum(Panal, na.rm = T), MC = sum(Mc, na.rm = T),
              PT = sum(Pt, na.rm = T), MORENA = sum(Morena, na.rm = T),
              PES = sum(Pes, na.rm = T),
              PAN_PRD_MC = sum(C_pan_prd_mc, na.rm = T),
              PAN_PRD = sum(C_pan_prd, na.rm = T),
              PAN_MC = sum(C_pan_mc, na.rm = T),
              PRD_MC = sum(C_prd_mc, na.rm = T),
              PRI_PVEM_PANAL = sum(C_pri_pvem_panal, na.rm = T),
              PRI_PVEM = sum(C_pri_pvem, na.rm = T),
              PRI_PANAL = sum(C_pri_panal, na.rm = T),
              PVEM_PANAL = sum(C_pri_pvem_panal, na.rm = T),
              PT_MORENA_PES = sum(C_pt_morena_pes, na.rm = T),
              PT_MORENA = sum(C_pt_morena, na.rm = T),
              PT_PES = sum(C_pt_pes, na.rm = T),
              MORENA_PES = sum(C_morena_pes, na.rm = T),
              INDEP_1 = sum(Cand_ind_01, na.rm = T),
              INDEP_2 = sum(Cand_ind_02, na.rm = T),
              No_registrado = sum(No_registrados, na.rm = T))

# write.csv(e_estados, "e_estados2018.csv", row.names = F)
aux <- apply(e_estados[,-1], 1, which.max)
aux <- names(e_estados[,-1])[aux]
mat_final <- data.frame(Estado = e_estados$Estado,
                        Partido_2006 = rep(NA,32),
                        Partido_2012 = rep(NA,32),
                        Partido_2018 = aux)

# DATOS DEL PREP 2012 ----
# Cargamos los nuevos datos
datos_12_1 <- read.delim("PREP_2012/base_1.csv", sep = "|")
datos_12_2 <- read.delim("PREP_2012/base_2.csv", sep = "|",
                         na.strings = c("null", "Sin dato"))

glimpse(datos_12_2)
#Damos formato a las variables
datos_12_2 <- datos_12_2 %>% mutate(ESTADO = as.factor(ESTADO),
                                    DISTRITO = as.factor(DISTRITO),
                                    SECCION = as.factor(SECCION),
                                    ID_CASILLA = as.factor(ID_CASILLA),
                                    TIPO_CASILLA = as.factor(TIPO_CASILLA),
                                    EXT_CONTIGUA = as.factor(EXT_CONTIGUA),
                                    UBICACION_CASILLA =
                                        as.factor(UBICACION_CASILLA),
                                    TIPO_ACTA = as.factor(TIPO_ACTA),
                                    PRI = as.integer(PRI),
                                    PAN = as.integer(PAN),
                                    PRD = as.integer(PRD),
                                    PVEM = as.integer(PVEM),
                                    PT = as.integer(PT),
                                    MC = as.integer(MC),
                                    PANAL = as.integer(PANAL),
                                    C_PRI_PVEM = as.integer(C_PRI_PVEM),
                                    C_PRD_PT_MC = as.integer(C_PRD_PT_MC),
                                    C_PRD_PT = as.integer(C_PRD_PT),
                                    C_PRD_MC = as.integer(C_PRD_MC),
                                    C_PT_MC = as.integer(C_PT_MC),
                                    NO_REGISTRADOS = as.integer(NO_REGISTRADOS),
                                    NULOS = as.integer(NULOS),
                                    TOTAL_VOTOS = as.integer(TOTAL_VOTOS),
                                    LISTA_NOMINAL = as.integer(LISTA_NOMINAL),
                                    OBSERVACIONES = as.factor(OBSERVACIONES),
                                    CONTABILIZADA = as.factor(CONTABILIZADA),
                                    CRYT = as.factor(CRYT),
                                    HORA_ACOPIO =
                                        ymd_hms(HORA_ACOPIO,
                                                tz = "America/Mexico_City"),
                                    HORA_CAPTURA =
                                        ymd_hms(HORA_CAPTURA,
                                                tz ="America/Mexico_City"),
                                    HORA_REGISTRO =
                                        ymd_hms(HORA_REGISTRO,
                                                tz ="America/Mexico_City"))
glimpse(datos_12_2)

# Buscamos el partido que ganó en cada estado en 2012
e_estados_2012 <- datos_12_2 %>% group_by(ESTADO) %>%
    summarise(PAN = sum(PAN, na.rm = T), PRI = sum(PRI, na.rm = T),
              PRD = sum(PRD, na.rm = T), PVEM = sum(PVEM, na.rm = T),
              PT = sum(PT, na.rm = T), MC = sum(MC, na.rm = T),
              PANAL = sum(PANAL, na.rm = T),
              PRI_PVEM = sum(C_PRI_PVEM, na.rm = T),
              PRD_PT_MC = sum(C_PRD_PT_MC, na.rm = T),
              PRD_PT = sum(C_PRD_PT, na.rm = T),
              PRD_MC = sum(C_PRD_MC, na.rm = T),
              PT_MC = sum(C_PT_MC, na.rm = T),
              No_registrado = sum(NO_REGISTRADOS, na.rm = T))

# write.csv(e_estados_2012, "e_estados2012.csv", row.names = F)
aux <- apply(e_estados_2012[,-1], 1, which.max)
aux2 <- names(e_estados[,-1])[aux]

aux3 <- aux2[1:4]; aux4 <- aux2[5:6]; aux5 <- aux2[7:8]; aux6 <- aux2[9]
aux7 <- aux2[10:32]

aux_f <- c(aux3, aux5, aux6, aux4, aux7)
mat_final$Partido_2012 <- aux_f

# mat_final$Cambio <- ifelse(mat_final$Partido_2012 == mat_final$Partido_2018,
#                            0, 1)

# DATOS DEL PREP 2006 ----
datos_06 <- read.csv("PREP_2006/datos_2006.csv")

aux <- apply(datos_06[,-1], 1, which.max)
aux <- names(datos_06[,-1])[aux]
aux2 <- aux[1:4]; aux3 <- aux[5:6]; aux4 <- aux[7:8]; aux5 <- aux[9]
aux6 <- aux[10:32]
aux <- c(aux2, aux4, aux5, aux3, aux6)
mat_final$Partido_2006 <- aux

#write.csv(mat_final, "mat_final.csv", row.names = F)
