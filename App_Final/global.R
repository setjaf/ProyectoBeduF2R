library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(reshape2)
library(gridExtra)
library(glm2)

# Archivo que acompaña a la app. Este archivo es similar a Proyecto.R pero con
# la diferencia de que aquí están el resto de funciones que requerimos para
# correr la app.

# PREP 2018 ----
datos_2 <- read.delim("PREP_2018/base_2.0.csv", sep = "|", na.strings = c("-"))
datos_2$CLAVE_CASILLA <- str_remove_all(datos_2$CLAVE_CASILLA, "'")
datos_2$CLAVE_ACTA <- str_remove_all(datos_2$CLAVE_ACTA, "'")
colnames(datos_2) <- str_to_title(names(datos_2))

# Formato a los datos
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

# Funcion para graficar. Estanmos interesados en las graficas por estado de los
# partidos y los candidatos
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
            geom_bar(fill = "#108546", stat = "identity") +
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
            geom_bar(fill = "#108546", stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por candidato\n",
                           estado)) + xlab("")
        return(final)}
    else{stop("Argumento(s) no valido(s). Los argumentos aceptan acentos")}
}

# Tabla de votos por estado
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

# Estamos interesados en saber que partido obtuvo más votos en cada estado,
# para esto se debe encontrar el máximo de los votos sobre cada estado
aux <- apply(e_estados[,-1], 1, which.max)
aux <- names(e_estados[,-1])[aux]

# Creacion de la matriz de transiciones de partidos. La matriz tiene 4 columnas
# La primera corresponde al estado, la segunda al partido que ganó en dicho
# estado en el 2006. Las siguientes columnas son similares pero ahora son para
# los años 2012 y 2018. 
mat_final <- data.frame(Estado = e_estados$Estado,
                        Partido_2006 = rep(NA,32),
                        Partido_2012 = rep(NA,32),
                        Partido_2018 = aux)

# PREP 2012 ----
datos_12_2 <- read.delim("PREP_2012/base_2.csv", sep = "|",
                         na.strings = c("null", "Sin dato"))

# Damos formato a las columnas
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

# Creamos una tabla con votos por estado
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

# El PREP 2012 no tiene los nombres de los estados, sino que tiene números ṕara
# hacer referencia a éstos, por lo que debemos conectar esos números con la
# columna de etados de mat_final´
aux <- apply(e_estados_2012[,-1], 1, which.max)
aux2 <- names(e_estados[,-1])[aux]
aux3 <- aux2[1:4]; aux4 <- aux2[5:6]; aux5 <- aux2[7:8]; aux6 <- aux2[9]
aux7 <- aux2[10:32]
aux_f <- c(aux3, aux5, aux6, aux4, aux7)
mat_final$Partido_2012 <- aux_f
aux <- e_estados$Estado
aux2 <- c(1:4, 7:9, 5:6, 10:32)
e_estados_2012 <- e_estados_2012[aux2,]
e_estados_2012$ESTADO <- e_estados$Estado

# PREP 2006 ----
datos_06 <- read.csv("PREP_2006/datos_2006.csv")
datos_06$Estado <- str_to_upper(datos_06$Estado)

# Nuevamente los estados están en un orden distinto a la columna de estados de
# mat_final, por lo hay que arreglarlo
aux <- apply(datos_06[,-1], 1, which.max)
aux <- names(datos_06[,-1])[aux]
aux2 <- aux[1:4]; aux3 <- aux[5:6]; aux4 <- aux[7:8]; aux5 <- aux[9]
aux6 <- aux[10:32]
aux <- c(aux2, aux4, aux5, aux3, aux6)
datos_06[c(1:4, 7:8, 9, 5:6, 10:32),]
datos_06[9,1] = "CIUDAD DE MÉXICO"
mat_final$Partido_2006 <- aux

# Ya con la matriz mat_final completa nos interesan las transiciones de partidos
# por estado en el periodo 2006-2012 y 2012-2018, por lo que crearemos dos
# matrices adicionales que contendran el nombre de los partidos que habia antes
# contra los que hubo despues. Si el partido cambió se asigna un 1, en otro caso
# se asigna un 0.
aux_0612 <- mat_final[,1:3]
aux_0612$Cambio = ifelse(aux_0612$Partido_2006 == aux_0612$Partido_2012, 0, 1)
aux_0612$Partido_2006 = as.factor(aux_0612$Partido_2006)
aux_0612$Partido_2012 = as.factor(aux_0612$Partido_2012)

aux_1218 <- mat_final[,-2]
aux_1218$Cambio = ifelse(aux_1218$Partido_2012 == aux_1218$Partido_2018, 0, 1)
aux_1218$Partido_2012 = as.factor(aux_1218$Partido_2012)

# Detalles finales (dar un formato uniforma para la aplicación)----
colnames(e_estados) <- str_to_upper(names(e_estados))
colnames(e_estados_2012) <- str_to_upper(names(e_estados_2012))
colnames(datos_06) <- str_to_upper(names(datos_06))

datos_06$ESTADO = as.factor(datos_06$ESTADO)

# Resto de funciones ----
# Estas funciones grafican el número de votos obtenidos por estado, ya sea para
# por candidato o por partido político 
graficas_2 <- function(partcand, estado){
    estado = str_to_upper(estado)
    if(partcand == "Partido"){
        aux = e_estados_2012 %>%
            filter(ESTADO == estado)
        aux = aux[,-1]
        aux = melt(aux, value.name = "Votos")
        final1 = aux %>% ggplot(aes(x = variable, y = Votos)) +
            geom_bar(fill = "#0A3161", stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por partidos y coaliciones\n",
                           estado)) + xlab("")
        return(final1)}
    if(partcand == "Candidato"){
        aux = e_estados_2012 %>% filter(ESTADO == estado) %>%
            mutate(ESTADO = ESTADO, E.N.P = sum(PRI,PRI_PVEM,  na.rm = T),
                      A.M.L.O = sum(PRD, PRD_PT_MC, PRD_PT, PRD_MC,  na.rm = T),
                      J.V.M = sum(PAN, na.rm = T),
                      G.Q.T  = sum(PVEM, na.rm = T)) %>%
            select(ESTADO, E.N.P, A.M.L.O, J.V.M, G.Q.T)
        aux = aux[,-1]
        aux = melt(aux, value.name = "Votos")
        final2 = aux %>% ggplot(aes(x = variable, y = Votos)) +
            geom_bar(fill = "#0A3161", stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por candidato\n",
                           estado)) + xlab("")
        return(final2)}
    else{stop("Argumento(s) no valido(s). Los argumentos aceptan acentos")}
}

graficas_3 <- function(partcand, estado){
    estado = str_to_upper(estado)
    if(partcand == "Partido"){
        aux = datos_06 %>%
            filter(ESTADO == estado)
        aux = aux[,-1]
        aux = melt(aux, value.name = "Votos")
        final1 = aux %>% ggplot(aes(x = variable, y = Votos)) +
            geom_bar(fill = "#AF2B39", stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por partidos y coaliciones\n",
                           estado)) + xlab("")
        return(final1)}
    else if(partcand == "Candidato"){
        aux = datos_06 %>% filter(ESTADO == estado) %>%
            mutate(ESTADO = ESTADO, F.C = sum(PAN,  na.rm = T),
                      A.M.L.O = sum(PRD,na.rm = T),
                      R.M = sum(PRI_VERDE, na.rm = T)) %>%
            select(ESTADO, F.C, A.M.L.O, R.M)
        aux = aux[,-1]
        aux = melt(aux, value.name = "Votos")
        final2 = aux %>% ggplot(aes(x = variable, y = Votos)) +
            geom_bar(fill = "#AF2B39", stat = "identity") +
            theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
            ggtitle(paste0("Votos recibidos por candidato\n",
                           estado)) + xlab("")
        return(final2)}
    else{stop("Argumento(s) no valido(s). Los argumentos aceptan acentos")}}

# NOS PREGUNTAMOS SI LAS TRANSICIONES ENTRE PARTIDOS POR ESTADO PUEDEN DEBERSE A
# ALGUNOS INDICADORES ECONOMICOS-SOCIALES (POR EJEMPLO PERCEPCION DE
# INSEGURIDAD, PIB, EXPORTACIONES, ETC)

# Parte de los indicadores ----
# Los indicadores considerados fueron:
# 
# PERCEPCION DE INSEGURIDAD
# EXPORTACIONES
# PIB
# MATRICULACION
# SERVICIOS PERSONALES
# MATERIALES Y SUMINISTROS
# SERVICIOS GENERALES
# 
# Estos datos se obtuvieron de la pagina del INEGI y se tomaron en periodos
# intermedios a las elecciones a comparar (2009 y 2015)
Ind_2015 <- read.csv("Indicadores_2015.csv")
Ind_2015 <- Ind_2015 %>% mutate(Estado = as.factor(Estado),
                                Prevalecia.delictiva.2015 = as.numeric(Prevalecia.delictiva.2015))

Ind_2009 <- read.csv("Indicadores_2009.csv")
Ind_2009 <- Ind_2009 %>% mutate(Estado = as.factor(Estado),
                                Prevalencia.delictiva.2010 = as.numeric(Prevalencia.delictiva.2010))

Ind_2009$Estado = str_to_upper(Ind_2009$Estado)
Ind_2015$Estado = str_to_upper(Ind_2015$Estado)

# Regresion ----
# A PARTIR DE LOS DATOS OBTENIDOS DE LAS BASES DEL PREP DE LAS ULTIMAS 3
# ELECCIONES PRESIDENCIALES SE ARMÓ UN DATAFRAME QUE CONTENIA LAS TRANSICIONES
# DE PARTIDOS POR ESTADOS. ESTA TRANSICION SE INTENTA EXPLICAR A PARTIR DE
# INDICADORES SOCIO ECONOMICOS. EL METODO SELECCIONADO FUE UNA REGRESION
# LOGISTICA CUYA VARIABLE EXPLICATIVA ES LA TRANSICION DE PARTIDOS EN LOS
# ESTADOS (VARIABLE BINARIO) Y LOS PREDICTORES FUERON LOS INDICADORES
# SOCIO-ECONOMICOS (VARIABLES CONTINUAS)
mat_reg09 <- cbind(Ind_2009, Cambio = aux_0612$Cambio)
mat_reg09 <- mat_reg09[-7, -1]
mat_reg15 <- cbind(Ind_2015, Cambio = aux_1218$Cambio)
mat_reg15 <- mat_reg09[-7, -1]

mod_09 <- glm(Cambio ~ ., data = mat_reg09, family = binomial(link = "logit"))
aux <- summary(mod_09)
sal_09 <- round(aux$coefficients, 5)

mod_15 <- glm(Cambio ~ ., data = mat_reg15, family = binomial(link = "logit"))
aux <- summary(mod_15)
sal_15 <- round(aux$coefficients, 5)

# CONCLUSION ----
# A PARTIR DE LOS HALLAZGOS DEL MODELO DE REGRESION LOGISTICA SE PUEDE CONCLUIR
# QUE LOS INDICADORES SOCIOECONOMICOS NO INFLUYERON EN LA TOMA DE DECISIONES
# POLITICAS, AL MENOS AL NIVEL DE ELECCIONES PRESIDENCIALES DURANTE EL PERIODO
# DEL 2006 AL 2018. CABE DESTACAR QUE EN ESTE PERIODO OCURRIERON DOS TRANSICIONES
# DE LOS PARTIDOS EN EL PODER.
