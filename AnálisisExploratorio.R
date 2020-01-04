


# CARGA DE PAQUETES Y FUNCIONES PRE-DEFINIDAS------------------------------
suppressMessages(library(DBI))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(summarytools))
suppressMessages(library(caret))
suppressMessages(library(ROSE))
suppressMessages(library(ROCR))
suppressMessages(library(klaR))
suppressMessages(library(xgboost))
# suppressMessages(library(lightgbm))
suppressMessages(library(Matrix))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(qcc))
suppressMessages(library(forcats))
suppressMessages(library(data.table))
suppressMessages(library(smotefamily))
suppressMessages(library(randomForest))
suppressMessages(library(e1071))

suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))

# C:/Users/vinuezab/Desktop/Paquetes/ResultadosModeloDesersion+Informe/Nueva Ejecucion
setwd("C:/Users/vinuezab/Desktop/Paquetes/ResultadosModeloDesersion+Informe")

source("./FuncionesChurn.R")
{
  # CONEXION A LA BASE DE DATOS ---------------------------------------------
  # con <- dbConnect(odbc::odbc(), .connection_string = 'driver={SQL Server};server=10.1.1.4;trusted_connection=true', encoding = "latin1") # "windows-1252"

  # DETERMINACION DE LAS FECHAS DE ESTUDIO ----------------------------------
  # FechasObjetivo <- dbGetQuery(con, "SELECT DISTINCT(FechaCorte) FROM lTab_VariablesTotal_Micro_Churn ORDER BY FechaCorte DESC" )[[1]]
  # [1] "2018-08-31" "2018-07-31" "2018-06-30" "2018-05-31" "2018-04-30" "2018-03-31" "2018-02-28" "2018-01-31" "2017-12-31" "2017-11-30" "2017-10-31"
  # [12] "2017-09-30" "2017-08-31" "2017-07-31" "2017-06-30" "2017-05-31" "2017-04-30" "2017-03-31" "2017-02-28" "2017-01-31" "2016-12-31" "2016-11-30"
  # [23] "2016-10-31" "2016-09-30" "2016-08-31" "2016-07-31" "2016-06-30" "2016-05-31" "2016-04-30" "2016-03-31" "2016-02-29" "2016-01-31" "2015-12-31"
  # [34] "2015-11-30" "2015-10-31" "2015-09-30" "2015-08-31" "2015-07-31" "2015-06-30" "2015-05-31" "2015-04-30" "2015-03-31" "2015-02-28" "2015-01-31"

  # CONSULTA DE TABLAS  -----------------------------------------------------
  # ref <- 1      # 2018-08-31
  # fechasEstudio <- rev(gsub("-", "", FechasObjetivo[ref:(ref+5)]))
  # Fecha <- list(Fecha1=fechasEstudio[1], Fecha4=fechasEstudio[4], Fecha5=fechasEstudio[5], Fecha6=fechasEstudio[6])

  dataChurn <- dbGetQuery( con, paste0("SELECT cast('",Fecha$Fecha6,"' as date) as FechaCorteAnalisis, tablaReciente.*, tablaChurnCliente.bandera as churn
                                       FROM(
                                       SELECT case when table2.IdCliente is null then table1.IdCliente else table2.IdCliente end as IdCliente, case when table2.IdCliente is null then 1 else 0 end as bandera
                                       FROM (SELECT distinct IdCliente FROM lTab_VariablesTotal_Micro_Churn WHERE FechaCorte BETWEEN '", Fecha$Fecha1, "' AND '", Fecha$Fecha4, "' ) AS table1
                                       FULL OUTER JOIN
                                       (SELECT distinct IdCliente FROM lTab_VariablesTotal_Micro_Churn WHERE FechaCorte BETWEEN '", Fecha$Fecha5, "' AND '", Fecha$Fecha6, "' ) table2 ON table1.IdCliente = table2.IdCliente) tablaChurnCliente
                                       left join (
                                       SELECT *
                                       FROM ( SELECT *,  ROW_NUMBER() OVER (partition by IdCliente  ORDER BY FechaCorte desc) AS seq
                                       FROM lTab_VariablesTotal_Micro_Churn
                                       WHERE FechaCorte BETWEEN '", Fecha$Fecha1, "' AND '", Fecha$Fecha6, "' ) AS t
                                       WHERE  t.seq = 1 ) tablaReciente
                                       on tablaChurnCliente.IdCliente=tablaReciente.IdCliente" )) %>%
    dplyr::mutate(FechaCorte=as.Date(FechaCorte)) %>% data.table::as.data.table()



  # CREACION DE NUEVAS VARIABLES A PARTIR DE LAS BASES ANTERIORES -----------
  # Las variables se crean en un archivo a parte para evitar sobrecargar el programa.
  # dfNroOperaciones                      <- data.table(readRDS("./dfNroOperaciones.rds"))
  # names(dfNroOperaciones)[3:length(names(dfNroOperaciones))] <- paste0("Externa_", names(dfNroOperaciones)[3:length(names(dfNroOperaciones))])
  # setkey(dfNroOperaciones, "IdCliente")
  # 
  # dfPivotTipoCuenta                     <- data.table(readRDS("./dfPivotTipoCuenta.rds"))
  # names(dfPivotTipoCuenta)[3:length(names(dfPivotTipoCuenta))] <- paste0("Externa_", names(dfPivotTipoCuenta)[3:length(names(dfPivotTipoCuenta))])
  # setkey(dfPivotTipoCuenta, "IdCliente")
  # 
  # dfAcumuladosValorSaldo_CuotaEstimada  <- data.table(readRDS("./dfAcumuladosValorSaldo_CuotaEstimada.rds"))
  # names(dfAcumuladosValorSaldo_CuotaEstimada)[3:length(names(dfAcumuladosValorSaldo_CuotaEstimada))] <- paste0("Externa_", names(dfAcumuladosValorSaldo_CuotaEstimada)[3:length(names(dfAcumuladosValorSaldo_CuotaEstimada))])
  # setkey(dfAcumuladosValorSaldo_CuotaEstimada, "IdCliente")
  # 
  # # dfChurnBimensual                      <- data.table(readRDS("./VariablesIndicaChurnBimensual.rds"))
  # # names(dfChurnBimensual)[2:length(names(dfChurnBimensual))] <- paste0("Churn_", names(dfChurnBimensual)[2:length(names(dfChurnBimensual))])
  # # setkey(dfChurnBimensual, "IdCliente")
  # dfTiempoEsperaXCliente                <- data.table(readRDS("./TiemposEsperaCliente2.rds"))
  # setkey(dfTiempoEsperaXCliente, "IdCliente")


  # CRUCE DE LAS BASES PARA LA BASE TOTAL -----------------------------------
  # cruce de la consulta con las otras bases hechas
  # dataTotal <- dataChurn %>%
  #   dplyr::left_join(., dfNroOperaciones, by=c("IdCliente"="IdCliente", "FechaCorte"="FechaCorte")) %>%
  #   dplyr::left_join(., dfPivotTipoCuenta, by=c("IdCliente"="IdCliente", "FechaCorte"="FechaCorte")) %>%
  #   dplyr::left_join(., dfAcumuladosValorSaldo_CuotaEstimada, by=c("IdCliente"="IdCliente", "FechaCorte"="FechaCorte")) %>%
  #   dplyr::left_join(., dfTiempoEsperaXCliente[, c("IdCliente", "Promedio", "Total", "N_espacios", "MesesEsperaEnt"), with = FALSE], by=c("IdCliente"="IdCliente")) %>%
  #   dplyr::rename_all(funs(gsub(" ", "_", toupper(.)))) %>%
  #   dplyr::mutate_at(vars(contains('FECHA')), funs(as.Date(.))) %>%
  #   dplyr::mutate(CHURN=factor(CHURN))
  # glimpse(dataTotal)
}


# hist(dataTotal$ATRASOMAXU12M[dataTotal$ATRASOMAXU12M>100], main="Distribuci?n de ATRASOMAXU12M",
#      xlab = "", ylab = "Frecuencia", col = dataTotal$CHURN[dataTotal$ATRASOMAXU12M>100])
hist(baseVariables_15dias$MADUREZPROM, main="Distribuci?n de ATRASOMAXU12M (Total)",
     xlab = "", ylab = "Frecuencia", col = if_else(baseVariables_15dias$CHURN==0, "cyan","yellow"))
# legend(3000, 80000, c("Cliente", "Desertor"), lwd=6, col=c("cyan","yellow"))
# abline(v=41, col="red")

dim(baseVariables_15dias)
# CORRECION DE LA VARIABLE CHURN (desertor: bueno-malo) -------------------
VariableChurn_ini <- factor(baseVariables_15dias[["CHURN"]])
VariableChurn_aux <- factor(if_else(baseVariables_15dias$ATRASOMAXU12M<=41, 0, 1)) # Buen desertor vs. Mal desertor
# table(VariableChurn_ini)
# table(VariableChurn_aux)
# table(VariableChurn_ini, VariableChurn_aux)
VariableChurn_fin <- factor(if_else(VariableChurn_ini==1 & VariableChurn_aux==0, 1, 
                                    if_else(VariableChurn_ini==1 & VariableChurn_aux==1, 2, 
                                            if_else(VariableChurn_ini==0 & VariableChurn_aux==0, 0, 4))))
# table(VariableChurn_fin)
#
baseVariables_15dias <- baseVariables_15dias %>% dplyr::select(-CHURN, -DESERTOR) %>% dplyr::mutate(CHURN=VariableChurn_fin) %>% dplyr::filter(CHURN==0 | CHURN==1)
baseVariables_15dias$CHURN <- factor(baseVariables_15dias$CHURN)
# # table(dataTotal$CHURN)
# VariableChurn <- factor(dataTotal[["CHURN"]])
# dataTotal <- dataTotal %>% dplyr::select(-CHURN)











# dataTotal <- readRDS("C:/Users/vinuezab/Desktop/Paquetes/++ Compartido ++/BaseDesercionPRE_2018-08-31.rds")
# View(head(dataTotal, 20))

# VARIABLES CONSTANTES O TODAS DIFERENTES O TIPO DATE ---------------------
# indCteDifDate <- as.numeric()
# for(i in seq_len(NCOL(dataTotal))){
#   if((NROW(unique(dataTotal[,i]))==NROW(dataTotal)) | (NROW(unique(dataTotal[,i]))==1) | (class(dataTotal[,i])=="Date")) {
#     indCteDifDate<-c(indCteDifDate,i)
#   }
# }
# # paste("Variables que salen:" names(dataTotal)[indCteDifDate])   -*******************************
# dataTotal <- dataTotal[,-indCteDifDate]

# VARIABLES CON 2 VALORES UNICOS SE TRANFORMA A FACTOR --------------------
 for(i in 1:NCOL(dataTotal)){ 
  if((length(unique(dataTotal[,i]))==2) & (class(dataTotal[,i]) %in% c("character", "factor", "integer", "numeric"))) { 
     dataTotal[,i] <- as.factor(dataTotal[,i])
   }
 }

# VARIABLES CARACTERES A FACTOR -------------------------------------------
# dataTotal <- dataTotal %>% dplyr::mutate_if(is.character, as.factor)
# # glimpse(dataTotal)
# dataTotal <- dataTotal %>% dplyr::select(CHURN, everything())
# dim(dataTotal)
getwd()
setwd("C:/Users/Diana/Documents/trabajo")
library(summarytools)
view(dfSummary(baseVariables_15dias), file = "./Estad?sticaBaseTotal.html")


dim(baseVariables_15dias)
table(unlist(lapply(baseVariables_15dias, class)))
sum(is.na(baseVariables_15dias))/prod(dim(baseVariables_15dias))
#'**********************************************************************
#'
#'    ANALISIS EXPLORATORIO --------------------------------------------
#'
#'**********************************************************************
#'paquetes
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
dataTotal<-baseVariables_15dias
#separacion de las variables numericas y de los factores
var_numeric <- which(unlist(lapply(dataTotal, class)) %in% c("numeric", "integer", "integer64"))
var_factor  <- which(unlist(lapply(dataTotal, class)) %in% c("character", "factor"))
# var_date    <- which(unlist(lapply(dataTotal, class)) %in% "Date")

dim(dataTotal)[1] # registros.
dim(dataTotal)[2] # variables incluyendo la variable desercion (CHURN).
# table(unlist(lapply(dataTotal, class)))
# glimpse(dataTotal)
# view(dfSummary(dataTotal[, var_numeric, with=FALSE]), file = "./Estad?sticaBaseTotalNumericas.html")
# view(dfSummary(dataTotal[, var_factor, with=FALSE]), file = "./Estad?sticaBaseTotalFactores.html")

dataTotal <- as.data.table(dataTotal)

# PROPORCION DE VALORES PERDIDOS
# naTotal   <- colMeans(sapply(dataTotal, is.na)); naTotal <- naTotal[naTotal>0]


# naNumeric <- colMeans(sapply(dataTotal[,.SD, .SDcols = var_numeric], is.na)); naNumeric <- naNumeric[naNumeric>0]
# naFactor  <- colMeans(sapply(dataTotal[,.SD, .SDcols = var_factor], is.na)); naFactor <- naFactor[naFactor>0]
# naDate    <- colMeans(sapply(dataTotal[,.SD, .SDcols = var_date], is.na)); naDate <- naDate[naDate>0]



# VISUALIZAR GRAFICAMENTE LOS VALORES PERDIDOS
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  # data_temp <- data_temp[(ncol(data_temp)-150):(ncol(data_temp)-50),]
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + 
    scale_fill_manual(values=c("white", "gray"), name="Faltante\n(0=Si, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}  
#esta primera linea dice es una muestr
plot_Missing(dataTotal[, sample(names(colSums(is.na(dataTotal))[var_numeric]>0), 50), with = FALSE])
plot_Missing(dataTotal[, names(colSums(is.na(dataTotal))[var_factor]>0), with = FALSE])



# RESUMEN ESTADISTICO DE LAS VARIABLES NUMERICAS --------------------------
# data.frame(ID=1:ncol(dataTotal), NOMBRES=names(dataTotal))
# idnum <- names(dataTotal) %in% c("EDAD", "CARGAFAMILIAR", "MADUREZPROM", "SCORESOBREENDEUDAMIENTO", 
#                                                               "INGRESOESTIMADO","SALDO_MICRO", "OPE_U12M_MICRO", "ATRASOMAXU12M")
# summary(dataTotal[,.SD, .SDcols = var_numeric])
summary(dataTotal[,.SD, .SDcols = var_numeric])

# idfac <- names(dataTotal) %in% c("CHURN", "LUGARNACIMIENTOPROVINCIA", "LUGARDOMICILIOPROVINCIA", "INSTRUCCION",
#                                                              "ESTADOCIVIL", "GENERO", "FUENTEINGRESO", "SEGMENTOTDC")
# summary(dataTotal[,.SD, .SDcols = var_factor])
summary(dataTotal[,.SD, .SDcols = var_factor])

# summary(dataTotal[,.SD, .SDcols = var_date])

# PORCENTAJE DE VALORES PERDIDOS EN LA BASE -------------------------------
sum(is.na(dataTotal)) / (nrow(dataTotal) *ncol(dataTotal))



# ANALISIS GRAFICO PARA LAS VARIABLES NUMERICAS Y CATEGORICAS -------------
dataTotal_numeric <- dataTotal[,.SD, .SDcols = var_numeric] #  idnum
dataTotal_factor  <- dataTotal[,.SD, .SDcols = var_factor] #  idfac
# dataTotal_date  <- dataTotal[,.SD,.SDcols = var_date]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]]) #, SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1, alpha = 1.0) +
    # xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light()
    xlab(colnames(data_in)[i]) + theme_light() 
  return(p)
}

# GRAFICO DE BARRAS PARA VARIABLES FACTORES
library(gridExtra)
doPlots(dataTotal_factor, fun = plotHist, ii = 1:6, ncol = 3)
# doPlots(dataTotal_factor, fun = plotHist, ii = 7:12, ncol = 3)
# doPlots(dataTotal_factor, fun = plotHist, ii = 13:18, ncol = 3)
# doPlots(dataTotal_factor, fun = plotHist, ii = 19:24, ncol = 3)

# GRAFICO DE DENSIDADES PARA VARIABLES NUMERICAS
doPlots(dataTotal_numeric, fun = plotDen, ii = 1:8, ncol = 4)
# doPlots(dataTotal_numeric, fun = plotDen, ii = 7:12, ncol = 3)
# doPlots(dataTotal_numeric, fun = plotDen, ii = 13:18, ncol = 3)
# doPlots(dataTotal_numeric, fun = plotDen, ii = 19:24, ncol = 3)

# GRAFICO DE CAJA Y BIGOTE PARA VARIBLES NUMERICAS
suppressMessages(library(reshape2))
# dimension <- 1:12
reshape2::melt(dataTotal[,var_numeric, with=FALSE]) %>%
  ggplot(data=., aes(factor(variable), value)) +
  labs(x = "", title = "Diagramas de caja y bigote para varibles num?ricas") +
  geom_boxplot(aes(fill = factor(variable))) + facet_wrap(~variable,scales = "free", ncol=4) +
  theme(axis.text.x = element_blank()) + 
  guides(fill=FALSE) +
  theme_light()




# GRAFICO DE CORRELACION DE LAS VARIABLES NUMERICAS -----------------------
dataTotal_numeric_cor <- na.omit(dataTotal_numeric[, with = FALSE])
# indcor_num <- integer()
# for(i in seq_len(NCOL(dataTotal_numeric_cor))){
#   if((NROW(unique(dataTotal_numeric_cor[[i]]))==NROW(dataTotal_numeric_cor)) | (NROW(unique(dataTotal_numeric_cor[[i]]))==1) | (class(dataTotal_numeric_cor[[i]])=="Date")) {
#     indcor_num<-c(indcor_num,i)
#   }
# }
# dataTotal_numeric_cor <- dataTotal_numeric_cor[,-indcor_num, with=FALSE]
correlations_num <- cor(dataTotal_numeric_cor)
library(corrplot)
corrplot(correlations_num, method="square", tl.cex = 0.4)
#' VARIABLES NUMERICAS CON ALTA CORRELACION 
library(caret)
ind_cor_num <- findCorrelation(correlations_num, cutoff=0.50)
diag(correlations_num) <- 999
ind_cor_num_name <- unique(row.names(which(abs(correlations_num) > 0.50 & correlations_num != 999, arr.ind=TRUE)))
ind_cor_num <- which(names(dataTotal_numeric_cor) %in% ind_cor_num_name)
names(dataTotal_numeric_cor)[ind_cor_num]
corrplot(cor(dataTotal_numeric_cor[, ind_cor_num, with=FALSE]), method="square", tl.cex = 0.4)
# APLICACION DEL PCA ------------------------------------------------------
hclust_cor <- hclust(dist(method = abscorrelation)(cor(dataTotal_numeric_cor[,ind_cor_num, with=FALSE])), method = "ward.D2")
plot(hclust_cor, hang = -1, cex = 0.6, main="Dendograma de las variables num?ricas m?s correlacionadas")
corte_cor <- cutree(hclust_cor, k = 4)
rect.hclust(hclust_cor , k = 5, border = 2:7)

library(clValid);  dunn(dist(cor(dataTotal_numeric_cor[, ind_cor_num, with=FALSE])), corte_cor)
list_var2acp <- split(names(dataTotal_numeric_cor[,ind_cor_num,  with=FALSE]), corte_cor)
# corrplot(cor(dataMod_A_num[,ind_cor]), method="square", tl.cex = 0.5)
library(factoextra)
library(FactoMineR)
VariablesImportACP <- as.character(unlist(lapply(list_var2acp, function(x){
  if(length(x)!=1){
    res.pca <- PCA(dataTotal_numeric_cor[, x, with=FALSE], graph = FALSE)
    var <- get_pca_var(res.pca)
    VarImportACP <- first(names(sort(var$coord[,1], decreasing=TRUE)))
  }else{
    VarImportACP <- x[[1]]
  }
  return(VarImportACP)
})))
library(scales)
id_segmento <- 3   # variar este parametro
corrplot(cor(dataTotal_numeric_cor[,list_var2acp[[id_segmento]], with=FALSE]), method="square", tl.cex = 0.6)
# ANALISIS DE COMPONENTES PRINCIPALES EN CADA SEGMENTO DE ALTA CORRELACION
res.pca <- PCA(dataTotal_numeric_cor[,list_var2acp[[id_segmento]], with=FALSE], graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE)
#cargar el paquete de los colores  354
fviz_pca_var(res.pca, col.var="contrib",
             #            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             geom = c("arrow", "text", "point"))
fviz_contrib(res.pca, choice = "var", axes = 1, top = 20)











# GRAFICO DE CORRELACION DE LAS VARIABLES FACTORES -----------------------
dataTotal_factor_cor <- na.omit(dataTotal_factor[, with = FALSE]) #_%>% dplyr::select(-DESERTOR, -CHURN)
# indcor_fac <- integer()
# for(i in seq_len(NCOL(dataTotal_factor_cor))){
#   if((NROW(unique(dataTotal_factor_cor[[i]]))==NROW(dataTotal_factor_cor)) | (NROW(unique(dataTotal_factor_cor[[i]]))==1) | (class(dataTotal_factor_cor[[i]])=="Date")) {
#     indcor_fac<-c(indcor_fac,i)
#   }
# }
#dataTotal_factor_cor <- dataTotal_factor_cor[,-indcor_fac, with=FALSE]
correlations_fac <- cor2(dataTotal_factor_cor)
#grafico de asociacion
corrplot(correlations_fac, method="square", tl.cex = 0.4)
# VARIABLES FACTORES CON ALTA CORRELACION (ASOCIACION)
# ind_cor_fac <- findCorrelation(correlations_fac, cutoff=0.80)
diag(correlations_fac) <- 999
ind_cor_fac_name <- unique(row.names(which(abs(correlations_fac) > 0.20 & correlations_fac != 999, arr.ind=TRUE)))
ind_cor_fac <- which(names(dataTotal_factor_cor) %in% ind_cor_fac_name)
names(dataTotal_factor_cor)[ind_cor_fac]
corrplot(cor2(dataTotal_factor_cor[, ind_cor_fac, with=FALSE]), method="square", tl.cex = 0.6)
# APLICACION DEL corres ------------------------------------------------------
hclust_cor <- hclust(dist(cor2(dataTotal_factor_cor[,ind_cor_fac, with=FALSE])), method = "ward.D2")
plot(hclust_cor, hang = -1, cex = 0.6, main="Dendograma de las variables factores m?s asociadas")
corte_cor <- cutree(hclust_cor, k = 5)
rect.hclust(hclust_cor , k = 5, border = 2:7)
library(clValid);  dunn(dist(cor2(dataTotal_factor_cor[, ind_cor_fac, with=FALSE])), corte_cor)
list_var2mca <- split(names(dataTotal_factor_cor[,ind_cor_fac,  with=FALSE]), corte_cor)
# corrplot(cor(dataMod_A_num[,ind_cor]), method="square", tl.cex = 0.5)
VariablesImportMCA <- as.character(unlist(lapply(list_var2mca, function(x){
  if(length(x)!=1){
    res.pca <- MCA(dataTotal_factor_cor[, x, with=FALSE], graph = FALSE)
    var <- get_mca_var(res.pca)
    VarImportMCA <- first(names(sort(var$coord[,1], decreasing=TRUE)))
  }else{
    VarImportMCA <- x[[1]]
  }
  return(VarImportMCA)
})))

id_segmento <- 1   # variar este parametro
corrplot(cor2(dataTotal_factor_cor[,list_var2mca[[id_segmento]], with=FALSE]), method="square", tl.cex = 0.6)
# ANALISIS DE COMPONENTES PRINCIPALES EN CADA SEGMENTO DE ALTA CORRELACION
res.mca <- MCA(dataTotal_factor_cor[,list_var2mca[[id_segmento]], with=FALSE], graph = FALSE)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(res.mca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             geom = c("arrow", "text", "point"))
fviz_contrib(res.mca, choice = "var", axes = 1, top = 20)






# ANALISIS DE COMPONENTES PRINCIPALES -------------------------------------
# install.packages(c("FactoMineR", "factoextra"), dependencies = TRUE)
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))

res.pca <- PCA(dataTotal_numeric, graph = FALSE)
get_eig(res.pca)
fviz_screeplot(res.pca, addlabels = TRUE)
var <- get_pca_var(res.pca)
var
head(var$coord)
head(var$cor)
head(var$cos2)
head(var$contrib)

res.desc.pca <- dimdesc(res.pca, axes = c(1,2))
res.desc.pca$Dim.1$quanti[1:15,]
res.desc.pca$Dim.2$quanti[1:15,]
res.desc.pca <- dimdesc(res.pca, axes = 1:3, proba = 0.05)
res.desc.pca$Dim.1$quanti[1:15,]
res.desc.pca$Dim.2$quanti[1:15,]
res.desc.pca$Dim.3$quanti[1:15,]

dim(res.desc.pca$Dim.1$quanti)
dim(res.desc.pca$Dim.2$quanti)
dim(res.desc.pca$Dim.3$quanti)

fviz_pca_var(res.pca, col.var="contrib",
             #             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
             ,geom = c("arrow")
             # ,ggtheme = theme_minimal()
)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)







# dataTotal_factor <- dataTotal_factor %>% dplyr::select(-DESERTOR, -CHURN)
dataTotal_factor <- dataTotal_factor %>% dplyr::select(-CHURN)
# ANALSIS DE CORRESPONDENCIAS PARA LAS VARIABLES FACTORES -----------------
res.mca <- MCA(dataTotal_factor, graph = FALSE)
get_eig(res.mca)
fviz_screeplot(res.mca, addlabels=TRUE)
var <- get_mca_var(res.mca)
var
head(var$coord)
head(var$cos2)
head(var$contrib)

res.desc.mca <- dimdesc(res.mca, axes = c(1,2))
res.desc.mca$Dim.1$quanti[1:15,]
res.desc.mca$Dim.2$quanti[1:15,]
res.desc.mca <- dimdesc(res.mca, axes = 1:3, proba = 0.05)
res.desc.mca$Dim.1$quanti[1:15,]
res.desc.mca$Dim.2$quanti[1:15,]
res.desc.mca$Dim.3$quanti[1:15,]

dim(res.desc.mca$Dim.1$quanti)
dim(res.desc.mca$Dim.2$quanti)
dim(res.desc.mca$Dim.3$quanti)

fviz_mca_var(res.mca, col.var="contrib",
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
             ,geom = c("arrow", "text")
             # ,ggtheme = theme_minimal()
)

# Contributions of variables to PC1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 10)
