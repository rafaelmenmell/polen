library(tm)
library(dplyr)
library(raster)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(verification)
library(lubridate)
library(meteologica)
library(pdftools)
library(tabulizer)
library(stationaRy)

#http://www.foolabs.com/xpdf/download.html

readPDFobservaciones <- function(tipo="CUPR",fecha){ 
  if(file.exists(sprintf("attach/%s/%s.pdf",fecha,tipo))){
  pdf <- readPDF(control=list(text="-table"))
  pdftxt <- pdf(elem = list(uri=sprintf("attach/%s/%s.pdf",fecha,tipo)),language = "Spanish")
  pdftxt <- content(pdftxt)
  pdftxt <- strsplit(pdftxt,"\n")[[1]]
  puntos <- c("Alcalá","Aranjuez","Collado","Coslada","Getafe","Las Rozas","Salamanca","Universitaria","Media")
  cabecera <- pdftxt[15]
  cut <- sapply(puntos, FUN=function(x) {gregexpr(x,cabecera)[[1]][1]})
  #quienes tienen y quienes no
  puntos.no <- names(which(cut==-1))
  puntos.si <- names(which(cut!=-1))
  cut <- c(98,cut[cut!=-1])-1
  posiciones <- diff(cut)
  
  #ln <- c(27,29,31,33,35,37,39)
  ln <- c(16,17,18,19,20,21,22)
  datos <- pdftxt[ln]
  datos <- gsub("^\\s","",datos)
  datos.for <- list()
  for (i in 1:length(datos)){
    datos.for[[i]] <- substring(datos[[i]],cut,cut+8)
    datos.for[[i]] <- gsub(" ","",datos.for[[i]])
    datos.for[[i]][datos.for[[i]]==""] <- NA
    datos.for[[i]] <- as.data.frame(t(datos.for[[i]]))
  }
  datos <- bind_rows(datos.for)
  colnames(datos) <- c("fechas",puntos.si)
  if (length(puntos.no!=0)){
    for (p in puntos.no){
      datos <- cbind(datos,rep("NA",7))
      colnames(datos) <- c(colnames(datos)[-ncol(datos)],p)
    }
  }
  datos <- datos[!is.na(datos$fechas),]
  datos$fechas <- as.Date(datos$fechas,format="%d/%m/%y")
  
  return(datos)
  } else {
    print("Ese archivo no existe")
  }
}

readPDFobservaciones_tabulizer <- function(tipo="CUPR",fecha){ 
  if(file.exists(sprintf("attach/%s/%s.pdf",fecha,tipo))){
    pdffile <- sprintf("attach/%s/%s.pdf",fecha,tipo)
    table <- extract_tables(file = pdffile)[[1]]
    coltables <- table[1,]
    #si hay columna media la quito porque da problemas y la puedo calcular a posterior
    if ("media" %in% tolower(coltables)){
      table <- table[,-which(tolower(coltables)=="media")]
    } else {
      #si no hay es la ultima columna
      table <- table[,-ncol(table)]
    }
    coltables <- c("fechas",table[1,])
    #si hay alguna columna con todo vacio la quitamos
    columnas <- which(apply(table,2,FUN=function(x) sum(x!=""))==0)
    if (length(columnas)!=0){
      table <- table[,-columnas]
    }
    filas <- which(apply(table,1,FUN=function(x) sum(x!=""))==0)
    if (length(filas)!=0){
      table <- table[-filas,]
    }
    coltables <- coltables[coltables!=""]
    table <- as.data.frame(table[-1,])
    colnames(table) <- coltables
    
    #un poco de tuneo de los nombre de las columnas
    cols <- colnames(table)
    cols <- gsub(pattern = "Media",replacement = "",cols)
    cols <- gsub(pattern = 'Madrid-.* ',"",cols)
    cols[cols=="Madrid-Arganzuela"] <- "Arganzuela"
    cols[cols=="Barrio"] <- "Salamanca"
    cols[cols=="Ciudad"] <- "Universitaria"
    colnames(table) <- cols
    return(table)
  } else {
    print("Ese archivo no existe")
  }
}


SerieObservaciones <- function(tipo="CUPR"){
  lf <- list.dirs("attach/",full.names = FALSE)
  obs <- list()
  n <- 1
  for (l in lf){
    print(l)
    #o <- readPDFobservaciones(fecha=l)
    o <- readPDFobservaciones_tabulizer(fecha=l)
    if (is.data.frame(o)){
      obs[[n]] <- o
      n <- n+1
    }
  }
  fechas <- unique(bind_rows(obs)$fechas)
  fechas <- fechas[!is.na(fechas)]
  cols <- colnames(obs[[which(sapply(obs, ncol)==max(sapply(obs, ncol)))[1]]])
  obs.def <- as.data.frame(matrix(NA,nrow=length(fechas),ncol=length(cols)))
  colnames(obs.def) <- cols
  obs.def$fechas <- fechas
  #relleno el pavo
  for (i in 1:length(obs)){
    print(i)
    for (r in 1:nrow(obs[[i]])){
      o <- obs[[i]][r,]
      if(!is.na(o$fecha)){
      #print(o$fecha)
      columnas.quetengo <- colnames(o)[colnames(o) %in% cols]
      columnas.quetengo <- columnas.quetengo[columnas.quetengo!="fechas"]
      obs.def[obs.def$fechas==o$fecha,columnas.quetengo] <- as.integer(o[,columnas.quetengo])
      }
    }
  }
  obs.def$media <- rowMeans(obs.def[,-1],na.rm = TRUE)
  return(obs.def)
}

readPrediccionesPDF <- function(tipo="CUPR",fecha){
  if (tipo=="CUPR") {
    texto <- "Cupre"
    texto2 <- "cup"
    }
  lf <- list.files(path=sprintf("attach/%s",fecha))
  file <- lf[grep(texto,lf)]
  file <- c(file,lf[grep(tipo,lf)])
  file <- c(file,lf[grep(texto2,lf)])
  file <- file[file!="CUPR.pdf"]
  if (length(file)>0){
  file <- file[which(nchar(file)==max(nchar(file)))]
  command <- sprintf('pdfimages -j attach//%s//"%s" image',fecha,file)
  system(command)
  colores <- list()
  valores <- character()
  lf.jpg <- list.files(path=".",pattern = ".jpg")
  lf.jpg <- lf.jpg[!grepl("0000.jpg",lf.jpg)]
  lf.jpg <- lf.jpg[!grepl("000.jpg",lf.jpg)]
  for (n in 1:4){
    colores[[n]] <- raster::extract(stack(lf.jpg[n]),cbind(0,0))
    if (sum(colores[[n]]==c(153,1,52))==3) {valores[n] <- "MA"}
    if (sum(colores[[n]]==c(254,153,0))==3) {valores[n] <- "A"}
    if (sum(colores[[n]]==c(0,129,2))==3) {valores[n] <- "M"}
    if (sum(colores[[n]]==c(0,128,1))==3) {valores[n] <- "M"}
    if (sum(colores[[n]]==c(48,0,100))==3) {valores[n] <- "B"}
    if (sum(colores[[n]]==c(255,193,214))==3) {valores[n] <- "MA"}
    if (sum(colores[[n]]==c(255,197,11))==3) {valores[n] <- "A"}
    if (sum(colores[[n]]==c(178,215,110))==3) {valores[n] <- "M"}
    if (sum(colores[[n]]==c(50,2,102))==3) {valores[n] <- "B"}
    if (sum(colores[[n]]==c(51,0,101))==3) {valores[n] <- "B"}
    if (sum(colores[[n]]==c(236,217,245))==3) {valores[n] <- "B"}
    if (sum(colores[[n]]==c(237,217,245))==3) {valores[n] <- "B"}
    if (sum(colores[[n]]==c(62,79,105))==3) {valores[n] <- "B"}
    if (sum(colores[[n]]==c(61,164,75))==3) {valores[n] <- "M"}
  }
  unlink(lf.jpg)
  return(valores)
  } else {
    return(NA)
  }
}

SeriePredicciones <- function(tipo="CUPR"){
  lf <- list.dirs("attach/",full.names = FALSE)[-1]
  preds <- list()
  n <- 1
  for (l in lf){
    print(l)
    o <- readPrediccionesPDF(fecha=l)
    print(o)
    if (is.character(o)){
      preds[[n]] <- as.data.frame(t(c(l,o)))
      n <- n+1
    }
  }
  preds <- bind_rows(preds)
  colnames(preds) <- c("fecha","D-1","D0","D1","D2")
  preds$fecha <- as.Date(preds$fecha,format="%Y%m%d")
  return(preds)
}

CreaSerieMedia <- function(tipo="CUPR"){
  obs <- SerieObservaciones()
  obs <- obs %>% dplyr::filter(obs$fechas!="")
  obs <- obs[,c("fechas","media")]
  obs$fechas <- as.Date(obs$fechas,forma="%d/%M/%y")
  obs <- obs %>% dplyr::group_by(dia=yday(fechas)) %>% dplyr::summarise(clima=mean(media,na.rm=TRUE))
  obs$cat <- cut(obs$clima,breaks=c(0,111,201,479,5000),labels=c("B","M","A","MA"))
  obs$cat <- as.character(obs$cat)
  return(obs)
}


AnalizaPredicciones <- function(preds=preds,obs=obs){
  fechas <- as.data.frame(seq(first(preds$fecha),last(preds$fecha),by=1))
  colnames(fechas) <- c("fecha")
  preds.comp <- left_join(fechas,preds)
  preds.comp$obs <- c(preds.comp[-1,]$`D-1`,NA)
  #rellenamos con un bucle poco elegante
  preds.comp$media <- NA
  for (i in 1:nrow(preds.comp)){
    print(i)
    if(length(obs[obs$dia==yday(preds.comp[i,]$fecha),]$cat)!=0){
    preds.comp[i,]$media <- obs[obs$dia==yday(preds.comp[i,]$fecha),]$cat
    }
  }
  preds.comp <- preds.comp[complete.cases(preds.comp),]
  preds.res.pers <- preds.comp %>% dplyr::group_by(obs,`D-1`) %>% dplyr::summarise(count=n())
  
  preds.res.pers$obs <- factor(preds.res.pers[,]$obs,levels=c("B","M","A","MA"))
  preds.res.pers$`D-1` <- factor(preds.res.pers[,]$`D-1`,levels=c("B","M","A","MA"))
  
  preds.res.D0 <- preds.comp %>% dplyr::group_by(obs,D0) %>% dplyr::summarise(count=n())
  
  preds.res.D0$obs <- factor(preds.res.D0[,]$obs,levels=c("B","M","A","MA"))
  preds.res.D0$D0 <- factor(preds.res.D0[,]$D0,levels=c("B","M","A","MA"))
  
  preds.res.media <- preds.comp %>% dplyr::group_by(obs,media) %>% dplyr::summarise(count=n())
  
  preds.res.media$obs <- factor(preds.res.media[,]$obs,levels=c("B","M","A","MA"))
  preds.res.media$media <- factor(preds.res.media[,]$media,levels=c("B","M","A","MA"))
  
  g.pers <- ggplot(data=preds.res.pers)+geom_tile(aes(y=`D-1`,x=obs,fill=count))+geom_text(aes(y=`D-1`,x=obs,label=count))+theme_fivethirtyeight()+theme(legend.position="none")+labs(title="Predicción por persistencia vs observaciones")+scale_fill_gradient_tableau()
  g.D0 <- ggplot(data=preds.res.D0)+geom_tile(aes(y=D0,x=obs,fill=count))+geom_text(aes(y=D0,x=obs,label=count))+theme_fivethirtyeight()+theme_fivethirtyeight()+theme(legend.position="none")+labs(title="Predicción D0 vs observaciones")+scale_fill_gradient_tableau()
  g.media <- ggplot(data=preds.res.media)+geom_tile(aes(y=media,x=obs,fill=count))+geom_text(aes(y=media,x=obs,label=count))+theme_fivethirtyeight()+theme_fivethirtyeight()+theme(legend.position="none")+labs(title="Predicción media vs observaciones")+scale_fill_gradient_tableau()
  plot(g.pers)
  plot(g.D0)
  plot(g.media)
  #lo convierto en matrices para multi.cont
  pers <- matrix(NA,4,4)
  d0 <- matrix(NA,4,4)
  media <- matrix(NA,4,4)
  cats <- c("B","M","A","MA")
  for (i in 1:4){
    for (j in 1:4){
      if (length(preds.res.pers[(preds.res.pers$`D-1`==cats[i]) & (preds.res.pers$obs==cats[j]),]$count)!=0){
        pers[i,j] <- preds.res.pers[(preds.res.pers$`D-1`==cats[i]) & (preds.res.pers$obs==cats[j]),]$count
      } else {
        pers[i,j] <- 0
      }
      if (length(preds.res.D0[(preds.res.D0$D0==cats[i]) & (preds.res.D0$obs==cats[j]),]$count)!=0){
        d0[i,j] <- preds.res.D0[(preds.res.D0$D0==cats[i]) & (preds.res.D0$obs==cats[j]),]$count
      } else {
        d0[i,j] <- 0
      }
      if (length(preds.res.media[(preds.res.media$media==cats[i]) & (preds.res.media$obs==cats[j]),]$count)!=0){
        media[i,j] <- preds.res.media[(preds.res.media$media==cats[i]) & (preds.res.media$obs==cats[j]),]$count
      } else {
        media[i,j] <- 0
      }
    }
  } 
  print("GS para persistecia")
  print(multi.cont(pers)$gs)
  print("GS para D0")
  print(multi.cont(d0)$gs)
  print("GS para media")
  print(multi.cont(media)$gs)
}

obs <- SerieObservaciones()
#quitamos una rareza
obs <- obs %>% dplyr::filter(obs$fechas!="")
obs$fecha2 <- as.Date(obs$fechas,format = "%d/%m/%y")
obs$year <- year(obs$fecha2)
obs$fecha2 <- as.Date(obs$fechas,format = "%d/%m")
obs$yday <- yday(obs$fecha2)
obs2 <- CreaSerieMedia()
obs <- inner_join(obs,obs2,by=c("yday"="dia"))
#graficos bonitos
gb1 <- ggplot(obs) + geom_step(aes(x=fecha2,y=media)) + geom_line(aes(x=fecha2,y=clima),color="grey",size=0.5) + geom_hline(yintercept = 111,color="green") + geom_hline(yintercept = 202,color="orange") + geom_hline(yintercept = 479,color="red") + facet_grid(year~.) + labs(title="Niveles medios de pólenes de cupresácesas/taxáceas en la Comunidad de Madrid 2014-2018",x="fecha",y="granos por m³",caption="Fuente: boletines de RED Palinocam") + theme_bw()
preds <- SeriePredicciones()
obs2 <- obs2 %>% dplyr::filter(!is.na(dia))
AnalizaPredicciones(obs = obs2,preds = preds)

#cual es la serie de observaciones mas completa
colSums(apply(obs, 2, is.na))
#las series mas completas con "Las Rozas" y "Universitaria"
obs.parciales <- obs %>% dplyr::select("fechas","Las Rozas","Universitaria")
obs.parciales$fechas <- as.Date(obs.parciales$fechas,format = "%d/%m/%y")
# plot(TimeLine(obs.parciales,"fechas"))
# son las dos muy parecidas así que cualquiera me vale
# vamos a descargar observaciones meteorológivas a ver si se puede hacer algo
# son los dato de colmenar, es la serie mmás larga que puede servir como proxy de CU y las Rozas
meteo.obs <- get_isd_station_data(station_id = "82190-99999",startyear = 2014,endyear = 2018)
#como tenemos valores dairio vamos a quedarnos con el valor a las 12
meteo.obs <- meteo.obs %>% dplyr::select(year,month,day,hour,minute,wd,ws,ceil_hgt,temp,dew_point,atmos_pres,rh) %>% dplyr::filter(hour==12 & minute==0)
meteo.obs <- meteo.obs %>% dplyr::select(year,month,day,wd,ws,ceil_hgt,temp,dew_point,atmos_pres,rh)
meteo.obs$fecha <- as.Date(ISOdate(year = meteo.obs$year,month = meteo.obs$month,day = meteo.obs$day))
meteo.obs <- meteo.obs %>% dplyr::select(fecha,wd,ws,temp,dew_point,rh)

obs.parciales.meteo <- left_join(obs.parciales,meteo.obs,by=c("fechas"="fecha"))
#que no haya NA
obs.parciales.meteo <- obs.parciales.meteo[complete.cases(obs.parciales.meteo),]
#veamos algunos graficos
ggplot(obs.parciales.meteo) + geom_point(aes(x=rh,y=Universitaria))
modelo1 <- lm(data = obs.parciales.meteo,formula=Universitaria ~ temp + rh)
obs.parciales.meteo$pred <- predict(modelo1)
ggplot(obs.parciales.meteo) + geom_point(aes(x=pred,y=Universitaria))
