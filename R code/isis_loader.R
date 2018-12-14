library(devtools)
library(plyr)
#install_github('dirkmjk/read.survey')


getData.ISIS <- function(path){
  data <- read.survey::read.surveymonkey(path, convert = TRUE)
  
  isis.frec <- data.frame(Aburrimiento = data$Â.Con.quÃ..frecuencia.ha.experimentado.las.siguientes.emociones.durante.su.proceso.de.aprendizaje., 
                          Alivio = data$c..Alivio....Algunas.veces....Muchas.veces....Muchas.veces..., 
                          Ansiedad = data$c..Ansidedad....Casi.siempre....Casi.siempre....Muchas.veces..., 
                          Confusion = data$c..ConfusiÃ³n....Muchas.veces....Muchas.veces....Algunas.veces..., 
                          Curiosidad = data$c..Curiosidad....Algunas.veces....Casi.siempre....Casi.siempre..., 
                          Decepcion = data$c..DecepciÃ³n....Muchas.veces....Algunas.veces....Algunas.veces..., 
                          Disfrute = data$c..Disfrute....Muchas.veces....Muchas.veces....Muchas.veces..., 
                          Frustracion = data$c..FrustraciÃ³n....Casi.siempre....Muchas.veces....Muchas.veces..., 
                          Satisfaccion = data$c..SatisfacciÃ³n....Algunas.veces....Siempre....Muchas.veces..., 
                          Temor = data$c..Temor....Casi.siempre....Casi.nunca....Casi.nunca....Algunas.veces...)
  
  isis.frec <- lapply(isis.frec, frecuen.emot)
  
  isis.emotive <- data.frame(EMOT.EMOTIVE = data$Recuerde.la.situaciÃ³n.mÃ.s.emotiva.que.haya.vivido.este.semestre..Â..Seleccione.la.emociÃ³n.mÃ.s.relacionada.Â.con.esaÂ.situaciÃ³n.y.describa.la.situaciÃ³n.,
                             SIT.EMOTIVE = data$ c..SituaciÃ³n....Al.cancelar.dos.materias..porque.me.sentÃ..decepcionado.de.mi.mismo...)
  
  isis.emotive$EMOT.EMOTIVE <- emots.isis(isis.emotive$EMOT.EMOTIVE)
  isis.emotive$SIT.EMOTIVE <- as.character(isis.emotive$SIT.EMOTIVE)
  
  isis.difficult <- data.frame(EMOT.DIFFICULT = data$Recuerde.la.situaciÃ³n.mÃ.s.difÃ.cil.que.haya.vivido.durante.toda.su.carrera..Seleccione.la.emociÃ³n.mÃ.s.relacionada.con.esaÂ.situaciÃ³n.y.describa.la.situaciÃ³n.,
                              SIT.DIFFICULT = data$c..SituaciÃ³n....A.perder.las.materias.que.estoy.cursando.ya.que.perderÃ.a.el.tiempo.y.plata.....)
  
  isis.difficult$EMOT.DIFFICULT <- emots.isis(isis.difficult$EMOT.DIFFICULT)
  isis.difficult$SIT.DIFFICULT <- as.character(isis.difficult$SIT.DIFFICULT)
  
  isis.imagine <- data.frame(EMOT.IMAGINE = data$Imagine.una.situaciÃ³n.que.quisiera.vivir.en.su.Ãºltimo.semestre..Seleccione.la.emociÃ³n.mÃ.s.relacionada.con.esa.situaciÃ³n.y.describa.la.situaciÃ³n.,
                             SIT.IMAGINE = data$c..SituaciÃ³n....Al.pasar.todas.las.materias..subir.el.promedio.y.avanzar.en.mi.carrera...)
  
  isis.imagine$EMOT.IMAGINE <- emots.isis(isis.imagine$EMOT.IMAGINE)
  isis.imagine$SIT.IMAGINE <- as.character(isis.imagine$SIT.IMAGINE)
  
  isis.noChance <- data.frame(EMOT.NOCHANCE = data$Han.estado.trabajando.en.un.problema.durante.dÃ.as.sin.lograr.resolverlo..La.fecha.de.entrega.de.la.soluciÃ³n.es.maÃ.ana.a.primera.hora..El.grupo.decide.solicitar.al.profesor.unos.dÃ.as.mÃ.s.de.plazo..pero.Ã.l.no.lo.concede.Â.QuÃ..emociÃ³n.sentirÃ.a.en.ese.momento.,
                              SIT.NOCHANCE = data$c..Si.pudiera.decirle.abiertamente.Â.al.Â.profesor.lo.que.siente..Â.quÃ..le.dirÃ.a....)
  
  isis.noChance$EMOT.NOCHANCE <- emots.isis(isis.noChance$EMOT.NOCHANCE)
  isis.noChance$SIT.NOCHANCE <- as.character(isis.noChance$SIT.NOCHANCE)
  
  isis.solved <- data.frame(EMOT.SOLVED = data$EstÃ.n.en.la.recta.final.para.entregar.el.proyecto.del.semestre..Su.proyecto.genera.un.error.y.ustedes.no.han.encontrado.la.causa..Unas.horas.antes.de.la.entrega.un.compaÃ.ero.mira.su.proyecto.y.les.indica.el.problema..Ustedes.alcanzan.a.resolverlo.Â.QuÃ..emociÃ³n.sentirÃ.a.en.ese.momento.,
                            SIT.SOLVED = data$c..Si.pudiera.decirle.abiertamente.al.Â.compaÃ.ero.que.les.ayudÃ³.lo.que.siente..Â.quÃ..le.dirÃ.a....)
  
  isis.solved$EMOT.SOLVED <- emots.isis(isis.solved$EMOT.SOLVED)
  isis.solved$SIT.SOLVED <- as.character(isis.solved$SIT.SOLVED)
  
  isis.copy <- data.frame(EMOT.COPY = data$Su.profesor.les.comunica.que.ha.encontrado.evidencia.de.copia.en.algunos.de.los.trabajos.del.grupo.y.les.pide.a.todos.que.se.acerquen.a.su.oficina.para.aclarar.la.situaciÃ³n..Â.QuÃ..emociÃ³n.sentirÃ.a.en.ese.momento.,
                          SIT.COPY = data$c..Si.pudiera.decirle.abiertamente.Â.a.su.mejor.amigo.lo.que.siente..Â.quÃ..le.dirÃ.a....)
  
  isis.copy$EMOT.COPY <- emots.isis(isis.copy$EMOT.COPY)
  isis.copy$SIT.COPY <- as.character(isis.copy$SIT.COPY)
  
  isis.gender <- data.frame(Gender = data$Â.CuÃ.l.es.su.gÃ.nero.)
  
  isis.asign <- data.frame(PIMB = data$Â.CuÃ.l.de.estos.cursos.estÃ..cursando.actualmente.,
                            PIMO = data$c..PIMO................................................PIMO...,
                            MBDA = data$c..MBDA.......................................................,
                            POOB = data$c..POOB.......................................................,
                            PDSW = data$c..PDSW........................................PDSW....PDSW...,
                            ARSW = data$c..ARSW.......................................................,
                            COSW = data$c..COSW........................COSW...........................)
  
  levels(isis.gender$Gender)[levels(isis.gender$Gender)=="MÃ¡sculino"] <- "Masculino"
  levels(isis.gender$Gender) <- list("Masculino" = 1, "Femenino" = 2)

  isis.asign <- lapply(isis.asign, asign.isis)
  
  
  isis <- data.frame(isis.frec,isis.emotive,isis.difficult,isis.imagine,isis.noChance,isis.solved,isis.copy,isis.gender,isis.asign)
  
  return(isis)
}

frecuen.emot <- function(data){
  levels(data) <- list("Nunca" = 1, "Casi nunca" = 2, "Algunas veces" = 3, "Muchas veces" = 4, "Casi siempre" = 5, "Siempre" = 6)
  return(data)
}

emots.isis <- function(data){
  levels(data)[levels(data)=="Aburrimiento"] <- "Aburrimiento"
  levels(data)[levels(data)=="Alivio"] <- "Alivio"
  levels(data)[levels(data)=="Ansiedad"] <- "Ansiedad"
  levels(data)[levels(data)=="ConfusiÃ³n"] <- "Confusion"
  levels(data)[levels(data)=="Curiosidad"] <- "Curiosidad"
  levels(data)[levels(data)=="DecepciÃ³n"] <- "Decepcion"
  levels(data)[levels(data)=="Disfrute"] <- "Disfrute"
  levels(data)[levels(data)=="FrustraciÃ³n"] <- "Frustracion"
  levels(data)[levels(data)=="SatisfacciÃ³n"] <- "Satisfaccion"
  levels(data)[levels(data)=="Temor"] <- "Temor"
  
  levels(data) <- list("Aburrimiento" = 1, "Alivio" = 2, "Ansiedad" = 3, "Confusion" = 4, "Curiosidad" = 5, "Decepcion" = 6, "Disfrute" = 7, "Frustracion" = 8, "Satisfaccion" = 9, "Temor" = 10)
  return(data)
}

asign.isis <- function(data){
  levels(data) <- c("No","Si")
  levels(data) <- list("No" = 1, "Si" = 2)
  return(data)
}


# emots <- as.factor(c(isis$EMOT.EMOTIVE,isis$EMOT.DIFFICULT,isis$EMOT.IMAGINE,isis$EMOT.NOCHANCE,isis$EMOT.SOLVED,isis$EMOT.COPY))
# levels(emots) <- list("Aburrimiento" = "1", "Alivio" = "2", "Ansiedad" = "3", "Confusion" = "4", "Curiosidad" = "5", "Decepcion" = "6", "Disfrute" = "7", "Frustracion" = "8", "Satisfaccion" = "9", "Temor" = "10")
# sits <- c(isis$SIT.EMOTIVE,isis$SIT.DIFFICULT,isis$SIT.IMAGINE,isis$SIT.NOCHANCE,isis$SIT.SOLVED,isis$SIT.COPY)
# isis.comp <- data.frame(SIT = sits, EMOT = emots)
# isis.comp <- isis.comp[!is.na(isis.comp$EMOT),]
# explore.data(isis.comp$SIT,isis.comp$EMOT)