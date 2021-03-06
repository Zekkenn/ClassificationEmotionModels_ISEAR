library(devtools)
library(plyr)
#install_github('dirkmjk/read.survey')


getData.ISIS <- function(path){
  data <- read.survey::read.surveymonkey(path, convert = TRUE)
  
  isis.frec <- data.frame(Aburrimiento = data$�.Con.qu�..frecuencia.ha.experimentado.las.siguientes.emociones.durante.su.proceso.de.aprendizaje., 
                          Alivio = data$c..Alivio....Algunas.veces....Muchas.veces....Muchas.veces..., 
                          Ansiedad = data$c..Ansidedad....Casi.siempre....Casi.siempre....Muchas.veces..., 
                          Confusion = data$c..Confusión....Muchas.veces....Muchas.veces....Algunas.veces..., 
                          Curiosidad = data$c..Curiosidad....Algunas.veces....Casi.siempre....Casi.siempre..., 
                          Decepcion = data$c..Decepción....Muchas.veces....Algunas.veces....Algunas.veces..., 
                          Disfrute = data$c..Disfrute....Muchas.veces....Muchas.veces....Muchas.veces..., 
                          Frustracion = data$c..Frustración....Casi.siempre....Muchas.veces....Muchas.veces..., 
                          Satisfaccion = data$c..Satisfacción....Algunas.veces....Siempre....Muchas.veces..., 
                          Temor = data$c..Temor....Casi.siempre....Casi.nunca....Casi.nunca....Algunas.veces...)
  
  isis.frec <- lapply(isis.frec, frecuen.emot)
  
  isis.emotive <- data.frame(EMOT.EMOTIVE = data$Recuerde.la.situación.m�.s.emotiva.que.haya.vivido.este.semestre..�..Seleccione.la.emoción.m�.s.relacionada.�.con.esa�.situación.y.describa.la.situación.,
                             SIT.EMOTIVE = data$ c..Situación....Al.cancelar.dos.materias..porque.me.sent�..decepcionado.de.mi.mismo...)
  
  isis.emotive$EMOT.EMOTIVE <- emots.isis(isis.emotive$EMOT.EMOTIVE)
  isis.emotive$SIT.EMOTIVE <- as.character(isis.emotive$SIT.EMOTIVE)
  
  isis.difficult <- data.frame(EMOT.DIFFICULT = data$Recuerde.la.situación.m�.s.dif�.cil.que.haya.vivido.durante.toda.su.carrera..Seleccione.la.emoción.m�.s.relacionada.con.esa�.situación.y.describa.la.situación.,
                              SIT.DIFFICULT = data$c..Situación....A.perder.las.materias.que.estoy.cursando.ya.que.perder�.a.el.tiempo.y.plata.....)
  
  isis.difficult$EMOT.DIFFICULT <- emots.isis(isis.difficult$EMOT.DIFFICULT)
  isis.difficult$SIT.DIFFICULT <- as.character(isis.difficult$SIT.DIFFICULT)
  
  isis.imagine <- data.frame(EMOT.IMAGINE = data$Imagine.una.situación.que.quisiera.vivir.en.su.último.semestre..Seleccione.la.emoción.m�.s.relacionada.con.esa.situación.y.describa.la.situación.,
                             SIT.IMAGINE = data$c..Situación....Al.pasar.todas.las.materias..subir.el.promedio.y.avanzar.en.mi.carrera...)
  
  isis.imagine$EMOT.IMAGINE <- emots.isis(isis.imagine$EMOT.IMAGINE)
  isis.imagine$SIT.IMAGINE <- as.character(isis.imagine$SIT.IMAGINE)
  
  isis.noChance <- data.frame(EMOT.NOCHANCE = data$Han.estado.trabajando.en.un.problema.durante.d�.as.sin.lograr.resolverlo..La.fecha.de.entrega.de.la.solución.es.ma�.ana.a.primera.hora..El.grupo.decide.solicitar.al.profesor.unos.d�.as.m�.s.de.plazo..pero.�.l.no.lo.concede.�.Qu�..emoción.sentir�.a.en.ese.momento.,
                              SIT.NOCHANCE = data$c..Si.pudiera.decirle.abiertamente.�.al.�.profesor.lo.que.siente..�.qu�..le.dir�.a....)
  
  isis.noChance$EMOT.NOCHANCE <- emots.isis(isis.noChance$EMOT.NOCHANCE)
  isis.noChance$SIT.NOCHANCE <- as.character(isis.noChance$SIT.NOCHANCE)
  
  isis.solved <- data.frame(EMOT.SOLVED = data$Est�.n.en.la.recta.final.para.entregar.el.proyecto.del.semestre..Su.proyecto.genera.un.error.y.ustedes.no.han.encontrado.la.causa..Unas.horas.antes.de.la.entrega.un.compa�.ero.mira.su.proyecto.y.les.indica.el.problema..Ustedes.alcanzan.a.resolverlo.�.Qu�..emoción.sentir�.a.en.ese.momento.,
                            SIT.SOLVED = data$c..Si.pudiera.decirle.abiertamente.al.�.compa�.ero.que.les.ayudó.lo.que.siente..�.qu�..le.dir�.a....)
  
  isis.solved$EMOT.SOLVED <- emots.isis(isis.solved$EMOT.SOLVED)
  isis.solved$SIT.SOLVED <- as.character(isis.solved$SIT.SOLVED)
  
  isis.copy <- data.frame(EMOT.COPY = data$Su.profesor.les.comunica.que.ha.encontrado.evidencia.de.copia.en.algunos.de.los.trabajos.del.grupo.y.les.pide.a.todos.que.se.acerquen.a.su.oficina.para.aclarar.la.situación..�.Qu�..emoción.sentir�.a.en.ese.momento.,
                          SIT.COPY = data$c..Si.pudiera.decirle.abiertamente.�.a.su.mejor.amigo.lo.que.siente..�.qu�..le.dir�.a....)
  
  isis.copy$EMOT.COPY <- emots.isis(isis.copy$EMOT.COPY)
  isis.copy$SIT.COPY <- as.character(isis.copy$SIT.COPY)
  
  isis.gender <- data.frame(Gender = data$�.Cu�.l.es.su.g�.nero.)
  
  isis.asign <- data.frame(PIMB = data$�.Cu�.l.de.estos.cursos.est�..cursando.actualmente.,
                            PIMO = data$c..PIMO................................................PIMO...,
                            MBDA = data$c..MBDA.......................................................,
                            POOB = data$c..POOB.......................................................,
                            PDSW = data$c..PDSW........................................PDSW....PDSW...,
                            ARSW = data$c..ARSW.......................................................,
                            COSW = data$c..COSW........................COSW...........................)
  
  levels(isis.gender$Gender)[levels(isis.gender$Gender)=="Másculino"] <- "Masculino"
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
  levels(data)[levels(data)=="Confusión"] <- "Confusion"
  levels(data)[levels(data)=="Curiosidad"] <- "Curiosidad"
  levels(data)[levels(data)=="Decepción"] <- "Decepcion"
  levels(data)[levels(data)=="Disfrute"] <- "Disfrute"
  levels(data)[levels(data)=="Frustración"] <- "Frustracion"
  levels(data)[levels(data)=="Satisfacción"] <- "Satisfaccion"
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