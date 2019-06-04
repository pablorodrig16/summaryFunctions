#' Creates a summary statistics table of data comparing by group with the appropiated statistical test
#' @title Summary table with statistical tests for groups comparison
#' @param data is a vector with data
#' @param group is a factor vector for grouping
#' @param non.parametric is a logical vector. FALSE allows the function to choose the test according to the distribution. If TRUE, the function uses non parametric tests by default.
#' @export
#' @examples
#' buildTable (mtcars$mpg, mtcars$cyl, non.parametric=FALSE)

buildTable<-function (data, group=1, non.parametric=FALSE){
    if (missing(data)){
        warning("Data vectors are missing")
    }else{
        if (any(length (group)!=1 & (table(group)<3 | table(group)>5000))){
            warning("Table can not be computed. Data size is outside of range (3-5000 per group).")
            return(NA)
        }else{
            #################Procesamiento de variables de agrupamiento###########

            ####Identificar variables cuantitativas y cualitativas: escribir el numero de columna####
            data<-as.data.frame(data)

            var.analizar<-1:length (data)


            ###convierte variables character a factor########
            for (a in 1:length (data)){
                if (is.character(data[,a])){
                    data[,a]<-as.factor(data[,a])
                }
            }

            tabla <-c()
            filas<-1

            for (k in 1:length (data)){
                if (is.numeric(data[,k])||is.integer(data[,k])){
                    if (length(levels(as.factor(group)))==2)
                    {

                        #######variables cuantitatitvas group = 2 levels #####################
                        if(is.normal(as.numeric(data[,k]),group) & !non.parametric)
                        {
                            linea<-c(tapply (as.numeric(data[,k]),group, meanSD),
                                     meanSD (as.numeric(data[,k])),
                                     is.normal(as.numeric(data[,k]),group),
                                     sum (is.na(data[,k])),
                                     p.Ttest(data[,k],group))
                            tabla<-rbind(tabla,linea)
                        }
                        else
                        {
                            linea<-c(tapply (as.numeric(data[,k]),group, medianIQR),
                                     medianIQR (as.numeric(data[,k])),
                                     is.normal(as.numeric(data[,k]),group),
                                     sum (is.na(data[,k])),
                                     p.Wilcox(data[,k],group))
                            tabla<-rbind(tabla,linea)
                        }
                        rownames(tabla)[filas]<-names(data[var.analizar[k]])
                        filas<-filas+1
                    } else if (length(levels(as.factor(group)))>2)
                    {
                        #######variables cuantitatitvas group > 2 levels#####################
                        if(is.normal(as.numeric(data[,k]),group) & !non.parametric)
                        {
                            linea<-c(tapply (as.numeric(data[,k]),group, meanSD),
                                     meanSD (as.numeric(data[,k])),
                                     is.normal(as.numeric(data[,k]),group),
                                     sum (is.na(data[,k])),
                                     p.Anova (data[,k],group))
                            tabla<-rbind(tabla,linea)
                        }else
                        {
                            linea<-c(tapply (as.numeric(data[,k]),group, medianIQR),
                                     medianIQR (as.numeric(data[,k])),
                                     is.normal(as.numeric(data[,k]),group),
                                     sum (is.na(data[,k])),
                                     p.Kruskal(data[,k],group))
                            tabla<-rbind(tabla,linea)
                        }
                        rownames(tabla)[filas]<-names(data[var.analizar[k]])
                        filas<-filas+1

                    } else {

                        #######variables cuantitatitvas group = 1 o nada levels #####################
                        if(is.normal(as.numeric(data[,k])) & !non.parametric)
                        {
                            linea<-c(meanSD (as.numeric(data[,k])),
                                     is.normal(as.numeric(data[,k])),
                                     sum (is.na(data[,k])))
                            tabla<-rbind(tabla,linea)
                        }
                        else
                        {
                            linea<-c(medianIQR (as.numeric(data[,k])),
                                     is.normal(as.numeric(data[,k]),group),
                                     sum (is.na(data[,k])))
                            tabla<-rbind(tabla,linea)
                        }
                        rownames(tabla)[filas]<-names(data[var.analizar[k]])
                        filas<-filas+1
                    }

                }else{
                    if (length(levels(as.factor(group)))==2)
                    {

                        #######variables cualitatitvas#####################
                        #####analisis de variables TRUE y FALSE#####
                        if (is.logical(data[,k])==TRUE)
                        {
                            linea<-c(
                                paste (tapply (na.omit(data[,k]),group[!is.na(data[,k])], sum),
                                       " (",
                                       round(
                                           tapply (na.omit(data[,k]),group[!is.na(data[,k])], sum)*100/
                                               tapply (na.omit(data[,k]), group[!is.na(data[,k])], length)
                                           ,2),
                                       "%)", sep=""),
                                paste (sum (na.omit (data[,k])), " (",
                                       round (
                                           sum (na.omit (data[,k]))*100/length(na.omit(data[,k])),2),
                                       "%)", sep=""),
                                "no normal",
                                sum (is.na(data[,k])),
                                p.Fisher (data[,k],group))
                            tabla<-rbind(tabla,linea)
                            rownames(tabla)[filas]<-names(data[var.analizar[k]])
                            filas<-filas+1
                        }
                        #####analisis de variables cualitativas no logical####
                        else
                        {
                            for (l in 1:length (levels(as.factor(data[,k]))))
                            {
                                nivel<-levels(as.factor(data[,k]))[l]
                                linea<-c(
                                    paste (tapply (na.omit(data[,k])==nivel,group[!is.na(data[,k])], sum),
                                           " (",
                                           round(
                                               tapply (na.omit(data[,k])==nivel,group[!is.na(data[,k])], sum)*100/
                                                   tapply (na.omit(data[,k]), group[!is.na(data[,k])], length)
                                               ,2),
                                           "%)", sep=""),
                                    paste (sum (na.omit (data[,k])==nivel), " (",
                                           round (
                                               sum (na.omit (data[,k])==nivel)*100/length(na.omit(data[,k])),2),
                                           "%)", sep=""),
                                    "no normal",
                                    sum (is.na(data[,k])),
                                    p.Chisq (data[,k],group))
                                tabla<-rbind(tabla,linea)
                                rownames(tabla)[filas]<-paste(names(data[var.analizar[k]]),nivel)
                                filas<-filas+1
                            }
                        }

                    }else if (length(levels(as.factor(group)))>2)
                    {
                        #######variables cualitatitvas#####################
                        #####analisis de variables TRUE y FALSE#####
                        if (is.logical(data[,k])==TRUE)
                        {
                            linea<-c(
                                paste (tapply (na.omit(data[,k]),group[!is.na(data[,k])], sum),
                                       " (",
                                       round(
                                           tapply (na.omit(data[,k]),group[!is.na(data[,k])], sum)*100/
                                               tapply (na.omit(data[,k]), group[!is.na(data[,k])], length),
                                           2),
                                       "%)", sep=""),
                                paste (sum (na.omit (data[,k])), " (",
                                       round (
                                           sum (na.omit (data[,k]))*100/length(na.omit(data[,k])),2),
                                       "%)", sep=""),
                                "no normal",
                                sum (is.na(data[,k])),
                                p.Chisq (data[,k],group))
                            tabla<-rbind(tabla,linea)
                            rownames(tabla)[filas]<-names(data[var.analizar[k]])
                            filas<-filas+1
                        }
                        #####analisis de variables cualitativas no logical####
                        else
                        {
                            for (l in 1:length(levels(as.factor(data[,k])))){
                                nivel<-levels(as.factor(data[,k]))[l]
                                linea<-c(
                                    paste (tapply (na.omit(data[,k])==nivel,group[!is.na(data[,k])], sum),
                                           " (",
                                           round(
                                               tapply (na.omit(data[,k])==nivel,group[!is.na(data[,k])], sum)*100/
                                                   tapply (na.omit(data[,k]), group[!is.na(data[,k])], length),
                                               2),
                                           "%)", sep=""),
                                    paste (sum (na.omit (data[,k])==nivel), " (",
                                           round (
                                               sum (na.omit (data[,k])==nivel)*100/length(na.omit(data[,k])),2),
                                           "%)", sep=""),
                                    "no normal",
                                    sum (is.na(data[,k])),
                                    p.Chisq (data[,k],group))
                                tabla<-rbind(tabla,linea)
                                rownames(tabla)[filas]<-paste(names(data[var.analizar[k]]),nivel)
                                filas<-filas+1
                            }
                        }
                    }else{
                        #######variables cualitatitvas#####################
                        #####analisis de variables TRUE y FALSE#####
                        if (is.logical(data[,k])==TRUE)
                        {
                            linea<-c(
                                paste (sum (na.omit (data[,k])), " (",
                                       round (
                                           sum (na.omit (data[,k]))*100/length(na.omit(data[,k])),2),
                                       "%)", sep=""),
                                "no normal",
                                sum (is.na(data[,k])),
                                p.Fisher (data[,k],group))
                            tabla<-rbind(tabla,linea)
                            rownames(tabla)[filas]<-names(data[var.analizar[k]])
                            filas<-filas+1
                        }
                        #####analisis de variables cualitativas no logical####
                        else
                        {
                            for (l in 1:length (levels(as.factor(data[,k]))))
                            {
                                nivel<-levels(as.factor(data[,k]))[l]
                                linea<-c(
                                    paste (sum (na.omit (data[,k])==nivel), " (",
                                           round (
                                               sum (na.omit (data[,k])==nivel)*100/length(na.omit(data[,k])),2),
                                           "%)", sep=""),
                                    "no normal",
                                    sum (is.na(data[,k])),
                                    p.Chisq (data[,k],group))
                                tabla<-rbind(tabla,linea)
                                rownames(tabla)[filas]<-paste(names(data[var.analizar[k]]),nivel)
                                filas<-filas+1
                            }
                        }

                    }

                }
            }


            ####asigna nombres a las columnas####
            l<-length (levels (as.factor(group)))

            if (l<2){
                colnames(tabla)<-c("Stat","Is normal","Missing")
                return (tabla)
            }

            colnames(tabla)<-c(levels (as.factor(group)),"All","Is normal","Missing","P")

            nLine<-character()
            for (i in 1:l){
                nLine[i]<-paste("(n=",
                                summary (as.factor(group))[i],
                                ")",
                                sep="")
            }
            nLine<-c(nLine,
                     paste("(n=",
                           nrow(data),
                           ")",
                           sep = ""),
                     "","","")

            #for (i in 1:l){

            #    colnames(tabla)[i]<-paste(colnames(tabla)[i],
            #                              " (n=",
            #                              summary (as.factor(group))[i],
            #                              ")", sep="")
            #}

            tabla<-rbind(nLine,tabla)
            row.names(tabla)[1]<-""


            ####Devuelve la tabla como resultado de la funcion
            return(tabla)
        }
    }



}
