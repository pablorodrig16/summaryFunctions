#' Calculates Fisher exact test P (two tails) of data by group
#' @title Fisher exact P of data x group
#' @param data is a factor vector with data
#' @param group is a factor vector for grouping
#' @export
#' @examples
#' p.Fisher (mtcars$mpg, mtcars$cyl)

p.Fisher<-function (data,group){
    if (missing(data)|missing(group)){
        message("Data or group vectors are missing")
    }else{
        data<-as.factor(data)
        group<-as.factor(group)
        ifelse(fisher.test(na.omit(data),group[!is.na(data)])[[1]]<0.001,
               "<0.001",
               round(fisher.test(na.omit(data),group[!is.na(data)])[[1]],3))
    }
}

#' Calculates Chi square test P (two tails) of data by group
#' @title Chi square P of data x group
#' @param data is a factor vector with data
#' @param group is a factor vector for grouping
#' @export
#' @examples
#' p.Chisq (mtcars$mpg, mtcars$cyl)
p.Chisq<-function (data,group){
    if (missing(data)|missing(group)){
        message("Data or group vectors are missing")
    }else{
        suppressWarnings({
            ifelse(chisq.test(na.omit(data),group[!is.na(data)])[[3]]<0.001,
                   "<0.001",
                   round(chisq.test(na.omit(data),group[!is.na(data)])[[3]],3))
        })
    }
}


#' Calculates t test P (two tails) of data by group
#' @title T test P of data x group
#' @param data is a numeric or integer vector with data
#' @param group is a factor vector for grouping
#' @export
#' @examples
#' p.Ttest (mtcars$mpg, mtcars$cyl)

p.Ttest<-function (data,group){
    if (missing(data)|missing(group)){
        message("Data or group vectors are missing")
    }else{
        group<-as.factor(group)
        if (length(levels(group))!=2){
            return(message("Group must have only 2 levels"))
        }else{
            ifelse(t.test(na.omit(data)~group[!is.na(data)])[[3]]<0.001,
                   "<0.001",
                   round(t.test(na.omit(data)~group[!is.na(data)])[[3]],3))
        }
    }
}

#' Calculates Wilcox P (two tails) of data by group (or Mann-Whitney test)
#' @title Wilcoxon Rank Sum test of data x group
#' @param data is a numeric or integer vector with data
#' @param group is a factor vector for grouping
#' @export
#' @examples
#' p.Wilcox (mtcars$mpg, mtcars$cyl)
p.Wilcox<-function (data,group){
    if (missing(data)|missing(group)){
        message("Data or group vectors are missing")
    }else{
        group<-as.factor(group)
        if (length(levels(group))!=2){
            return(message("Group must have only 2 levels"))
        }else{
        ifelse(wilcox.test(na.omit(data)~group[!is.na(data)])$p.value<0.001,
               "<0.001",
               round(wilcox.test(na.omit(data)~group[!is.na(data)])$p.value,3))
        }
    }
}

#' Calculates one way ANOVA P (two tails) of data by group
#' @title One way ANOVA P of data x group
#' @param data is a numeric or integer vector with data
#' @param group is a factor vector for grouping
#' @export
#' @examples
#' p.Anova (mtcars$mpg, mtcars$cyl)
p.Anova<-function (data,group){
    if (missing(data)|missing(group)){
        message("Data or group vectors are missing")
    }else{
        ifelse(anova(aov(na.omit(data)~group[!is.na(data)]))[[5]][1]<0.001,
               "<0.001",
               round(anova(aov(na.omit(data)~group[!is.na(data)]))[[5]][1],3))
    }
}

#' Calculates Kruskal Wallis test P (two tails) of data by group
#' @title Kruskal-Wallis rank sum test P of data x group
#' @param data is a numeric or integer vector with data
#' @param group is a factor vector for grouping
#' @export
#' @examples
#' p.Kruskal (mtcars$mpg, mtcars$cyl)
p.Kruskal<-function (data,group){
    if (missing(data)|missing(group)){
        message("Data or group vectors are missing")
    }else{
        ifelse(kruskal.test(na.omit(data)~group[!is.na(data)])$p.value<0.001,
               "<0.001",
               round(kruskal.test(na.omit(data)~group[!is.na(data)])$p.value,3))
    }
}


