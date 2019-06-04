#' Calculates mean plus minus SD from a numeric vector
#'
#' @title meanSD
#' @param data is a numeric vector
#' @return the mean plus minus SD of data
#' @author Pablo O Rodriguez
#' @export
#' @examples
#' meanSD (mtcars$mpg)
#'
meanSD<-function (data)
{
    if (class(data)=="numeric"|class(data)=="integer"){
        paste(round(mean(data, na.rm=T),2),"\u00B1",round(sd(data, na.rm=T),2),sep="")
    }else{
        message("Incorrect data class")
    }

}

#' Calculates mean and interquartile range from a numeric vector
#' @title medianIQR
#' @param data is a numeric vector
#' @return the median (IQR) of data
#' @author Pablo O Rodriguez
#' @export
#' @examples
#' medianIQR (mtcars$cyl)
#'
medianIQR<-function (data)
{
    if (class(data)=="numeric"|class(data)=="integer"){
        paste(round (median(data, na.rm=T),2)," (",round(quantile(data, na.rm=T)[[2]],2),
              "-",round(quantile(data, na.rm=T)[[4]],2),")", sep="")
    }else{
        message("Incorrect data class")
    }

}

#' Calculates shapiro test in data by groups
#' @title shapiroGroup
#' @param data is a numeric vector
#' @param group is a vector with grouping data
#' @return the shapiro test output of data for each group
#' @author Pablo O Rodriguez
#' @export
#' @examples
#' shapiroGroup (mtcars$mpg, mtcars$cyl)
#'
shapiroGroup<-function(data,group=NA){
    if (class(data)=="numeric"|class(data)=="integer"){
        if (length(group)!=length(data)){
            warning("group variable is missing or it is of different length than data")
            if (length(data)<3|length (data)>5000){
                return(message("Value can not be calculated. Data size is outside of range."))
            }else{
                shapiro.test(data)
            }

        }else{
            group<-as.factor(group)
            if (any(table(group)<3 | table(group)>5000)){
                return(message("Value can not be calculated.Group size is outside of range."))
            }else{
                by (data,group,shapiro.test)
            }
        }
    }else{
        message("Incorrect data class or no grouping variable")
    }
}


#' Calculates shapiro test in data by groups
#' @title is.normal
#' @param data is a numeric vector
#' @param group is a vector with grouping data
#' @return logical value, TRUE if normality is verified in all groups according to shapiro test P
#' @author Pablo O Rodriguez
#' @export
#' @examples
#' is.normal (mtcars$mpg, mtcars$cyl)
#'
is.normal<-function(data,group=NA)
{
    if (class(data)=="numeric"|class(data)=="integer"){
        if (length(group)!=length(data)){
            if (length(data)<3|length (data)>5000){
                warning("Value can not be calculated. Data size is outside of range.")
                return(NA)
            }else{
                return(ifelse(shapiro.test(data)[[2]]>=0.05,TRUE,FALSE))
            }
        }else{
            group<-as.factor(group)
            if (any(table(group)<3 | table(group)>5000)){
                warning("Value can not be calculated. Data size is outside of range.")
                return(NA)
            }else{
                k<-sapply (levels(group), function (x) {
                    w<-which(group==x)
                    shapiro.test(data[w])[[2]]
                })
                return (all(k>=0.05))
            }
        }
    }else{
        message("Incorrect data class")
    }

}


#' Calculates age in years
#' @title ageFun
#' @param born is a character or date class value/vector representing the date of birth
#' @param date is a character or date class value/vector representing the date to calculate age
#' @param format is a character string the format for character to date conversion (as in as.Date() function)
#' @param include.months is a logical to decide if months are shown in results
#' @return a list including age in years and months (if specified)
#' @author Pablo O Rodriguez
#' @export
#' @examples
#' ageFun(born = "1900-02-01",date = "1950-01-01",format = "%Y-%m-%d",include.months = FALSE)
#'
ageFun<-function (born, date=Sys.time(), format="%Y-%m-%d",include.months=TRUE){
    ## checking classes
    cBorn<-class(born)
    cDate<-class(date)


    if (!any(class(born)%in%c("character","Date","POSIXct","POSIXt"))){
        return (warning("Wrong born class"))
    }else if (!any(class(date)%in%c("character","Date","POSIXct","POSIXt"))){
        return (warning("Wrong born class"))
    }else if (!class (include.months)=="logical"){
        return (warning("Wrong include.months class"))
    }

    ## converting character to date format
    if (cBorn=="character"){
        born<-as.Date(born,format)
    }else if (cDate=="character"){
        date<-as.Date(date,format)
    }

    ## converting dates to POSXIlt
    born<-as.POSIXlt(born)
    date<-as.POSIXlt(date)

    ## calculating differences of years, month and days
    age<-date$year-born$year
    ageMonth<-date$mon-born$mon
    ageDay<-date$mday-born$mday

    if (age<0){
        return (message("date should be after than born"))
    }

    if(ageMonth<0 | (ageMonth==0 & ageDay<0)){
        age<-age-1
    }

    if (ageMonth<0){
        ageMonth<-12+ageMonth
    }

    result<-if (include.months){
        list (years=age,months=ageMonth)
    }else{
        list(years=age)
    }

    return (result)
}
