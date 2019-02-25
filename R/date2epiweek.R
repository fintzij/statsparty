#' Convert a date to an epiweek (code taken from the epiweek package, no longer on CRAN).
#'
#' @param date date to be converted to epiweek
#' @param format date format
#' @param firstday index day of week 
#' 
#' Source: https://github.com/cran/EpiWeek/blob/master/R/epiweek.R
#'
#' @export
date2epiweek<-function(date,format="%Y-%m-%d",firstday="Sunday"){
    date=strptime(date,format=format)
    
    if(class(date)[2]!="POSIXt" || is.na(date)){
        stop("Wrong format for date!")
    }
    if(!(firstday=="Sunday"|| firstday=="Monday")){
        stop("Wrong firstday!")
    }
    year=1900+date$year
    
    jan4=strptime(paste(year,1,4,sep="-"),format="%Y-%m-%d")
    wday=jan4$wday
    
    wday[wday==0]=7
    wdaystart=ifelse(firstday=="Sunday",7,1)
    if(wday== wdaystart) weekstart=jan4
    if(wday!= wdaystart) weekstart=jan4-(wday-ifelse(firstday=="Sunday",0,1))*86400
    
    weeknum=ceiling(as.numeric((difftime(date,weekstart,units="days")+0.1)/7))
    mday=date$mday
    wday=date$wday
    
    year=ifelse(weeknum==53 & mday-wday>=(ifelse(firstday=="Sunday",29,28)),year+1,year)
    weeknum=ifelse(weeknum==53 & mday-wday>=(ifelse(firstday=="Sunday",29,28)),1,weeknum)
    year.shift=year-1
    jan4.shift=strptime(paste(year.shift,1,4,sep="-"),format="%Y-%m-%d")
    wday=jan4.shift$wday
    wday[wday==0]=7
    
    wdaystart=ifelse(firstday=="Sunday",7,1)
    if(wday== wdaystart) weekstart=jan4.shift
    if(wday!= wdaystart) weekstart=jan4.shift-(wday-ifelse(firstday=="Sunday",0,1))*86400
    
    weeknum.shift=ceiling(as.numeric((difftime(date,weekstart)+0.1)/7))
    year=ifelse(weeknum==0,year.shift,year)
    weeknum=ifelse(weeknum==0,weeknum.shift,weeknum)
    
    return(list("year"=year,"weekno"=weeknum))
}