#' Convert an epiweek to date (code taken from the epiweek package, no longer on CRAN).
#'
#' @param year epiweek year
#' @param weekno epiweek number
#' @param firstday index day of week 
#' 
#' Source: https://github.com/cran/EpiWeek/blob/master/R/epiweek.R
#'
#' @export
epiweekToDate<-function(year,weekno,firstday="Sunday"){
    if(!(firstday=="Sunday"|| firstday=="Monday")){
        stop("Wrong firstday!")
    }
    if(year<0 || weekno<0){
        stop("Wrong Input!")
    }
    
    jan4=strptime(paste(year,1,4,sep="-"),format="%Y-%m-%d")
    wday=jan4$wday
    wday[wday==0]=7
    wdaystart=ifelse(firstday=="Sunday",7,1)
    if(wday== wdaystart) weekstart=jan4
    if(wday!= wdaystart) weekstart=jan4-(wday-ifelse(firstday=="Sunday",0,1))*86400
    
    jan4_2=strptime(paste(year+1,1,4,sep="-"),format="%Y-%m-%d")
    
    wday_2=jan4_2$wday
    wday_2[wday_2==0]=7
    wdaystart_2=ifelse(firstday=="Sunday",7,1)
    if(wday_2== wdaystart_2) weekstart_2=jan4_2
    if(wday_2!= wdaystart_2) weekstart_2=jan4_2-(wday_2-ifelse(firstday=="Sunday",0,1))*86400
    
    if(weekno>((weekstart_2-weekstart)/7)){
        stop(paste("There are only ",(weekstart_2-weekstart)/7," weeks in ",year,"!",sep=""))
    }
    
    d0=weekstart+(weekno-1)*7*86400
    d1=weekstart+(weekno-1)*7*86400+6*86400
    
    return(list("d0"=strptime(d0,format="%Y-%m-%d"),"d1"=strptime(d1,format="%Y-%m-%d")))
}