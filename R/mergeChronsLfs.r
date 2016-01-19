
#     Merge lfs with the chrons. One reason is that only the +ve part of the lf data are typically stored
#     and you need a complete list of stations to find the zero hauls.
#     You also need the extra information in the chron file, ie. latitude and longitude.
#' @title XXX
#'
#' @description XXX
#'
#' @export
mergeChronsLfs <- function(chrons=chrons,length.frequencies=spurdog) {

#Input are the raw chrons and the lfs by species

short.chrons <- data.frame(quarter=chrons$quarter,country=chrons$country,ship=chrons$ship,
gear=chrons$gear,stno=chrons$stno,haulno=chrons$haulno,year=chrons$year,
shootlat=chrons$shootlat,
shootlong=chrons$shootlong,
haullat=chrons$haullat,
haullong=chrons$haullong,
daynight=chrons$daynight,
statrec=chrons$statrec,
depth=chrons$depth,
haulval=chrons$haulval,
distance=chrons$distance,
hauldur=chrons$hauldur,
netopening=chrons$netopening,
wingspread=chrons$wingspread,
speedwater=chrons$speedwater,
groundspeed=chrons$groundspeed,
doorspread=chrons$doorspread,
windspeed = chrons$windspeed,
surtemp=chrons$surtemp,
bottemp=chrons$bottemp,
datim.shoot=chrons$datim.shot,datim.haul=chrons$datim.haul,
stringsAsFactors = FALSE)


short.length.frequencies <- length.frequencies[,-1]
short.length.frequencies$country <- as.character(short.length.frequencies$country)
short.length.frequencies$ship <- as.character(short.length.frequencies$ship)
short.length.frequencies$gear <- as.character(short.length.frequencies$gear)

# Einar: do not get this to work
#merged.lfs  <- base::merge(short.chrons,short.length.frequencies,
#                           by.x = c("quarter", "country", "ship", "gear", "stno", "haulno", "year"),
#                           by.y = c("quarter", "country", "ship", "gear", "stno", "haulno", "year"),
#                           all=T)
library(dplyr)
merged.lfs <- short.length.frequencies  %>%
  full_join(short.chrons)



merged.lfs$scientific.name <- short.length.frequencies$scientific.name[1]
merged.lfs$hlnoatlngt <- ifelse(is.na(merged.lfs$hlnoatlngt),0,merged.lfs$hlnoatlngt)
merged.lfs$hlwtatlngt <- ifelse(is.na(merged.lfs$hlwtatlngt),0,merged.lfs$hlwtatlngt)

merged.lfs$statrec <- as.character(merged.lfs$statrec)

#Note: In some cases there are missing haul longs and lats.

merged.lfs

}

