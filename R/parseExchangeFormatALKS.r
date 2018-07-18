
#### Data to extract Age Length keys from ICES Exchange format  #####
#' @title XXX
#'
#' @description XXX
#'
#' @export
parseExchangeFormatALKs <- function (wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")  {

  oldwd <- getwd()

 setwd(wd)

fnames<-list.files()
fnames<-fnames[grep("Exchange Data_",fnames)]
fnames<-fnames[grep(".csv",fnames)]

#Age length keys data

out<-NULL
for(i in 1:length(fnames)){
cnts<-count.fields(fnames[i],sep=",")
alk<-readLines(fnames[i])
alk<-cbind(alk,cnts)
header <- alk[alk[,2] == 25,][1,-2]
alk<-alk[alk[,2]=="25",][,-2]  # think in the latest exchange these are 23
print(length(alk))
out<-c(out,alk[-1])
}

header <- tolower(strsplit(header, ",")$alk)
alk <- data.frame(matrix(unlist(strsplit(out,",")),byrow=T,ncol=length(header)))
dimnames(alk)[[2]] <- header

alk$quarter <- as.numeric(as.character(alk$quarter))
alk$sweeplngt <- as.numeric(as.character(alk$sweeplngt))
alk$haulno <- as.numeric(as.character(alk$haulno))
 alk$stno <- as.numeric(as.character(alk$stno)) # NA's sometimes created by coercion
 alk$year <- as.numeric(as.character(alk$year))

alk$speccodetype <-as.character(alk$speccodetype)
alk$speccodetype <-ifelse(alk$speccodetype=='TRUE','t',alk$speccodetype)



 alk$speccode <-as.numeric(as.character(alk$speccode))
 alk$areatype <- as.character(alk$areatype)
 alk$areacode <- as.character(alk$areacode)
 alk$lngtclass <- as.numeric(as.character(alk$lngtclass))
 alk$noatalk <- as.numeric(as.character(alk$noatalk))
 alk$indwgt  <- as.numeric(as.character(alk$indwgt))
 alk$age  <- as.numeric(as.character(alk$age))


print(str(alk))

setwd(oldwd)

alk

}


### Example ######

### alk <-  parseExchangeFormatALKs(wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")






