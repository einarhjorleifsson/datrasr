
#### Data to extract length frequencies from ICES Exchange format  #####

#' @title XXX
#'
#' @description XXX
#'
#' @export
parseExchangeFormatLFs <- function (wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")  {

#wd <- "D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010"

  oldwd <- getwd()
setwd(wd)

fnames<-list.files()
fnames<-fnames[grep("Exchange Data_",fnames)]
fnames<-fnames[grep(".csv",fnames)]


out<-NULL
for(i in 1:length(fnames)){
cnts<-count.fields(fnames[i],sep=",")
lf<-readLines(fnames[i])
lf<-cbind(lf,cnts)
lf<-lf[lf[,2]=="26",][,-2] # think in the most recent datras exchange these are 24
print(length(lf))
if(i == 1){hds<-lf[1]}
out<-c(out,lf[-1])
}

### Write data out and read it back in (temporarily) ###
write(t(out),file="lfs")
lf<-read.table("lfs",sep=",", stringsAsFactors = FALSE)
#gc(reset=T)

system("rm lfs")
#dimnames(lf)[[2]] <- c("recordtype","quarter","country","ship","gear","sweeplngt","gearexp","doortype",
#"stno","haulno","year","speccodetype","speccode","specval","sex","totalno","catidentifier","nomeas","subfactor","subwgt","ascatcatchwgt",
#"lngtcode","lngtclass","hlnoatlngt")
names(lf) <- tolower(strsplit(hds,",")[[1]])


lf$quarter <- as.numeric(as.character(lf$quarter))
lf$sweeplngt <- as.numeric(as.character(lf$sweeplngt))
lf$haulno <- as.numeric(as.character(lf$haulno))
lf$stno <- as.character(lf$stno)
lf$year <- as.numeric(as.character(lf$year))
lf$speccodetype <-as.character(lf$speccodetype)
lf$speccodetype <-ifelse(lf$speccodetype=='TRUE','t',lf$speccodetype)

lf$speccode <-as.numeric(as.character(lf$speccode))

lf$specval <-as.numeric(as.character(lf$specval))
lf$totalno <-as.numeric(as.character(lf$totalno))
lf$catidentifier <-as.numeric(as.character(lf$catidentifier))
lf$nomeas <-as.numeric(as.character(lf$nomeas))
lf$subfactor <-as.numeric(as.character(lf$subfactor))
lf$subwgt <-as.numeric(as.character(lf$subwgt))
lf$lngtcode <-as.character(lf$lngtcode)
lf$lngtclass <- as.numeric(as.character(lf$lngtclass))
lf$hlnoatlngt <- as.numeric(as.character(lf$hlnoatlngt))

#print(str(lf))
setwd(oldwd)
lf

}


#### Example of use ####

#lf <-  parseExchangeFormatLFs(wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")


