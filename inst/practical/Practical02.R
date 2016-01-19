
######################################################################################################################################################

############# ICES TRAWL SURVEY AND EVALUATION course #################################################################################################

################# Practical 02 - Exploring the Length-Frequencies ###########################################################################################################

################## Doug Beare and Dave Reid   ##########################################################################################################

# The aim of this exercise is download ICES exchange format data from www.ices.dk, extract, analyse and visualise the length-frequency data for individual species.

######################################################################################################################################################
## 1. Install R (http://cran.r-project.org/), packages  and Tinn-R (http://sourceforge.net/projects/tinn-r/) ##
######################################################################################################################################################


#install.packages('maps');
#install.packages('maptools');
#install.packages('rgdal');
#install.packages('sp');
#install.packages('spatial');
#install.packages('rgdal');
#install.packages('fields');
#
library(sp);library(mda);library(doBy);library(PBSmapping);library(maps);library(maptools);library(rgdal);library(spatial);library(fields)  # attach to R search path

######################################################################################################################################################
## 2. Select some data from the ICES website - http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx
######################################################################################################################################################

######################################################################################################################################################
## 3. Save the data to a directory of your choice, eg. 'c:/datras/data' and extract them from the zip files.
######################################################################################################################################################

######################################################################################################################################################
## 4. Install the R library datras. Do this by downloading datras*zip from the sharepoint. Open R, go to packages, install package from local zip files.
######################################################################################################################################################

######################################################################################################################################################
## 5. Attach datrasR package to search path
######################################################################################################################################################

library(datrasR)

#######################################################################################################################################################
## 6. Extract the length-frequency data in the relevant directory
 ######################################################################################################################################################

#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/RockallData")
#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2010-2011")
lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2005-2011")
#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2001-2011-Q1")

######################################################################################################################################################
## 7. Add on scientific name using TSN (downloaded from ITIS website). 
######################################################################################################################################################

data(tsn)  # itis database of all species in the world

head(tsn)  # first 5 lines

lfs$scientific.name <- as.character(tsn$completename[match(lfs$speccode,tsn$tsn)])   # match scientific names using tsn 

unique(lfs$scientific.name) # print all fish species in the data set. 

#####################################################################################################################################################
## 8. Tidy up the data
#####################################################################################################################################################

lfs <- lfs[!duplicated(lfs),]                  #  remove any duplicates
lfs <- lfs[lfs$speccode != -9,]                #  remove missing species codes
lfs <- lfs[lfs$lngtclass != -9,]               #  remove missing length classes
lfs <- lfs[!is.na(lfs$lngtclass),]             #  remove missing length classes
lfs <- lfs[!is.na(lfs$lngtcode),]              #  remove missing length codes
lfs <- lfs[!is.na(lfs$scientific.name),]       #  remove any missing scientific names 
lfs$hlnoatlngt <- lfs$hlnoatlngt*lfs$subfact   #  Multiply by the subfactor

#####################################################################################################################################################
## 9. Standardise length codes to cms (some are in mms) and create bins of 0.5.
#####################################################################################################################################################

table(lfs$lngtcode)

lfs$lngtclass[lfs$lngtcode == "."] <- lfs$lngtclass[lfs$lngtcode == '.']/10
lfs$lngtclass[lfs$lngtcode == "0"] <- lfs$lngtclass[lfs$lngtcode == "0"]/10

lfs$lngtclass <- floor(lfs$lngtclass)  #Round length down and add 0.5
lfs$lngtclass <- lfs$lngtclass+0.5

#####################################################################################################################################################
## 10. Estimate weight of each fish.
#####################################################################################################################################################

# Note: It can be useful to quote results in terms of weight rather than numbers.
# This is usually done with parameters (a & b) from non-linear equations where
# Weight in grammes = aL^b  where L = length.

data(length.weight)   # attach a list of parameters courtesy of IMARES and Marine Science Scotland.

# Match a and b parameters onto the correct species

lfs$a <- length.weight$a[match(lfs$scientific.name,length.weight$scientific.name)]
lfs$b <- length.weight$b[match(lfs$scientific.name,length.weight$scientific.name)]

lfs$hlwtatlngt<-((lfs$a*lfs$lngtclass^lfs$b)*lfs$hlnoatlngt) /1000    # calculate weight in kilos 

print(head(lfs))   # are the weights reasonable ?

#####################################################################################################################################################
##  11. Extract a species. These are just examples but you can let your curiosity roam freely! 
#####################################################################################################################################################

#cod <- lfs[lfs$scientific.name == 'Gadus morhua',]
#spurdog <- lfs[lfs$scientific.name == 'Squalus acanthias',]
#angler <- lfs[lfs$scientific.name == 'Lophius piscatorius',]
#turbot <- lfs[lfs$scientific.name == 'Psetta maxima',]
plaice <- lfs[lfs$scientific.name == 'Pleuronectes platessa',]
#dab    <- lfs[lfs$scientific.name == 'Limanda limanda',]
#brill  <- lfs[lfs$scientific.name == 'Scophthalmus rhombus',]
#lemon  <- lfs[lfs$scientific.name == 'Microstomus kitt',]
#haddock <-lfs[lfs$scientific.name == 'Melanogrammus aeglefinus',]
#
# plot some distributions 

barplot(table(rep(plaice$lngtclass,plaice$hlnoatlngt)),space=0   )  # these are the entire distributions for all the fish in the data you chose
#barplot(table(rep(angler$lngtclass,angler$hlnoatlngt)),space=0   )
#barplot(table(rep(cod$lngtclass,cod$hlnoatlngt)),space=0)

#####################################################################################################################################################
##  12. Merge lfs with the chrons. One reason is that only the +ve part of the lf data are typically stored and you need a complete list of stations to find the zero hauls.
#####################################################################################################################################################

# Get the chrons for the same survey (e.g. Practical01)

#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/RockallData")
#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2010-2011")
#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2001-2011-Q1")
chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2005-2011")

# Tidy up the chrons

chrons <- chrons[chrons$haulval=='V',]          #only valid hauls
chrons <- chrons[!duplicated(chrons),]          # chuck any duplicates

# Do the merging, ie. merge the positive component (the length frequencies) with the chrons. The result is a file which has zeros where you did
# a haul but caught none.

#merged.turbot  <- mergeChronsLfs(chrons=chrons,length.frequencies=turbot)
merged.plaice  <- mergeChronsLfs(chrons=chrons,length.frequencies=plaice)
#merged.spurdog <- mergeChronsLfs(chrons=chrons,length.frequencies=spurdog)
#merged.lemon   <- mergeChronsLfs(chrons=chrons,length.frequencies=lemon)
#merged.dab     <- mergeChronsLfs(chrons=chrons,length.frequencies=dab)
#merged.brill   <- mergeChronsLfs(chrons=chrons,length.frequencies=brill)
#merged.angler  <- mergeChronsLfs(chrons=chrons,length.frequencies=angler)
#merged.cod     <- mergeChronsLfs(chrons=chrons,length.frequencies=cod)
#merged.haddock <- mergeChronsLfs(chrons=chrons,length.frequencies=haddock)

#####################################################################################################################################################
##  13. Always useful to examine plots of length frequencies and/or probability density functions by year  (why ?) 
#####################################################################################################################################################

#fdat <- merged.cod
fdat <- merged.plaice
#fdat <- merged.angler
#fdat <- merged.haddock
#
table(chrons$year,chrons$quarter)   # find range of data 

par(mar=c(3,3,1,1),oma=c(3,3,3,3),mfcol=c(5,3))
plot.Lfs(fish=fdat,what.quarter="3",chrons1=chrons)             #e.g plot the length frequencies

plot.PDensity(fish=fdat,what.quarter="3",chrons1=chrons,bw=.5)   #e.g plot the density function
                                                                     

#####################################################################################################################################
## 14. Plot CPUE by year and quarter  
#####################################################################################################################################

# Note: In my opinion it is always preferable to keep catch and effort databases separate, chose level of aggregation, aggregate over that level and then
# divide the catch by the effort.

fish <- merged.plaice

fish <-fish[!is.na(fish$haullong),]     # sometimes there are incomplete chron records.

fish$cpue.by.n  <- fish$hlno/fish$hauldur # divide catch by effort (n)
fish$cpue.by.wt <- fish$hlwt/fish$hauldur # divide catch by effort (weight)

nfish <- aggregate(list(cpue.by.n=fish$cpue.by.n,cpue.by.wt=fish$cpue.by.wt),
list(year=fish$year,quarter=fish$quarter),mean,na.rm=T)     # average cpues caught over year and quarter. Is this correct ??


# Put on absolute time (qtrend).

nfish$qtrend <- qtrend.f(nfish$year,nfish$quarter,start.year=min(nfish$year))

par(mfrow=c(2,1),mar=c(2,4,2,2))
plot(nfish$qtrend,nfish$cpue.by.n,type='b',xlab='time',ylab='cpue',xaxt='n');title('cpue by n')
axis(side=1,at=nfish$qtrend,labels=as.character(nfish$year))
plot(nfish$qtrend,nfish$cpue.by.wt,type='b',xlab='time',ylab='cpue',xaxt='n');title('cpue by wt')
axis(side=1,at=nfish$qtrend,labels=as.character(nfish$year))

#####################################################################################################################################
## 15. Map total numbers of individuals caught and/or weights
#####################################################################################################################################

#####################################################################################################################################
# How about Bubble plots?
#####################################################################################################################################

# Select data (species) 

fish <- merged.plaice   # chose plaice or whatever
#fish <- merged.dab
#fish <- merged.angler
#fish <- merged.cod
#fish <- merged.haddock

fish <-fish[!is.na(fish$haullong),]      # sometimes there are incomplete chron records.

fish$cpue.by.n  <- fish$hlno/fish$hauldur # divide catch by effort (n)
fish$cpue.by.wt <- fish$hlwt/fish$hauldur # divide catch by effort (weight)

nfish2 <- aggregate(list(cpue.by.n=fish$cpue.by.n,cpue.by.wt=fish$cpue.by.wt),
list(scientific.name=fish$scientific.name,year=fish$year,quarter=fish$quarter,shootlat=fish$shootlat,shootlong=fish$shootlong),mean,na.rm=T) # average cpues caught over year and quarter

table(nfish2$year,nfish2$quarter)         # investigate the range of the data to get year quarter combination to plot

par(mfrow=c(1,1))                         # plot data for quarter 3 2007
plotMapBlobs(input=nfish2,what.year=2007,what.quarter=3,what.cpue='cpue.by.n',xlim0=c(-15,15),ylim0=c(48,62),scaling.factor=.5)

## Compare spatial distributions among groups of years

print(sort(unique(nfish2$year)))  # how many years ? adjust mfrow accordingly & remember correct quarter argument

par(mfrow=c(2,2),cex.main=.8)
for(i in 2007:2010){
plotMapBlobs(input=nfish2,what.year=i,what.quarter=3,what.cpue='cpue.by.n',xlim0=c(-2,8),ylim0=c(48,62),scaling.factor=.5) 
}

####################################################################################################################################
## Grid of colors scaled to abundance
####################################################################################################################################

## Note: same thing here. We want to first chose our level of aggregation, sum the numbers or weights over it and then the same for effort dividing one by the other.

fish <- merged.plaice    # Chose the species
#fish <- merged.dab
#fish <- merged.angler
#fish <- merged.cod
#fish <- merged.haddock
#

fish <-fish[!is.na(fish$haullong),]     # sometimes there are incomplete chron records.


grid.size.y <- 0.5 # grid size here is ICES statistical rectangle but this is entirely flexible depending on the resolution of your survey
grid.size.x <- 1  

we <- -16             # Set east and west boundaries for your grid
ea <- 10
so <- 48
no <- 62

par(mfrow=c(1,1))    # number of plots

# Take out dat for 1 year and 1 quarter

table(chrons$year,chrons$quarter) # examine distribution of observations using the chron data

what.year <- 2010
what.quarter <- 3

fish.yq <- fish[fish$quarter== what.quarter & fish$year == what.year,]         # create new dataset with selected year and quarter
chrons.yq <- chrons[chrons$quarter== what.quarter & chrons$year == what.year,] # create new dataset with selected year and quarter

# maps of total weights

wts <-TrawlSurveyGridPlot(fish.yq, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="hlwtatlngt",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats = "topo.colors",addICESgrid=TRUE,legendtitle="weights")

# maps of total numbers

nos <-TrawlSurveyGridPlot(fish.yq, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="hlnoatlngt",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats = "topo.colors",outGridFile="text.txt",addICESgrid=TRUE,legendtitle="numbers")

# map of trawling effort from chron files

effort<-TrawlSurveyGridPlot(chrons.yq, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="hauldur",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats="heat.colors",addICESgrid=TRUE,legendtitle="minutes trawled")


# Cpue using the output from TrawlSurveyGridPlot

im.nos <-  as.image.SpatialGridDataFrame(nos, attr=2)     # extract nos from output of TrawlSurveyGridPlot
im.eff <-  as.image.SpatialGridDataFrame(effort, attr=2)  # extract effort from output of TrawlSurveyGridPlot

im.cpue <- im.nos
im.cpue$z <- im.nos$z/im.eff$z                             # divide catch by effort


data(nseaBathy)    # North Sea
data(nwestBathy)   # North west

library(fields)

image.plot(im.nos$x,im.cpue$y,log(im.cpue$z+1),col=topo.colors(10),xlab='',ylab='',xlim=c(-5,8))                     # plot the data on a map
contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,add=T,level=c(20,40,60),col='black')         # add on depth contour
contour(nwestBathy$x,nwestBathy$y,nwestBathy$z,add=T,level=c(100,200,500,1000,2000,4000),col='black') 

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))



#######################  FURTHER EXERCISES ####################################

 # Explore length frequency data for different species from different datasets downloaded from ICES website. 
 # Compare distributions of fish species from different areas accross Europe.
 
#######################  FURTHER EXERCISES ####################################
 