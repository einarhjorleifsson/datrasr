lf <- list.files('D:/bearedo/Projects/datras/datras/R')
for(i in 1:length(lf)){
source(paste('D:/bearedo/Projects/datras/datras/R/',lf[i],sep=''))}

lf <- list.files('D:/bearedo/Projects/datras/datras/data')
for(i in 1:length(lf)){
load(paste('D:/bearedo/Projects/datras/datras/data/',lf[i],sep=''))}



######################################################################################################################################################
############# ICES TRAWL SURVEY AND EVALUATION course #################################################################################################
################# Practical 05 - Simple Community Analyses  ###########################################################################################################
################## Doug Beare and Dave Reid   ##########################################################################################################
######################################################################################################################################################

# The aim of this exercise is download ICES exchange format data from www.ices.dk and calculate various types of fish community indicators. 

######################################################################################################################################################
## 1. Install R (http://cran.r-project.org/) packages and Tinn-R   (http://sourceforge.net/projects/tinn-r/).
######################################################################################################################################################

#install.packages('maps');
#install.packages('maptools');
#install.packages('rgdal');
#install.packages('sp');
#install.packages('spatial');
#install.packages('gam');
#install.packages('akima');
install.packages('vegan');

library(sp);library(mda);library(doBy);library(PBSmapping);library(maps);library(maptools);library(rgdal);library(spatial);library(fields)  # attach to R search path

library(vegan);
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
## 5. Attach datras package ano others to search path
######################################################################################################################################################

#detach(package:datrasR)  # do this if you want to detach the old one.

library(datrasR);

######################################################################################################################################################
## 6. Extract the chrons, lf data from the relevant directory
######################################################################################################################################################

lfs    <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")

#lfs    <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2001-2011-Q1")
#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2001-2011-Q1")

######################################################################################################################################################
## 7. Tidy up the data. This time using the functions tidy** which simply makes things tidier.
######################################################################################################################################################

data(tsn)
data(length.weight)
data(DatrasSpeciesCodes)

lfs    <- tidyLfData(input=lfs)
chrons <- tidyChronData(input=chrons)

lfs$scientific.name[lfs$scientific.name == 'Solea vulgaris'] <- 'Solea solea'      # Change Solea solea  for Solea vulgaris ( I do this here since there is only a and b lw parameters

##  have a look at the data to make sure they are still there

head(lfs)
head(chrons)

# Set the temporal range of your data

start.year <- min(chrons$year,na.rm=T)
end.year   <- max(chrons$year,na.rm=T)

sort(unique(lfs$scientific.name))

## Throw out any non-fish or keep them in if you want. You decide.

species.to.omit <- c("Aequipecten opercularis", "Arctica islandica" , "Buccinum undatum", "Cancer pagurus" ,"Homarus gammarus",
 "Loligo subulata", "Loligo vulgaris",  "Loliginidae" ,"Loligo" ,"Loligo forbesii","Pecten maximus" ,
  "Nephrops norvegicus" ,"Macropipus puber",  "Cephalopoda"  ,"Illex coindetii", "Sepia officinalis","Sepiola","Sepiola atlantica", "Sepiolida","Sepietta owenina","Sepia")
  
w0 <- (1:length(lfs[,1]))[lfs$scientific.name %in% species.to.omit]
  
nlfs <- lfs[-w0,]              # Create new data.frame with species that you selected thrown out

######################################################################################################################################################
## 8. Merge the length-frequencies with the chrons.
######################################################################################################################################################

## Make a shorter chron file with some important covariates to make computation quicker

short.chrons<- data.frame(quarter=chrons$quarter,country=chrons$country,ship=chrons$ship,stno=chrons$stno,
year=chrons$year,shootlong=chrons$shootlong,shootlat=chrons$shootlat,
hauldur=(chrons$hauldur/60))

memory.size(4000)   # this can help sometimes with big jobs.

afish <- merge(nlfs,short.chrons,all=T)

######################################################################################################################################################
## 9. Total numbers of fish divide by the total effort.
######################################################################################################################################################

afish.yr <- tapply(afish$hlnoatlngt, list(afish$year,as.character(afish$scientific.name)), sum,na.rm=T)
hd <- tapply(short.chrons$hauldur, list(short.chrons$year),sum,na.rm=T)

dat <- matrix(NA,nrow=dim(afish.yr)[1],ncol=dim(afish.yr)[2])
for (i in 1: dim(afish.yr)[2]){
dat[,i] <- afish.yr[,i]/hd      # divide by effort
}


######################################################################################################################################################
## 10. Calculate diversity indices using package vegan
######################################################################################################################################################

dat <- ifelse(is.na(dat),0,dat) # replace NAs with 0s

dat<-round(dat)
library(vegan)

# Calculate and plot the Diversity

H <- diversity(dat)
plot(start.year:end.year,H,type='l',xlab='',lwd=2)
abline(v=start.year:end.year,lty=2,col='blue')


# Calculate and plot the Simpson
simp <- diversity(dat, "simpson");
par(mfrow=c(1,1))
plot(start.year:end.year,simp,type='l',xlab='',lwd=2)
abline(v=start.year:end.year,lty=2,col='blue')

# Calculate and plot the Inverse Simpson

invsimp <- diversity(dat, "inv");
plot(start.year:end.year,invsimp,type='l',xlab='',lwd=2)
abline(v=start.year:end.year,lty=2,col='blue')

# Calculate and plot the r2

r.2 <- rarefy(dat, 2)

plot(start.year:end.year,r.2,type='l',xlab='',lwd=2)
abline(v=start.year:end.year,lty=2,col='blue')

# Alpha 

alpha <- fisher.alpha(dat)

plot(start.year:end.year,alpha,type='l',xlab='',lwd=2)
abline(v=start.year:end.year,lty=2,col='blue')

# Plot them all against each other

pairs(cbind(H, simp, invsimp,r.2), pch="+", col="blue")   # They're all so correlated !

## Species richness (S) and Pielou's evenness (J):

S <- specnumber(dat) 

J <- H/log(S)

dat1<-cbind(H, simp, invsimp, r.2, S, J)

# Plot them all together

par(mfrow=c(2,3),mar=c(3,3,3,2),oma=c(4,4,4,4),cex=.9)
tit <- c("H","simp","invsimp","r.2","S","J")
tit <- c("Shannon","Simp","Invsimp","r.2","S","J")
y.labs <- c("H","Simp","Inv","r.2","S","J")
atime <- start.year:end.year
for(j in 1:6){
plot(atime,(dat1[,j]),xaxt="n",,xlab="",type='b',pch=16,ylab=y.labs[i]);title(tit[j]);
lines(supsmu(atime,(dat1[,j]),span=0.5), col='red',lwd=3,xlim=c(start.year,end.year))
box(lwd=2)
abline(v=seq(start.year,end.year,by=5),lty=2,col="blue")
abline(v=c(1989,1994),lwd=2,col='black')
axis(side=1,at=seq(start.year,end.year,by=1),seq(start.year,end.year,by=1),cex.axis=1)

mtext(outer=T,paste('Piscean Diversity',sep=' '),cex=1.7)
}

### Create a plot to insert into a Word Document ### 

savePlot(filename = "Piscean-diversity.wmf", type=c("wmf"), device=dev.cur())

#######################################################################################################################
## 11. Some Simple Indices based on Average lengths####################################################################
#######################################################################################################################

### Average length by year and scientific name. Note you may want to include seasonal information (quarter) if available in your survey. ###

avl <- aggregate(list(a=(nlfs$lngtclass*nlfs$hlnoatlngt),b=nlfs$hlnoatlngt), list(year=nlfs$year,scientific.name=nlfs$scientific.name),sum,na.rm=T)

avl$mean.length <-avl$a/avl$b

print(sort(unique(as.character(avl$scientific.name))))

which.species.to.plot <- sort(c("Zeus faber","Solea solea","Gadus morhua" ,  "Platichthys flesus", "Psetta maxima",  "Pollachius virens", "Hippoglossoides platessoides", 
"Lepidorhombus whiffiagonis", "Mustelus mustelus","Pleuronectes platessa","Melanogrammus aeglefinus","Scyliorhinus canicula","Squalus acanthus"))

xyplot(mean.length~year|scientific.name,data=avl[avl$scientific.name %in% which.species.to.plot,],type='l')

### Average demersal fish length ### 

# Would be ridiculous for me to 'hardwire' these choices into any code. Far better for you to make choices based on what you want.

group.to.omit <- c('Alosa agone','Alosa alosa','Alosa fallax','Ammodytes','Ammodytes marinus','Ammodytes tobianus', 'Ammodytidae',
'Clupea harengus','Engraulis encrasicolus','Scomber scombrus','Sardina pilchardus')    # Easier to omit the pelagics

w1 <- (1:length(nlfs[,1]))[nlfs$scientific.name %in% group.to.omit]                    # Find where the omitted group are.
  
nlfs0 <- nlfs[-w1,]                                                                    # Create new data.frame with species that you selected thrown out

avl <- aggregate(list(a=(nlfs0$lngtclass*nlfs0$hlnoatlngt),b=nlfs0$hlnoatlngt),
list(year=nlfs0$year),sum,na.rm=T)                                     # Average length over year.

avl$mean.length <-avl$a/avl$b
                                                                        # Plot average length against year for group chosen
par(mfrow=c(1,1))
plot(start.year:end.year, avl$mean.length,type='l',xlab='',ylab='')
abline(v=start.year:end.year,lty=2,col='blue')


#############################################################################################################################################
#############12. Large Fish Indicator########################################################################################################
#############################################################################################################################################

## Note: here we calculate LFI by weight. Substitute hlwtatlngt to calculate numbers.

how.big <- 40 #   Set your large fish threshold

big.fish <- afish[!is.na(afish$lngtclass) & afish$lngtclass >= how.big ,]

## By year ##

afish.yr <- tapply(afish$hlwtatlngt, list(afish$year), sum,na.rm=T)
bfish.yr <- tapply(big.fish$hlwtatlngt, list(big.fish$year), sum,na.rm=T)

lfi     <- bfish.yr/afish.yr

par(mfrow=c(1,1))
plot(start.year:end.year,lfi,xlab='',ylab='',type='l',lwd=2)
abline(v=start.year:end.year,lty=3,col='blue')
lines(supsmu(start.year:end.year,lfi),col='red')
title('Large Fish Indicator')


## Spatially ##

## Sum total weights over year and location 
afish.xy <- aggregate(list(all.hlwt=afish$hlwtatlngt), list(shootlong=afish$shootlong,shootlat=afish$shootlat,year=afish$year), sum,na.rm=T)

## Sum the weights of big fish over year and location

bfish.xy <- aggregate(list(big.hlwt=big.fish$hlwtatlngt), list(shootlong=big.fish$shootlong,shootlat=big.fish$shootlat,year=big.fish$year), sum,na.rm=T)

## Plot it on a grid/map using TrawlSurveyGridPlot ##

grid.size.y <- .5                                                # grid size here is ICES statistical rectangle but this is entirely flexible
grid.size.x <- 1  

we <- -5                                                        # east and west boundaries
ea <- 10
so <- 48
no <- 62

par(mfrow=c(1,1))                                                # number of plots

awt <-TrawlSurveyGridPlot(afish.xy, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="all.hlwt",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats = "topo.colors",addICESgrid=TRUE,legendtitle="weight")

bwt <-TrawlSurveyGridPlot(bfish.xy, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="big.hlwt",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats = "topo.colors",outGridFile='test.txt',addICESgrid=TRUE,legendtitle="weight")


im.awt <-  as.image.SpatialGridDataFrame(awt, attr=2)      # extract total weights from output of TrawlSurveyGridPlot
im.bwt <-  as.image.SpatialGridDataFrame(bwt, attr=2)      # extract weights of big fish from output of TrawlSurveyGridPlot

im.lfi <- im.awt
im.lfi$z <- im.bwt$z/im.awt$z                              # divide weight of the big fish by weight of all the fish

data(nseaBathy)                                            # North Sea
data(nwestBathy)                                           # North west

library(fields)

image.plot(im.lfi$x,im.lfi$y,im.lfi$z,col=topo.colors(10),xlab='',ylab='')                     # plot the data on a map

contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,add=T,level=c(20,40,60),col='black')         # add on depth contour
#contour(nwestBathy$x,nwestBathy$y,nwestBathy$z,add=T,level=c(100,200,500,1000,2000,4000),col='black') 

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))

######################################################################################################################################################
######################################################################################################################################################










#######################################################################################################################################################

## Exercises ## 

## As usual explore the indices among different surveys and groups of years
## Calculate the large fish index






#lf <- list.files('D:/bearedo/Projects/datras/datras/R')
#for(i in 1:length(lf)){
#source(paste('D:/bearedo/Projects/datras/datras/R/',lf[i],sep=''))}
#
#lf <- list.files('D:/bearedo/Projects/datras/datras/data')
#for(i in 1:length(lf)){
#load(paste('D:/bearedo/Projects/datras/datras/data/',lf[i],sep=''))}
