
######################################################################################################################################################

############# ICES TRAWL SURVEY AND EVALUATION course #################################################################################################

################# Practical 03 - Exploring the Age Length Keys ###########################################################################################################

################## Doug Beare and Dave Reid   ##########################################################################################################


# The aim of this exercise is download ICES exchange format data from www.ices.dk, extract and visualise the age length keys and combine them 
# with the length-frequency data and chrons to create nos-at-age indices.  We can then look at the implications of various assumptions.

######################################################################################################################################################
## 1. Install R (http://cran.r-project.org/) packages and Tinn-R   (http://sourceforge.net/projects/tinn-r/).
######################################################################################################################################################

#install.packages('maps');
#install.packages('maptools');
#install.packages('rgdal');
#install.packages('sp');
#install.packages('spatial');
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
## 5. Attach datras package to search path
######################################################################################################################################################

library(datrasR)

######################################################################################################################################################
## 6. Extract the age length, sex, maturity data from the relevant directory
######################################################################################################################################################

#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/RockallData")
alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2010-2011")
#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2005-2011")
#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2001-2011-Q1")
#

head(alk) # have a look at the data

######################################################################################################################################################
## 7. Standardise to cm length categories
######################################################################################################################################################

alk$lngtclass[alk$lngtcode == "."] <- alk$lngtclass[alk$lngtcode == '.']/10
alk$lngtclass[alk$lngtcode == 0] <- alk$lngtclass[alk$lngtcode == 0]/10

#####################################################################################################################################################
## 8. Round down length classes & add 0.5
#####################################################################################################################################################

alk$lngtclass[alk$lngtcode != "5"] <- round(alk$lngtclass[alk$lngtcode != "5"])
alk$lngtclass[alk$lngtcode != "5"] <- alk$lngtclass[alk$lngtcode != "5"]+0.5

print(table(alk$lngtclass)) # print distribution of ALL length categories

print(table(alk$age))      # print distribution of ALL ages

#####################################################################################################################################################
## 9. Put on species name (see Practical 2).  
#####################################################################################################################################################

data(tsn)  # itis database of ALL species in the world

head(tsn)  # first 5 lines

alk$scientific.name <- as.character(tsn$completename[match(alk$speccode,tsn$tsn)])   # match scientific name onto species code 

sort(unique(alk$scientific.name)) # List of species with at least some age data

# Note: you tend to only get more commercially important species being aged etc.

table(alk$scientific.name,alk$age) # print distribution of ages by species (Note: this is for all ages combined).

#####################################################################################################################################################
## 10. Tidy up
#####################################################################################################################################################

alk <- alk[!duplicated(alk),]     #  remove any duplicates

alk <- alk[alk$speccode != -9,]   #  missing species codes

alk <- alk[alk$lngtclass != -9,]  #  missing length classes

alk <- alk[alk$age != -9,]        # missing ages

alk <- alk[!is.na(alk$lngtcode),] #  missing length codes

alk <- alk[!is.na(alk$scientific.name),] # missing species codes

alk$age <- as.numeric(as.character(alk$age))

#####################################################################################################################################################
## 11. Select a species for a particular year and quarter and examine the age length key 
#####################################################################################################################################################

table(alk$year,alk$scientific.name) # investigate range of years covered for each species
table(alk$quarter)                  # see range of quarters

#####################################################################################################################################################
## 12. Chose a species, year and quarter to examine
#####################################################################################################################################################

what.species    <- 'Pleuronectes platessa'   
what.year      <- 2001
what.quarter   <- 3

alk1 <- alk[alk$scientific.name == what.species & alk$year == what.year & alk$quarter == what.quarter,]

alk1t<-table(alk1$lngtclass,alk1$age) # create a table of the distribution of length by ages for the chosen species
 
print(alk1t)                          # have a look at the age distribution by length
 
rowSms <- apply(alk1t,1,sum,na.rm=T) # use apply to calculate row sums

alk1tp <- round(alk1t/rowSms,3)      # divide numbers by total to get proportions

print(alk1tp)                        # have a look at the proportions

#####################################################################################################################################################
###  13. Plot the data.
#####################################################################################################################################################

# Try a barplot

par(mfrow=c(1,1))
barplot(t(alk1tp),space=0,col=1:dim(alk1tp)[[2]],ylab="P",xlab="Length(cms)",
legend.text=dimnames(alk1tp)[[2]],args.legend=list(x='topleft',bg='wheat'))

# Try a matrix plot 

matplot(alk1tp,type='l',ylab='P(age)',xlab="Length(cms)")

# Since data are a matrix why not try a 3D plot ?

alk1tp[alk1tp==0] <- NA
library(fields)
image.plot(as.numeric(dimnames(alk1tp)[[1]]), as.numeric(dimnames(alk1tp)[[2]]),alk1tp,col=topo.colors(100),xlab='Length(cms)',ylab='P(age)')
contour(as.numeric(dimnames(alk1tp)[[1]]), as.numeric(dimnames(alk1tp)[[2]]),alk1tp,add=T)

#####################################################################################################################################################
### 14. Apply ALK to Length frequency data ###
#####################################################################################################################################################

# Simplest approach is to multiply the estimate of the proportion of each age at each length by the number of individuals at that length caught. If you
# do this you must ensure that you have a length in your ALK available for the corresponding length in your length frequency data.

# Get the length frequency data from the relevant survey

#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/RockallData")
lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2010-2011")
#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2001-2011-Q1")
#
#  Again make sure to add on scientific name using TSN (downloaded from ITIS website). 

data(tsn)  # itis database of all species in the world

head(tsn)  # first 5 lines

lfs$scientific.name <- as.character(tsn$completename[match(lfs$speccode,tsn$tsn)])   # match scientific name 

table(lfs$scientific.name)        # examine species available with length-frequencies. 

# Tidy up

lfs <- lfs[!duplicated(lfs),]                  #  remove any duplicates
lfs <- lfs[lfs$speccode != -9,]                #  remove missing species codes
lfs <- lfs[lfs$lngtclass != -9,]               #  remove missing length classes
lfs <- lfs[!is.na(lfs$lngtclass),]             #  remove missing length classes
lfs <- lfs[!is.na(lfs$lngtcode),]              #  remove missing length codes
lfs <- lfs[!is.na(lfs$scientific.name),]       #  remove any missing scientific names 
lfs$hlnoatlngt <- lfs$hlnoatlngt*lfs$subfact   #  multiply by the subfactor


# Standardise length codes to cms (some are in mms).

lfs$lngtclass[lfs$lngtcode == "."] <- lfs$lngtclass[lfs$lngtcode == '.']/10
lfs$lngtclass[lfs$lngtcode == "0"] <- lfs$lngtclass[lfs$lngtcode == "0"]/10

lfs$lngtclass[lfs$lngtcode != "5"] <- round(lfs$lngtclass[lfs$lngtcode != "5"])  # Don't do this to fish binned at 5cm intervals
lfs$lngtclass[lfs$lngtcode != "5"] <- lfs$lngtclass[lfs$lngtcode != "5"]+0.5


# Useful to insert a weight for each length category for each species.
# Remember that this is usually done with parameters (a & b) from non-linear equations where
# Weight in grammes = aL^b  where L = length.

data(length.weight)

# Match a and b parameters onto the correct species

lfs$a <- length.weight$a[match(lfs$scientific.name,length.weight$scientific.name)]
lfs$b <- length.weight$b[match(lfs$scientific.name,length.weight$scientific.name)]

# Use the parameters to estimate the weight in kgs.  Remember that weight = aL^b

lfs$hlwtatlngt<-((lfs$a*lfs$lngtclass^lfs$b)*lfs$hlnoatlngt) /1000

## 15. Extract a species to 'play' with. Remember that this should be one for which age length keys are available.

#turbot <- lfs[lfs$scientific.name == 'Psetta maxima',]
plaice <- lfs[lfs$scientific.name == 'Pleuronectes platessa',]
#dab    <- lfs[lfs$scientific.name == 'Limanda limanda',]
#brill  <- lfs[lfs$scientific.name == 'Scophthalmus rhombus',]

#####################################################################################################################################################
# 16. Get the chrons (e.g. Practical01) because it's easier to use them to estimate effort at different spatial levels of aggregation.
#####################################################################################################################################################

#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/RockallData")
chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")

#  Usual 'tidy up' procedure.

chrons <- chrons[chrons$haulval=='V',]          # only valid hauls
chrons <- chrons[!duplicated(chrons),]          # chuck any duplicates

#####################################################################################################################################################
# 17. Make sure we have zeros in the dataset for stations that were trawled but no individuals caught.
#####################################################################################################################################################

#merged.haddock <- mergeChronsLfs(chrons=chrons,length.frequencies=haddock)
merged.plaice  <- mergeChronsLfs(chrons=chrons,length.frequencies=plaice)

#fish <- merged.haddock  # chose species here.
fish <- merged.plaice 

#####################################################################################################################################################
## 18. Calculate a very simple index to see the principles using the year quarter combination selected above.
#####################################################################################################################################################

lf1 <- fish[fish$year == what.year & fish$quarter == what.quarter,]  # select dat for what.year and what.quarter

head(lf1)

lf2 <- tapply(lf1$hlnoatlngt,lf1$lngtclass,sum,na.rm=T) # the total sum of individuals caught on that survey 

print(lf2) #Examine the data

# See if the available lengths correspond to the available aged lengths

length(lf2)
dim(alk1tp)

alk2tp <- data.frame(alk1tp)
dimnames(alk2tp)[[2]] <- c("lngtclass","age","p")   # Convert to a dataframe which can make life easier (relational database format)

alk2tp$lngtclass <- as.numeric(as.character(alk2tp$lngtclass))  # Make 'factors' into 'characters'
alk2tp$age <- as.numeric(as.character(alk2tp$age))              # 


head(alk2tp)

# Match the length frequencies with the lengths

alk2tp$hlnoatlngt <- lf2[match(alk2tp$lngtclass,as.numeric(dimnames(lf2)[[1]]))]
alk2tp$no.at.age  <- alk2tp$hlnoatlngt * alk2tp$p         # multiply the frequencies by the proportions
alk2tp$no.at.age[is.na(alk2tp$no.at.age)] <- 0            # replace Nas with zeros

# create the most straightforward index possible for that survey

index <- aggregate(list(no.at.age=alk2tp$no.at.age),list(age=alk2tp$age),sum,na.rm=T)

# Plot the index for the year selected.

par(mfrow=c(1,1))
plot(index$age,index$no.at.age,ylab='nos at age', xlab='age',type='b',lwd=1)  # Linear

plot(index$age,log(index$no.at.age+1),ylab='nos at age', xlab='age',type='b')   # Log scale
abline(lm(log(no.at.age+1)~age,data=index),lty=3,col='blue')

#####################################################################################################################################################
## 19. Now create a dataset of numbers by age per ICES statistical rectangle using the function NosatAgebyYearStatSquare
#####################################################################################################################################################

 #out <- NosatAgebyYearStatSquare(lfdat = merged.haddock,alk = alk, chrons=chrons,  what.species ='Melanogrammus aeglefinus') 
 
 out <- NosatAgebyYearStatSquare(lfdat = merged.plaice,alk = alk, chrons=chrons,  what.species ='Pleuronectes platessa') 
 
 print(head(out))
  
  
  # Plot the nos by year age and stat rectangle.
  
  library(lattice)
   
  xyplot(log(hlnoatage)~year|age,groups=statrec,type='l',data=out)
  
#####################################################################################################################################################    
## 20. Compare indices calculated in different ways
#####################################################################################################################################################
   
   # total numbers divided by total effort
   
     a <- aggregate(list(hlnoatage=out$hlnoatage), list(age=out$age,year=out$year),sum,na.rm=T)   # sum the total numbers
     hd <- aggregate(list(haulduration=chrons$hauldur/60), list(year=chrons$year),sum,na.rm=T)            # sum the effort 
     
     b0 <- merge(a,hd,all=T)                        # merge
     b0$cpue <- b0$hlnoatage/b0$haulduration        # calculate cpue
     b0 <- b0[!is.na(b0$hlnoatage),]
     
     print(head(b0)) # examine the data b0
   
   # versus AVERAGE of the separate cpue in each statistical rectangle
   
   out$cpue <- out$hlnoatage/out$haulduration
   
    b1 <- aggregate(list(cpue=out$cpue), list(age=out$age,year=out$year),mean,na.rm=T)
     
     print(head(b1))
  
   # Plot them both to compare
   
         par(mfrow=c(3,4),mar=c(2,2,3,2))     # This bit just finds the range of the data for plotting
         u.ages <- sort(unique(b0$age))
         r0 <- tapply(b0$cpue,b0$age,range)
         r1 <- tapply(b1$cpue,b1$age,range)
         r2 <- as.list(u.ages)
         for (i in u.ages){
         r2[[i+1]] <- range(c(r0[as.character(i)],r1[as.character(i)])) 
         }                                   
         
    # Do the plotting
         
         for(i in u.ages) {
      plot(b0$year[b0$age==i],b0$cpue[b0$age==i],type='l',xlab='',ylab='',ylim=r2[[i+1]])
      lines(b1$year[b1$age==i],b1$cpue[b1$age==i],type='l',col='blue')
      title(paste('age',i,sep=' '),cex.main=1.2)    }
      
#####################################################################################################################################################
## 20. Now create a dataset of numbers by age each station (eg. by lat and long) using the function NosatAgebyYearLonLat
#####################################################################################################################################################   
 
out <- NosatAgebyYearLonLat(lfdat = merged.plaice,alk = alk, chrons=chrons,  what.species ='Pleuronectes platessa') 
  
out[1:5,]                                        # First 5 lines

out$cpue.by.n <- out$hlnoatage/out$haulduration  # add on CPUE 
out$cpue.by.wt <- out$hlwtatage/out$haulduration
 
#####################################################################################################################################################
## 21. Plot data by age
#####################################################################################################################################################   

## On a bubble plots ##

 par(mfrow=c(1,1))
 plotMapBlobs(out[out$age==3,],what.quarter=3,what.year = 2007,what.cpue='cpue.by.n',xlim0=c(-16,12),ylim0=c(45,62),scaling.factor=.5)
  
##   Or a grid        ##
 
grid.size.y <- .5                                                # grid size here is ICES statistical rectangle but this is entirely flexible
grid.size.x <- 1  

we <- -16                                                        # east and west boundaries
ea <- 10
so <- 48
no <- 62

par(mfrow=c(1,1))                                                # number of plots

out3 <- out[is.na(out$age) | out$age==3 & out$year ==2007,]


nos <-TrawlSurveyGridPlot(out3, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="hlnoatage",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats = "topo.colors",addICESgrid=TRUE,legendtitle="nos")
 
effort <-TrawlSurveyGridPlot(out3, we=we,ea=ea,so=so,no=no,nameLon = "shootlong", nameLat = "shootlat",plotMap=T,
nameVarToSum="haulduration",cellsizeX = grid.size.x, cellsizeY = grid.size.y,
legendx='bottomright',numcats=10,plotPoints=T,legendncol=2,paletteCats = "topo.colors",addICESgrid=TRUE,legendtitle="haul duration")
      
im.nos <-  as.image.SpatialGridDataFrame(nos, attr=2)      # extract nos from output of TrawlSurveyGridPlot
im.eff <-  as.image.SpatialGridDataFrame(effort, attr=2)   # extract effort from output of TrawlSurveyGridPlot

im.cpue <- im.nos
im.cpue$z <- im.nos$z/im.eff$z                             # divide catch by effort

data(nseaBathy)                                            # North Sea
data(nwestBathy)                                           # North west

library(fields)

image.plot(im.nos$x,im.cpue$y,log(im.cpue$z+1),col=topo.colors(10),xlab='',ylab='')                     # plot the data on a map
contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,add=T,level=c(20,40,60),col='black')         # add on depth contour
#contour(nwestBathy$x,nwestBathy$y,nwestBathy$z,add=T,level=c(100,200,500,1000,2000,4000),col='black') 

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))



######################################################################################################################################################
#### Multi-nomial logit modeling of age length keys. The problem is that the data are packed full of missing lengths at various categories.
# The problem with the simple arithmetic approach above is that there are often no data available. There may be instances where data can be modeled
# over missing length categories to provide more information.
#######################################################################################################################################################


## Install the library 

library(datrasR)

## Get the data

#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/RockallData")

alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")

#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2010-2011")

head(alk) # have a look at the data

#Note: There is something wrong with the French IBTS data? 
#alk$lngtclas[alk$ship == 'THA2' & alk$year ==2006] <- alk$lngtclas[alk$ship == 'THA2' & alk$year ==2006]/10

## Standardise to cm length categories

alk$lngtclass[alk$lngtcode == "."] <- alk$lngtclass[alk$lngtcode == '.']/10
alk$lngtclass[alk$lngtcode == 0]   <- alk$lngtclass[alk$lngtcode == 0]/10

## Round it down

alk$lngtclass <- round(alk$lngtclass)

## Put on species name (see Practical 2).  

data(tsn)  # itis database of ALL species in the world

head(tsn)  # first 5 lines

alk$scientific.name <- as.character(tsn$completename[match(alk$speccode,tsn$tsn)])   # match scientific name onto species code 

unique(alk$scientific.name)# List of species with at least some age data

## e.g. "Melanogrammus aeglefinus" "Pollachius virens" "Gadus morhua" "Merlangius merlangus" 

# Note: you tend to only get more commercially important species being aged etc.

##  Tidy up

alk <- alk[!duplicated(alk),]     #  remove any duplicates

alk <- alk[alk$speccode != -9,]   #  missing species codes

alk <- alk[alk$lngtclass != -9,]  #  missing length classes

alk <- alk[!is.na(alk$lngtcode),] #  missing length codes

alk$age <- as.numeric(as.character(alk$age))

## Select a species for a particular year and quarter and examine the age length key 

table(alk$year,alk$scientific.name) # investigate range of years covered for each species
table(alk$quarter) # see range of quarters

## Chose a species, year and quarter to examine

what.species    <- 'Gadus morhua' # These are examples.

alk1 <- alk[alk$scientific.name == what.species,]


#Extract the columns of interest

alk2 <- data.frame(lngtclass=alk1$lngtclass,age=alk1$age,scientific.name = alk1$scientific.name)

xtabs(~age+lngtclass,data=alk2)     # examine the data


# install.packages('mlogit') # Install package if necessary

library(mlogit)

mldata <- mlogit.data(alk2,varying=NULL,choice='age',shape='wide')

mlogit.model<- mlogit(age~1|lngtclass, data = mldata, reflevel="1")

summary(mlogit.model)

#Call:
#mlogit(formula = age ~ 1 | lngtclass, data = mldata, reflevel = "1", 
#    method = "nr", print.level = 0)
#
#Frequencies of alternatives:
#         1          0          2          3          4          5          6          7 
#0.47521866 0.07215743 0.31122449 0.10641399 0.02660350 0.00583090 0.00182216 0.00072886 
#
#nr method
#11 iterations, 0h:0m:3s 
#g'(-H)^-1g = 1.22E-07 
#gradient close to zero 
#
#Coefficients :
#                  Estimate  Std. Error  t-value  Pr(>|t|)    
#alt0            13.9517399   1.2361844  11.2861 < 2.2e-16 ***
#alt2            -6.5674373   0.2890674 -22.7194 < 2.2e-16 ***
#alt3           -10.0993640   0.3917688 -25.7789 < 2.2e-16 ***
#alt4           -15.8707748   0.7141366 -22.2237 < 2.2e-16 ***
#alt5           -18.9503112   1.2929005 -14.6572 < 2.2e-16 ***
#alt6           -19.8364277   2.1063412  -9.4175 < 2.2e-16 ***
#alt7           -21.9500391   3.3799632  -6.4942 8.350e-11 ***
#alt0:lngtclass  -0.8576694   0.0749124 -11.4490 < 2.2e-16 ***
#alt2:lngtclass   0.1988813   0.0092909  21.4060 < 2.2e-16 ***
#alt3:lngtclass   0.2625404   0.0112776  23.2798 < 2.2e-16 ***
#alt4:lngtclass   0.3558634   0.0159991  22.2427 < 2.2e-16 ***
#alt5:lngtclass   0.3840026   0.0241738  15.8851 < 2.2e-16 ***
#alt6:lngtclass   0.3791767   0.0374518  10.1244 < 2.2e-16 ***
#alt7:lngtclass   0.3995615   0.0557045   7.1729 7.343e-13 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Log-Likelihood: -2225.3
#McFadden R^2:  0.37045 
#Likelihood ratio test : chisq = 2618.9 (p.value=< 2.22e-16)
#
#

#  Calculate the logit values

    newdata <- data.frame(lngtclass = 1:90)
    
    logit0 <- 13.9517 +  -0.8576*newdata$lngtclass
    logit1 <- rep(0, 70)
    logit2 <- -6.5674 +   0.1988*newdata$lngtclass
    logit3 <- -10.099 +   0.2625*newdata$lngtclass
    logit4 <- -15.8707 +  0.3558*newdata$lngtclass
    logit5 <- -18.9503 +  0.3840*newdata$lngtclass
    logit6 <- -19.8364 +  0.3791*newdata$lngtclass
    logit7 <- -21.9500 +  0.3995*newdata$lngtclass
    

# We can exponentiate these logit values and scale them, knowing that the probabilities of for all the ages must sum to one.


    logits <- cbind(logit0, logit1, logit2, logit3,logit4,logit5,logit6,logit7)
    p.unscaled <- exp(logits)
    p <- cbind(newdata, (p.unscaled / rowSums(p.unscaled)))
    colnames(p) <- c("lngtclass", "age0", "age1", "age2", "age3","age4","age5","age6","age7")
    apply(p[,-1],1,sum) #check
    
    np <- p[,-1]

par(mfrow=c(1,1),mar=c(5,5,3,3))
barplot(t(np),space=0,col=1:dim(np)[[2]],ylab="P",xlab="Length(cms)",
legend.text=dimnames(np)[[2]],args.legend=list(x='topleft',bg='wheat'))
    

# This basic approach can be extended to incorporate other 'predictor' variables, e.g. sex.

# As an exercise see if you can expand the model to do this and apply the predicted or modeled ALKs to the appropriate length-frequency data.


alk2 <- data.frame(lngtclass=alk1$lngtclass,age=alk1$age,sex=alk1$sex)

# Just males and females ano no missing data

alk3 <- alk2[alk2$sex %in% c('M','F'),]
alk3$sex <- ifelse(alk3$sex =="M",0,1)


xtabs(~age+lngtclass+sex,data=alk3)     # examine the data

mldata <- mlogit.data(alk3,varying=NULL,choice='age',shape='wide')

mlogit.model<- mlogit(age~1|lngtclass + sex, data = mldata, reflevel="1")

summary(mlogit.model)

 
