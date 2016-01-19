
######################################################################################################################################################

############# ICES TRAWL SURVEY AND EVALUATION course #################################################################################################

################# Practical 04 - Exploring trawl data with statistical models  ###########################################################################################################

################## Doug Beare and Dave Reid   ##########################################################################################################


# The aim of this exercise is download ICES exchange format data from www.ices.dk create nos-at-age data and investigate the output by
# various types of interpolation and regression models.  We can then look at the implications of various assumptions.

######################################################################################################################################################
## 1. Install R (http://cran.r-project.org/) packages and Tinn-R   (http://sourceforge.net/projects/tinn-r/).
######################################################################################################################################################

#install.packages('maps');
#install.packages('maptools');
#install.packages('rgdal');
#install.packages('sp');
#install.packages('spatial');
#install.packages('mgcv');

install.packages('akima');

#library(sp);library(mda);library(doBy);library(PBSmapping);library(maps);library(maptools);library(rgdal);library(spatial);library(fields)  # attach to R search path

library(mgcv);library(akima);

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
## 5. Attach datras package and others to search path
######################################################################################################################################################

library(datrasR);library(akima);library(fields);library(mgcv);library(maps);library(mapdata);

######################################################################################################################################################
## 6. Extract the chrons, lf and alk data for the survey of your choice from the relevant directory ##################################################
######################################################################################################################################################

#alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
#lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")
#chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/BTS-TRIDENS-1987-2010")

alk <- parseExchangeFormatALKs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2005-2011")
lfs <- parseExchangeFormatLFs(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2005-2011")
chrons <- parseExchangeFormatChrons(wd="D:/bearedo/Consultancy/TrawlSurveyCourseCopenhagen2011/Practicals/NS-IBTS-2005-2011")

######################################################################################################################################################
## 7. Tidy up the data. This time using the functions tidy** which simply makes things tidier.
######################################################################################################################################################

alk    <- tidyALKdata(input=alk)
lfs    <- tidyLfData(input=lfs)
chrons <- tidyChronData(input=chrons)

lfs$scientific.name[lfs$scientific.name == 'Solea vulgaris'] <- 'Solea solea'      # Change Solea solea  for Solea vulgaris ( I do this here since there is only a and b lw parameters 
alk$scientific.name[alk$scientific.name == 'Solea vulgaris'] <- 'Solea solea'

head(alk) # have a look at the data to make sure they are still there. 
head(lfs)
head(chrons)

##  NOTE: You can ONLY proceed in this Practical with Age data available look at the species with ALK data  ##

sort(unique(alk$scientific.name))

 #[1] "Brosme brosme"              "Clupea harengus"            "Culex impatiens"            "Culex melanurus"            "Culex pinguis"             
# [6] "Culiseta incidens"          "Engraulis encrasicolus"     "Gadus morhua"               "Glyptocephalus cynoglossus" "Limanda limanda"           
#[11] "Lophius budegassa"          "Lophius piscatorius"        "Melanogrammus aeglefinus"   "Merlangius merlangus"       "Merluccius merluccius"     
#[16] "Microstomus kitt"           "Molva molva"                "Mullus barbatus"            "Mullus surmuletus"          "Pleuronectes platessa"     
#[21] "Pollachius virens"          "Psetta maxima"              "Sardina pilchardus"         "Scomber scombrus"           "Scophthalmus rhombus"      
#[26] "Solea solea"                "Sprattus sprattus"          "Trachurus trachurus"        "Trisopterus esmarkii"
#

ww <- grep('Culex', alk$scientific.name)      # There are mosquitoes in the NS IBTS data

alk[ww,][1:5,]


######################################################################################################################################################
## 8. Merge the length-frequencies and chrons
######################################################################################################################################################

what.species <- 'Gadus morhua'  # chose a species
what.species <- 'Pleuronectes platessa'
what.species <- 'Scomber scombrus'
what.species <- 'Melanogrammus aeglefinus'


merged.fish  <- mergeChronsLfs(chrons=chrons,length.frequencies=lfs[lfs$scientific.name==what.species,])

######################################################################################################################################################
## 9. Get the numbers at age at each station. Note all modeling work below will be based on these files.
######################################################################################################################################################

nage <- NosatAgebyYearLonLat(lfdat = merged.fish,alk = alk, chrons=chrons,  what.species = what.species)

head(nage,15) # Examine first 10 lines


#####################################################################################################################################################
## 10. Put on CPUEs at each station #################################################################################################################
#####################################################################################################################################################

nage$cpue.by.n   <- nage$hlnoatage/nage$haulduration
nage$cpue.by.wt  <- nage$hlwtatage/nage$haulduration

#####################################################################################################################################################
## 11. Simple linear interpolation can be handy.
#####################################################################################################################################################
 
 what.age  <- 4  # chose ages
 grid.size <- 40  # chose grid size (larger number = finer grid)
 
 nage.a <- nage[is.na(nage$age) | nage$age == what.age,]    # select age to plot. Remember this is all the data for all the years. 
 
 

summary(nage.a$cpue.by.n) ## check you have any +ve data. Sometimes you just have zeros which makes interpolation tricky!

nage.a <- nage.a[!is.na(nage.a$cpue.by.n),]



inage <- interp(jitter(nage.a$shootlong),jitter(nage.a$shootlat),
xo=seq(min(nage.a$shootlong), max(nage.a$shootlong), length = grid.size),
       yo=seq(min(nage.a$shootlat), max(nage.a$shootlat), length = grid.size),nage.a$cpue.by.n) # do the interpolation

par(mfrow=c(1,1))
image.plot(inage$x,inage$y,log(inage$z+1),col=topo.colors(100),xlab='latititude',ylab='longitude') # plot the nos at age
contour(inage$x,inage$y,log(inage$z+1),add=T)                                                  # contour too
data(nseaBathy)
contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,col='white',levels=c(40,100),add=T)                # depths  too
map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",                         
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))                                                                  # add a map

## IDEAS for EXERCISES 

# Try for different years and ages and species.
# Try increasing the grid resolution. 
# Try adding depth contours instead. 

#####################################################################################################################################################
## 12. Regression modeling where a variable, say CPUE, is 'modeled' as a function of some PREDICTOR variables (lat, lon, depth, time of day etc.).
#####################################################################################################################################################


# In this file (nage) we only potentially have year, quarter, shootlong, shootlat, and age as potential PREDICTOR variables. There are of course 
# many other possibilities, eg. depth, temperature, salinity, and these can be incorporated from the chron files if needs be. For now, however,
# the complexity with even these few PREDICTORS is enough to be vexing as you will see.


#####################################################################################################################################################
## 13. Linear models
#####################################################################################################################################################

# 13.1 Model the CPUE as a function of year and age using linear models

nage$age <- as.factor(nage$age)

table(nage$age)

 # 0   1   2   3   4   5   6   7   8   9  10 
# 20 365 667 643 701 621 575 549 432 307 364 

# Since there are so few age 0s (in this example, chuck em out).

mdat <- nage[nage$age != "0",]    # optional

m0 <- lm(cpue.by.n ~ 1,data=mdat)         # The 'null' model against which others will be selected
m1 <- lm(cpue.by.n ~ year,data=mdat)      # The linear effect of year
m2 <- lm(cpue.by.n ~ year+age,data=mdat)  # plus the linear effect of age
m3 <- lm(cpue.by.n ~ year*age,data=mdat)  # plus the interaction between year and age. Are the lines parallel ?

anova(m0,m1,m2,m3)         # which model would you chose ?
                           
summary(m3)                # what do you think of this model ?

# Create a data set of years and ages over which to demonstrate the results

newdata <- expand.grid(year = min(mdat$year,na.rm=T):max(mdat$year,na.rm=T), age=as.factor(min(as.numeric(as.character(mdat$age)),na.rm=T ):  max(as.numeric(as.character(mdat$age)),na.rm=T )))

# Have a look at the 'new data'

head(newdata)

# Predict over the newdata using the model 

newdata$pred <- predict(m3,newdata=newdata,type='response')

xyplot(pred ~ year | age, data= newdata)  # anything odd ?  Negative predictions ? # Investigate what might be odd patterns. Try again without the age 0s since there are so 

# few observations these are distorting the out


## 13.2 Select a single age group and model the trend with bendy functions

mdat <- nage[nage$age != "0",]

z0 <- lm(cpue.by.n ~ 1,data=mdat)         # The 'null' model against which others will be selected
z1 <- lm(cpue.by.n ~ ns(year,df=5),data=mdat)      # The linear effect of year
z2 <- lm(cpue.by.n ~ ns(year,df=5)+age,data=mdat)  # plus the linear effect of age
z3 <- lm(cpue.by.n ~ ns(year,df=5)*age,data=mdat)  # plus the interaction between year and age. Are the lines parallel ?

# Have a look at the model params 

summary(z3)

# Predict over the same new data using the model 

newdata$pred <- predict(z3,newdata=newdata,type='response')

xyplot(log(pred) ~ year | age, data= newdata)   # Compare the plots output from the different models, ie. substitute z2, for z3.

#####################################################################################################################################################
##  14. Generalized Linear and Additive Models 
#####################################################################################################################################################

# Often data are not normal or log-normal. These models give you the ability to model data from different (discrete) distributions 
# such as the Binomial (Bernoulli), Poisson, etc. With these models the abundance measure (usually a count) should be modeled with 
# the effort measure (hours towed, distance towed as an offset. Beware, however. Just because these models are more sophisticated
# doesn't make them necessarily better. In general you should use the simplest model possible. 

## 14.1 Model the probabability of catching a fish at each age in each year using a binomial GLM. 

mdat      <- nage[nage$age != "0",]        # remove age 0s as usual 
mdat$bin  <- ifelse(mdat$hlnoatage==0,0,1) # create a binary vector (0=0 and 1= anything greater than zero). 

q0 <- glm(bin ~ offset(haulduration)+1,data=mdat,family=binomial)                  # The 'null' model against which others will be selected
q1 <- glm(bin ~ offset(haulduration)+ year,data=mdat,family=binomial)      # The  effect of year
q2 <- glm(bin ~ offset(haulduration)+ year+age,data=mdat,family=binomial)  # plus the  effect of age
q3 <- glm(bin ~ offset(haulduration)+ year*age,data=mdat,family=binomial)  # plus the interaction between year and age. Are the lines parallel ?

anova(q0,q1,q2,q3,test='Chi')    # which model would you chose ??  
 
# We can assess the fit with

    1-pchisq(q3$null.deviance-q3$deviance, q3$df.null-q3$df.residual)
    
# The chi-square of q3$null.deviance-q3$deviance with q3$df.null - q3$df.residual degrees of freedom and an associated p-value of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood).

# Make some predictions with the model 
# Note: now that we have the 'offset' in the model we must add it to the grid. Let's standardise all our predictions to 30 mins

newdata$haulduration <- 1

newdata$pred <- predict(q3,newdata=newdata,type='response')

xyplot(log(pred) ~ year | age, data= newdata, type='l')   # Compare the plots output from the different models, ie. substitute z2, for z3.

#####################################################################################################################################################
## 14.2 Model counts of fish using a GLM. 
#####################################################################################################################################################

mdat      <- nage[nage$age != "0",] # remove age 0s as usual 
mdat$freq <- round(mdat$cpue.by.n)  # make sure numbers are integers. A bit of a fudge this and there are probably better ways...


p0 <- glm(freq ~ offset(haulduration)+1,data=mdat,family=quasipoisson)                  # The 'null' model against which others will be selected
p1 <- glm(freq ~ offset(haulduration)+ year,data=mdat,family=quasipoisson)      # The  effect of year
p2 <- glm(freq ~ offset(haulduration)+ year+age,data=mdat,family=quasipoisson)  # plus the  effect of age
p3 <- glm(freq ~ offset(haulduration)+ year*age,data=mdat,family=quasipoisson)  # plus the interaction between year and age. Are the lines parallel ?

anova(p0,p1,p2,p3,test='Chi')    # which model would you chose ??  
 
# We can assess the fit with

    1-pchisq(p3$null.deviance-p3$deviance, p3$df.null-p3$df.residual)
    
# The chi-square of q3$null.deviance-q3$deviance with q3$df.null - q3$df.residual degrees of freedom and an associated p-value of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood).

# Make some predictions with the model 
# Note: now that we have the 'offset' in the model we must add it to the grid. Let's standardise all our predictions to 30 mins

newdata$haulduration <- 30

newdata$pred <- predict(p3,newdata=newdata,type='response')

xyplot(log(pred) ~ year | age, data= newdata, type='l')   # Compare the plots output from the different models, ie. substitute z2, for z3.

#####################################################################################################################################################
## 14.2 Model counts of fish using a GAM. 
#####################################################################################################################################################

# These models use 'non-parametric' smoothing functions based on the data to model the abundances. Can be handy for summarizing spatial patterns. 

library(mgcv)

mdat      <- nage[nage$age != "0",] # remove age 0s as usual 
mdat$freq <- round(mdat$cpue.by.n)  # make sure numbers are integers. A bit of a fudge this and there are probably better ways...
mdat.a    <- mdat[mdat$age==7,]     # in this case reduce the complexity and model a single age category.

g0 <- gam(freq ~ offset(haulduration)+1,data=mdat.a,family=quasipoisson)                  # The 'null' model against which others will be selected
g1 <- gam(freq ~ offset(haulduration)+ s(shootlong,k=100),data=mdat.a,family=quasipoisson)      # The  effect of longitude
g2 <- gam(freq ~ offset(haulduration)+ s(shootlong,k=100)+s(shootlat,k=100),data=mdat.a,family=quasipoisson)  # plus the  effect of latitude

g3 <- gam(freq ~ offset(haulduration)+ s(shootlong,shootlat,k=25),data=mdat.a,family=quasipoisson)  # plus the interaction between longitude and latitude. Are the lines parallel ?

anova(g0,g1,g2,g3,test='Chi')    # which model would you chose ??  

summary(g3)

 1-pchisq(g3$null.deviance-g3$deviance, g3$df.null-g3$df.residual)
 
# Do the predictions. This time we have to make a spatial grid. 
# Use the datras function create.grid which creates a grid with a flag for whether or not it is outside the survey area. Handy
# since you do not want to extrapolate.

lon.range =  c(-3,8)  # Set the long and lat ranges for the grid depending on survey
lat.range <- c(52,58)
grid.size <- 50       # Set the resolution of the grid 

grid <- create.grid(lon.range=lon.range, lat.range=lat.range, lon.n=grid.size, lat.n=grid.size, lon.obs=mdat.a$shootlong, lat.obs=mdat.a$shootlat)

# Extract some data from the output of create.grid that are handy for plotting.

lonnie <- grid$lonnie
lattie <- grid$lattie
grid <- grid$grid

plot(grid$shootlong,grid$shootlat,type='n',xlab='',ylab='')   # check grid looks ok
points(grid$shootlong[grid$ok.lat==T & grid$ok.lon==T ],grid$shootlat[grid$ok.lat==T & grid$ok.lon==T],col='red',pch='.')
points(chrons$shootlong,chrons$shootlat,pch=".",col='black')


grid$haulduration <- 1 # standardise to 60 minute tows

grid$pred <- predict(g3,grid)
grid$pred[grid$ok.lon==F | grid$ok.lat==F] <- NA

image.plot(lonnie,lattie,matrix(grid$pred,50,50))
contour(lonnie,lattie,matrix(grid$pred,50,50),add=T)
# points(mdat.a$shootlong,mdat.a$shootlat,pch=16)   # put on locations of raw data to see if some of the extrapolation is justified.
map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))

#####################################################################################################################################################
###################### 15. EXERCISES ################################################################################################################ 
#####################################################################################################################################################

# These are the numbers at a specific age modeled for a group of years. The last map therefore represents some sort of 'average' spatial distribution.
# Explore how the average spatial distribution DIFFERS by age.
# Explore how the spatial distribution for single age groups varies among years.
# Note this can be done, either by looking at each age/year combination separately, or by incorporating age and year into the model, modifying the grid
# and predicting over it.  

#lf <- list.files('D:/bearedo/Projects/datras/datras/R')
#for(i in 1:length(lf)){
#source(paste('D:/bearedo/Projects/datras/datras/R/',lf[i],sep=''))}
#
#lf <- list.files('D:/bearedo/Projects/datras/datras/data')
#for(i in 1:length(lf)){
#load(paste('D:/bearedo/Projects/datras/datras/data/',lf[i],sep=''))}
#
