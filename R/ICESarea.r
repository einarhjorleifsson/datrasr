# function to assign a ICES area code to each ping
# based on a simplified version of the ICES_areas shape file
# compatible area code when uploading to fishframe

 #hello world
#' @title XXX
#'
#' @description XXX
#'
#' @export
 ICESarea <- function(chrons,roman=F){

    library(sp)

    ICES.area <- rep(NA, dim(chrons)[1])  # init

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(68.5,30.7,26,26,30,30,68.5),
              pol.y=c(63,63,70.648,72,72,90,90))>0] <- ifelse(roman,"Ib","1b")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(33.7,34.55,35.28,36.38,37.57,
              38.31,39.05,39.61,41.24,42.81,43.06,44.68,43.51,43.18,41.73,41.56,40.66,40.51,39.76,38.96,37.74,36.61,35.70,33.70),
              pol.y=c(73.98,74.18,74.36,74.71,75.14,75.45,75.84,76.26,76.61,76.90,76.9,76.75,75.99,75.39,74.82,73.98,73.17,72.20,
              72.26,72.62,73.04,73.37,73.56,73.98))>0] <- ifelse(roman,"Ia","1a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-0.2,7.21,7.28,7.83,8.65,9.33,9.83,
              10.29,9.94,9.7,8.75,7.93,7.42,6.73,5.64,5.01,4.74,4.32,4,3.73,3.57,3.4,3.27,3.19,3.16,3.15,3.18,3.24,3.31,
              3.42,3.27,3.18,2.79,2.24,1.79,1.44,1.26,0.72,0.04,-0.489,-1.309,-1.559,-2.169,-2.539,-3.189,-3.729,-4.189,
              -4.559,-5.579,-5.599,-5.669,-5.779,-6.249,-6.619,-5.329,-4.189,-3.419,-2.389,-1.559,-0.609,0.08,0.68,1.18,
              1.46,1.72,1.94,2.09,2.25,2.35,2.39,2.38,2.31,2.22,2.06,1.89,1.68,1.48,1.08,0.34,-0.199), pol.y=c(73.5,73.5,
              73.45,73.14,72.76,72.49,72.31,72.18,71.98,71.91,71.64,71.36,71.13,70.79,70.17,69.79,69.56,69.32,69.1,68.86,
              68.69,68.46,68.23,67.98,67.77,67.57,67.37,67.18,67.01,66.84,66.43,66.39,66.23,65.95,65.64,65.38,65.32,65.08,
              64.72,64.43,64.84,64.92,65.13,65.22,65.39,65.47,65.55,65.59,65.69,65.96,66.22,66.47,67.09,67.61,67.77,67.96,
              68.1,68.33,68.55,68.86,69.14,69.44,69.76,69.97,70.21,70.43,70.63,70.89,71.14,71.35,71.61,71.83,72.01,72.24,
              72.43,72.6,72.75,72.99,73.31,73.5))>0] <- ifelse(roman,"IIa1","2a1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-11,-0.2,0.34,1.08,1.48,1.68,
              1.89,2.06,2.22,2.31,2.38,2.39,2.35,2.25,2.09,1.94,1.72,1.46,1.18,0.68,0.08,-0.609,-1.559,-2.389,-3.419,-4.189,
              -5.329,-6.619,-6.249,-5.779,-5.669,-5.599,-5.579,-4.559,-4.189,-3.729,-3.189,-2.539,-2.169,-1.559,-1.309,
              -0.489,0.04,0.72,1.26,1.44,1.79,2.24,2.79,3.18,3.27,3.42,3.31,3.24,3.18,3.15,3.16,3.19,3.27,3.4,3.57,3.73,
              4,4.32,4.74,5.01,5.64,6.73,7.42,7.93,8.75,9.7,9.94,10.29,9.83,9.33,8.65,7.83,7.28,7.21,30,30,26,26,11,-4,-4),
              pol.y=c(63,73.5,73.5,73.31,72.99,72.75,72.6,72.43,72.24,72.01,71.83,71.61,71.35,71.14,70.89,70.63,70.43,70.21,
              69.97,69.76,69.44,69.14,68.86,68.55,68.33,68.1,67.96,67.77,67.61,67.09,66.47,66.22,65.96,65.69,65.59,65.55,
              65.47,65.39,65.22,65.13,64.92,64.84,64.43,64.72,65.08,65.32,65.38,65.64,65.95,66.23,66.39,66.43,66.84,67.01,
              67.18,67.37,67.57,67.77,67.98,68.23,68.46,68.69,68.86,69.1,69.32,69.56,69.79,70.17,70.79,71.13,71.36,71.64,
              71.91,71.98,72.18,72.31,72.49,72.76,73.14,73.45,73.5,73.5,72,72,69.8,62,62,63))>0] <- ifelse(roman,"IIa2","2a2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(7.21,-0.2,-0.48,-1.88,-2.7,-5,-4.38,
             -4.29,-4.19,-4.3,-4.09,-2.52,-2.1,-1.6,0.8,1.12,1.71,3.06,4.07,4.55,5.19,6.39,6.51,6.74,7.06,7.21), pol.y=c(73.5,
             73.5,73.6,73.94,74.09,74.21,74.5,75,75.3,76.05,76.18,76.57,76.67,76.56,76,75.87,75.64,75.21,74.96,74.86,74.69,74.34,
             74.13,73.89,73.6,73.5))>0]<- ifelse(roman,"IIb1","2b1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-0.199,-0.479,-1.879,-2.699,-4.999,
             -4.379,-4.289,-4.189,-4.299,-4.089,-2.519,-2.099,-1.599,0.8,1.12,1.71,3.06,4.07,4.55,5.19,6.39,6.51,6.74,7.06,7.21,
             30,30,-11), pol.y=c(73.5,73.5,73.6,73.94,74.09,74.21,74.5,75,75.3,76.05,76.18,76.57,76.67,76.56,76,75.87,75.64,75.21,
             74.96,74.86,74.69,74.34,74.13,73.89,73.6,73.5,73.5,90,90))>0] <- ifelse(roman,"IIb2","2b2")


   ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(8.648336, 7.034822,  7.357525,  9.083985,  9.608377, 10.229580, 10.689431, 11.084742, 11.617201, 12.068985, 11.972174, 10.592620,  9.971417,  9.398620,  8.648336),
       pol.y=c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015, 59.86417, 59.99375, 59.88040, 58.96783, 58.07740, 57.46530, 57.74247, 57.50441, 57.10708, 57.08073
              ))>0] <- ifelse(roman,"IIIan","3an")

   ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c( 10.59262, 11.97217, 12.15883, 12.70796, 13.12992, 12.80622, 12.95073, 12.72185, 12.45127,  12.29556, 12.13384, 11.99063, 11.58487, 11.58487, 11.63281, 11.49492, 11.30940, 11.27652,
    10.71374, 10.70218, 10.24553, 10.19351, 10.42472, 10.59262), pol.y=c(57.74247, 57.46530, 57.48032, 56.94085, 56.46389, 56.36135, 56.19091, 56.16918, 56.29535, 56.12728, 55.49119, 55.28764, 55.63113, 55.91101, 55.90623, 55.94866, 55.97965, 56.00988,
    56.14253, 56.25853, 56.49587, 57.11107, 57.63566, 57.74247))>0] <- ifelse(roman,"IIIas","3as")


    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat,
      pol.x=c(-4,7,8,7,7,7.906163,-4,-4.6,-4.6,-4), pol.y=c(62,62,61.5,60,58,57.5,57.5,57.3,58.2,58.4))>0] <- ifelse(roman,"IVa","4a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat,
     pol.x=c(7.906163,8.6488368,8.5,10.3,10.3,9.2,9.2,7.11,-1,-4,-1.78),
              pol.y=c(57.5,57.08073,57,57.3,57,56.2,52.7,53.5,53.5,56.1,57.5))>0] <- ifelse(roman,"IVb","4b")



    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(0,0.5,7.5,7.5),
                pol.y=c(53.5,51,51,53.5))>0] <- ifelse(roman,"IVc","4c")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-24,-27,-27,-24,-24),
                pol.y=c(62,62,63,63,62)) >0] <- ifelse(roman,"Va1","5a1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-27,-11,-11,-15,-15,-24,-24,-27),
                pol.y=c(68,68,63,63,62,62,63,63))>0] <- ifelse(roman,"Va2","5a2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-13.5,-15,-15,-14,-13.29,-13.5),
                pol.y=c(60,60,60,60.7,60.15,60)) > 0] <- ifelse(roman,"Vb1a","5b1a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-4,-5,-5,-8,-8,-7.5,-7.5,-8,-10,-10,-12,
                -13.5,-13.29,-13.99,-15,-15,-15,-11,-4), pol.y=c(60.5,60.5,60,60,60.5,60.5,61.25,61.5,61.5,60,60,60,60.15,60.71,
                60.49,62,63,63,63)) > 0] <- ifelse(roman,"Vb1b","5b1b")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-8,-10,-10,-8,-7.5,-7.5,-8,-8), pol.y=c(60,
                60,61.5,61.5,61.25,60.5,60.5,60)) > 0] <- ifelse(roman,"Vb2","5b2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-4,-4,-4.6,-4.6,-4,-6,-8,-12,-12,-5,-5),
                pol.y=c(60.5,58.4,58.2,57.3,55,55,54.5,54.5,60,60,60.5)) > 0] <- ifelse(roman,"VIa","6a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-15.99,-18,-18,-15,-13.5,-13.99,-14.57,
               -14.79,-14.88,-14.63,-14.34,-14.44,-14.54,-14.62,-14.72,-14.8,-14.89,-14.97,-15.04,-15.11,-15.19,-15.27,-15.34,-15.41,-15.47,
                -15.54,-15.6,-15.65,-15.7,-15.75,-15.79,-15.83,-15.87,-15.9,-15.92,-15.95,-15.97,-15.99,-15.99), pol.y = c(54.5,54.5,60,60,
                60,59.65,59.01,58.51,57.87,57.01,56.57,56.5,56.44,56.37,56.31,56.24,56.17,56.09,56.02,55.95,55.88,55.8,55.73,55.65,55.57,
                55.5,55.42,55.34,55.26,55.18,55.09,55.01,54.93,54.84,54.76,54.68,54.59,54.51,54.5)) >0] <- ifelse(roman,"VIb1","6b1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-12,-15.99,-15.99,-15.97,-15.95,-15.92,
                -15.9,-15.87,-15.83,-15.79,-15.75,-15.7,-15.65,-15.6,-15.54,-15.47,-15.41,-15.34,-15.27,-15.19,-15.11,-15.04,-14.97,-14.89,
                -14.8,-14.72,-14.62,-14.54,-14.44,-14.34,-14.63,-14.88,-14.79,-14.57,-13.99,-13.5,-12,-12), pol.y=c(54.5,54.5,54.51,54.59,
                54.68,54.76,54.84,54.93,55.01,55.09,55.18,55.26,55.34,55.42,55.5,55.57,55.65,55.73,55.8,55.88,55.95,56.02,56.09,56.17,56.24,
                56.31,56.37,56.44,56.5,56.57,57.01,57.87,58.51,59.01,59.65,60,60,54.5)) > 0] <- ifelse(roman,"VIb2","6b2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat,
    pol.x=c(-6.3,-7.8,-2.5,-2.5), pol.y=c(55,52,52,55)) > 0] <- ifelse(roman,"VIIa","7a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat,
    pol.x=c(-8,-8,-12,-12),pol.y=c(54.5,52.5,52.5,54.5)) >0] <- ifelse(roman,"VIIb","7b")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-16.06,-18,-18,-15.99,-15.99,-16,-16.01,-16.01,
                -16.01,-16,-15.99,-15.97,-15.96,-15.94,-15.91,-15.9,-15.89,-15.88,-15.86,-15.84,-15.88,-15.92,-15.95,-15.98,-16,-16.02,
                -16.04,-16.06,-16.06), pol.y=c(52.5,52.5,54.5,54.5,54.42,54.34,54.25,54.17,54.08,53.99,53.91,53.82,53.74,53.66,53.57,
                53.49,53.42,53.34,53.2,53.18,53.10,53.02,52.94,52.86,52.77,52.69,52.61,52.52,52.5)) >0] <- ifelse(roman,"VIIc1","7c1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-12,-16.06,-16.06,-16.04,-16.02,-16,-15.98,
                -15.95,-15.92,-15.88,-15.84,-15.86,-15.88,-15.89,-15.9,-15.91,-15.94,-15.96,-15.97,-15.99,-16,-16.01,-16.01,-16.01,
                -16,-15.99,-15.99,-12,-12), pol.y=c(52.5,52.5,52.52,52.61,52.69,52.77,52.86,52.94,53.02,53.1,53.18,53.26,53.34,53.42,
                53.49,53.57,53.66,53.74,53.82,53.91,53.99,54.08,54.17,54.25,54.34,54.42,54.5,54.5,52.5)) >0]<- ifelse(roman,"VIIc2","7c2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(2,-2,-2,-1.956,-1,2),
              pol.y=c(51,51,50.6,49.705,49,49)) >0] <- ifelse(roman,"VIId","7d")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-2,-2.22,-5.17,-5.24,-7,-7,-5,-5,-1,-1,-1.956),
                pol.y=c(50.6,50.88,50.21,50,50,49.5,49.5,48,48,49.3,49.705)) > 0] <- ifelse(roman,"VIIe","7e")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-7,-7,-5.2,-5.2,-2.4,-2.4,-5,-5,-6,-6),
                pol.y=c(50.5,50,50,50.25,51.4,51.9,51.9,51,51,50.5)) >0] <- ifelse(roman,"VIIf","7f")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-9,-7,-7,-6,-6,-5,-5,-9),
                pol.y=c(50,50,50.5,50.5,51,51,52,52)) > 0] <- ifelse(roman,"VIIg","7g")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-5,-8,-9,-9,-7,-7,-5,-5), pol.y=c(48,48,
                48,50,50,49,49,48)) >0] <- ifelse(roman,"VIIh","7h")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-10.64,-11,-12,-12,-11.99,-11.87,-11.75,-11.64,
                -11.52,-11.39,-11.27,-11.14,-11.02,-10.89,-10.77,-10.68,-10.64), pol.y=c(48,48,48,48.43,48.42,48.39,48.36,48.33,48.3,
                48.27,48.25,48.23,48.21,48.19,48.17,48.03,48)) >0] <- ifelse(roman,"VIIj1","7j1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-9,-9,-10.64,-10.68,-10.77,-10.89,-11.02,-11.14,
                -11.27,-11.39,-11.52,-11.64,-11.75,-11.87,-11.99,-12,-12), pol.y=c(52.5,48,48,48.03,48.17,48.19,48.21,48.23,48.25,48.27,
                48.3,48.33,48.36,48.39,48.42,48.43,52.5)) >0] <- ifelse(roman,"VIIj2","7j2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-12,-12,-18,-18,-16.06,-16.07,-16.08,-16.09,-16.09,
                -16.09,-16.08,-16.07,-16.07,-16.07,-16.04,-16.02,-16,-15.96,-15.93,-15.9,-15.86,-15.82,-15.77,-15.73,-15.68,-15.63,-15.57,
                -15.52,-15.47,-15.42,-15.36,-15.3,-15.24,-15.17,-15.11,-15.04,-14.97,-14.89,-14.82,-14.74,-14.65,-14.57,-14.48,-14.39,-14.3,
                -14.22,-14.13,-14.04,-13.95,-13.86,-13.77,-13.67,-13.57,-13.47,-13.37,-13.27,-13.17,-13.07,-12.96,-12.85,-12.74,-12.64,-12.54,
                -12.43,-12.32,-12.22,-12.11,-12), pol.y=c(48.43,48,48,52.5,52.5,52.44,52.36,52.27,52.19,52.11,52.02,51.94,51.85,51.77,51.68,
                51.6,51.52,51.43,51.34,51.27,51.18,51.1,51.02,50.94,50.86,50.78,50.7,50.62,50.54,50.47,50.39,50.32,50.24,50.17,50.1,50.03,
                49.96,49.89,49.82,49.75,49.69,49.62,49.56,49.5,49.44,49.38,49.32,49.27,49.21,49.15,49.1,49.05,49,48.95,48.9,48.86,48.81,
                48.77,48.73,48.69,48.65,48.62,48.58,48.55,48.52,48.49,48.46,48.43)) >0] <- ifelse(roman,"VIIk1","7k1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-12,-12.11,-12.22,-12.32,-12.43,-12.54,-12.64,-12.74,
                -12.85,-12.96,-13.07,-13.17,-13.27,-13.37,-13.47,-13.57,-13.67,-13.77,-13.86,-13.95,-14.04,-14.13,-14.22,-14.3,-14.39,-14.48,
                -14.57,-14.65,-14.74,-14.82,-14.89,-14.97,-15.04,-15.11,-15.17,-15.24,-15.3,-15.36,-15.42,-15.47,-15.52,-15.57,-15.63,-15.68,
                -15.73,-15.77,-15.82,-15.86,-15.9,-15.93,-15.96,-15.99,-16.02,-16.04,-16.05,-16.07,-16.07,-16.08,-16.09,-16.09,-16.09,-16.08,
                -16.07,-16.06,-12,-12), pol.y=c(48.43,48.46,48.49,48.52,48.55,48.58,48.62,48.65,48.69,48.73,48.77,48.81,48.86,48.9,48.95,49,
                49.05,49.1,49.15,49.21,49.27,49.32,49.38,49.44,49.5,49.56,49.62,49.69,49.75,49.82,49.89,49.96,50.03,50.1,50.17,50.24,50.32,
                50.39,50.47,50.54,50.62,50.7,50.78,50.86,50.94,51.02,51.1,51.18,51.27,51.34,51.43,51.52,51.6,51.68,51.77,51.85,51.94,52.02,
                52.11,52.19,52.27,52.36,52.44,52.5,52.5,48.43)) >0] <- ifelse(roman,"VIIk2","7k2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-8,-8,-6,-6,-5,-5,-0.5,-0.5),
                pol.y=c(48,47.5,47.5,47,57,46,46,48)) >0] <- ifelse(roman,"VIIIa","8a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-4,-4,-3,-3,-2,-2,0,0), pol.y=c(46,45.5,45.5,
                44.5,44.5,43,43,46)) >0] <- ifelse(roman,"VIIIb","8b")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-11,-2,-2), pol.y=c(43,44.5,44.5,43)) >0] <- ifelse(roman,"VIIIc","8c")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-11,-10.64,-10.37,-9.89,-9.62,-10.95,-11),
                pol.y = c(46.32,48,48,47.77,47.45,46.88,46.34,46.32)) > 0] <- ifelse(roman,"VIIId1","8d1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-4,-4,-3,-3,-11,-11,-10.95,-9.62,-9.89,-10.37,-10.64,
                -9,-8,-8,-6,-5,-4,-4,-4),
                 pol.y=c(46,45.5,45.5,44.5,44.5,46.32,46.34,46.88,47.45,47.77,48,48,48,47.5,47.5,47,47,46,46))>0] <- ifelse(roman,"VIIId2","8d2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(11,-13.31,-13.49,-13.8,-18,-18,-12,-11,-11),
                pol.y=c(46.32,44.72,44.07,43,43,48,48,48,46.32)) >0] <- ifelse(roman,"VIIIe1","8e1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-11,-13.8,-13.49,-13.31,-11,-11), pol.y=c(
                44.5,43,43,44.07,44.72,46.32,44.5)) >0] <- ifelse(roman,"VIIIe2","8e2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat,
     pol.x=c(-11,-11,-5.5,-5.5), pol.y=c(36,43,43,36))>0] <- ifelse(roman,"IXa","9a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-17,-18,-13.8,-13.84,-13.64,-13.27,-13.27,-13.49,
                -13.78,-13.69,-12.73,-15.3,-17.9,-18), pol.y=c(36,43,43,42.88,42.04,41.38,41.13,40.06,38.75,38.17,36.03,36.04,36.02,36))
                >0] <- ifelse(roman,"IXb1","9b1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-18,-17.9,-15.3,-12.73,-13.69,-13.78,-13.49,
                -13.27,-13.27,-13.64,-13.84,-13.8,-11,-11), pol.y=c(36,36,36,36.04,36.03,38.17,38.75,40.06,41.13,41.38,42.04,42.88,43,43,
                36))>0] <- ifelse(roman,"IXb2","9b2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-18,-22.25,-20.62,-21.32,-23.91,-24.65,-25.79,
                -28.45,-29.95,-35.11,-35.26,-35.48,-31.76,-32.03,-42,-42,-18,-18), pol.y=c(36,36,37.58,39.16,40.97,41.35,41.91,42.34,
                42.05,41.02,40.04,38.74,36.03,36,36,43,43,36))>0] <- ifelse(roman,"Xa1","10a1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-22.25,-31.76,-32.03,-35.48,-35.26,-35.11,-29.95,
    -28.45,-25.79,-24.65,-23.91,-21.32,-20.62,-22.25), pol.y=c(36,36,36.03,38.74,40.04,41.02,42.05,42.34,41.91,41.35,40.97,39.16,37.58,
    36))>0] <- ifelse(roman,"Xa2","10a2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-18,-42,-42,-18,-18),
         pol.y=c(43,43,48,48,43))>0] <- ifelse(roman,"Xb","10b")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-20.55,-24,-24,-18,-18,-42,-42,-41.5,-41,-40.5,
                -40,-39.5,-39,-38.5,-38.25,-38,-37.5,-37.2,-37,-36.77,-27,-27,-26.46,-25.09,-23.96,-23.27,-21.77,-20.57), pol.y=c(60,60,54.5,
                54.5,52.5,52.5,56.55,56.64,56.75,56.88,57.03,57.2,57.37,57.62,57.78,57.97,58.26,58.5,58.63,59,59,60.85,60.69,60.45,60.37,60.22,
                60.02,60))>0] <- ifelse(roman,"XIIa1","12a1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-20.55,-18.65,-17.32,-15.22,-15,-15,-18,-20.55),
                pol.y=c(60,60.05,60.11,60.44,60.49,60,60,60))>0] <- ifelse(roman,"XIIa2","12a2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-42,-36.77,-37,-37.2,-37.5,-38,-38.25,-38.5,-39,
                -39.5,-40,-40.5,-41,-41.5,-42,-42), pol.y=c(59,59,58.63,58.5,58.26,57.97,57.78,57.62,57.37,57.2,57.03,56.88,56.75,56.64,
                56.55,59))>0]<-ifelse(roman,"XIIa3","12a3")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-27,-27,-24,-15,-15,-15.22,-17.32,-18.65,-20.55,
                -21.76,-23.27,-23.96,-25.09,-26.46,-27), pol.y=c(60.85,62,62,62,60.49,60.44,60.11,60.05,60,60.02,60.22,60.37,60.45,
                60.69,60.85))>0]<-ifelse(roman,"XIIa4","12a4")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-18,-24,-24,-20.55,-18,-18), pol.y=c(54.5,54.5,
                60,60,60,54.5))>0]<-ifelse(roman,"XIIb","12b")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-18,-42,-42,-18,-18), pol.y=c(48,48,52.5,52.5,
                48))>0]<-ifelse(roman,"XIIc","12c")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-11,-27,-27,-40,-40,-11), pol.y=c(68,68,68.57,
                68.57,90,90))>0]<-ifelse(roman,"XIVa","14a")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-27,-27,-36.77,-36.5,-36.35,-36.16,-35.96,-35.76,
                -35.5,-35.37,-35.15,-34.97,-34.65,-34.5,-34.31,-34,-33.7,-33.53,-33.27,-33,-32.5,-32.3,-32,-31.5,-31,-30.86,-30.61,-29.87,
                -29.25,-28.61,-27.69,-27), pol.y=c(60.85,59,59,59.35,59.5,59.75,60,60.25,60.55,60.75,61,61.25,61.5,61.6,61.75,61.98,62.25,
                62.45,62.5,62.56,62.69,62.75,62.87,63.03,63.25,63.31,63,62.23,61.79,61.44,61.06,60.85))>0]<-ifelse(roman,"XIVb1","14b1")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(-27,-27,-27.69,-28.61,-29.25,-29.87,-30.61,-30.86,
                -31,-31.5,-32,-32.3,-32.5,-33,-33.27,-33.53,-33.7,-34,-34.31,-34.5,-34.65,-34.97,-35.15,-35.37,-35.5,-35.76,-35.96,-36.16,
                -36.35,-36.5,-36.77,-42,-44,-44), pol.y=c(68.6,60.85,61.06,61.44,61.79,62.23,63,63.31,63.25,63.03,62.87,62.75,62.69,62.56,
                62.5,62.45,62.25,61.98,61.75,61.6,61.5,61.25,61,60.75,60.55,60.25,60,59.75,59.5,59.35,59,59,59,68.6))>0]<-ifelse(roman,"XIVb2","14b2")

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(12,12,11.94,11.97,12,12,15,15,14.78,14.2,13.7,
                12.81,12.44), pol.y=c(55.3,54.75,54.67,54.56,54.56,53.5,53.5,55,55.3,55.4,55.5,55.38,55.33))>0]<-"24"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(14.2,14.78,15,15,18,18,14.2), pol.y=c(55.4,55.3,
                55,53,53,56.5,56.5))>0]<-"25"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(18,18,22,22),pol.y=c(56.5,53.5,53.5,56.5))>0]<-"26"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(18,18,18.32,18.32,19,19,16,16), pol.y=c(56.5,57,57,
                57.5,57.925,59.762,59.762,56.5))>0]<-"27"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(19,19,18.45,18.3,18,18,21.5,21.72,21.98,22.17,
                22.24,21.93), pol.y=c(58.5,57.9,57.58,57,57,56.65,56.5,57.57,57.97,58.04,58.15,58.5))>0]<-"28-1"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(21.5,21.72,21.98,22.17,22.24,22.24,23,25,25),
                pol.y=c(56.5,57.57,57.97,58.04,58.15,58.35,58.5,58.5,56.5))>0] <- "28-2"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(19,17.975,21.6,21.8,23.325,23.325,23.191,23,
                23,23.5,23.6,24,23.692,22.5,22.1,21.92,19), pol.y=c(59.762,60.5,60.5,60.7,60.5,59.965,59.867,59.827,59,59,59.05,
                58.75,59.5,59.5,58.35,58.5,58.5))>0]<-"29"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(16.5,16.5,19.7,19.7,22.6,21.4), pol.y=c(60.5,
                63.7,63.7,63.5,63.5,60.5))>0]<-"30"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(19.7,19.7,25.7,25.7,19.7), pol.y=c(63.7,63.5,
                63.5,67,67))>0]<-"31"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(23.325,23.325,23.191,23,23,30.5,30.5),
                pol.y=c(60.5,59.965,59.867,59.827,59,59,60.5))>0]<-"32"


    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(12.297,12.13,12.45,12.81,12.94,13.21,12.5,12.448),
                pol.y=c(56.13,55.48,55.31,55.38,55.41,55.71,56.29,56.305))>0]<-"23"

    ICES.area[point.in.polygon(point.x=chrons$shootlong, point.y=chrons$shootlat, pol.x=c(10.1,10.75,10.71,11.58,11.58,11.99,11.94,11.97,
                12,12,9.3,9.3), pol.y=c(56.6,56.3,56.15,55.9,55.65,55,54.67,54.56,54.56,53.75,53.75,56.6))>0]<-"22"



 return(ICES.area)
 }



