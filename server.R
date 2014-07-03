
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyIncubator)
library(lubridate)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(rgdal)
library(png)

#### Set-up Environment ####
# Set-up DEM Data & Function
if(!file.exists("082o03_0201_demw.dem")){
  download.file("ftp://ftp2.cits.rncan.gc.ca/pub/geobase/official/cded/50k_dem/082/082o03.zip","082o03.zip")
  unzip("082o03.zip")
}
dem<-raster("082o03_0201_demw.dem")

slope<-terrain(dem,opt = "slope",unit = "radians")
aspect<-terrain(dem,opt="aspect",unit = "radians")

sunPosition <- function(year, month, day, hour=12, min=0, sec=0,
                        lat=46.5, long=6.5) {
  
  twopi <- 2 * pi
  deg2rad <- pi / 180
  
  # Get day of the year, e.g. Feb 1 = 32, Mar 1 = 61 on leap years
  month.days <- c(0,31,28,31,30,31,30,31,31,30,31,30)
  day <- day + cumsum(month.days)[month]
  leapdays <- year %% 4 == 0 & (year %% 400 == 0 | year %% 100 != 0) & 
    day >= 60 & !(month==2 & day==60)
  day[leapdays] <- day[leapdays] + 1
  
  # Get Julian date - 2400000
  hour <- hour + min / 60 + sec / 3600 # hour plus fraction
  delta <- year - 1949
  leap <- trunc(delta / 4) # former leapyears
  jd <- 32916.5 + delta * 365 + leap + day + hour / 24
  
  # The input to the Atronomer's almanach is the difference between
  # the Julian date and JD 2451545.0 (noon, 1 January 2000)
  time <- jd - 51545.
  
  # Ecliptic coordinates
  
  # Mean longitude
  mnlong <- 280.460 + .9856474 * time
  mnlong <- mnlong %% 360
  mnlong[mnlong < 0] <- mnlong[mnlong < 0] + 360
  
  # Mean anomaly
  mnanom <- 357.528 + .9856003 * time
  mnanom <- mnanom %% 360
  mnanom[mnanom < 0] <- mnanom[mnanom < 0] + 360
  mnanom <- mnanom * deg2rad
  
  # Ecliptic longitude and obliquity of ecliptic
  eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
  eclong <- eclong %% 360
  eclong[eclong < 0] <- eclong[eclong < 0] + 360
  oblqec <- 23.439 - 0.0000004 * time
  eclong <- eclong * deg2rad
  oblqec <- oblqec * deg2rad
  
  # Celestial coordinates
  # Right ascension and declination
  num <- cos(oblqec) * sin(eclong)
  den <- cos(eclong)
  ra <- atan(num / den)
  ra[den < 0] <- ra[den < 0] + pi
  ra[den >= 0 & num < 0] <- ra[den >= 0 & num < 0] + twopi
  dec <- asin(sin(oblqec) * sin(eclong))
  
  # Local coordinates
  # Greenwich mean sidereal time
  gmst <- 6.697375 + .0657098242 * time + hour
  gmst <- gmst %% 24
  gmst[gmst < 0] <- gmst[gmst < 0] + 24.
  
  # Local mean sidereal time
  lmst <- gmst + long / 15.
  lmst <- lmst %% 24.
  lmst[lmst < 0] <- lmst[lmst < 0] + 24.
  lmst <- lmst * 15. * deg2rad
  
  # Hour angle
  ha <- lmst - ra
  ha[ha < -pi] <- ha[ha < -pi] + twopi
  ha[ha > pi] <- ha[ha > pi] - twopi
  
  # Latitude to radians
  lat <- lat * deg2rad
  
  # Azimuth and elevation
  el <- asin(sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha))
  az <- asin(-cos(dec) * sin(ha) / cos(el))
  
  # For logic and names, see Spencer, J.W. 1989. Solar Energy. 42(4):353
  cosAzPos <- (0 <= sin(dec) - sin(el) * sin(lat))
  sinAzNeg <- (sin(az) < 0)
  az[cosAzPos & sinAzNeg] <- az[cosAzPos & sinAzNeg] + twopi
  az[!cosAzPos] <- pi - az[!cosAzPos]
  
  # if (0 < sin(dec) - sin(el) * sin(lat)) {
  #     if(sin(az) < 0) az <- az + twopi
  # } else {
  #     az <- pi - az
  # }
  
  
  el <- el / deg2rad
  az <- az / deg2rad
  lat <- lat / deg2rad
  
  return(list(elevation=el, azimuth=az))
}

# Set-up DEM for plot
DEM<-data.frame(rasterToPoints(dem))
colnames(DEM)<-c("x","y","layer")
b.dem <- seq(min(DEM$layer,na.rm=TRUE),max(DEM$layer,na.rm=TRUE),length.out=100)


#### Run Shiny Server ####

shinyServer(function(input, output) {

    output$demPlot <- renderImage({
      
      fname<-paste0("img/hs082o03_",formatC(input$mon,width = 2,flag="0"),formatC(input$dy,width=2,flag="0"),formatC(input$hr,width=2,flag="0"),".png")
      if(!file.exists(fname)){
        
        sunpos<-sunPosition(year = 2014,month = input$mon,day = input$dy,hour = input$hr+7,min = 30,sec = 0,lat = mean(DEM$y),long=mean(DEM$x))
        hs<-hillShade(slope,aspect,angle = sunpos$elevation,direction = sunpos$azimuth,normalize = TRUE)/255.0
  
        HS<-data.frame(rasterToPoints(hs))
        colnames(HS)<-c("x","y","layer")
              
        #p<-ggplot(HS,aes(x=x,y=y))+
        #        geom_raster(data=DEM,aes(fill=layer))+
        #        geom_raster(aes(alpha=1-layer),fill="gray20")+
        #        scale_alpha(guide=FALSE,range = c(0,1.00))+
        #        #scale_x_continuous(expand = c(0,0))+
        #        #scale_y_continuous(expand = c(0,0))+
        #        scale_fill_gradientn(name="Elevation (m)",colours = terrain.colors(20))+
        #        theme_bw()+coord_equal()+xlab("Longitude")+ylab("Latitude")+ggtitle(paste0("Canmore, AB Canada - DEM w Lighting for ",input$mon,"-",input$dy," @ ",input$hr,":00"))
              
        if(!file.exists("img")){
          dir.create("img")
        }
  
          png(filename = fname ,width = 480, height= 450)
          #print(p)
        plot(hs,col=grey(0:100/100),legend=F,main=paste0("Canmore, AB Canada - DEM w Lighting for ",input$mon,"-",input$dy," @ ",input$hr,":00"),alpha=1.0)
          plot(dem,col = terrain.colors(255),alpha=0.35,add=TRUE)

          
        
          dev.off()
    }
            
      list(src = fname,
           contentType = "image/png",
           width = 600,
           height = 550,
           alt = "DEM with Hillshade")
    }, deleteFile=FALSE)
})
