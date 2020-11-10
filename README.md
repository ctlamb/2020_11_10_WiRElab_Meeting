WiRE Lab–Spatial R
================
Clayton Lamb
10 November, 2020

## Load packages and bear data

``` r
library(hrbrthemes)
library(tidyverse)
library(here)
library(lubridate)
library(raster)
library(sf)
library(mapview)
library(adehabitatHR)
library(MuMIn)
library(ggeffects)
library(viridisLite)

##read in bear CSV
bears <- read.csv(here::here("data","bears.csv"))

##get dates in order
bears <- bears%>%
  mutate(DateTime=ymd_hms(DateTime,
                          tz="America/Edmonton")
         ) 

##check it out
head(bears)

bears%>%
  group_by(Name)%>%
  summarise(n=n(),
            start=min(DateTime),
            end=max(DateTime),
            duration=end-start)

##make spatial
bears.spat <-bears%>%
  st_as_sf(coords=c("Longitude","Latitude"),
           crs=4326)

##plot
plot(bears.spat["Name"])
```

![](README_files/figure-gfm/Load%20packages%20and%20data-1.png)<!-- -->

``` r
##map
#mapview(bears.spat["Name"],map.types="Esri.WorldImagery")
```

## Load landscape data

``` r
#first import all files in a single folder as a list 
rastlist <- list.files(path = here::here("data"), pattern='.tif$', all.files=TRUE, full.names=TRUE)

#stack 'em
stack<- stack(rastlist)

#plot
plot(stack)
```

![](README_files/figure-gfm/Load%20spatial-1.png)<!-- -->

``` r
##overlay with bear data, important
##remember-- spatial data are just plots with an X and Y (and confusing AF projections...if only the world were truly flat)
plot(stack[[2]])
plot(bears.spat%>%as("Spatial"),add=TRUE)
```

![](README_files/figure-gfm/Load%20spatial-2.png)<!-- -->

## Extract landscape data to bear data

``` r
bears.spat.data <- raster::extract(stack, as(bears.spat, "Spatial"), sp=TRUE)%>%st_as_sf()

head(bears.spat.data )
```

    ## Simple feature collection with 6 features and 6 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: -115.0147 ymin: 49.53903 xmax: -115.0064 ymax: 49.53988
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs
    ##              DateTime    Name       cc  deltaNDVI hum_density roaddist                   geometry
    ## 1 2019-05-09 14:00:51 Bubbles 39.99400 0.06986283    4.680254 115.4744  POINT (-115.0147 49.5394)
    ## 2 2019-05-09 16:01:24 Bubbles 39.99400 0.06986283    4.680254 115.4744 POINT (-115.0142 49.53903)
    ## 3 2019-05-09 18:00:41 Bubbles 39.99400 0.06986283    4.680254 115.4744 POINT (-115.0141 49.53903)
    ## 4 2019-05-09 20:00:30 Bubbles 39.99400 0.06986283    4.680254 115.4744 POINT (-115.0144 49.53945)
    ## 5 2019-05-09 22:00:40 Bubbles 39.99400 0.06986283    4.680254 115.4744  POINT (-115.013 49.53917)
    ## 6 2019-05-10 00:00:49 Bubbles 37.10379 0.10271567    1.445403 166.3968 POINT (-115.0064 49.53988)

``` r
#mapview(bears.spat.data["roaddist"],map.types="Esri.WorldImagery")
```

## Explore data

``` r
bears.spat.data%>%
  tibble%>%
  dplyr::select(-geometry)%>%
  pivot_longer(cc:roaddist)%>%
  mutate(Date_yearless=ymd(paste("2020", month(DateTime), day(DateTime),sep="-")))%>%
  ggplot(aes(x=Date_yearless,y=value, color=Name))+
  geom_point()+
  facet_wrap(vars(name),scales="free_y")+
  theme_ipsum()+
    theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/explore-1.png)<!-- -->

``` r
bears.spat.data%>%
  tibble%>%
  dplyr::select(-geometry)%>%
  pivot_longer(cc:roaddist)%>%
  mutate(month=month(DateTime))%>%
  group_by(Name,month,name)%>%
  summarise(value=mean(value))%>%
  ggplot(aes(x=month,y=value, color=Name))+
  geom_point()+
  facet_wrap(vars(name),scales="free_y")+
  geom_smooth()+
    theme_ipsum()+
    theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/explore-2.png)<!-- -->

## Add in some availables

``` r
mcp <- mcp(bears.spat%>%as("Spatial")%>%as("SpatialPoints"), percent=95)

plot(mcp)
plot(bears.spat%>%as("Spatial"),add=TRUE)
```

![](README_files/figure-gfm/availables-1.png)<!-- -->

``` r
##sample from within this mcp
##1:5 for every used, pair with individual used attribute data
avail <- st_sample(mcp%>%st_as_sf(),size=nrow(bears.spat)*5)%>%
  tibble%>%
  mutate(Name=rep(bears.spat$Name, each=5),
         DateTime=rep(bears.spat$DateTime, each=5))%>%
  st_as_sf()



plot(mcp)
plot(avail%>%as("Spatial"),add=TRUE) ##looks like furry animal
```

![](README_files/figure-gfm/availables-2.png)<!-- -->

``` r
##extract landscape data to availables
avail.data <- raster::extract(stack, as(avail, "Spatial"), sp=TRUE)%>%st_as_sf()
```

## Get used-available data together

``` r
bears.spat.data <-bears.spat.data%>%
  tibble%>%
  dplyr::select(-geometry)%>%
  mutate(used=1)

data <-bears.spat.data%>%
  rbind(
    avail.data%>%
      tibble%>%
      mutate(used=0)%>%
            dplyr::select(colnames(bears.spat.data))
  )%>%
  drop_na(cc:roaddist)%>%
  mutate(month=as.factor(month(DateTime)))
  


data%>%
  pivot_longer(cc:roaddist)%>%
  group_by(month,name,used)%>%
  summarise(value=mean(value))%>%
  ggplot(aes(x=month,y=value, color=as.factor(used)))+
  geom_point()+
  facet_wrap(vars(name),scales="free_y")+
  geom_smooth()+
    theme_ipsum()+
    theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/use-avail%20together-1.png)<!-- -->

## Model

``` r
null <- glm(used~1, data=data,family="binomial")
m1 <-glm(used~cc+deltaNDVI, data=data,family="binomial")
m2 <-glm(used~hum_density+roaddist, data=data,family="binomial")
m3 <-glm(used~cc+deltaNDVI+hum_density+roaddist, data=data,family="binomial")
m4 <-glm(used~cc+deltaNDVI+hum_density*month+roaddist*month, data=data,family="binomial")

model.sel(null,m1,m2,m3,m4)
```

    ## Model selection table 
    ##       (Int)      cc    dND   hum_dns        rdd mnt hum_dns:mnt mnt:rdd df    logLik    AICc   delta weight
    ## m4   -2.550 0.02340  8.083 -0.002427 -0.0007152   +           +       + 20 -16781.13 33602.3    0.00      1
    ## m3   -2.580 0.02209  8.153 -0.002809 -0.0005202                          5 -17182.94 34375.9  773.58      0
    ## m1   -3.125 0.02411 10.950                                               3 -17393.52 34793.0 1190.75      0
    ## m2   -1.248                -0.004228 -0.0007023                          3 -17615.84 35237.7 1635.39      0
    ## null -1.609                                                              1 -18116.84 36235.7 2633.39      0
    ## Models ranked by AICc(x)

## Plot results

``` r
plot(ggpredict(m4, terms = c("deltaNDVI")))
```

![](README_files/figure-gfm/plot%20results-1.png)<!-- -->

``` r
plot(ggpredict(m4, terms = c("hum_density","month[7,10]")))
```

![](README_files/figure-gfm/plot%20results-2.png)<!-- -->

``` r
plot(ggpredict(m4, terms = c("roaddist","month[7,10]")))
```

![](README_files/figure-gfm/plot%20results-3.png)<!-- -->

## Map results

``` r
month <- stack[[1]]
values(month)=7
month<- ratify(month)
names(month)="month"

map.summer <- predict(stack%>%
  addLayer(month),m4, type="response")
plot(map.summer)
```

![](README_files/figure-gfm/map%20results-1.png)<!-- -->

``` r
values(month)=10
map.fall <-predict(stack%>%
  addLayer(month),m4, type="response")
plot(map.fall)
```

![](README_files/figure-gfm/map%20results-2.png)<!-- -->

## Map results

``` r
##change to tibble
sum.rast<-  map.summer%>%
  as.data.frame(xy = TRUE)%>%
  drop_na(layer)%>%
  mutate(Season="Summer")

fall.rast<-  map.fall%>%
  as.data.frame(xy = TRUE)%>%
  drop_na(layer)%>%
  mutate(Season="Fall")

map.dat <-rbind(sum.rast,fall.rast)%>%
  mutate(Season=fct_relevel(Season,"Summer","Fall"))

##load some town data
cities <- tribble(
~city,~lat,~long,
"Fernie", 49.505770,-115.065131,
"Sparwood",  49.731576,-114.888112,
"Elkford",  50.024611,-114.924228
)%>%
  st_as_sf(coords=c("long","lat"),
           crs=4326)

  ggplot() +
  geom_raster(data = map.dat,
              aes(x = x, y = y, fill =layer))+
  geom_sf(data=cities,fill =NA, color = "black", size=3, alpha=0.5)+
  facet_wrap(vars(Season))+
    theme_ipsum()+
    scale_fill_gradientn(colours = rev(magma(8, alpha = 0.8)[2:7]), name = "Selection")
```

![](README_files/figure-gfm/map%20results2-1.png)<!-- -->
