---
title: "MCMP_2"
output: 
  html_document: 
    keep_md: yes
---







```r
data<-read.csv("Trial2_11_20_MCMP.csv")
```


```r
cal<-read.csv("Cal_MCMP_Revised.csv")
```


```r
O2sat<-read.csv("MCMP_O2_Sat.csv")
```



```r
optode<-function(cal0,T0,cal100,T100,phase,temp) {
f1=0.801
deltaPsiK=-0.08
deltaKsvK=0.000383
m=22.9
tan0T100=tan(((cal0+deltaPsiK*(T100-T0)))*pi/180)
tan0Tm=tan((cal0+(deltaPsiK*(temp-T0)))*pi/180)
tan100T100=tan(cal100*pi/180)
tanmTm=tan(phase*pi/180)
A=tan100T100/tan0T100*1/m*100^2
B=tan100T100/tan0T100*100+tan100T100/tan0T100*1/m*100-f1*1/m*100-100+f1*100
C=tan100T100/tan0T100-1
KsvT100=(-B+(sqrt(B^2-4*A*C)))/(2*A)
KsvTm=KsvT100+(deltaKsvK*(temp-T100))
a=tanmTm/tan0Tm*1/m*KsvTm^2
b=tanmTm/tan0Tm*KsvTm+tanmTm/tan0Tm*1/m*KsvTm-f1*1/m*KsvTm-KsvTm+f1*KsvTm
c=tanmTm/tan0Tm-1
saturation=(-((tan(phase*pi/180))/(tan((cal0+(deltaPsiK*(temp-T0)))*pi/180))*
(KsvT100+(deltaKsvK*(temp-T100)))+(tan(phase*pi/180))/(tan((cal0+(deltaPsiK*
(temp-T0)))*pi/180))*1/m*(KsvT100+(deltaKsvK*(temp-T100)))-f1*1/m*(KsvT100+
(deltaKsvK*(temp-T100)))-(KsvT100+(deltaKsvK*(temp-T100)))+f1*(KsvT100+
(deltaKsvK*(temp-T100))))+(sqrt((((tan(phase*pi/180))/(tan((cal0+(deltaPsiK*
(temp-T0)))*pi/180))*(KsvT100+(deltaKsvK*(temp-T100)))+(tan(phase*pi/180))/
(tan((cal0+(deltaPsiK*(temp-T0)))*pi/180))*1/m*(KsvT100+(deltaKsvK*(temp-T100)))
-f1*1/m*(KsvT100+(deltaKsvK*(temp-T100)))-
(KsvT100+(deltaKsvK*(temp-T100)))+f1*
(KsvT100+(deltaKsvK*(temp-T100)))))^2-4*
((tan(phase*pi/180))/(tan((cal0+(deltaPsiK*
(temp-T0)))*pi/180))*1/m*(KsvT100+
(deltaKsvK*(temp-T100)))^2)*((tan(phase*pi/180))/
(tan((cal0+(deltaPsiK*
(temp-T0)))*pi/180))-1))))/(2*((tan(phase*pi/180))/
(tan((cal0+(deltaPsiK*(temp-T0)))*pi/180))*
1/m*(KsvT100+(deltaKsvK*(temp-T100)))^2))
}
```


```r
dataCombin<-merge(data,cal,by="vial")
```


```r
dataCombin<-merge(dataCombin,O2sat)
```


```r
f<-function(d) optode(d$cal0,d$T0,d$cal100,d$T100,d$phase,d$temp)
```


```r
dataCombin$oxygen<-f(dataCombin)
```


```r
dataCombin$umoleO2<-(dataCombin$oxygen/100)*dataCombin$O2sat
```




```r
head(dataCombin)
```

```
##   temp trial treatment vial time phase round salinity.... pressure..inHg.
## 1 13.9    2f         1    3 1780 32.63     6         37.5            30.2
## 2 13.9    2f         1    3 1780 32.63     6         37.5            30.2
## 3 13.9    2f         1    2 1165 46.15     4         37.5            30.2
## 4 13.9    2f         1    1 1417 35.27     5         37.5            30.2
## 5 13.9    2f         1    1 1417 35.27     5         37.5            30.2
## 6 13.9    2f         1    7 2490 30.13     8         37.5            30.2
##   amplitude day  cal0 cal100   T0 T100 Salinity.... O2sat   oxygen  umoleO2
## 1       422   1 59.68  31.51 13.5 13.5         37.5  8.27 90.24126 7.462952
## 2       422   1 58.59  28.91 20.4 20.2         37.5  8.27 80.71499 6.675130
## 3       508   1 56.67  28.04 20.2 20.2         37.5  8.27 20.66207 1.708753
## 4       381   1 58.14  29.32 20.2 20.1         37.5  8.27 67.00227 5.541088
## 5       381   1 59.16  31.71 13.3 13.2         37.5  8.27 72.94692 6.032711
## 6       283   1 59.36  28.09 20.1 20.2         37.5  8.27 92.33113 7.635785
```

```r
dataCombin$time<-as.numeric(dataCombin$time)
```

```r
dataCombin$treatment<-as.numeric(dataCombin$treatment)
```




```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 4.1.1
```

```r
mlist<-dlply(dataCombin,.(vial, treatment), function(d) lm(umoleO2~time, data=d))
output<-ldply(mlist, function(m) coef(m))
output$slope<-abs(output$time)
```




```r
meanControl<-mean(output[output$vial=="7","slope"])
```


```r
meanControl
```

```
## [1] 0.0002469444
```



```r
output$adj.slope<-output$slope-meanControl
```


```r
head(output)
```

```
##   vial treatment (Intercept)          time        slope    adj.slope
## 1    1         1    7.286401 -0.0010783484 0.0010783484 0.0008314040
## 2    1         2    2.468413 -0.0008617217 0.0008617217 0.0006147773
## 3    2         1    3.198575 -0.0011590570 0.0011590570 0.0009121126
## 4    2         2    1.782922 -0.0008606753 0.0008606753 0.0006137309
## 5    3         1    2.163494  0.0027940275 0.0027940275 0.0025470831
## 6    3         2    5.395133 -0.0007469819 0.0007469819 0.0005000374
```


```r
mass<-read.csv("MCMP_mass.csv")
```


```r
final<-merge(output,mass)
```


```r
final$umoleO2minmg<-(final$adj.slope*(70/1000))/(final$mass)
final$umoleO2hrg<-final$umoleO2minmg*1000*60
```


```r
head(final)
```

```
##   vial treatment (Intercept)          time        slope    adj.slope mass
## 1    1         1    7.286401 -0.0010783484 0.0010783484 0.0008314040  1.2
## 2    1         2    2.468413 -0.0008617217 0.0008617217 0.0006147773  1.2
## 3    2         1    3.198575 -0.0011590570 0.0011590570 0.0009121126  3.0
## 4    2         2    1.782922 -0.0008606753 0.0008606753 0.0006137309  3.0
## 5    3         1    2.163494  0.0027940275 0.0027940275 0.0025470831  1.5
## 6    3         2    5.395133 -0.0007469819 0.0007469819 0.0005000374  1.5
##   umoleO2minmg umoleO2hrg
## 1 4.849856e-05  2.9099139
## 2 3.586201e-05  2.1517205
## 3 2.128263e-05  1.2769576
## 4 1.432039e-05  0.8592233
## 5 1.188639e-04  7.1318327
## 6 2.333508e-05  1.4001049
```



```r
library(plotrix)
```

```
## Warning: package 'plotrix' was built under R version 4.1.1
```

```r
meanMR<-ddply(final, .(treatment), function(d) mean(d$umoleO2hrg, na.rm=T))
names(meanMR)<-c("treatment","meanMR")
sdMR<-ddply(final, .(treatment), function(d) sd(d$umoleO2hrg, na.rm=T))
names(sdMR)<-c("treatment","sdMR")
seMR<-ddply(final, .(treatment), function(d) std.error(d$umoleO2hrg, na.rm=T))
names(seMR)<-c("treatment","seMR")
meanData<-merge(meanMR, sdMR)
meanData<-merge(meanData, seMR)
meanData<-merge(meanData, seMR)
CI95<-meanData$seMR*1.96
meanData$CI95<-CI95
```


```r
library(plotrix)
meanMR<-ddply(final, .(vial), function(d) mean(d$umoleO2hrg, na.rm=T))
names(meanMR)<-c("vial","meanMR")
sdMR<-ddply(final, .(vial), function(d) sd(d$umoleO2hrg, na.rm=T))
names(sdMR)<-c("vial","sdMR")
seMR<-ddply(final, .(vial), function(d) std.error(d$umoleO2hrg, na.rm=T))
names(seMR)<-c("treatment","seMR")
meanData<-merge(meanMR, sdMR)
meanData<-merge(meanData, seMR)
meanData<-merge(meanData, seMR)
CI95<-meanData$seMR*1.96
meanData$CI95<-CI95
```


```r
library(ggplot2)
library(grid)
p=ggplot(meanData,aes(vial, meanMR))
p+geom_line()+
geom_errorbar(aes(ymax=meanMR+CI95, ymin=meanMR-CI95), width=0.1)+
theme(axis.text.x=element_text(size=12),
axis.text.y=element_text(size=12),
axis.title.y=element_text(vjust=-0.1,angle=90, size=14),
axis.title.x=element_text(size=14),
plot.margin=unit(c(1, 1, 1, 2), "lines"))+
theme_bw()
```

![](MCMP_R_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.1.1
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v tibble  3.1.5     v dplyr   1.0.7
## v tidyr   1.1.4     v stringr 1.4.0
## v readr   2.0.2     v forcats 0.5.1
## v purrr   0.3.4
```

```
## Warning: package 'tibble' was built under R version 4.1.1
```

```
## Warning: package 'tidyr' was built under R version 4.1.1
```

```
## Warning: package 'readr' was built under R version 4.1.1
```

```
## Warning: package 'purrr' was built under R version 4.1.1
```

```
## Warning: package 'stringr' was built under R version 4.1.1
```

```
## Warning: package 'forcats' was built under R version 4.1.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::arrange()   masks plyr::arrange()
## x purrr::compact()   masks plyr::compact()
## x dplyr::count()     masks plyr::count()
## x dplyr::failwith()  masks plyr::failwith()
## x dplyr::filter()    masks stats::filter()
## x dplyr::id()        masks plyr::id()
## x dplyr::lag()       masks stats::lag()
## x dplyr::mutate()    masks plyr::mutate()
## x dplyr::rename()    masks plyr::rename()
## x dplyr::summarise() masks plyr::summarise()
## x dplyr::summarize() masks plyr::summarize()
```

```r
library(ggplot2)
```


```r
final$vial<-as.character(final$vial)
final%>%
  ggplot(aes(x=vial,y=umoleO2hrg,fill=vial))+
  geom_col()+
  facet_wrap(~treatment)
```

![](MCMP_R_files/figure-html/unnamed-chunk-25-1.png)<!-- -->
