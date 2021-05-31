---
title: "Reef data analysis"
author: "Eric Coyle"
date: "5/28/2021"
output: 
  word_document: 
    keep_md: yes
    fig_width: 8
    fig_height: 5
  html_document: 
    keep_md: yes
---







```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.1.1     v dplyr   1.0.6
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(StereoMorph)
library(ggplot2)
library(here)
```

```
## here() starts at C:/Users/ericc/Desktop/General-Repository
```

```r
library(ggthemes)
library(paletteer)
```


```r
library(ggstatsplot)
```

```
## You can cite this package as:
##      Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
##      PsyArxiv. doi:10.31234/osf.io/p7mku
```

```r
reef_fish <- readr::read_csv("my_data/105_coral_reef_dataset.csv")
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   Species = col_character(),
##   Family = col_character(),
##   Prey = col_character(),
##   Body_length = col_double(),
##   Head_length = col_double(),
##   Upper_jaw_length = col_double(),
##   Body_depth = col_double(),
##   Caudal_peduncle_depth = col_double(),
##   Jaws_to_eye_distance = col_double(),
##   Fineness_ratio = col_double()
## )
```


```r
glimpse(reef_fish)
```

```
## Rows: 81
## Columns: 10
## $ Species               <chr> "Abudefduf_sordidus", "Acanthurus_achilles", "Ac~
## $ Family                <chr> "Pomacentridae", "Acanthuridae", "Acanthuridae",~
## $ Prey                  <chr> "benthic", "benthic", "benthic", "benthic", "pla~
## $ Body_length           <dbl> 0.7224, 0.7030, 0.7217, 0.7148, 0.7586, 0.6963, ~
## $ Head_length           <dbl> 0.2283, 0.1804, 0.1562, 0.1569, 0.1627, 0.1873, ~
## $ Upper_jaw_length      <dbl> 0.0455, 0.0277, 0.0328, 0.0356, 0.0276, 0.0343, ~
## $ Body_depth            <dbl> 0.4162, 0.3783, 0.3689, 0.3757, 0.2865, 0.3592, ~
## $ Caudal_peduncle_depth <dbl> 0.1444, 0.0791, 0.0966, 0.0881, 0.1328, 0.0945, ~
## $ Jaws_to_eye_distance  <dbl> 0.0906, 0.1398, 0.1704, 0.1742, 0.1003, 0.0901, ~
## $ Fineness_ratio        <dbl> 1.7359, 1.8584, 1.9565, 1.9027, 2.6482, 1.9385, ~
```


```r
reef_fish%>%
  group_by(Prey)%>%
  summarise(mean_body_length=mean(Body_length),
            mean_body_depth=mean(Body_depth),
            mean_fineness=mean(Fineness_ratio),
            mean_cd_depth=mean(Caudal_peduncle_depth),
            mean_jaw_distance=mean(Jaws_to_eye_distance))
```

```
## # A tibble: 2 x 6
##   Prey       mean_body_length mean_body_depth mean_fineness mean_cd_depth
##   <chr>                 <dbl>           <dbl>         <dbl>         <dbl>
## 1 benthic               0.747           0.345          2.22        0.104 
## 2 planktonic            0.762           0.270          3.06        0.0900
## # ... with 1 more variable: mean_jaw_distance <dbl>
```




```r
#install.packages("ggExtra")
#install.packages("ColorPalette")
library(ggExtra)
library(ColorPalette)
```




```r
#install.packages("ggpubr")
library(ggpubr)
```

```r
reef_fish%>%
  ggplot(aes(x=Prey,y=Caudal_peduncle_depth,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "CPD of Planktivorous and NP Fish",x="Prey Type",y="Caudal Peduncle Depth")+
  stat_compare_means(method = "t.test")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-8-1.png)<!-- -->

```r
reef_fish%>%
  ggplot(aes(x=Family,y=Caudal_peduncle_depth,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "CPD of Planktivorous and NP Fish by Family",x="Family",y="Caudal Peduncle Depth")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-9-1.png)<!-- -->



```r
reef_fish%>%
  ggplot(aes(x=Prey,y=Fineness_ratio,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "FR of Planktivorous and NP Fish",x="Prey Type",y="Fineness Ratio")+stat_compare_means(method = "t.test")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-10-1.png)<!-- -->

```r
reef_fish%>%
  ggplot(aes(x=Family,y=Fineness_ratio,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "FR of Planktivorous and NP Fish by Family",x="Family",y="Caudal Peduncle Depth")+
  theme(axis.text.x = element_text(angle = 60, hjust=1))
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-11-1.png)<!-- -->


```r
reef_fish%>%
  ggplot(aes(x=Prey,y=Body_depth,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "BD of Planktivorous and NP Fish",x="Prey Type",y="Body Depth")+stat_compare_means(method = "t.test")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-12-1.png)<!-- -->

```r
reef_fish%>%
  ggplot(aes(x=Family,y=Body_depth,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "BD of Planktivorous and NP Fish by Family",x="Family",y="Body Depth")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-13-1.png)<!-- -->


```r
P<-reef_fish%>%
  ggplot(aes(x=Prey,y=Body_length,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "BL of Planktivorous and NP Fish",x="Prey Type",y="Body Length")
P+stat_compare_means(method = "t.test")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-14-1.png)<!-- -->



```r
reef_fish%>%
  ggplot(aes(x=Family,y=Body_length,color=Prey))+
  geom_boxplot()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "BL of Planktivorous and NP Fish by Family",x="Family",y="Body Length")
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-15-1.png)<!-- -->




```r
reef_fish%>%
  ggplot(aes(x=Caudal_peduncle_depth,y=Fineness_ratio,color=Prey))+
  geom_point(size=2.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  geom_smooth(method = lm,se=F)+
  stat_regline_equation(label.y=4.9,aes(label = ..eq.label..)) +
  stat_regline_equation(label.y=4.5,aes(label = ..rr.label..))+
  facet_wrap(~Prey)+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "FR across CPD Values",x="Caudal Peducle Depth",y="Fineness Ratio")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-16-1.png)<!-- -->


```r
reef_fish%>%
  ggplot(aes(x=Body_depth,y=Fineness_ratio,color=Prey))+
  geom_point(size=2.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  geom_smooth(method = lm,se=F)+
  stat_regline_equation(label.y=1.5,aes(label = ..eq.label..)) +
  stat_regline_equation(label.y=1,aes(label = ..rr.label..))+
  facet_wrap(~Prey)+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "Contribution of BD to Different FR Values",x="Body Depth",y="Fineness Ratio")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-17-1.png)<!-- -->

```r
reef_fish%>%
  ggplot(aes(x=Body_length,y=Fineness_ratio,color=Prey))+
  geom_point(size=2.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  geom_smooth(method = lm,se=F)+
  stat_regline_equation(label.y=4.5,aes(label = ..eq.label..)) +
  stat_regline_equation(label.y=4,aes(label = ..rr.label..))+
  facet_wrap(~Prey)+
   scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, hjust=1,size = 14),axis.title = element_text(size = 17),plot.title = element_text(size = 20,face = "bold",hjust = .5))+
  labs(title = "Contribution of BL to Different FR Values",x="Body Length",y="Fineness Ratio")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Fish-data-analysis_files/figure-docx/unnamed-chunk-18-1.png)<!-- -->


```r
citation()
```

```
## 
## To cite R in publications use:
## 
##   R Core Team (2021). R: A language and environment for statistical
##   computing. R Foundation for Statistical Computing, Vienna, Austria.
##   URL https://www.R-project.org/.
## 
## A BibTeX entry for LaTeX users is
## 
##   @Manual{,
##     title = {R: A Language and Environment for Statistical Computing},
##     author = {{R Core Team}},
##     organization = {R Foundation for Statistical Computing},
##     address = {Vienna, Austria},
##     year = {2021},
##     url = {https://www.R-project.org/},
##   }
## 
## We have invested a lot of time and effort in creating R, please cite it
## when using it for data analysis. See also 'citation("pkgname")' for
## citing R packages.
```

