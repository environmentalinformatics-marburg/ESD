

### A "Quick-and-Dirty" Introduction

#### What it is About
**ESD** is an R package in the making providing a means to downscale coarse-resolution NDVI datasets to a higher spatial resolution based on empirical orthogonal teleconnections (EOT; van den Dool *et al.* 2000). Currently supported datasets include the Moderate Resolution Imaging Spectroradiometer (MODIS) MOD13 series of products and the Advanced Very High Resolution Radiometer (AVHRR) Global Inventory Modeling and Mapping Studies (GIMMS) archive.

#### How to Install
The development version of the package is currently available from [GitHub](https://github.com/environmentalinformatics-marburg/ESD) only. It can be installed via 


```r
library(devtools)
install_github("environmentalinformatics-marburg/ESD")
```

Additional dependencies which are not (yet) officially on CRAN currently include

+ **Rsenal** (`install_github("environmentalinformatics-marburg/Rsenal")`).

<hr>

#### Data Download
Data sets currently available for download originate from either the MODIS sensors aboard the Terra and Aqua satellite platforms, or the AVHRR GIMMS archive. Depending on the 'type' argument, the `download` function calls either `runGdal` (from **MODIS**) or `downloadGimms` (from **gimms**) and hands over all additional arguments via '...'.


```r
library(ESD)

## MODIS download
mds <- download("MODIS", product = "M*D13A1", collection = "006", 
                begin = "2015001", end = "2015031", 
                SDSstring = "101000000011", job = "MCD13A1.006_Alb", 
                extent = readRDS(system.file("extdata/alb.rds", package = "ESD")))
mds
```


```
## $MOD13A1.006
## $MOD13A1.006$`2015-01-01`
## [1] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015001.500m_16_days_NDVI.tif"                     
## [2] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015001.500m_16_days_VI_Quality.tif"               
## [3] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015001.500m_16_days_composite_day_of_the_year.tif"
## [4] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015001.500m_16_days_pixel_reliability.tif"        
## 
## $MOD13A1.006$`2015-01-17`
## [1] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015017.500m_16_days_NDVI.tif"                     
## [2] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015017.500m_16_days_VI_Quality.tif"               
## [3] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015017.500m_16_days_composite_day_of_the_year.tif"
## [4] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MOD13A1.A2015017.500m_16_days_pixel_reliability.tif"        
## 
## 
## $MYD13A1.006
## $MYD13A1.006$`2015-01-09`
## [1] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015009.500m_16_days_NDVI.tif"                     
## [2] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015009.500m_16_days_VI_Quality.tif"               
## [3] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015009.500m_16_days_composite_day_of_the_year.tif"
## [4] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015009.500m_16_days_pixel_reliability.tif"        
## 
## $MYD13A1.006$`2015-01-25`
## [1] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015025.500m_16_days_NDVI.tif"                     
## [2] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015025.500m_16_days_VI_Quality.tif"               
## [3] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015025.500m_16_days_composite_day_of_the_year.tif"
## [4] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/MCD13A1.006_Alb/MYD13A1.A2015025.500m_16_days_pixel_reliability.tif"
```


```r
## GIMMS download
gms <- download("GIMMS", x = as.Date("2015-01-01"), y = as.Date("2015-01-31"),
                dsn = paste0(getOption("MODIS_outDirPath"), "NDVI3g.v1_alb"))
gms
```

```
## [1] "/media/permanent/programming/r/MODIS/MODIS_ARC/PROCESSED/NDVI3g.v1_alb/ndvi3g_geo_v1_2015_0106.nc4"
```

<hr>

#### Data Preprocessing
Similar to `download`, data preprocessing via `preprocess` is currently available for MODIS (`preprocessMODIS`) and GIMMS (`preprocessGIMMS`) only and includes

* optional layer clipping given that 'ext' is specified; 
* application of scale factors;
* comprehensive quality control;
* in the case of MODIS, 
  + creation of product-specific half-monthly composites similar to the release interval of GIMMS NDVI3g
  + calculation of half-monthly maximum value composites from Terra and Aqua-MODIS imagery;
* subsequent application of the modified Whittaker smoother. 

Again, `preprocess` serves merely as a wrapper around the underlying functions and, consequently, additional arguments can be specified via '...'.


```r
## Number of cores for parallel processing (use all but one)
cores <- parallel::detectCores() - 1

## MODIS preprocessing
mds_ppc <- preprocess("MODIS", mds, cores = cores)
mds_ppc
```


```
## class       : RasterStack 
## dimensions  : 33, 79, 2607, 2  (nrow, ncol, ncell, nlayers)
## resolution  : 0.004855338, 0.004782765  (x, y)
## extent      : 9.198695, 9.582267, 48.35297, 48.5108  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## names       : MCD.2015001.yL5000.ndvi, MCD.2015015.yL5000.ndvi 
## min values  :               0.2516754,               0.2439365 
## max values  :               0.7823204,               0.7684598
```


```r
## GIMMS preprocessing
gms_ppc <- preprocess("GIMMS", gms, cores = cores, keep = 0, # keep 'good' values only
                      ext = readRDS(system.file("extdata/alb.rds", package = "ESD")))
gms_ppc
```


```
## class       : RasterStack 
## dimensions  : 3, 5, 15, 2  (nrow, ncol, ncell, nlayers)
## resolution  : 0.08333333, 0.08333333  (x, y)
## extent      : 9.166667, 9.583333, 48.33333, 48.58333  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
## names       : MCD.2015001.yL5000.ndvi, MCD.2015015.yL5000.ndvi 
## min values  :               0.4063279,               0.3793259 
## max values  :               0.5634018,               0.5497134
```

<hr>

#### Evaluation of EOT-Based Spatial Downscaling
Of course, the creation of temporal composites -- and not to mention the application of the Whittaker smoother -- makes little sense when working with such a small number of layers. In the following, we will therefore utilize built-in NDVI datasets (2012--2015) originating from MODIS (MOD/MYD13A1.006) and GIMMS (NDVI3g.v1) to evaluate the performance of the EOT-based spatial downscaling procedure over a longer time period.

Once the data is in, model evaluation is rather straightforward and can easily be carried out through `evaluate`. The function allows to dynamically specify the number of downscaling repetitions and size of the training dataset and, for each run, calculates selected error and regression metrics which are subsequently averaged.


```r
mtr <- evaluate(pred = albGIMMS, resp = albMODIS, cores = cores, 
                n = 10L,     # calculate 10 EOT modes
                times = 10L, # repeat procedure 10 times
                size = 2L)   # with 2 years of training data
mtr

## ...
##
## Calculating linear model ... 
## Locating 10. EOT ...
## Location: 9.375 48.54167 
## Cum. expl. variance (%): 84.77944 
```


```
##             ME       ME.se        MAE       MAE.se       RMSE     RMSE.se       Rsq
## 1 -0.006718157 0.001613909 0.03370197 0.0009592951 0.04105545 0.001156508 0.8120697
```


```r
plotRegressionStats(mtr, 
                    xlim_rsq = c(0.75, 1.05), 
                    xlim_err = c(-0.012, 0.052))
```

<center>
  <img src="http://i.imgur.com/68X7Gle.png" alt="Error and Regression Metrics" style="width: 400px;"/>
  <br><br><b>Figure 1.</b> Error and regression metrics of EOT-based spatial downscaling of 8-km GIMMS NDVI (NDVI3g.v1) using 1-km MODIS NDVI (MOD/MYD13A1.006) from a 2-year training period.
</center>

<hr>

#### Actual EOT-Based Spatial Downscaling
Now that we know the method performs reasonably well, it is time to actually deduce EOT-based predictions from the coarse-resolution input data using `downscale`. As a short reminder, this is how the 1/12-degree GIMMS input data looks like: 


```r
plot(albGIMMS[[49:52]], zlim = c(0.59, 0.71))
```

<center>
  <img src="http://i.imgur.com/XLZVjAT.png" alt="1/12-degree GIMMS NDVI3g.v1" style="width: 550px;"/>
  <br><br><b>Figure 2.</b> 1/12-degree GIMMS NDVI3g.v1 during the first four half-monthly time steps in 2014.
</center>

And here is the downscaled data resulting from EOT analysis:


```r
prd <- downscale(pred = albGIMMS[[1:48]], resp = albMODIS[[1:48]], neot = 10L, 
                 newdata = albGIMMS[[49:96]], var = 0.85)
plot(prd[[1:4]], zlim = c(0.31, 0.81))
```

<center>
  <img src="http://i.imgur.com/IGSO9Ad.png" alt="1-km EOT-based NDVI" style="width: 550px;"/>
  <br><br><b>Figure 3.</b> Resulting 1-km EOT-based NDVI during the first four half-monthly time steps in 2014.
</center>
