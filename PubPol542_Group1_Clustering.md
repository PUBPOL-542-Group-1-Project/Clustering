PubPol542: Group 1 Clustering
================

### Set up R session

1.  First, we need to clean the data environment.

<!-- end list -->

``` r
## Clean data environment  
  rm(list=ls()) 
```

2.  Next, we need to load our data from github.

<!-- end list -->

``` r
## Loading data
  dat <- readRDS(gzcon(url("https://github.com/PUBPOL-542-Group-1-Project/Merging-Dataframes/raw/main/datafiles/finaldata.RDS")))
```

### Preparing the Data

We need to explore the variables we will use for clustering.

1.  We can create a subset of the full data set that includes just the
    variables of interest (“Graduate”, “White”, “Expenditure”).

<!-- end list -->

``` r
## Subset the full dataset with variables of interest
  dfClus=dat[,c('Graduate','White','Expenditure')]
```

2.  Let’s look at the summary statistics of these variables (minimum,
    median, mean, maximum, etc.).

<!-- end list -->

``` r
## Explore Variables
  summary(dfClus)
```

    ##     Graduate         White        Expenditure      
    ##  Min.   :  0.0   Min.   :  0.0   Min.   :   12229  
    ##  1st Qu.: 21.5   1st Qu.: 21.5   1st Qu.: 2758407  
    ##  Median : 92.0   Median : 69.0   Median : 6621264  
    ##  Mean   :157.7   Mean   :111.6   Mean   :10444077  
    ##  3rd Qu.:280.0   3rd Qu.:174.5   3rd Qu.:18851846  
    ##  Max.   :581.0   Max.   :556.0   Max.   :36481816

Next, we need to rescale the units if needed into a new variable so our
data is standardized.

1.  Let’s rescale the variables of interest (using the subset of the
    dataset created earlier).

<!-- end list -->

``` r
## Rescale Variables
  dfClus=scale(dfClus)
```

2.  Now, let’s look at the summary statistics of these variables again
    to see if this changed anything.

<!-- end list -->

``` r
## Explore Variables
  summary(dfClus)
```

    ##     Graduate           White          Expenditure     
    ##  Min.   :-1.0362   Min.   :-1.0143   Min.   :-1.1842  
    ##  1st Qu.:-0.8949   1st Qu.:-0.8189   1st Qu.:-0.8725  
    ##  Median :-0.4317   Median :-0.3871   Median :-0.4340  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.8037   3rd Qu.: 0.5720   3rd Qu.: 0.9544  
    ##  Max.   : 2.7815   Max.   : 4.0401   Max.   : 2.9558

Now, we need to rename subset indexes to the School names.

``` r
## Rename subset indexes to the school names
  row.names(dfClus)=dat$SchoolName
```

Let’s verify that this worked by looking at the first 12 rows of our
subsetted data.

``` r
## Verify input
  head(dfClus)
```

    ##                               Graduate      White Expenditure
    ## Harbor High School         -0.94418579 -0.7416205 -0.98578736
    ## J M Weatherwax High School  0.02173945  0.1310804  0.42725339
    ## Anacortes High School       0.02173945  0.2856212  0.08537848
    ## Cap Sante High School      -0.93761487 -0.7052580 -1.10725081
    ## Arlington High School       1.36220714  1.9401166  1.21768095
    ## Weston High School         -0.90476027 -0.6143516 -0.86297020

Now, let’s set a random seed for replicability of our results.

``` r
## Set random seed
  set.seed(123) # this is for replicability of results
```

Next, we need to decide on the distance method and compute the distance
matrix. A distance matrix is a table that shows the distance between
pairs of objects.

1.  To do this, we need to use the cluster package in R.

<!-- end list -->

``` r
## Load the cluster package 
  library(cluster)
```

2.  Before we compute the distance matrix, we need to remove duplicated
    rows (duplicated School Names).

<!-- end list -->

``` r
## Remove duplicated elements
  rownames(dfClus) <- c()
```

3.  Now, we can compute the distance matrix on our subsetted data.

<!-- end list -->

``` r
## Compute the distance matrix
  dfClus_D=cluster::daisy(x=dfClus)
```

### Hierarchizing: Divisive

We will be using the divisive hierarchical clustering technique. In
using the hierarchizing technique, we will be asking the algorithm to
find all possible ways cases can be clustered, individually and in
subgroups following a tree-construction/deconstruction approach. The
divisive hierarchical clustering technique uses a top-down approach.
Initially, all the points in the dataset belong to one cluster. Then, we
partition the cluster into two least similar clusters. Finally, we
proceed reecursively to from new clusters until the desired number of
clusters are obtained.

## 1\. Applying the function

1.1. To do this, we need to use the cluster package in R. we will load
the ggplot2 package first, as this is required before uploading the
factoextra package.

``` r
## Load the ggplot2 and factoextra packages
  library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.2

``` r
  library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 3.6.2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

1.2. Now, we need to indicate the amount of clusters that are required.
We will use 4 in this case.

``` r
## Indicating the number of clusters required
  NumCluster=4
```

1.3. Finally, we can apply the function. We will have to indicate that
we are using the subsetted data, indicate that 4 clusters are required,
indicate the function (“diana”), and indicate the method (“ward.D”).

``` r
## Apply the function
  res.diana = hcut(dfClus_D, 
                k = NumCluster,
                hc_func="diana",
                hc_method = "ward.D")
```

## 2\. Clustering Results

2.1. To do this, we need to add results to the original data frame as a
factor variable.

``` r
## Add results to original dataframe
  dat$dia=as.factor(res.diana$cluster)
```

2.2. Let’s check the data frame to see if this worked.

``` r
## Query dataframe
  dat[dat$dia==1,'SchoolName']
```

    ##   [1] "Harbor High School"                                               
    ##   [2] "J M Weatherwax High School"                                       
    ##   [3] "Anacortes High School"                                            
    ##   [4] "Cap Sante High School"                                            
    ##   [5] "Weston High School"                                               
    ##   [6] "Asotin Jr Sr High"                                                
    ##   [7] "West Auburn Senior High School"                                   
    ##   [8] "Eagle Harbor High School"                                         
    ##   [9] "Homelink River"                                                   
    ##  [10] "Summit View High School"                                          
    ##  [11] "Options High School"                                              
    ##  [12] "Acceleration Academy"                                             
    ##  [13] "Challenger High School"                                           
    ##  [14] "Blaine High School"                                               
    ##  [15] "Renaissance Alternative High School"                              
    ##  [16] "Bridgeport High School"                                           
    ##  [17] "Burlington-Edison Alternative School"                             
    ##  [18] "Hayes Freedom High School"                                        
    ##  [19] "Cascade High School"                                              
    ##  [20] "CASHMERE HIGH SCHOOL"                                             
    ##  [21] "Castle Rock High School"                                          
    ##  [22] "Barker Creek Community School"                                    
    ##  [23] "Klahowya Secondary"                                               
    ##  [24] "CVSD Open Doors Programs"                                         
    ##  [25] "Mica Peak High School"                                            
    ##  [26] "Futurus High School"                                              
    ##  [27] "Lewis County Alternative School"                                  
    ##  [28] "Cheney Open Doors"                                                
    ##  [29] "Three Springs High School"                                        
    ##  [30] "Quartzite Learning"                                               
    ##  [31] "Chief Leschi Schools"                                             
    ##  [32] "Chimacum Junior/Senior High School"                               
    ##  [33] "Charles Francis Adams High School"                                
    ##  [34] "Educational Opportunity Center"                                   
    ##  [35] "Cle Elum Roslyn High School"                                      
    ##  [36] "Swiftwater Alternative High School"                               
    ##  [37] "CPSD Open Doors Program"                                          
    ##  [38] "Oakridge Group Home"                                              
    ##  [39] "Colfax High School"                                               
    ##  [40] "College Place High School"                                        
    ##  [41] "Columbia High School"                                             
    ##  [42] "Colville Senior High School"                                      
    ##  [43] "Panorama School"                                                  
    ##  [44] "Concrete High School"                                             
    ##  [45] "Twin Cedars High School"                                          
    ##  [46] "Coupeville High School"                                           
    ##  [47] "Crescent School"                                                  
    ##  [48] "Curlew Elem & High School"                                        
    ##  [49] "Cusick Jr Sr High School"                                         
    ##  [50] "Darrington High School"                                           
    ##  [51] "Davenport Senior High School"                                     
    ##  [52] "Dayton High School"                                               
    ##  [53] "Deer Park High School"                                            
    ##  [54] "EV Online"                                                        
    ##  [55] "EV Parent Partnership"                                            
    ##  [56] "East Valley High School"                                          
    ##  [57] "Eatonville High School"                                           
    ##  [58] "Edmonds eLearning Academy"                                        
    ##  [59] "Edmonds Heights K-12"                                             
    ##  [60] "Scriber Lake High School"                                         
    ##  [61] "K-12 Ellensburg Learning Center"                                  
    ##  [62] "Elma High School"                                                 
    ##  [63] "Entiat Middle and High School"                                    
    ##  [64] "Ephrata High School"                                              
    ##  [65] "Everett Reengagement Academy"                                     
    ##  [66] "Port Gardner"                                                     
    ##  [67] "Sequoia High School"                                              
    ##  [68] "Legacy High School"                                               
    ##  [69] "Career Academy at Truman High School"                             
    ##  [70] "Internet Academy"                                                 
    ##  [71] "Open Doors Youth Reengagement (1418)"                             
    ##  [72] "River View High School"                                           
    ##  [73] "Gates Secondary School"                                           
    ##  [74] "Washington High School"                                           
    ##  [75] "Garfield at Palouse High School"                                  
    ##  [76] "Goldendale High School"                                           
    ##  [77] "Lake Roosevelt Alternative School"                                
    ##  [78] "Contract Learning Center"                                         
    ##  [79] "Grandview High School"                                            
    ##  [80] "Granger High School"                                              
    ##  [81] "Crossroads High School"                                           
    ##  [82] "Granite Falls High School"                                        
    ##  [83] "Highland High School"                                             
    ##  [84] "Evergreen High School"                                            
    ##  [85] "Highline High School"                                             
    ##  [86] "Highline Open Doors 1418"                                         
    ##  [87] "New Start"                                                        
    ##  [88] "Tyee High School"                                                 
    ##  [89] "Hockinson High School"                                            
    ##  [90] "Hoquiam High School"                                              
    ##  [91] "Inchelium High School"                                            
    ##  [92] "Gibson Ek High School"                                            
    ##  [93] "Kalama High School"                                               
    ##  [94] "Loowit High School"                                               
    ##  [95] "Legacy High School"                                               
    ##  [96] "Mid-Columbia Parent Partnership"                                  
    ##  [97] "Phoenix High School"                                              
    ##  [98] "Individualized Graduation & Degree Program"                       
    ##  [99] "Kent Phoenix Academy"                                             
    ## [100] "Columbia Virtual Academy - Kettle Falls"                          
    ## [101] "Kettle Falls High School"                                         
    ## [102] "Kiona-Benton City High School"                                    
    ## [103] "Kittitas High School"                                             
    ## [104] "La Center High School"                                            
    ## [105] "La Conner High School"                                            
    ## [106] "Chelan High School"                                               
    ## [107] "Emerson High School"                                              
    ## [108] "Futures School"                                                   
    ## [109] "Lakewood High School"                                             
    ## [110] "Lind-Ritzville High School"                                       
    ## [111] "Discovery High School"                                            
    ## [112] "Mark Morris High School"                                          
    ## [113] "R A Long High School"                                             
    ## [114] "Lopez Middle High School"                                         
    ## [115] "Lummi Nation School"                                              
    ## [116] "Lyle High School"                                                 
    ## [117] "Lynden High School"                                               
    ## [118] "Mabton Jr. Sr. High"                                              
    ## [119] "Manson High School"                                               
    ## [120] "Mary M. Knight School"                                            
    ## [121] "Mary Walker High School"                                          
    ## [122] "Heritage School"                                                  
    ## [123] "Legacy High School"                                               
    ## [124] "Mead Alternative High School (Closed after 2018-2019 school year)"
    ## [125] "Medical Lake Endeavors"                                           
    ## [126] "Medical Lake High School"                                         
    ## [127] "Meridian High School"                                             
    ## [128] "Leaders In Learning"                                              
    ## [129] "Sky Valley Education Center"                                      
    ## [130] "Montesano Jr-Sr High"                                             
    ## [131] "Morton Junior-Senior High"                                        
    ## [132] "Skill Source Reingagement Program"                                
    ## [133] "White Swan High School"                                           
    ## [134] "Mount Baker Senior High"                                          
    ## [135] "Skagit Academy"                                                   
    ## [136] "Muckleshoot Tribal School"                                        
    ## [137] "ACES High School"                                                 
    ## [138] "Mukilteo Reengagement Academy Open Doors"                         
    ## [139] "Naches Valley High School"                                        
    ## [140] "Napavine Jr Sr High School"                                       
    ## [141] "Newport High School"                                              
    ## [142] "Pend Oreille River School"                                        
    ## [143] "Lakeside High School"                                             
    ## [144] "Nooksack Valley High School"                                      
    ## [145] "North Beach Senior High School"                                   
    ## [146] "Palouse Junction High School"                                     
    ## [147] "Kingston High School"                                             
    ## [148] "James A. Taylor High School"                                      
    ## [149] "North Mason Senior High School"                                   
    ## [150] "South Sound High School"                                          
    ## [151] "Northport High School"                                            
    ## [152] "Northshore Networks"                                              
    ## [153] "Northshore Online Reengagement Program"                           
    ## [154] "Secondary Academy for Success"                                    
    ## [155] "Oakville High School"                                             
    ## [156] "Ilwaco High School"                                               
    ## [157] "Ocosta Junior - Senior High"                                      
    ## [158] "Odessa High School"                                               
    ## [159] "Okanogan Alternative High School"                                 
    ## [160] "Okanogan Outreach Alternative School"                             
    ## [161] "Avanti High School"                                               
    ## [162] "Olympia Regional Learning Academy"                                
    ## [163] "Omak High School"                                                 
    ## [164] "Washington Virtual Academy Omak High School"                      
    ## [165] "Orcas Island High School"                                         
    ## [166] "Oroville Middle-High School"                                      
    ## [167] "Orting High School"                                               
    ## [168] "Desert Oasis High School"                                         
    ## [169] "Othello High School"                                              
    ## [170] "New Horizons High School"                                         
    ## [171] "Pe Ell School"                                                    
    ## [172] "Henderson Bay Alt High School"                                    
    ## [173] "Lincoln High School"                                              
    ## [174] "Port Townsend High School"                                        
    ## [175] "Prosser High School"                                              
    ## [176] "Pullman High School"                                              
    ## [177] "E B Walker High School"                                           
    ## [178] "Puyallup Online Academy/POA"                                      
    ## [179] "Forks High School"                                                
    ## [180] "Quincy High School"                                               
    ## [181] "Quincy Innovation Academy"                                        
    ## [182] "Rainier Senior High School"                                       
    ## [183] "Talley High School"                                               
    ## [184] "Republic Senior High School"                                      
    ## [185] "Rivers Edge High School"                                          
    ## [186] "Three Rivers Home Link"                                           
    ## [187] "Ridgefield High School"                                           
    ## [188] "Ritzville High School"                                            
    ## [189] "Riverside High School"                                            
    ## [190] "CLIP"                                                             
    ## [191] "H.e.a.r.t. High School"                                           
    ## [192] "Rochester High School"                                            
    ## [193] "Royal High School"                                                
    ## [194] "Friday Harbor High School"                                        
    ## [195] "Alan T. Sugiyama High School"                                     
    ## [196] "Chief Sealth International High School"                           
    ## [197] "Cleveland High School STEM"                                       
    ## [198] "Interagency Programs"                                             
    ## [199] "Middle College High School"                                       
    ## [200] "Nova High School"                                                 
    ## [201] "Rainier Beach High School"                                        
    ## [202] "Seattle World School"                                             
    ## [203] "The Center School"                                                
    ## [204] "State Street High School"                                         
    ## [205] "Selah Academy Online"                                             
    ## [206] "Sequim Senior High"                                               
    ## [207] "Choice Middle and High School"                                    
    ## [208] "AIM High School"                                                  
    ## [209] "Two Rivers School"                                                
    ## [210] "Soap Lake Middle & High School"                                   
    ## [211] "South Bend High School"                                           
    ## [212] "Discovery"                                                        
    ## [213] "Explorer Academy"                                                 
    ## [214] "South Whidbey Academy"                                            
    ## [215] "South Whidbey High School"                                        
    ## [216] "On Track Academy"                                                 
    ## [217] "Pratt Academy"                                                    
    ## [218] "Lincoln Hill High School"                                         
    ## [219] "Steilacoom High"                                                  
    ## [220] "Stevenson High School"                                            
    ## [221] "Sultan Senior High School"                                        
    ## [222] "Summit Public School: Olympus"                                    
    ## [223] "Summit Public School: Sierra"                                     
    ## [224] "Chief Kitsap Academy"                                             
    ## [225] "Foss"                                                             
    ## [226] "Oakland High School"                                              
    ## [227] "Tacoma Open Doors"                                                
    ## [228] "Taholah High School"                                              
    ## [229] "Tekoa High School"                                                
    ## [230] "Tenino High School"                                               
    ## [231] "Cowlitz Prairie Academy"                                          
    ## [232] "Toledo High School"                                               
    ## [233] "Tonasket High School"                                             
    ## [234] "Computer Academy Toppenish High School"                           
    ## [235] "Northwest Allprep"                                                
    ## [236] "Toppenish High School"                                            
    ## [237] "Foster Senior High School"                                        
    ## [238] "A G West Black Hills High School"                                 
    ## [239] "Cascadia High School"                                             
    ## [240] "New Market High School"                                           
    ## [241] "Paideia High School"                                              
    ## [242] "Student Link"                                                     
    ## [243] "Vashon Island High School"                                        
    ## [244] "Wahkiakum High School"                                            
    ## [245] "Sentinel Tech Alt School"                                         
    ## [246] "Wahluke High School"                                              
    ## [247] "Walla Walla Open Doors"                                           
    ## [248] "Pace Alternative High School"                                     
    ## [249] "Wapato High School"                                               
    ## [250] "Warden High School"                                               
    ## [251] "Wellpinit Fort Simcoe SEA"                                        
    ## [252] "Wellpinit High School"                                            
    ## [253] "Westside High School"                                             
    ## [254] "Dishman Hills High School"                                        
    ## [255] "Spokane Valley Transition School"                                 
    ## [256] "West Valley High School"                                          
    ## [257] "WEST VALLEY VIRTUAL ACADEMY 9-12"                                 
    ## [258] "White Pass Jr. Sr. High School"                                   
    ## [259] "Columbia High School"                                             
    ## [260] "White Salmon Academy"                                             
    ## [261] "Winolequa Learning Academy"                                       
    ## [262] "Woodland Alternative School"                                      
    ## [263] "Woodland High School"                                             
    ## [264] "Stanton Academy"                                                  
    ## [265] "Yakima Online"                                                    
    ## [266] "Yelm Extension School"                                            
    ## [267] "Zillah High School"

``` r
  dat[dat$SchoolName=="Homelink River",'dia']
```

    ## [1] 1
    ## Levels: 1 2 3 4

2.3. Now, we can make a table of the 4 clusters to tell us how many
observations are in each cluster.

``` r
## REPORT: Table of clusters
  table(dat$dia)
```

    ## 
    ##   1   2   3   4 
    ## 267  50  97   1

## 3\. Evaluate Results

3.1. To evaluate the results, let’s look at a dendrogram. This is a
diagram the illustrates the arrangement of clusters.

``` r
## REPORT: Dendogram
  fviz_dend(res.diana, k=NumCluster, cex = 0.7, horiz = T)
```

![](PubPol542_Group1_Clustering_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

3.2. Now, let’s look at the average silhouette of observations. The
average silhouette approach measures the quality of a clustering. That
is, it determines how well each object lies within its cluster. A high
average silhouette width indicates a good clustering. The average
silhouette method computes the average silhouette of observations for
different values of k.

``` r
## REPORT: Average silhouettes
  fviz_silhouette(res.diana)
```

    ##   cluster size ave.sil.width
    ## 1       1  267          0.64
    ## 2       2   50          0.38
    ## 3       3   97          0.42
    ## 4       4    1          0.00

![](PubPol542_Group1_Clustering_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

With an average silhouette scoree of 0.56, this seems reasonable.

3.3. Now, let’s try to detect any anomalies in our analysis (any
negative silhouettes).

First, we need to save the silhouettes as a dataframe (diaEval) and
check if this worked by calling the first 6 rows in that dataframe.

``` r
## REPORT: Detecting anomalies
  diaEval = data.frame(res.diana$silinfo$widths) # saving silhouettes
  head(diaEval)
```

    ##     cluster neighbor sil_width
    ## 81        1        3 0.8044321
    ## 254       1        3 0.8042798
    ## 189       1        3 0.8041018
    ## 291       1        3 0.8035818
    ## 384       1        3 0.8035799
    ## 79        1        3 0.8034809

Now, let’s look the anomalies (negative silhoueettes)

``` r
## Requesting negative silhouettes  
  diaEval[diaEval$sil_width<0,] 
```

    ##     cluster neighbor   sil_width
    ## 270       1        3 -0.02860021
    ## 372       1        3 -0.03256010
    ## 86        1        3 -0.09890075
    ## 2         1        3 -0.16357652
    ## 399       1        3 -0.17478573
    ## 136       1        3 -0.18182697
    ## 181       1        3 -0.18195141
    ## 44        1        3 -0.18211402
    ## 256       1        3 -0.20867157
    ## 345       1        3 -0.23434423
    ## 180       1        3 -0.24250913
    ## 125       1        3 -0.25309974
    ## 185       1        3 -0.25408195
    ## 319       1        3 -0.25670097
    ## 301       1        3 -0.29831181

There are 15 rows with negative silhouettes, which is okay.
