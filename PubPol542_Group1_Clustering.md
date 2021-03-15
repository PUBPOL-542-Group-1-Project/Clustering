PubPol 542: Group 1 Clustering
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
    ##  Min.   :  0.0   Min.   :  0.0   Min.   :   12228  
    ##  1st Qu.: 19.5   1st Qu.: 20.0   1st Qu.: 2630490  
    ##  Median : 86.0   Median : 65.0   Median : 6321022  
    ##  Mean   :154.7   Mean   :109.5   Mean   :10256791  
    ##  3rd Qu.:278.5   3rd Qu.:171.0   3rd Qu.:18548976  
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
    ##  Min.   :-1.0162   Min.   :-0.9957   Min.   :-1.1602  
    ##  1st Qu.:-0.8881   1st Qu.:-0.8138   1st Qu.:-0.8637  
    ##  Median :-0.4513   Median :-0.4046   Median :-0.4457  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.8130   3rd Qu.: 0.5592   3rd Qu.: 0.9391  
    ##  Max.   : 2.7998   Max.   : 4.0598   Max.   : 2.9700

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

    ##                                  Graduate      White Expenditure
    ## Harbor High School            -0.92421174 -0.7228874  -0.9622534
    ## J M Weatherwax High School     0.04127068  0.1499943   0.4474569
    ## Anacortes High School          0.04127068  0.3045671   0.1063878
    ## Cap Sante High School         -0.91764383 -0.6865173  -1.0834305
    ## Arlington High School          1.38112384  1.9594053   1.2360215
    ## Arlington Special Educ School -1.01616245 -0.9865704  -1.0197666

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
    ##   [5] "Arlington Special Educ School"                                    
    ##   [6] "Weston High School"                                               
    ##   [7] "Asotin Jr Sr High"                                                
    ##   [8] "West Auburn Senior High School"                                   
    ##   [9] "Eagle Harbor High School"                                         
    ##  [10] "Homelink River"                                                   
    ##  [11] "Summit View High School"                                          
    ##  [12] "Options High School"                                              
    ##  [13] "Acceleration Academy"                                             
    ##  [14] "Challenger High School"                                           
    ##  [15] "Blaine High School"                                               
    ##  [16] "Renaissance Alternative High School"                              
    ##  [17] "Bridgeport High School"                                           
    ##  [18] "Burlington-Edison Alternative School"                             
    ##  [19] "Hayes Freedom High School"                                        
    ##  [20] "Cascade High School"                                              
    ##  [21] "CASHMERE HIGH SCHOOL"                                             
    ##  [22] "Castle Rock High School"                                          
    ##  [23] "Barker Creek Community School"                                    
    ##  [24] "Klahowya Secondary"                                               
    ##  [25] "CVSD Open Doors Programs"                                         
    ##  [26] "Mica Peak High School"                                            
    ##  [27] "Futurus High School"                                              
    ##  [28] "Lewis County Alternative School"                                  
    ##  [29] "Cheney Open Doors"                                                
    ##  [30] "Three Springs High School"                                        
    ##  [31] "Quartzite Learning"                                               
    ##  [32] "Chief Leschi Schools"                                             
    ##  [33] "Chimacum Junior/Senior High School"                               
    ##  [34] "Charles Francis Adams High School"                                
    ##  [35] "Educational Opportunity Center"                                   
    ##  [36] "Special Services"                                                 
    ##  [37] "Cle Elum Roslyn High School"                                      
    ##  [38] "Swiftwater Alternative High School"                               
    ##  [39] "CPSD Open Doors Program"                                          
    ##  [40] "Oakridge Group Home"                                              
    ##  [41] "Transition Day Students"                                          
    ##  [42] "Colfax High School"                                               
    ##  [43] "College Place High School"                                        
    ##  [44] "Columbia High School"                                             
    ##  [45] "Colville Senior High School"                                      
    ##  [46] "Panorama School"                                                  
    ##  [47] "Concrete High School"                                             
    ##  [48] "Twin Cedars High School"                                          
    ##  [49] "Coupeville High School"                                           
    ##  [50] "Crescent School"                                                  
    ##  [51] "Curlew Elem & High School"                                        
    ##  [52] "Cusick Jr Sr High School"                                         
    ##  [53] "Home Pride"                                                       
    ##  [54] "Darrington High School"                                           
    ##  [55] "Davenport Senior High School"                                     
    ##  [56] "Dayton High School"                                               
    ##  [57] "Deer Park High School"                                            
    ##  [58] "EV Online"                                                        
    ##  [59] "EV Parent Partnership"                                            
    ##  [60] "East Valley High School"                                          
    ##  [61] "Eatonville High School"                                           
    ##  [62] "ESD New Beginnings"                                               
    ##  [63] "Edmonds eLearning Academy"                                        
    ##  [64] "Edmonds Heights K-12"                                             
    ##  [65] "Scriber Lake High School"                                         
    ##  [66] "K-12 Ellensburg Learning Center"                                  
    ##  [67] "Elma High School"                                                 
    ##  [68] "Entiat Middle and High School"                                    
    ##  [69] "Ephrata High School"                                              
    ##  [70] "Everett Reengagement Academy"                                     
    ##  [71] "Port Gardner"                                                     
    ##  [72] "Sequoia High School"                                              
    ##  [73] "Legacy High School"                                               
    ##  [74] "Career Academy at Truman High School"                             
    ##  [75] "Internet Academy"                                                 
    ##  [76] "Open Doors Youth Reengagement (1418)"                             
    ##  [77] "River View High School"                                           
    ##  [78] "Gates Secondary School"                                           
    ##  [79] "Washington High School"                                           
    ##  [80] "Garfield at Palouse High School"                                  
    ##  [81] "Goldendale High School"                                           
    ##  [82] "Lake Roosevelt Alternative School"                                
    ##  [83] "Contract Learning Center"                                         
    ##  [84] "Grandview High School"                                            
    ##  [85] "Granger High School"                                              
    ##  [86] "Crossroads High School"                                           
    ##  [87] "Granite Falls High School"                                        
    ##  [88] "Highland High School"                                             
    ##  [89] "Evergreen High School"                                            
    ##  [90] "Highline High School"                                             
    ##  [91] "Highline Open Doors 1418"                                         
    ##  [92] "New Start"                                                        
    ##  [93] "Tyee High School"                                                 
    ##  [94] "Hockinson High School"                                            
    ##  [95] "Hoquiam High School"                                              
    ##  [96] "Inchelium High School"                                            
    ##  [97] "Gibson Ek High School"                                            
    ##  [98] "Kalama High School"                                               
    ##  [99] "Loowit High School"                                               
    ## [100] "Legacy High School"                                               
    ## [101] "Mid-Columbia Parent Partnership"                                  
    ## [102] "Phoenix High School"                                              
    ## [103] "Individualized Graduation & Degree Program"                       
    ## [104] "Kent Phoenix Academy"                                             
    ## [105] "Columbia Virtual Academy - Kettle Falls"                          
    ## [106] "Kettle Falls High School"                                         
    ## [107] "Kiona-Benton City High School"                                    
    ## [108] "Kittitas High School"                                             
    ## [109] "La Center High School"                                            
    ## [110] "La Conner High School"                                            
    ## [111] "Chelan High School"                                               
    ## [112] "Emerson High School"                                              
    ## [113] "Futures School"                                                   
    ## [114] "Lakewood High School"                                             
    ## [115] "Lind-Ritzville High School"                                       
    ## [116] "Discovery High School"                                            
    ## [117] "Mark Morris High School"                                          
    ## [118] "R A Long High School"                                             
    ## [119] "Lopez Middle High School"                                         
    ## [120] "Lummi Nation School"                                              
    ## [121] "Lyle High School"                                                 
    ## [122] "IMPACT Reengagement Program"                                      
    ## [123] "Mabton Jr. Sr. High"                                              
    ## [124] "Manson High School"                                               
    ## [125] "Mary M. Knight School"                                            
    ## [126] "Mary Walker High School"                                          
    ## [127] "Heritage School"                                                  
    ## [128] "Legacy High School"                                               
    ## [129] "Mead Alternative High School (Closed after 2018-2019 school year)"
    ## [130] "Medical Lake Endeavors"                                           
    ## [131] "Medical Lake High School"                                         
    ## [132] "Meridian High School"                                             
    ## [133] "Leaders In Learning"                                              
    ## [134] "Sky Valley Education Center"                                      
    ## [135] "Montesano Jr-Sr High"                                             
    ## [136] "Morton Junior-Senior High"                                        
    ## [137] "Skill Source Reingagement Program"                                
    ## [138] "White Swan High School"                                           
    ## [139] "Mount Baker Senior High"                                          
    ## [140] "Skagit Academy"                                                   
    ## [141] "Muckleshoot Tribal School"                                        
    ## [142] "ACES High School"                                                 
    ## [143] "Mukilteo Reengagement Academy Open Doors"                         
    ## [144] "Naches Valley High School"                                        
    ## [145] "Napavine Jr Sr High School"                                       
    ## [146] "Newport High School"                                              
    ## [147] "Pend Oreille River School"                                        
    ## [148] "Lakeside High School"                                             
    ## [149] "Nooksack Valley High School"                                      
    ## [150] "North Beach Senior High School"                                   
    ## [151] "Palouse Junction High School"                                     
    ## [152] "Kingston High School"                                             
    ## [153] "James A. Taylor High School"                                      
    ## [154] "North Mason Senior High School"                                   
    ## [155] "South Sound High School"                                          
    ## [156] "Northport High School"                                            
    ## [157] "Northshore Networks"                                              
    ## [158] "Northshore Online Reengagement Program"                           
    ## [159] "Secondary Academy for Success"                                    
    ## [160] "Oakville High School"                                             
    ## [161] "Ilwaco High School"                                               
    ## [162] "Ocosta Junior - Senior High"                                      
    ## [163] "Odessa High School"                                               
    ## [164] "Okanogan Alternative High School"                                 
    ## [165] "Okanogan Outreach Alternative School"                             
    ## [166] "Avanti High School"                                               
    ## [167] "Olympia Regional Learning Academy"                                
    ## [168] "Omak High School"                                                 
    ## [169] "Washington Virtual Academy Omak High School"                      
    ## [170] "Orcas Island High School"                                         
    ## [171] "Oroville Middle-High School"                                      
    ## [172] "Orting High School"                                               
    ## [173] "Desert Oasis High School"                                         
    ## [174] "Othello High School"                                              
    ## [175] "New Horizons High School"                                         
    ## [176] "Pe Ell School"                                                    
    ## [177] "Henderson Bay Alt High School"                                    
    ## [178] "Lincoln High School"                                              
    ## [179] "Port Townsend High School"                                        
    ## [180] "Prosser High School"                                              
    ## [181] "Pullman High School"                                              
    ## [182] "E B Walker High School"                                           
    ## [183] "Puyallup Online Academy/POA"                                      
    ## [184] "Forks High School"                                                
    ## [185] "Quincy High School"                                               
    ## [186] "Quincy Innovation Academy"                                        
    ## [187] "Rainier Senior High School"                                       
    ## [188] "Renton Academy"                                                   
    ## [189] "Talley High School"                                               
    ## [190] "Republic Senior High School"                                      
    ## [191] "Rivers Edge High School"                                          
    ## [192] "Three Rivers Home Link"                                           
    ## [193] "Ridgefield High School"                                           
    ## [194] "Ritzville High School"                                            
    ## [195] "Riverside High School"                                            
    ## [196] "CLIP"                                                             
    ## [197] "H.e.a.r.t. High School"                                           
    ## [198] "Rochester High School"                                            
    ## [199] "Royal High School"                                                
    ## [200] "Friday Harbor High School"                                        
    ## [201] "Alan T. Sugiyama High School"                                     
    ## [202] "Cleveland High School STEM"                                       
    ## [203] "Interagency Programs"                                             
    ## [204] "Middle College High School"                                       
    ## [205] "Nova High School"                                                 
    ## [206] "Rainier Beach High School"                                        
    ## [207] "Seattle World School"                                             
    ## [208] "The Center School"                                                
    ## [209] "State Street High School"                                         
    ## [210] "Selah Academy Online"                                             
    ## [211] "Sequim Senior High"                                               
    ## [212] "Choice Middle and High School"                                    
    ## [213] "Fircrest Residential Habilitation"                                
    ## [214] "AIM High School"                                                  
    ## [215] "Two Rivers School"                                                
    ## [216] "Soap Lake Middle & High School"                                   
    ## [217] "South Bend High School"                                           
    ## [218] "Discovery"                                                        
    ## [219] "Explorer Academy"                                                 
    ## [220] "South Whidbey Academy"                                            
    ## [221] "South Whidbey High School"                                        
    ## [222] "On Track Academy"                                                 
    ## [223] "Pratt Academy"                                                    
    ## [224] "Lincoln Hill High School"                                         
    ## [225] "Steilacoom High"                                                  
    ## [226] "Stevenson High School"                                            
    ## [227] "Sultan Senior High School"                                        
    ## [228] "Summit Public School: Olympus"                                    
    ## [229] "Summit Public School: Sierra"                                     
    ## [230] "Chief Kitsap Academy"                                             
    ## [231] "Foss"                                                             
    ## [232] "Oakland High School"                                              
    ## [233] "Tacoma Open Doors"                                                
    ## [234] "Taholah High School"                                              
    ## [235] "Tekoa High School"                                                
    ## [236] "Tenino High School"                                               
    ## [237] "Cowlitz Prairie Academy"                                          
    ## [238] "Toledo High School"                                               
    ## [239] "Tonasket High School"                                             
    ## [240] "Computer Academy Toppenish High School"                           
    ## [241] "Northwest Allprep"                                                
    ## [242] "Toppenish High School"                                            
    ## [243] "Foster Senior High School"                                        
    ## [244] "A G West Black Hills High School"                                 
    ## [245] "Cascadia High School"                                             
    ## [246] "New Market High School"                                           
    ## [247] "Paideia High School"                                              
    ## [248] "Student Link"                                                     
    ## [249] "Vashon Island High School"                                        
    ## [250] "Wahkiakum High School"                                            
    ## [251] "Sentinel Tech Alt School"                                         
    ## [252] "Wahluke High School"                                              
    ## [253] "Walla Walla Open Doors"                                           
    ## [254] "Pace Alternative High School"                                     
    ## [255] "Wapato High School"                                               
    ## [256] "Warden High School"                                               
    ## [257] "Wellpinit Fort Simcoe SEA"                                        
    ## [258] "Wellpinit High School"                                            
    ## [259] "Westside High School"                                             
    ## [260] "Dishman Hills High School"                                        
    ## [261] "Spokane Valley Transition School"                                 
    ## [262] "West Valley High School"                                          
    ## [263] "WEST VALLEY VIRTUAL ACADEMY 9-12"                                 
    ## [264] "White Pass Jr. Sr. High School"                                   
    ## [265] "Columbia High School"                                             
    ## [266] "White Salmon Academy"                                             
    ## [267] "Winolequa Learning Academy"                                       
    ## [268] "Woodland Alternative School"                                      
    ## [269] "Woodland High School"                                             
    ## [270] "Stanton Academy"                                                  
    ## [271] "Yakima Online"                                                    
    ## [272] "Yelm Extension School"                                            
    ## [273] "Zillah High School"

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
    ## 273  50  99   1

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
    ## 1       1  273          0.65
    ## 2       2   50          0.38
    ## 3       3   99          0.42
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
    ## 85        1        3 0.8090322
    ## 260       1        3 0.8088961
    ## 298       1        3 0.8084699
    ## 195       1        3 0.8082257
    ## 83        1        3 0.8080059
    ## 340       1        3 0.8074296

Now, let’s look the anomalies (negative silhoueettes)

``` r
## Requesting negative silhouettes  
  diaEval[diaEval$sil_width<0,] 
```

    ##     cluster neighbor     sil_width
    ## 231       1        3 -0.0064174878
    ## 3         1        3 -0.0168348253
    ## 276       1        3 -0.0546356878
    ## 380       1        3 -0.0585122819
    ## 90        1        3 -0.1229116024
    ## 2         1        3 -0.1844101604
    ## 407       1        3 -0.1969623702
    ## 45        1        3 -0.1997714464
    ## 141       1        3 -0.2001271015
    ## 186       1        3 -0.2039865185
    ## 262       1        3 -0.2290877060
    ## 353       1        3 -0.2542752534
    ## 185       1        3 -0.2623178220
    ## 130       1        3 -0.2711354085
    ## 326       1        3 -0.2762367124
    ## 422       3        2 -0.0001870989

There are 15 rows with negative silhouettes, which is okay.
