PubPol542\_Clustering
================

``` r
  rm(list=ls()) ## clean data environment

  options(digits = 3) ## formats output to 3 digits
  set.seed(1) ## set seed for any randomization that may follow
  
## Loading data
  dat <- readRDS(gzcon(url("https://github.com/PUBPOL-542-Group-1-Project/Merging-Dataframes/raw/main/datafiles/finaldata.RDS")))
```

### Preparing Data

``` r
## Explore Variables
  dfClus=dat[,c('Graduate','White','Expenditure')]
  summary(dfClus)
```

    ##     Graduate       White      Expenditure      
    ##  Min.   :  0   Min.   :  0   Min.   :   12229  
    ##  1st Qu.: 22   1st Qu.: 22   1st Qu.: 2758407  
    ##  Median : 92   Median : 69   Median : 6621264  
    ##  Mean   :158   Mean   :112   Mean   :10444077  
    ##  3rd Qu.:280   3rd Qu.:174   3rd Qu.:18851846  
    ##  Max.   :581   Max.   :556   Max.   :36481816

``` r
## Rescale Variables
  dfClus=scale(dfClus)
  summary(dfClus)
```

    ##     Graduate          White        Expenditure    
    ##  Min.   :-1.036   Min.   :-1.01   Min.   :-1.184  
    ##  1st Qu.:-0.895   1st Qu.:-0.82   1st Qu.:-0.872  
    ##  Median :-0.432   Median :-0.39   Median :-0.434  
    ##  Mean   : 0.000   Mean   : 0.00   Mean   : 0.000  
    ##  3rd Qu.: 0.804   3rd Qu.: 0.57   3rd Qu.: 0.954  
    ##  Max.   : 2.782   Max.   : 4.04   Max.   : 2.956

``` r
## Rename subset indices
  row.names(dfClus)=dat$SchoolName
  head(dfClus)
```

    ##                            Graduate  White Expenditure
    ## Harbor High School          -0.9442 -0.742     -0.9858
    ## J M Weatherwax High School   0.0217  0.131      0.4273
    ## Anacortes High School        0.0217  0.286      0.0854
    ## Cap Sante High School       -0.9376 -0.705     -1.1073
    ## Arlington High School        1.3622  1.940      1.2177
    ## Weston High School          -0.9048 -0.614     -0.8630

``` r
## Set random seed
  set.seed(123) # this is for replicability of results
  
## Compute Distance Matrix
  library(cluster)
  dfClus_D=cluster::daisy(x=dfClus)
```

### Partitioning Technique

``` r
#### APPLY FUNCTION ####

## Indicating the number of clusters required
  NumCluster=4
  res.pam = pam(x=dfClus_D,
              k = NumCluster,
              cluster.only = F)
  
#### CLUSTERING RESULTS ####
  
## Add results to the orginal dataframe
  dat$pam=as.factor(res.pam$clustering)
  
## Query dataframe
  dat[dat$pam==1,'SchoolName']
```

    ##   [1] "Harbor High School"                                               
    ##   [2] "Cap Sante High School"                                            
    ##   [3] "Weston High School"                                               
    ##   [4] "West Auburn Senior High School"                                   
    ##   [5] "Eagle Harbor High School"                                         
    ##   [6] "Options High School"                                              
    ##   [7] "Renaissance Alternative High School"                              
    ##   [8] "Bridgeport High School"                                           
    ##   [9] "Burlington-Edison Alternative School"                             
    ##  [10] "Hayes Freedom High School"                                        
    ##  [11] "CVSD Open Doors Programs"                                         
    ##  [12] "Futurus High School"                                              
    ##  [13] "Lewis County Alternative School"                                  
    ##  [14] "Cheney Open Doors"                                                
    ##  [15] "Three Springs High School"                                        
    ##  [16] "Quartzite Learning"                                               
    ##  [17] "Chief Leschi Schools"                                             
    ##  [18] "Educational Opportunity Center"                                   
    ##  [19] "Cle Elum Roslyn High School"                                      
    ##  [20] "Swiftwater Alternative High School"                               
    ##  [21] "CPSD Open Doors Program"                                          
    ##  [22] "Oakridge Group Home"                                              
    ##  [23] "Colfax High School"                                               
    ##  [24] "Columbia High School"                                             
    ##  [25] "Panorama School"                                                  
    ##  [26] "Concrete High School"                                             
    ##  [27] "Twin Cedars High School"                                          
    ##  [28] "Crescent School"                                                  
    ##  [29] "Curlew Elem & High School"                                        
    ##  [30] "Cusick Jr Sr High School"                                         
    ##  [31] "Darrington High School"                                           
    ##  [32] "Dayton High School"                                               
    ##  [33] "EV Online"                                                        
    ##  [34] "EV Parent Partnership"                                            
    ##  [35] "Edmonds eLearning Academy"                                        
    ##  [36] "K-12 Ellensburg Learning Center"                                  
    ##  [37] "Entiat Middle and High School"                                    
    ##  [38] "Everett Reengagement Academy"                                     
    ##  [39] "Port Gardner"                                                     
    ##  [40] "Career Academy at Truman High School"                             
    ##  [41] "Internet Academy"                                                 
    ##  [42] "Open Doors Youth Reengagement (1418)"                             
    ##  [43] "River View High School"                                           
    ##  [44] "Gates Secondary School"                                           
    ##  [45] "Garfield at Palouse High School"                                  
    ##  [46] "Lake Roosevelt Alternative School"                                
    ##  [47] "Contract Learning Center"                                         
    ##  [48] "Crossroads High School"                                           
    ##  [49] "New Start"                                                        
    ##  [50] "Inchelium High School"                                            
    ##  [51] "Gibson Ek High School"                                            
    ##  [52] "Loowit High School"                                               
    ##  [53] "Mid-Columbia Parent Partnership"                                  
    ##  [54] "Phoenix High School"                                              
    ##  [55] "Kent Phoenix Academy"                                             
    ##  [56] "Columbia Virtual Academy - Kettle Falls"                          
    ##  [57] "Kettle Falls High School"                                         
    ##  [58] "Kittitas High School"                                             
    ##  [59] "La Conner High School"                                            
    ##  [60] "Emerson High School"                                              
    ##  [61] "Futures School"                                                   
    ##  [62] "Lind-Ritzville High School"                                       
    ##  [63] "Discovery High School"                                            
    ##  [64] "Lopez Middle High School"                                         
    ##  [65] "Lummi Nation School"                                              
    ##  [66] "Lyle High School"                                                 
    ##  [67] "Mabton Jr. Sr. High"                                              
    ##  [68] "Manson High School"                                               
    ##  [69] "Mary M. Knight School"                                            
    ##  [70] "Mary Walker High School"                                          
    ##  [71] "Heritage School"                                                  
    ##  [72] "Mead Alternative High School (Closed after 2018-2019 school year)"
    ##  [73] "Medical Lake Endeavors"                                           
    ##  [74] "Leaders In Learning"                                              
    ##  [75] "Morton Junior-Senior High"                                        
    ##  [76] "Skill Source Reingagement Program"                                
    ##  [77] "White Swan High School"                                           
    ##  [78] "Skagit Academy"                                                   
    ##  [79] "Muckleshoot Tribal School"                                        
    ##  [80] "ACES High School"                                                 
    ##  [81] "Mukilteo Reengagement Academy Open Doors"                         
    ##  [82] "Pend Oreille River School"                                        
    ##  [83] "North Beach Senior High School"                                   
    ##  [84] "Palouse Junction High School"                                     
    ##  [85] "James A. Taylor High School"                                      
    ##  [86] "Northport High School"                                            
    ##  [87] "Northshore Networks"                                              
    ##  [88] "Northshore Online Reengagement Program"                           
    ##  [89] "Secondary Academy for Success"                                    
    ##  [90] "Oakville High School"                                             
    ##  [91] "Odessa High School"                                               
    ##  [92] "Okanogan Alternative High School"                                 
    ##  [93] "Okanogan Outreach Alternative School"                             
    ##  [94] "Avanti High School"                                               
    ##  [95] "Olympia Regional Learning Academy"                                
    ##  [96] "Orcas Island High School"                                         
    ##  [97] "Oroville Middle-High School"                                      
    ##  [98] "Desert Oasis High School"                                         
    ##  [99] "New Horizons High School"                                         
    ## [100] "Pe Ell School"                                                    
    ## [101] "Henderson Bay Alt High School"                                    
    ## [102] "Lincoln High School"                                              
    ## [103] "E B Walker High School"                                           
    ## [104] "Puyallup Online Academy/POA"                                      
    ## [105] "Quincy Innovation Academy"                                        
    ## [106] "Republic Senior High School"                                      
    ## [107] "Three Rivers Home Link"                                           
    ## [108] "Ritzville High School"                                            
    ## [109] "CLIP"                                                             
    ## [110] "H.e.a.r.t. High School"                                           
    ## [111] "Alan T. Sugiyama High School"                                     
    ## [112] "Middle College High School"                                       
    ## [113] "Seattle World School"                                             
    ## [114] "The Center School"                                                
    ## [115] "Selah Academy Online"                                             
    ## [116] "Two Rivers School"                                                
    ## [117] "Soap Lake Middle & High School"                                   
    ## [118] "South Bend High School"                                           
    ## [119] "Explorer Academy"                                                 
    ## [120] "South Whidbey Academy"                                            
    ## [121] "Pratt Academy"                                                    
    ## [122] "Lincoln Hill High School"                                         
    ## [123] "Summit Public School: Olympus"                                    
    ## [124] "Chief Kitsap Academy"                                             
    ## [125] "Oakland High School"                                              
    ## [126] "Taholah High School"                                              
    ## [127] "Tekoa High School"                                                
    ## [128] "Cowlitz Prairie Academy"                                          
    ## [129] "Computer Academy Toppenish High School"                           
    ## [130] "Cascadia High School"                                             
    ## [131] "New Market High School"                                           
    ## [132] "Paideia High School"                                              
    ## [133] "Student Link"                                                     
    ## [134] "Wahkiakum High School"                                            
    ## [135] "Sentinel Tech Alt School"                                         
    ## [136] "Walla Walla Open Doors"                                           
    ## [137] "Pace Alternative High School"                                     
    ## [138] "Warden High School"                                               
    ## [139] "Wellpinit Fort Simcoe SEA"                                        
    ## [140] "Wellpinit High School"                                            
    ## [141] "Spokane Valley Transition School"                                 
    ## [142] "WEST VALLEY VIRTUAL ACADEMY 9-12"                                 
    ## [143] "White Pass Jr. Sr. High School"                                   
    ## [144] "White Salmon Academy"                                             
    ## [145] "Winolequa Learning Academy"                                       
    ## [146] "Woodland Alternative School"                                      
    ## [147] "Stanton Academy"                                                  
    ## [148] "Yakima Online"                                                    
    ## [149] "Yelm Extension School"

``` r
  dat[dat$SchoolName=="Homelink River",'pam']
```

    ## [1] 3
    ## Levels: 1 2 3 4

``` r
## REPORT: Table of clusters
  table(dat$pam)
```

    ## 
    ##   1   2   3   4 
    ## 149 101 103  62

``` r
#### EVALUATE RESULTS ####

## Loading required packages  
  library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.2

``` r
  library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 3.6.2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
## Cluster silhouette plot
  fviz_silhouette(res.pam)
```

    ##   cluster size ave.sil.width
    ## 1       1  149          0.67
    ## 2       2  101          0.34
    ## 3       3  103          0.13
    ## 4       4   62          0.38

![](PubPol542_Group1_Clustering_files/figure-gfm/Partioning%20Technique-1.png)<!-- -->

``` r
## Save individual silhouettes
  pamEval=data.frame(res.pam$silinfo$widths)
  head(pamEval)
```

    ##                             cluster neighbor sil_width
    ## Republic.Senior.High.School       1        3     0.783
    ## Lyle.High.School                  1        3     0.783
    ## South.Whidbey.Academy             1        3     0.782
    ## Middle.College.High.School        1        3     0.781
    ## Leaders.In.Learning               1        3     0.778
    ## Northport.High.School             1        3     0.778

``` r
## Request negative silhouettes (the ones that are poorly clustered)
  pamEval[pamEval$sil_width<0,]
```

    ##                                            cluster neighbor sil_width
    ## East.Valley.High.School.1                        2        3   -0.0433
    ## A.G.West.Black.Hills.High.School                 2        3   -0.1231
    ## Pullman.High.School                              2        3   -0.1255
    ## Westside.High.School                             3        1   -0.0275
    ## State.Street.High.School                         3        1   -0.0406
    ## Chimacum.Junior.Senior.High.School               3        1   -0.0488
    ## Legacy.High.School                               3        1   -0.0559
    ## Stevenson.High.School                            3        1   -0.0613
    ## Northwest.Allprep                                3        1   -0.0675
    ## Summit.Public.School..Sierra                     3        1   -0.0705
    ## Coupeville.High.School                           3        1   -0.0728
    ## Highline.Open.Doors.1418                         3        1   -0.0760
    ## Toledo.High.School                               3        1   -0.0842
    ## Granger.High.School                              3        1   -0.1093
    ## Legacy.High.School.1                             3        1   -0.1147
    ## Edmonds.Heights.K.12                             3        1   -0.1286
    ## Napavine.Jr.Sr.High.School                       3        1   -0.1309
    ## Rivers.Edge.High.School                          3        1   -0.1343
    ## Acceleration.Academy                             3        1   -0.1593
    ## Choice.Middle.and.High.School                    3        1   -0.1764
    ## Tonasket.High.School                             3        1   -0.1769
    ## Goldendale.High.School                           3        1   -0.1775
    ## Sequoia.High.School                              3        1   -0.1786
    ## Scriber.Lake.High.School                         3        1   -0.1990
    ## Asotin.Jr.Sr.High                                3        1   -0.2060
    ## Highland.High.School                             3        1   -0.2121
    ## Individualized.Graduation...Degree.Program       3        1   -0.2185
    ## South.Sound.High.School                          3        1   -0.2380
    ## Friday.Harbor.High.School                        3        1   -0.2424
    ## Rainier.Senior.High.School                       3        1   -0.2472
    ## Ilwaco.High.School                               3        1   -0.2786
    ## Ocosta.Junior...Senior.High                      3        1   -0.2799
    ## Mica.Peak.High.School                            3        1   -0.3011
    ## Discovery                                        3        1   -0.3012
    ## Talley.High.School                               3        1   -0.3014
    ## AIM.High.School                                  3        1   -0.3038
    ## Legacy.High.School.2                             3        1   -0.3262
    ## Davenport.Senior.High.School                     3        1   -0.3424
    ## Southridge.High.School                           4        2   -0.0080
    ## Enumclaw.Sr.High.School                          4        2   -0.0903

### Hierarchizing: Agglomerative

``` r
#### APPLY FUNCTION ####

## Remove duplicated elements
  rownames(dfClus) <- c()
  dfClus_D=cluster::daisy(x=dfClus)

## Indicating the number of clusters required
  res.agnes = hcut(dfClus_D, 
                k = NumCluster,isdiss=T,
                hc_func="agnes",
                hc_method = "ward.D2")
  
#### CLUSTERING RESULTS ####

## Add results to original dataframe
  dat$agn=as.factor(res.agnes$cluster)
  
## Query dataframe
  dat[dat$agn==1,'SchoolName']
```

    ##   [1] "Harbor High School"                                               
    ##   [2] "Cap Sante High School"                                            
    ##   [3] "Weston High School"                                               
    ##   [4] "Asotin Jr Sr High"                                                
    ##   [5] "West Auburn Senior High School"                                   
    ##   [6] "Eagle Harbor High School"                                         
    ##   [7] "Homelink River"                                                   
    ##   [8] "Summit View High School"                                          
    ##   [9] "Options High School"                                              
    ##  [10] "Acceleration Academy"                                             
    ##  [11] "Challenger High School"                                           
    ##  [12] "Renaissance Alternative High School"                              
    ##  [13] "Bridgeport High School"                                           
    ##  [14] "Burlington-Edison Alternative School"                             
    ##  [15] "Hayes Freedom High School"                                        
    ##  [16] "Cascade High School"                                              
    ##  [17] "CASHMERE HIGH SCHOOL"                                             
    ##  [18] "Castle Rock High School"                                          
    ##  [19] "Barker Creek Community School"                                    
    ##  [20] "CVSD Open Doors Programs"                                         
    ##  [21] "Mica Peak High School"                                            
    ##  [22] "Futurus High School"                                              
    ##  [23] "Lewis County Alternative School"                                  
    ##  [24] "Cheney Open Doors"                                                
    ##  [25] "Three Springs High School"                                        
    ##  [26] "Quartzite Learning"                                               
    ##  [27] "Chief Leschi Schools"                                             
    ##  [28] "Chimacum Junior/Senior High School"                               
    ##  [29] "Educational Opportunity Center"                                   
    ##  [30] "Cle Elum Roslyn High School"                                      
    ##  [31] "Swiftwater Alternative High School"                               
    ##  [32] "CPSD Open Doors Program"                                          
    ##  [33] "Oakridge Group Home"                                              
    ##  [34] "Colfax High School"                                               
    ##  [35] "College Place High School"                                        
    ##  [36] "Columbia High School"                                             
    ##  [37] "Panorama School"                                                  
    ##  [38] "Concrete High School"                                             
    ##  [39] "Twin Cedars High School"                                          
    ##  [40] "Coupeville High School"                                           
    ##  [41] "Crescent School"                                                  
    ##  [42] "Curlew Elem & High School"                                        
    ##  [43] "Cusick Jr Sr High School"                                         
    ##  [44] "Darrington High School"                                           
    ##  [45] "Davenport Senior High School"                                     
    ##  [46] "Dayton High School"                                               
    ##  [47] "EV Online"                                                        
    ##  [48] "EV Parent Partnership"                                            
    ##  [49] "Edmonds eLearning Academy"                                        
    ##  [50] "Edmonds Heights K-12"                                             
    ##  [51] "Scriber Lake High School"                                         
    ##  [52] "K-12 Ellensburg Learning Center"                                  
    ##  [53] "Elma High School"                                                 
    ##  [54] "Entiat Middle and High School"                                    
    ##  [55] "Everett Reengagement Academy"                                     
    ##  [56] "Port Gardner"                                                     
    ##  [57] "Sequoia High School"                                              
    ##  [58] "Legacy High School"                                               
    ##  [59] "Career Academy at Truman High School"                             
    ##  [60] "Internet Academy"                                                 
    ##  [61] "Open Doors Youth Reengagement (1418)"                             
    ##  [62] "River View High School"                                           
    ##  [63] "Gates Secondary School"                                           
    ##  [64] "Garfield at Palouse High School"                                  
    ##  [65] "Goldendale High School"                                           
    ##  [66] "Lake Roosevelt Alternative School"                                
    ##  [67] "Contract Learning Center"                                         
    ##  [68] "Granger High School"                                              
    ##  [69] "Crossroads High School"                                           
    ##  [70] "Granite Falls High School"                                        
    ##  [71] "Highland High School"                                             
    ##  [72] "Highline Open Doors 1418"                                         
    ##  [73] "New Start"                                                        
    ##  [74] "Hoquiam High School"                                              
    ##  [75] "Inchelium High School"                                            
    ##  [76] "Gibson Ek High School"                                            
    ##  [77] "Kalama High School"                                               
    ##  [78] "Loowit High School"                                               
    ##  [79] "Legacy High School"                                               
    ##  [80] "Mid-Columbia Parent Partnership"                                  
    ##  [81] "Phoenix High School"                                              
    ##  [82] "Individualized Graduation & Degree Program"                       
    ##  [83] "Kent Phoenix Academy"                                             
    ##  [84] "Columbia Virtual Academy - Kettle Falls"                          
    ##  [85] "Kettle Falls High School"                                         
    ##  [86] "Kiona-Benton City High School"                                    
    ##  [87] "Kittitas High School"                                             
    ##  [88] "La Conner High School"                                            
    ##  [89] "Chelan High School"                                               
    ##  [90] "Emerson High School"                                              
    ##  [91] "Futures School"                                                   
    ##  [92] "Lind-Ritzville High School"                                       
    ##  [93] "Discovery High School"                                            
    ##  [94] "Lopez Middle High School"                                         
    ##  [95] "Lummi Nation School"                                              
    ##  [96] "Lyle High School"                                                 
    ##  [97] "Mabton Jr. Sr. High"                                              
    ##  [98] "Manson High School"                                               
    ##  [99] "Mary M. Knight School"                                            
    ## [100] "Mary Walker High School"                                          
    ## [101] "Heritage School"                                                  
    ## [102] "Legacy High School"                                               
    ## [103] "Mead Alternative High School (Closed after 2018-2019 school year)"
    ## [104] "Medical Lake Endeavors"                                           
    ## [105] "Meridian High School"                                             
    ## [106] "Leaders In Learning"                                              
    ## [107] "Sky Valley Education Center"                                      
    ## [108] "Montesano Jr-Sr High"                                             
    ## [109] "Morton Junior-Senior High"                                        
    ## [110] "Skill Source Reingagement Program"                                
    ## [111] "White Swan High School"                                           
    ## [112] "Skagit Academy"                                                   
    ## [113] "Muckleshoot Tribal School"                                        
    ## [114] "ACES High School"                                                 
    ## [115] "Mukilteo Reengagement Academy Open Doors"                         
    ## [116] "Naches Valley High School"                                        
    ## [117] "Napavine Jr Sr High School"                                       
    ## [118] "Newport High School"                                              
    ## [119] "Pend Oreille River School"                                        
    ## [120] "Lakeside High School"                                             
    ## [121] "Nooksack Valley High School"                                      
    ## [122] "North Beach Senior High School"                                   
    ## [123] "Palouse Junction High School"                                     
    ## [124] "James A. Taylor High School"                                      
    ## [125] "South Sound High School"                                          
    ## [126] "Northport High School"                                            
    ## [127] "Northshore Networks"                                              
    ## [128] "Northshore Online Reengagement Program"                           
    ## [129] "Secondary Academy for Success"                                    
    ## [130] "Oakville High School"                                             
    ## [131] "Ilwaco High School"                                               
    ## [132] "Ocosta Junior - Senior High"                                      
    ## [133] "Odessa High School"                                               
    ## [134] "Okanogan Alternative High School"                                 
    ## [135] "Okanogan Outreach Alternative School"                             
    ## [136] "Avanti High School"                                               
    ## [137] "Olympia Regional Learning Academy"                                
    ## [138] "Omak High School"                                                 
    ## [139] "Orcas Island High School"                                         
    ## [140] "Oroville Middle-High School"                                      
    ## [141] "Desert Oasis High School"                                         
    ## [142] "New Horizons High School"                                         
    ## [143] "Pe Ell School"                                                    
    ## [144] "Henderson Bay Alt High School"                                    
    ## [145] "Lincoln High School"                                              
    ## [146] "Port Townsend High School"                                        
    ## [147] "E B Walker High School"                                           
    ## [148] "Puyallup Online Academy/POA"                                      
    ## [149] "Forks High School"                                                
    ## [150] "Quincy Innovation Academy"                                        
    ## [151] "Rainier Senior High School"                                       
    ## [152] "Talley High School"                                               
    ## [153] "Republic Senior High School"                                      
    ## [154] "Rivers Edge High School"                                          
    ## [155] "Three Rivers Home Link"                                           
    ## [156] "Ritzville High School"                                            
    ## [157] "Riverside High School"                                            
    ## [158] "CLIP"                                                             
    ## [159] "H.e.a.r.t. High School"                                           
    ## [160] "Royal High School"                                                
    ## [161] "Friday Harbor High School"                                        
    ## [162] "Alan T. Sugiyama High School"                                     
    ## [163] "Interagency Programs"                                             
    ## [164] "Middle College High School"                                       
    ## [165] "Nova High School"                                                 
    ## [166] "Seattle World School"                                             
    ## [167] "The Center School"                                                
    ## [168] "State Street High School"                                         
    ## [169] "Selah Academy Online"                                             
    ## [170] "Choice Middle and High School"                                    
    ## [171] "AIM High School"                                                  
    ## [172] "Two Rivers School"                                                
    ## [173] "Soap Lake Middle & High School"                                   
    ## [174] "South Bend High School"                                           
    ## [175] "Discovery"                                                        
    ## [176] "Explorer Academy"                                                 
    ## [177] "South Whidbey Academy"                                            
    ## [178] "South Whidbey High School"                                        
    ## [179] "Pratt Academy"                                                    
    ## [180] "Lincoln Hill High School"                                         
    ## [181] "Stevenson High School"                                            
    ## [182] "Sultan Senior High School"                                        
    ## [183] "Summit Public School: Olympus"                                    
    ## [184] "Summit Public School: Sierra"                                     
    ## [185] "Chief Kitsap Academy"                                             
    ## [186] "Oakland High School"                                              
    ## [187] "Tacoma Open Doors"                                                
    ## [188] "Taholah High School"                                              
    ## [189] "Tekoa High School"                                                
    ## [190] "Tenino High School"                                               
    ## [191] "Cowlitz Prairie Academy"                                          
    ## [192] "Toledo High School"                                               
    ## [193] "Tonasket High School"                                             
    ## [194] "Computer Academy Toppenish High School"                           
    ## [195] "Northwest Allprep"                                                
    ## [196] "Cascadia High School"                                             
    ## [197] "New Market High School"                                           
    ## [198] "Paideia High School"                                              
    ## [199] "Student Link"                                                     
    ## [200] "Vashon Island High School"                                        
    ## [201] "Wahkiakum High School"                                            
    ## [202] "Sentinel Tech Alt School"                                         
    ## [203] "Walla Walla Open Doors"                                           
    ## [204] "Pace Alternative High School"                                     
    ## [205] "Warden High School"                                               
    ## [206] "Wellpinit Fort Simcoe SEA"                                        
    ## [207] "Wellpinit High School"                                            
    ## [208] "Westside High School"                                             
    ## [209] "Dishman Hills High School"                                        
    ## [210] "Spokane Valley Transition School"                                 
    ## [211] "WEST VALLEY VIRTUAL ACADEMY 9-12"                                 
    ## [212] "White Pass Jr. Sr. High School"                                   
    ## [213] "Columbia High School"                                             
    ## [214] "White Salmon Academy"                                             
    ## [215] "Winolequa Learning Academy"                                       
    ## [216] "Woodland Alternative School"                                      
    ## [217] "Stanton Academy"                                                  
    ## [218] "Yakima Online"                                                    
    ## [219] "Yelm Extension School"                                            
    ## [220] "Zillah High School"

``` r
  dat[dat$SchoolName=="Homelink River",'agn']
```

    ## [1] 1
    ## Levels: 1 2 3 4

``` r
## REPORT: Table of clusters
  table(dat$agn)
```

    ## 
    ##   1   2   3   4 
    ## 220  73  39  83

``` r
#### EVALUATE RESULTS ####
  
## REPORT: Dendogram
  fviz_dend(res.agnes,k=NumCluster, cex = 0.7, horiz = T)
```

![](PubPol542_Group1_Clustering_files/figure-gfm/Hierarchizing:%20Agglomerative-1.png)<!-- -->

``` r
## REPORT: Average silhouettes
  fviz_silhouette(res.agnes)
```

    ##   cluster size ave.sil.width
    ## 1       1  220          0.70
    ## 2       2   73          0.30
    ## 3       3   39          0.36
    ## 4       4   83          0.31

![](PubPol542_Group1_Clustering_files/figure-gfm/Hierarchizing:%20Agglomerative-2.png)<!-- -->

``` r
## REPORT: Detecting anomalies
  agnEval = data.frame(res.agnes$silinfo$widths) # saving silhouettes
  head(agnEval)
```

    ##     cluster neighbor sil_width
    ## 332       1        2     0.811
    ## 291       1        2     0.811
    ## 81        1        2     0.810
    ## 254       1        2     0.810
    ## 154       1        2     0.809
    ## 328       1        2     0.809

``` r
  agnEval[agnEval$sil_width<0,] # requesting negative silhouettes
```

    ##     cluster neighbor sil_width
    ## 379       2        4   -0.0134
    ## 198       2        1   -0.0189
    ## 92        2        4   -0.0595
    ## 30        2        4   -0.0608
    ## 360       2        4   -0.1102
    ## 386       2        1   -0.2090
    ## 25        4        2   -0.0250
    ## 392       4        2   -0.0365
    ## 109       4        3   -0.0809
    ## 318       4        2   -0.1629
    ## 293       4        2   -0.1721
    ## 96        4        2   -0.1817

### Hierarchizing: Divisive

``` r
#### APPLY FUNCTION ####

## Indicating the number of clusters required
  res.diana = hcut(dfClus_D, 
                k = NumCluster,
                hc_func="diana",
                hc_method = "ward.D")
  
#### CLUSTERING RESULTS ####

## Add results to original dataframe
  dat$dia=as.factor(res.diana$cluster)
  
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

``` r
## REPORT: Table of clusters
  table(dat$dia)
```

    ## 
    ##   1   2   3   4 
    ## 267  50  97   1

``` r
#### EVALUATE RESULTS ####
  
## REPORT: Dendogram
  fviz_dend(res.diana,k=NumCluster, cex = 0.7, horiz = T)
```

![](PubPol542_Group1_Clustering_files/figure-gfm/Hierarchizing:%20Divisive-1.png)<!-- -->

``` r
## REPORT: Average silhouettes
  fviz_silhouette(res.diana)
```

    ##   cluster size ave.sil.width
    ## 1       1  267          0.64
    ## 2       2   50          0.38
    ## 3       3   97          0.42
    ## 4       4    1          0.00

![](PubPol542_Group1_Clustering_files/figure-gfm/Hierarchizing:%20Divisive-2.png)<!-- -->

``` r
## REPORT: Detecting anomalies
  diaEval = data.frame(res.diana$silinfo$widths) # saving silhouettes
  head(diaEval)
```

    ##     cluster neighbor sil_width
    ## 81        1        3     0.804
    ## 254       1        3     0.804
    ## 189       1        3     0.804
    ## 291       1        3     0.804
    ## 384       1        3     0.804
    ## 79        1        3     0.803

``` r
  diaEval[diaEval$sil_width<0,] # requesting negative silhouettes
```

    ##     cluster neighbor sil_width
    ## 270       1        3   -0.0286
    ## 372       1        3   -0.0326
    ## 86        1        3   -0.0989
    ## 2         1        3   -0.1636
    ## 399       1        3   -0.1748
    ## 136       1        3   -0.1818
    ## 181       1        3   -0.1820
    ## 44        1        3   -0.1821
    ## 256       1        3   -0.2087
    ## 345       1        3   -0.2343
    ## 180       1        3   -0.2425
    ## 125       1        3   -0.2531
    ## 185       1        3   -0.2541
    ## 319       1        3   -0.2567
    ## 301       1        3   -0.2983

### Compare Clustering

``` r
## Prepare a bidimensional map
  projectedData = cmdscale(dfClus_D, k=2)

  # save coordinates to original data frame
  dat$dim1 = projectedData[,1]
  dat$dim2 = projectedData[,2]
  
## See the Map
  base= ggplot(data=dat,
             aes(x=dim1, y=dim2,
                 label=SchoolName)) 
  base + geom_text(size=2)
```

![](PubPol542_Group1_Clustering_files/figure-gfm/Compare%20Clustering-1.png)<!-- -->

``` r
## Plot results from PAM:
  pamPlot=base + labs(title = "PAM") + geom_point(size=2, aes(color=pam), show.legend = F)
  
## Plot results from Hierarchical AGNES:
  agnPlot=base + labs(title = "AGNES") + geom_point(size=2, aes(color=agn), show.legend = F)
  
## Plot results from Hierarchical DIANA:
  diaPlot=base + labs(title = "DIANA") + geom_point(size=2, aes(color=dia), show.legend = F)
  
## Compare visually
  library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 3.6.2

``` r
  ggarrange(pamPlot, agnPlot, diaPlot,ncol = 3) # Hierarchizing (divisive) is the most appropriate clustering approach
```

![](PubPol542_Group1_Clustering_files/figure-gfm/Compare%20Clustering-2.png)<!-- -->

**Hierarchizing (divisive) is the most appropriate clustering
approach.**
