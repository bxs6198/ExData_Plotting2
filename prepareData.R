##
## prepareData.R
##
##  Load the data in preparation for the plots.
##

##
##  STEP 1: Set the RStudio and directory environment configuration
##

    ## Load the relevant libraries
    ##
    library("zoo")
    library("plyr")
    library("reshape2")
    library("httr")
    library("bitops")
    library("RCurl")
    library("httpuv")
    library("data.table")
    library("Hmisc")
    library("markdown")
    library("rstudio")

    ## Check for the './data' directory and if it doesn't exist create it as
    ## this is where the data file will be downloaded and stored.
    ##
    if (!file.exists("./data")) {dir.create("./data")}

##
##  STEP 2: Download the source .zip data file into the current working
##          directory.  Unzip the file to extract the two .rds data files
##          into the ./data directory.
##

    ## Source data file URL (taken from project instructions).
    ##
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    
    ## Local Zip file name for the 'National Emissions Inventory (NEI)' data.
    ##
    zipfile <- "./data/exdata-data-NEI_data.zip"
    
    ## Local data file name for the 'PM2.5 Emissions Data'.  This file contains
    ## a data frame with all th ePM2.5 emissions data for 1999, 2002, 2005, and
    ## 2008.  For each year, the table contains number of tons of PM2.5 emitted
    ## from a specific type of source for the entire year.
    ##
    datafile1 <- "./data/summarySCC_PM25.rds"
    
    ## Local data file name for the 'Source Classification Code Table'.  This
    ## table provides a mapping from the SCC digit strings in the Emissions
    ## table to the actual name of the PM2.5 source.  The sources are 
    ## categorized in a few ways from the more general to the more specific and 
    ## you may choose to explore whatever categories you think are most useful.
    ##
    datafile2 <- "./data/Source_Classification_Code.rds"
    
    ## If the .zip data file does not exist, then download the source .zip data
    ## file for the 'National Emissions Inventory (NEI)' dataset.
    ##
    if (!file.exists(zipfile)) {
        ## Download the .zip data file.
        download.file(url=fileURL,
                      destfile=zipfile,
                      quiet=TRUE)
        ## Record the date the file was downloaded.
        dateDownloaded <- date()
    }       
    
    ## If the .rds data files do not exist, then unzip the .zip data file for
    ## the 'PM2.5 Emissions Data' and 'Source Classification Code Table' files.
    ##
    if (!file.exists(datafile1) | !file.exists(datafile2)) {
        unzip(zipfile,
              overwrite=TRUE,
              exdir="./data")
    }
    
##
##  STEP 3: Read the data into the RStudio environment.
##

    NEI <- readRDS(datafile1)
    SCC <- readRDS(datafile2)
    
##
##  At this point we have two data frames loaded into the RStudio environment -
##  NEI and SCC - having the following structures:
##
    
##
##  NEI
##  ===
##  'data.frame':    6497651 obs. of  6 variables:
##  $ fips     : chr  "09001" "09001" "09001" "09001" ...
##  $ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
##  $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
##  $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
##  $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
##  $ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
##  
##  fips:       A five-digit number (represented as a string) indicating the 
##              U.S. county
##  SCC:        The name of the source as indicated by a digit string 
##              (see source code classification table)
##  Pollutant:  A string indicating the pollutant
##  Emissions:  Amount of PM emitted, in tons
##  type:       The type of source (point, non-point, on-road, or non-road)
##  year:       The year of emissions recorded    
    
##  
##  SCC
##  ===
##  'data.frame':    11717 obs. of  15 variables:
##  $ SCC                : Factor w/ 11717 levels "10100101","10100102",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Data.Category      : Factor w/ 6 levels "Biogenic","Event",..: 6 6 6 6 6 6 6 6 6 6 ...
##  $ Short.Name         : Factor w/ 11238 levels "","2,4-D Salts and Esters Prod /Process Vents, 2,4-D Recovery: Filtration",..: 3283 3284 3293 3291 3290 3294 3295 3296 3292 3289 ...
##  $ EI.Sector          : Factor w/ 59 levels "Agriculture - Crops & Livestock Dust",..: 18 18 18 18 18 18 18 18 18 18 ...
##  $ Option.Group       : Factor w/ 25 levels "","C/I Kerosene",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Option.Set         : Factor w/ 18 levels "","A","B","B1A",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ SCC.Level.One      : Factor w/ 17 levels "Brick Kilns",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ SCC.Level.Two      : Factor w/ 146 levels "","Agricultural Chemicals Production",..: 32 32 32 32 32 32 32 32 32 32 ...
##  $ SCC.Level.Three    : Factor w/ 1061 levels "","100% Biosolids (e.g., sewage sludge, manure, mixtures of these matls)",..: 88 88 156 156 156 156 156 156 156 156 ...
##  $ SCC.Level.Four     : Factor w/ 6084 levels "","(NH4)2 SO4 Acid Bath System and Evaporator",..: 4455 5583 4466 4458 1341 5246 5584 5983 4461 776 ...
##  $ Map.To             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ Last.Inventory.Year: int  NA NA NA NA NA NA NA NA NA NA ...
##  $ Created_Date       : Factor w/ 57 levels "","1/27/2000 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Revised_Date       : Factor w/ 44 levels "","1/27/2000 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Usage.Notes        : Factor w/ 21 levels ""," ","includes bleaching towers, washer hoods, filtrate tanks, vacuum pump exhausts",..: 1 1 1 1 1 1 1 1 1 1 ...
##