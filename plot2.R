##
## plot2.R
##
##  Plot to answer the following question:
##
##  Have total emissions from PM decreased in the Baltimore City, Maryland 
##  (fips == "24510") from 1999 to 2008? Use the base plotting system to make 
##  a plot answering this question.
##

##
##  Step 1: Verify two data frames (NEI and SCC) exist in the RStudio
##          environment.  If not, then source "prepareData.R" to load the
##          data into the RStudio environment.
##

    if (!(exists('NEI') & exists('SCC'))) {source("prepareData.R")}

    ##
    ##  At this point we have two data frames loaded into the RStudio 
    ##  environment - NEI and SCC - having the following structures:
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

##
##  Step 2: Load all the libraries
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
    library("RColorBrewer")
    
##
##  Step 3: Apply a transform function on the NEI data frame to get the total 
##          emissions for each year.  One approach is to use the "melt" and
##          "dcast" functions that were used in the "Getting and Cleaning Data"
##          course project.
##
    
    ##  Set all the data frame element names to lowercase to make it easier to
    ##  reference the data frame elements as there no longer any need to 
    ##  remember whether an element name is upper- or lower-case.
    ##
    names(NEI) <- tolower(names(NEI))

    ##  Order the data by 'year'
    ##
    NEI <- arrange(NEI,year)
    
    ##  Subset the data for Baltimore City, Maryland (fips == "24510")
    subNEI <- NEI[NEI$fips=="24510",]
    
    ##  Melt the data such that 'year' is the id variable and 'emissions" is the
    ##  measurement variable.
    ##
    meltNEI <- melt(subNEI, id.vars=c("year"), measure.vars=c("emissions"))
    
    ##  Cast the melted data back into a tidy data frame where each row contains
    ##  the 'year' followed by the sum of the emissions.  We now have a very
    ##  nice tidy data frame with observations of total emissions for
    ##  each year.
    ##
    ##      'data.frame':    4 obs. of  2 variables:
    ##      $ year     : int  1999 2002 2005 2008
    ##      $ emissions: num  3274 2454 3091 1862
    ##
    tidyNEI <- dcast(meltNEI, year ~ variable, sum)
    
##
##  Step 4: Graph the totals for each year as points along a lineplot and
##          a barplot.
##
    
    ##  To remove scientific notation in the printing of the plot, apply a
    ##  penalty when deciding to print numeric values in fixed or exponential
    ##  notation.  Positive values bias towards fixed and negative towards
    ##  scientific notation.
    ##
    options(scipen=999)
        
    ##  Open the PNG device
    ##
    png(filename="plot2.png",
        width=960,
        height=480)
    
    ##  Specify the number of rows and columns for the plots
    ##
    par(mfrow=c(1, 2))

    ##  Adjust the default text values for the plots
    ##
    par(font.main=2,
        font.lab=2,
        font.sub=2,
        cex.main=1.5,
        cex.sub=0.75)
    
    ##  Lineplot
    ##
    plot(tidyNEI$emissions ~ tidyNEI$year,
         main=expression("Total " * PM[2.5] * " Emissions by Year for Baltimore City, MD"),
         sub="(Lineplot Example)",
         xlab="Sample Year",
         ylab=expression("Total " * PM[2.5] * " Emissions"),
         xaxp=c(1999,2008,3),
         type="b",
         lty=1,
         lwd=2,
         col="darkblue",bty="l")
    
    ##  Barplot
    ##
    barplot(tidyNEI$emissions,
            main=expression("Total " * PM[2.5] * " Emissions by Year for Baltimore City, MD"),
            sub="(Barplot Example)",
            xlab="Sample Year",
            ylab=expression("Total " * PM[2.5] * " Emissions"),
            names.arg=tidyNEI$year,
            col=brewer.pal(4,"Set2"))
    
    ##  Close the PNG device
    ##
    dev.off()
