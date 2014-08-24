##
## plot6.R
##
##  Plot to answer the following question:
##
##  Compare emissions from motor vehicle sources in Baltimore City 
##  (fips == "24510") with emissions from motor vehicle sources in Los Angeles 
##  County, California (fips == "06037"). Which city has seen greater
##  changes over time in motor vehicle emissions?
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
    library("ggplot2")
    
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
    names(SCC) <- tolower(names(SCC))

    ##  Order the data by 'year'
    ##
    NEI <- arrange(NEI,year)
    
    ##  Subset the SCC codes to only include rows that contain 'ei.sector'
    ##  values with 'Coal' in the name.  This will select all SCC codes
    ##  from the following four 'ei.sector' categories:
    ##
    ##      Mobile – On-road – Diesel Heavy Duty Vehicles
    ##      Mobile – On-road – Diesel Light Duty Vehicles
    ##      Mobile – On-road – Gasoline Heavy Duty Vehicles
    ##      Mobile – On-road – Gasoline Light Duty Vehicles
    ##
    ##  'ei.sector' was selected because it appeared to be the highest-level
    ##  categorical description of "coal combustion-related sources" required
    ##  by the assignment.
    ##
    Codes <- SCC[grep("*Vehicles",SCC$ei.sector),c("scc","ei.sector")]
    Codes <- arrange(Codes, scc)
    
    ##  Subset the NEI data to select only the rows containing the vehicle code
    ##  values found in the Codes data frame AND are located in Baltimore City,
    ##  MD OR Los Angeles, CA.
    ##
    subNEI <- NEI[(NEI$scc %in% Codes$scc) & 
                  ((NEI$fips=="24510") | (NEI$fips=="06037")),]
    subNEI <- arrange(subNEI, fips, scc, year)
    
    ##  Merge the two data frames so that we now have the 'ei.sector' values
    ##  on each row of subNEI.
    ##
    subNEI <- merge(subNEI, Codes, by="scc")
    
    ##  Melt the data such that 'year' and 'ei.sector' are id variable and 
    ##  'emissions" is the measurement variable.
    ##
    meltNEI <- melt(subNEI, id.vars=c("fips","year","ei.sector"), measure.vars=c("emissions"))
    
    ##  Cast the melted data back into a tidy data frame where each row contains
    ##  the 'year' and 'ei.sector' followed by the sum of the emissions.  We now
    ##  have a very nice tidy data frame with observations of total emissions for
    ##  each year and ei.sector.
    ##
    ##  'data.frame':    32 obs. of  4 variables:
    ##  $ fips     : chr  "06037" "06037" "06037" "06037" ...
    ##  $ year     : Factor w/ 4 levels "1999","2002",..: 1 1 1 1 2 2 2 2 3 3 ...
    ##  $ ei.sector: Factor w/ 59 levels "Agriculture - Crops & Livestock Dust",..: 49 50 51 52 49 50 51 52 49 50 ...
    ##  $ emissions: num  1522 117 38.5 2253.6 2329.4 ...
    ##
    tidyNEI <- dcast(meltNEI, fips + year + ei.sector ~ variable, sum)
    
    ##  Change the year and fips to a factor to generate discrete values for 
    ##  the plot
    ##
    tidyNEI$year <- factor(tidyNEI$year)
    tidyNEI$fips <- factor(tidyNEI$fips, labels=c("Los Angeles, CA","Baltimore City, MD"))
 
##
##  Step 4: Graph the result using a barplot.
##
    
    ##  To remove scientific notation in the printing of the plot, apply a
    ##  penalty when deciding to print numeric values in fixed or exponential
    ##  notation.  Positive values bias towards fixed and negative towards
    ##  scientific notation.
    ##
    options(scipen=999)
    
    ##  Open the PNG device
    ##
    png(filename="plot6.png",
        width=1920,
        height=960)

    ##  Barplot with each 'ei.sector' in a separate facet
    ##
    gb <- ggplot(tidyNEI, aes(x=year, y=emissions, fill=ei.sector)) +
            geom_bar(stat="identity") + 
            facet_grid(. ~ fips ~ ei.sector, scales="free") +
            labs(x="Sample Year", 
                 y=expression("Total " * PM[2.5] * " Emissions"),
                 title=expression("Total " * PM[2.5] * " Emissions by Vehicle Source by City")) +
            geom_text(aes(label=paste(format(emissions, digits=2, nsmall=1))),
                      size=4,
                      color="black",
                      vjust=-0.5) +
            theme(title=element_text(face="bold",size=rel(1.5)),
                  strip.text = element_text(face="bold",size=rel(1.25)),
                  legend.position="none") +
            scale_fill_brewer(palette="Set2")
    
    ##  Display the plot
    ##
    print(gb)
    
    ##  Close the PNG device
    ##
    dev.off()
