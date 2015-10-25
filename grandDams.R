# Shape file test
# This code was used for the explorary data analysis

library("rgdal")
library("dplyr")
library("ggplot2")

ogrDrivers()

fileLayers <- ogrListLayers(dsn = "GRanD_dams_v1_1.shp")

fileInfo <- ogrInfo(dsn = "GRanD_dams_v1_1.shp", "GRanD_dams_v1_1")

fileData <- readOGR(dsn = "GRanD_dams_v1_1.shp", "GRanD_dams_v1_1")

# @data (fileData@data) contains the various data elements in a shape file

# Extract the attributes from the shape file

fileAttributes <- fileData@data

# Get a list of the attributes

shapeAttributes <- colnames(fileAttributes)


# Group By Country

byCountry <- group_by(fileAttributes, COUNTRY)

# Count of dams in each country

damPlotSummary <- summarise(byCountry, count = n())

damSummary <- summarise(damPlotSummary, mean(count), median(count), min(count), max(count))

colnames(damSummary) <- c("Mean", "Median", "Min", "Max")

# Create a basic plot

plot1 <- ggplot(data=damPlotSummary, aes(x=COUNTRY, y=count)) +
      geom_bar(stat="identity", fill="steelblue") +
      geom_text(aes(label=count), vjust=-0.3, size=3.5) +
      labs(title="Dams By County", x="Country", y = "Count") +
      theme(axis.text.x=element_text(angle=90, hjust=1, color = "black"))

print(plot1)