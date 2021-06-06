plot4 <- function()
{
  library(png)
  
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  rootpath <- file.path(".","Plots")
  filename <- "exdata_data_NEI_data.zip"
  
  if(!dir.exists(rootpath)) dir.create(rootpath)
  
  filepath <- file.path(rootpath,"summarySCC_PM25.rds")
  
  if(!file.exists(filepath))
  {
    filepath <- file.path(rootpath, filename)
    download.file(URL, filepath, 'curl')
    unzip(filepath, exdir = rootpath)
    unlink(filepath)
  }
  
  filepath <- file.path(rootpath, "summarySCC_PM25.rds")
  NEI <- readRDS(filepath)
  filepath <- file.path(rootpath, "Source_Classification_Code.rds")
  SCC2 <- readRDS(filepath)
  SCC2 <- SCC2[grep("fuel comb.+coal", SCC2$EI.Sector,ignore.case = T),][,c("SCC","EI.Sector")]
  SCC2$SCC <- as.character(SCC2$SCC)
  SCC2$EI.Sector <- as.character(SCC2$EI.Sector)
  NEI <- subset(NEI, SCC %in% SCC2$SCC)
  newvector <- c()
  for(i in 1:length(NEI$Emissions))
  {
    newvector[i] <- SCC2[SCC2$SCC == NEI[i,]$SCC,]$EI.Sector
  }
  NEI$EI.Sector <- newvector
  NEI$year_EI.Sector <- paste(NEI$year,NEI$EI.Sector, sep = "_")
  NEI <- tapply(NEI$Emissions, NEI$year_EI.Sector, sum)
  
  NEI <- data.frame("year_sector" = names(NEI),"emissions" = NEI / 1000, row.names = NULL)
  NEI <- NEI %>% separate(year_sector, c("year","sector"), sep = "_", remove = T)
  NEIlevels <- tapply(NEI$emissions, NEI$sector, max)
  NEIlevels <- data.frame("sectors" = names(NEIlevels),"max" = NEIlevels, row.names = NULL)
  NEIlevels <- NEIlevels[order(-NEIlevels$max),]
  NEIlevels$sectors <- as.factor(gsub("Fuel Comb - | - Coal","",NEIlevels$sectors))
  NEI$year <- as.numeric(NEI$year)
  NEI$sector <- as.factor(gsub("Fuel Comb - | - Coal","",NEI$sector))
  NEI$sector <- factor(NEI$sector, levels = NEIlevels$sectors)
  
  filepath <- file.path(rootpath, "plot4.png")
  if(exists(filepath)) unlink(filepath)

  ggplot(NEI, aes(year, emissions, col = sector)) +
    geom_point() +
    geom_line(stat = "smooth", method = "lm", formula = y ~ x, se = F, linetype = "dashed", alpha = 0.5) +
    geom_path() +
    facet_wrap(~sector, nrow = 3, scales = "free") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Emissions from Coal Combustion", y = "Thousands of tons")

  ggsave(filepath)
  img <- readPNG(filepath)
  grid::grid.raster(img)
}