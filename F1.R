#Loads XML package, and installs it if it isn't already available
if(!require(XML)){
	print("Installing XML Package")
	install.packages("XML")
	if(!require(XML)){
		stop("XML Package could not be installed")
	}
}

#The following functions obtain data from the F1 website and save it locally

#Obtains an individual dataset (season/team/driver) for a particular year (1950/1958-2012)
yearTableF1 <- function(year=2012, opt="season", url="http://www.formula1.com/results/"){
	urlF1 <- paste0(url,opt,'/',year,'/')
	htmlF1 <- htmlTreeParse(urlF1, useInternalNodes=T)
	tableF1 <- readHTMLTable(urlF1)[[1]]
	tableF1
}#Outputs a dataframe

#Obtains a list of datasets (season/team/driver) over multiple years
multiYearTablesF1 <- function(start=1950, end=2012, opt1="season"){
	yearList <- list()
	yearList <- lapply(start:end, yearTableF1, opt=opt1)
	yearList
}#Outputs a list of dataframes

#Saves each dataset between given years to a .csv file in the Data sub-folder
saveDataF1 <- function(start1=1950, end1=2012, opt2="season"){
	for(i in start1:end1){
		write.csv(yearTableF1(year=i,opt=opt2), paste0(".\\Data\\",opt2,i,"F1.csv"), row.names=FALSE)
	}
}#No output

#Lists of datasets stored in memory (not used)
#seasonList <- multiYearTablesF1()
#teamList <- multiYearTablesF1(start=1958, opt1="team") #Constructors Championship started in 1958
#driverList <- multiYearTablesF1(opt1="driver")

#All following functions manipulate and analyse obtained data (s - season,t - team, d - driver)

#Plots the number of drivers from a specific nation over a time period
driverNation <- function(nation="German", type="a", start=1950,end=2012){#i-individual, c-cumulative, w-wins, a-all
	numPerYear = vector()
  total = 0;
	wins <- vector()
	num <- 0  

  if(type=="a"){#Display all graphs at once
    par(mfrow=c(3,1))
  }
  
  #Clean up this part sometime
  if(type=="i" || type=="a"){#Number of individual competitors from a nation per year
  	for(i in start:end){ #Redo with lapply
  		numPerYear[i-start+1] <- sum(read.csv(paste0(".\\Data\\driver",i,"F1.csv"))$Nationality==nation)
  	}
  	scatter.smooth(start:end,numPerYear, xaxs="r", yaxs="i", xlab="Year", ylab=paste0(nation," Formula One Racing Drivers"), pch=19, type="h", col="#CC0000", family="gaussian") #Error when a nation that never participated is provided as input
  	abline(h=seq(0,max(numPerYear),1), col="lightgray", lty=3)
  }
  if(type=="c" || type=="a"){#Cumulative number of competitors offered over all years by a nation
    for(i in start:end){ #Redo with lapply
      total <- sum(read.csv(paste0(".\\Data\\driver",i,"F1.csv"))$Nationality==nation) + total
      numPerYear[i-start+1]  <- total    
    }
    plot(start:end,numPerYear, xaxs="i", yaxs="i", xlab="Year", ylab=paste0(nation," Formula One Racing Drivers"), pch=19, type="l", col="#CC0000") #Error when a nation that never participated is provided as input
    abline(h=seq(0,total,5), v=seq(1950,2012,2), col="lightgray", lty=3)    
  }
  if(type=="w" || type=="a"){#Cumulative Drivers Champions victories by citizens of a nation
    for(i in start:end){
      if(read.csv(paste0(".\\Data\\driver",i,"F1.csv"))[1,3]==nation){
        num <- num+1
      }
      wins[i-start+1] <- num
    }
    plot(start:end, wins, xlab="Year", ylab=paste0(nation, " Drivers Champions (cumulative)"), pch=19, type="l")
    abline(h=seq(0,num), col="lightgray", lty=3)    
  }
}

#To-do:
#Add Error Checking for inputs
#Succint function names
#Merge similar functions - Done for now
#Clean up graphs for presentation
  #Clean up grid for varying x/y lengths(done)

#Tweak LOESS fit in driverNationalityByYear
#Plot all nations over graph in driverNationalityByYear

#Plot of cumulative Championship/Grand Prix wins per driver/nation(done)/team over a time period
#Treemap of total wins by driver/nation/team

#Correlation between qualifying time and final time
#Correlation between starting grid position and final position
	#Figure out how to access qualifying times on F1 website easily/systematically

#More magic