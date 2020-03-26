

DATA <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",  header = TRUE, sep = ",")
H <- strsplit(readLines("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")[1], ",")[[1]]
H[1] <- "Province.State"
H[2] <- "Country.Region"
colnames(DATA) <- H

DATA.2 <- DATA[DATA$Province.State == "", ]


##
list.europe <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", "Holy See")

list.europe.2 <- c("France", "United Kingdom", "Denmark", "Netherlands")

#DF <- rbind(DATA[2, ], DATA[DATA[,1] =="", ])

#DF <- DATA[DATA[,1] =="", ]
#DF <- DF[, -c(1, 3)]

#DF.Europe <- DATA[DATA[, "WHO region"] =="European Region", ]

DF.Europe.raw <- DATA[DATA$Country.Region %in% list.europe, ]

DF.Europe <- rbind(
			DATA[DATA$Country.Region == "France" & DATA$Province.State == "France", ]
			, DATA[DATA$Country.Region == "United Kingdom" & DATA$Province.State == "United Kingdom", ]
			, DATA[DATA$Country.Region == "Denmark" & DATA$Province.State == "Denmark", ]
			, DATA[DATA$Country.Region == "Netherlands" & DATA$Province.State == "Netherlands", ]
			, DF.Europe.raw[!(DF.Europe.raw$Country.Region %in% list.europe.2), ]
			)[, -c(1, 3, 4)]

DF.Europe.all <- apply(DF.Europe[, -1], 2, sum)




#DF.China <- DATA[DATA[, "Country/Region"] == "China" & DATA[, "Province/State"] == "Confirmed", ]
DF.China <- DATA[DATA[, "Country.Region"] == "China" , -c(1, 3, 4)]
DF.China.2 <- apply(DF.China[, -1], 2, sum)

#




#########		SYNC CURVES

#openair::selectByDate




MIN.China <- which(DF.Europe.all > min(DF.China.2))[1]

DF.sync <- data.frame("China" = c(rep(0, MIN.China-1), DF.China.2), "Europe" = c(DF.Europe.all, rep(NA, MIN.China-1)))
DF.sync$day <- seq(length = dim(DF.sync)[1])


DF.sync.gg <- fortify(reshape2::melt(DF.sync, id.vars = "day"))
##
#Global
ggplot(DF.sync.gg)+
geom_line(aes(x = day, y = value, group = variable, colour = variable))+
theme_minimal()


##
#Country States
list.europe.reduced <- c("France", "Germany", "Italy", "Spain", "United Kingdom")
DF.Europe.reduced <- DF.Europe[DF.Europe $Country.Region %in% list.europe.reduced, ]
rownames(DF.Europe.reduced) <- DF.Europe.reduced[, 1]
DF.Europe.reduced <- DF.Europe.reduced[, -1]

matrix(data = NA, nrow = MIN, ncol = 5)

#DF.sync.2 <- cbind(DF.sync, t(cbind(DF.Europe.reduced, matrix(data = NA, nrow = 5, ncol = MIN))))


#shift.europe <- dim(DF.sync)[1]-dim(DF.Europe.reduced)[2]

DF.sync.2 <- DF.sync
for (c in list.europe.reduced){
	print(c)
	begin.europe <- which(DF.sync[, "Europe"] > 0)[1]
	begin <- which(DF.Europe.reduced[c, ] > 0)[1]
	#end <-
	print(begin)
	#DF.sync.2$'c' <- c(rep(NA, dim(DF.sync.2)[1]))
	data.scaled <- c(rep(0, (begin.europe-1))
						, unlist(DF.Europe.reduced[c, -(1:(begin-1))])
						)
	DF.sync.2[, c] <- c(data.scaled
						, rep(NA, dim(DF.sync)[1]-length(data.scaled))
						)
}


DF.sync.gg.2 <- fortify(reshape2::melt(DF.sync.2, id.vars = "day"))
##
#Global
pdf("CoV.sync.20.pdf")
ggplot(DF.sync.gg.2)+
geom_line(aes(x = day, y = value, group = variable, colour = variable))+
theme_minimal()
dev.off()

######################### 				R0					####	#########################

library(R0)
mGT<-generation.time("gamma", c(3, 1.5))


## ex FR



require(magicfor)
magic_for(print, silent = TRUE) # call magic_for()

RES <- list()
for (c in c("France", "Italy", "Spain")){
	print(c)

				DIM <- dim(DATA[DATA$Country.Region == c, ])[1]
				#print(DATA[DATA$Country.Region == c, ])


				if(DIM > 1){
						DAT <- unlist(DATA[DATA$Country.Region == c & DATA$Province.State == c, -c(1:4)])
				} else {
						DAT <- unlist(DATA[DATA$Country.Region == c, -c(1:4)])
				}

	#print((DAT))
	##	remove beginnong 0
	DAT <- DAT[c(which(DAT > 0)[1]):length(DAT)]
	#print(DAT)
	DAT.0 <- (c(DAT,0)-c(0, DAT))[(-c(length(DAT), length(DAT)+1))]
	## because last day = same as antepenultian
	#DAT.0 <- DAT.0[-length(DAT.0)]
	print(DAT.0)
	###
	#BEGIN <- as.integer(length(DAT.0) - 2)
	#END <- as.integer(length(DAT.0) - 1)
	BEGIN = 15
	END = 30
	#

	estR0<-estimate.R(DAT.0, mGT
					, begin = BEGIN, end = END
					, methods=c("EG"
					#, "ML", "TD", "AR", "SB"
					),
                  pop.size=100000, nsim=1000)

	RES[[c]] <- estR0$estimates$EG$conf.int



}

magic_result_as_dataframe()     # get the result




###	Asymptomatic estimates




####	#########################			PREDICTIONS (in progress)			####	#########################

DF.sync.2$day <- seq(length = dim(DF.sync.2)[1])
DF.sync.2 <- fortify(reshape2::melt(DF.sync.2, id.vars = "day"))


DF.China.melted <- fortify(reshape2::melt(DF.China, id.vars = "Country.Region"))







DF.China.melted <- DF.China.melted[!is.na(DF.China.melted$value), ]
DF.China.melted$variable <- as.Date(DF.China.melted$variable , format = "%m/%d/%y")


FIT <- glm(data = DF.China.melted, value ~ variable)

#####
DF <- DF.Europe
DF <- DF[, -c(1, 3)]
DF <- fortify(reshape2::melt(DF, id.vars = "Country/Region"))
DF <- DF[!is.na(DF$value), ]
DF$variable <- as.Date(DF$variable , format = "%m/%d/%y")
colnames(DF) <- c("Country", "variable", "value")




#####	PLOT		#####
ggplot(data = DF)+
geom_path(data = DF, aes(x = DF$variable, y = DF$value, color = DF$Country, group = DF$Country))



pdf("predictions.arima.pdf")
par(mfrow=c(2,2))
######	China

DF2 <- DF.China.melted
TS <- ts(DF2$value, start = 1, end = (dim(DF2)[1]), frequency=1)
FIT <- arima(TS, order = c(1,0,0))
#
library(forecast)
accuracy(FIT)


library(forecast)
forecast(FIT, 5)
plot(forecast(FIT, 100), main = "China")

#######	Italy
DF2 <- DF[DF$Country=="Italy", ]
TS <- ts(DF2$value, start = 1, end = (dim(DF2)[1]), frequency=1)
FIT <- arima(TS, order = c(0,2,2))
#
library(forecast)
accuracy(FIT)


library(forecast)
forecast(FIT, 5)
plot(forecast(FIT, 100), main = "Italy")

#######	Spain
DF2 <- DF[DF$Country=="Spain", ]
TS <- ts(DF2$value, start = 1, end = (dim(DF2)[1]), frequency=1)
FIT <- arima(TS, order = c(1,2,1))
#
library(forecast)
accuracy(FIT)


library(forecast)
forecast(FIT, 5)
plot(forecast(FIT, 100), main = "Spain")

#######	France
DF2 <- DF[DF$Country=="France", ]
TS <- ts(DF2$value, start = 1, end = (dim(DF2)[1]), frequency=1)
FIT <- arima(TS, order = c(1,2,1))
#
library(forecast)
accuracy(FIT)


library(forecast)
forecast(FIT, 5)
plot(forecast(FIT, 100), main = "France")

###
dev.off()


predict.Arima(FIT, n.ahead = 6)


     (fit <- arima(USAccDeaths, order = c(0,1,1),
                   seasonal = list(order = c(0,1,1))))
     predict(fit, n.ahead = 6)
     options(od)



DF.predict <- DF[DF[, "Country"] == "France", ]


############################		SHINY
before <- which(covdat$total_cases == 0)
covdat.sync <- covdat[-before, ]
covdat.sync$J <- 0
for (c in unique(covdat.sync$location)){
				L <- dim(covdat.sync[covdat.sync$location == c, ])[1]
				covdat.sync[covdat.sync$location == c, "J"] <- seq(length = L)
}




covdat_selected <- covdat.sync[covdat.sync$location %in% c("China", "Italy", "United States"), ]
ggplot(covdat_selected) +
					#scale_color_brewer(palette="Paired", name = "Country")
					scale_color_discrete(name = "Countries:") +
					theme_linedraw(base_size = 15)+
					labs(x = "Date", y = "Number of cases per capita")+
geom_line(mapping = aes(x = J, y = total_cases_percapita, colour = location), size=1)
