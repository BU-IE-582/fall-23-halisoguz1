### Linear Regression Codes for IE582 - Statistical Learning for Data Mining Project Fall 2024
### Instructor:    Assoc. Prof. Mustafa Gökçe Baydoğan
### TA:            Ilayda Çelenk
### Students:      Halis Oğuz, Abdullah Kayacan


##Data Preperation
setwd("/Users/halis/Desktop/IE/Semester9/IE582/project/20180101_20231121_bist30")
dat2 <- read.csv("20230327_20230625_bist30.csv")
dat1 <- read.csv("20230626_20230924_bist30.csv")
dat0 <- read.csv("20230925_20231224_bist30.csv")
dat4 <- read.csv("20231121_20231223_bist30.csv")
datf <- read.csv("Daily_Series.csv")

library(data.table)
library(ggplot2)
library(ggcorrplot)

str(dat1)

dat0$timestamp <- as.POSIXct(dat0$timestamp, format="%Y-%m-%d %H:%M:%S")
dat1$timestamp <- as.POSIXct(dat1$timestamp, format="%Y-%m-%d %H:%M:%S")
dat2$timestamp <- as.POSIXct(dat2$timestamp, format="%Y-%m-%d %H:%M:%S")
dat4$timestamp <- as.POSIXct(dat4$timestamp, format="%Y-%m-%d %H:%M:%S")
datf$timestamp <- as.POSIXct(datf$timestamp, format="%Y-%m-%d %H:%M:%S")

data <- datf

short_names <- unique(data$short_name)
time_stamps <- unique(data$timestamp)
p <- rep(0,length(short_names))
for(i in 1:length(short_names)){
  ind1 <- data$short_name==short_names[i]
  checker <- unique(data$timestamp[ind1])
  p[i] <- sum(checker-time_stamps)
}
p


ind1 <- (dat4$timestamp<datf$timestamp[1])
dat4 <- dat4[ind1,]

ind2 <- (dat0$timestamp<dat4$timestamp[1])
dat0 <- dat0[ind2,]

compdat <- data.table(matrix(0,nrow=790,ncol=31))
colnames(compdat) <- c("time_stamps",short_names)
compdat$time_stamps <- unique(rbind(dat0,dat4,datf)$timestamp)
data <- rbind(dat0,dat4,datf)
for(i in 1:length(short_names)){
  ind1 <- data$short_name==short_names[i]
  compdat[,i+1] <- data[ind1,2]
}



###Model training

compdat1 <- compdat #holding data temporarily


q <- rep(0,25)
fullmodels <- list()
desvals <- matrix(0,nrow=25,ncol=30)
testwmapes <- matrix(0,nrow=25,ncol=30)
wmapes <- matrix(0, nrow=25,ncol=30)
preds <- list()
for(i in 1:30){
  preds[[i]] <- 1
}
for(m in 1:25){
  compdat <- compdat1[(351+m*10):780,]
  totalmodels <- list()
  adjrsq <- matrix(0,nrow=30,ncol=29)
  wmape <- matrix(0,nrow=30,ncol=29)
  summy <- 0
  desiredval <- (410-m*10)
  bestvec <- c()
  yp <- c()
  yn <- c()
  yd <- c()
  for(i in 1:30){
    models <- list()
    z <- 1:30
    z <- z[-i]
    datq <- compdat
    datpr <- compdat
    datq[[1]] <- 0
    datq[[1]] <- c(rep(0,10),compdat[[i+1]][1:(dim(compdat)[1]-10)])
    datpr[[1]] <- 0
    datpr[[1]] <- c(rep(0,10),compdat[[i+1]][1:(dim(compdat)[1]-10)])
    datq[[i+1]] <- compdat[[i+1]]
    datpr[[i+1]] <- compdat[[i+1]]
    p <- 1:31
    p <- p[-c(1,i+1)]
    for(y in 1:length(p)){
      datq[[p[y]]][11:(dim(compdat)[1])] <- compdat[[p[y]]][1:(dim(compdat)[1]-10)]
      datpr[[p[y]]] <- compdat[[p[y]]]
    }
    colnames(datq)[c(1,i+1)] <- c("lag","target")
    colnames(datpr)[c(1,i+1)] <- c("lag","target")
    dattr <- datq[11:(desiredval+10),]
    datte <- datq[(desiredval+11):(desiredval+20),]
    lastdat <- datpr[(desiredval+11):(desiredval+20)]
    for(j in 1:length(z)){
      #datm <- matrix(c(traindat[[i+1]][11:dim(traindat)[1]],traindat[[z[j]+1]][1:(dim(traindat)[1]-10)]),ncol=2,nrow=dim(traindat)[1]-10,byrow=F)
      #datq[[i+1]] <- traindat[[i+1]][11:dim(traindat)[1]]
      #datq[[z[j]+1]] <- traindat[[z[j]+1]][1:(dim(traindat)[1]-10)]
      #datq <- dattr[,c(i+1,1,z[j]+1,dim(datq)[2])]
      #colnames(datq) <- c("target","lag",short_names[z[j]])
      w <- c(1,i+1,z[j]+1)
      datm <- dattr[ ,..w]
      models[[j]] <- lm(target~., data=datm)
    }
    totalmodels[[i]] <- models
    fullmodels[[m]] <- models
    for(j in 1:29){
      #adjrsq[i,j] <- as.numeric(summary(totalmodels[[i]][[j]])[9])
      wmape[i,j] <- as.numeric(sum(abs(datq$target[(desiredval+11):(desiredval+20)]- predict(totalmodels[[i]][[j]],newdata=datte)))/sum(abs(datq$target[(desiredval+11):(desiredval+20)])))
    }
    bestmodel <- which.min(wmape[i,])
    bestvec <- c(bestvec,bestmodel)
    testwmapes[m,i] <- wmape[i,bestmodel]
    fullmodels[[m]][[i]] <- totalmodels[[i]][[bestmodel]]
    desvals[m,i] <- bestmodel
    #fullmodels[[m]][[i]] <- totalmodels[[i]][[bestmodel]]
    yn <- c(yn,as.numeric(sum(abs(datq$target[(desiredval+11):(desiredval+20)]- rep(datq$target[(desiredval+10)],10))))) #naive forecasts
    yd <- c(yd,sum(abs(datq$target[(desiredval+11):(desiredval+20)])))
  }
  
  q[m] <- sum(yp/yd)/30
  sum(yn/yd)/30
}






d <- c()
for(i in 1:30){
  r <- c(r,which.min(testwmapes[,i]))
}



q <- rep(0,30)
preds <- list()
for(i in 1:30){
  preds[[i]] <- 1
}
for(i in 1:30){
  compdat <- compdat1[(351+r[i]*10):780,]
  totalmodels <- list()
  desiredval <- (410-r[i]*10)
  bestvec <- c()
  yp <- c()
  yn <- c()
  yd <- c()
  models <- list()
  z <- 1:30
  z <- z[-i]
  datq <- compdat
  datpr <- compdat
  datq[[1]] <- 0
  datq[[1]] <- c(rep(0,10),compdat[[i+1]][1:(dim(compdat)[1]-10)])
  datpr[[1]] <- 0
  datpr[[1]] <- c(rep(0,10),compdat[[i+1]][1:(dim(compdat)[1]-10)])
  datq[[i+1]] <- compdat[[i+1]]
  datpr[[i+1]] <- compdat[[i+1]]
  p <- 1:31
  p <- p[-c(1,i+1)]
  for(y in 1:length(p)){
    datq[[p[y]]][11:(dim(compdat)[1])] <- compdat[[p[y]]][1:(dim(compdat)[1]-10)]
    datpr[[p[y]]] <- compdat[[p[y]]]
  }
  colnames(datq)[c(1,i+1)] <- c("lag","target")
  colnames(datpr)[c(1,i+1)] <- c("lag","target")
  dattr <- datq[11:(desiredval+10),]
  datte <- datq[(desiredval+11):(desiredval+20),]
  lastdat <- datpr[(desiredval+11):(desiredval+20)]
  for(j in 1:length(z)){
    #datm <- matrix(c(traindat[[i+1]][11:dim(traindat)[1]],traindat[[z[j]+1]][1:(dim(traindat)[1]-10)]),ncol=2,nrow=dim(traindat)[1]-10,byrow=F)
    #datq[[i+1]] <- traindat[[i+1]][11:dim(traindat)[1]]
    #datq[[z[j]+1]] <- traindat[[z[j]+1]][1:(dim(traindat)[1]-10)]
    #datq <- dattr[,c(i+1,1,z[j]+1,dim(datq)[2])]
    #colnames(datq) <- c("target","lag",short_names[z[j]])
    w <- c(1,i+1,z[j]+1)
    datm <- dattr[ ,..w]
    models[[j]] <- lm(target~., data=datm)
  }
  totalmodels[[i]] <- models
  fullmodels[[m]] <- models
  for(j in 1:29){
    #adjrsq[i,j] <- as.numeric(summary(totalmodels[[i]][[j]])[9])
    wmape[i,j] <- as.numeric(sum(abs(datq$target[(desiredval+11):(desiredval+20)]- predict(totalmodels[[i]][[j]],newdata=datte)))/sum(abs(datq$target[(desiredval+11):(desiredval+20)])))
  }
  bestmodel <- which.min(wmape[i,])
  bestvec <- c(bestvec,bestmodel)
  testwmapes[m,i] <- wmape[i,bestmodel]
  fullmodels[[m]][[i]] <- totalmodels[[i]][[bestmodel]]
  desvals[m,i] <- bestmodel
  preds[[i]] <- predict(totalmodels[[i]][[bestmodel]],newdata=lastdat)
  #fullmodels[[m]][[i]] <- totalmodels[[i]][[bestmodel]]
}


install.packages("RJSONIO")
library(RJSONIO)
str_dict <- "{'AKBNK': [40.74, 40.74, 40.74, 40.74, 40.74, 40.74, 40.74, 40.74, 40.74, 40.74], 'ARCLK': [130.1, 130.1, 130.1, 130.1, 130.1, 130.1, 130.1, 130.1, 130.1, 130.1], 'ASELS': [47.48, 47.48, 47.48, 47.48, 47.48, 47.48, 47.48, 47.48, 47.48, 47.48], 'BIMAS': [318.25, 318.25, 318.25, 318.25, 318.25, 318.25, 318.25, 318.25, 318.25, 318.25], 'DOHOL': [12.11, 12.11, 12.11, 12.11, 12.11, 12.11, 12.11, 12.11, 12.11, 12.11], 'EKGYO': [8.25, 8.25, 8.25, 8.25, 8.25, 8.25, 8.25, 8.25, 8.25, 8.25], 'EREGL': [45.04, 45.04, 45.04, 45.04, 45.04, 45.04, 45.04, 45.04, 45.04, 45.04], 'FROTO': [768.0, 768.0, 768.0, 768.0, 768.0, 768.0, 768.0, 768.0, 768.0, 768.0], 'GARAN': [63.8, 63.8, 63.8, 63.8, 63.8, 63.8, 63.8, 63.8, 63.8, 63.8], 'GUBRF': [144.1, 144.1, 144.1, 144.1, 144.1, 144.1, 144.1, 144.1, 144.1, 144.1], 'HALKB': [13.22, 13.22, 13.22, 13.22, 13.22, 13.22, 13.22, 13.22, 13.22, 13.22], 'ISCTR': [25.7, 25.7, 25.7, 25.7, 25.7, 25.7, 25.7, 25.7, 25.7, 25.7], 'KCHOL': [147.1, 147.1, 147.1, 147.1, 147.1, 147.1, 147.1, 147.1, 147.1, 147.1], 'KOZAA': [42.66, 42.66, 42.66, 42.66, 42.66, 42.66, 42.66, 42.66, 42.66, 42.66], 'KOZAL': [19.6, 19.6, 19.6, 19.6, 19.6, 19.6, 19.6, 19.6, 19.6, 19.6], 'KRDMD': [25.42, 25.42, 25.42, 25.42, 25.42, 25.42, 25.42, 25.42, 25.42, 25.42], 'PETKM': [20.1, 20.1, 20.1, 20.1, 20.1, 20.1, 20.1, 20.1, 20.1, 20.1], 'PGSUS': [711.0, 711.0, 711.0, 711.0, 711.0, 711.0, 711.0, 711.0, 711.0, 711.0], 'SAHOL': [66.0, 66.0, 66.0, 66.0, 66.0, 66.0, 66.0, 66.0, 66.0, 66.0], 'SASA': [35.78, 35.78, 35.78, 35.78, 35.78, 35.78, 35.78, 35.78, 35.78, 35.78], 'SISE': [48.46, 48.46, 48.46, 48.46, 48.46, 48.46, 48.46, 48.46, 48.46, 48.46], 'TAVHL': [117.0, 117.0, 117.0, 117.0, 117.0, 117.0, 117.0, 117.0, 117.0, 117.0], 'TCELL': [61.5, 61.5, 61.5, 61.5, 61.5, 61.5, 61.5, 61.5, 61.5, 61.5], 'THYAO': [249.9, 249.9, 249.9, 249.9, 249.9, 249.9, 249.9, 249.9, 249.9, 249.9], 'TKFEN': [37.1, 37.1, 37.1, 37.1, 37.1, 37.1, 37.1, 37.1, 37.1, 37.1], 'TTKOM': [27.6, 27.6, 27.6, 27.6, 27.6, 27.6, 27.6, 27.6, 27.6, 27.6], 'TUPRS': [139.7, 139.7, 139.7, 139.7, 139.7, 139.7, 139.7, 139.7, 139.7, 139.7], 'VAKBN': [14.81, 14.81, 14.81, 14.81, 14.81, 14.81, 14.81, 14.81, 14.81, 14.81], 'VESTL': [48.64, 48.64, 48.64, 48.64, 48.64, 48.64, 48.64, 48.64, 48.64, 48.64], 'YKBNK': [23.56, 23.56, 23.56, 23.56, 23.56, 23.56, 23.56, 23.56, 23.56, 23.56]}"
result_list <- fromJSON(str_dict)
str_dict2 <- "{'AKBNK': [40.9275, 40.9679, 40.9693, 40.9214, 40.9407, 40.9676, 41.0545, 41.0809, 41.0778, 40.7888], 'ARCLK': [130.4428, 130.5689, 130.4537, 130.4226, 130.4826, 130.4391, 130.3661, 130.4159, 130.4264, 130.0501], 'ASELS': [47.7192, 47.7223, 47.721, 47.7239, 47.7245, 47.7188, 47.7468, 47.6723, 47.583, 47.4819], 'BIMAS': [319.1031, 319.6979, 319.2224, 319.5105, 319.8925, 320.2134, 320.4704, 319.5789, 319.2393, 318.388], 'DOHOL': [12.1662, 12.1875, 12.173, 12.1852, 12.1909, 12.17, 12.1645, 12.1446, 12.1142, 12.0859], 'EKGYO': [8.2843, 8.2917, 8.2964, 8.2952, 8.2943, 8.2934, 8.3059, 8.2993, 8.2802, 8.2678], 'EREGL': [45.2443, 45.2347, 45.2899, 45.2802, 45.2667, 45.28, 45.2545, 45.2457, 45.2073, 45.0872], 'FROTO': [770.454, 771.7546, 771.7967, 770.1864, 770.2764, 769.9551, 769.8761, 770.3027, 770.1085, 768.5922], 'GARAN': [64.0967, 64.1148, 64.0373, 64.0658, 64.179, 64.2861, 64.3808, 64.1244, 64.2599, 63.9786], 'GUBRF': [144.3185, 143.9534, 144.6486, 144.314, 143.9946, 144.1715, 144.3032, 144.54, 143.4684, 143.0953], 'HALKB': [13.3091, 13.2936, 13.2524, 13.2175, 13.2416, 13.168, 13.1899, 13.142, 13.1502, 13.181], 'ISCTR': [25.8345, 25.8194, 25.8147, 25.8188, 25.8479, 25.83, 25.9272, 25.7959, 25.8244, 25.6697], 'KCHOL': [147.6932, 147.7163, 147.7043, 147.6629, 147.8674, 147.8483, 147.6192, 147.5588, 147.4086, 147.0292], 'KOZAA': [42.8009, 42.8104, 42.8257, 42.7738, 42.7997, 42.7017, 42.7135, 42.5756, 42.575, 42.5339], 'KOZAL': [19.6848, 19.7123, 19.719, 19.7076, 19.7079, 19.6644, 19.6624, 19.6107, 19.5964, 19.5465], 'KRDMD': [25.5242, 25.5743, 25.5805, 25.5595, 25.5508, 25.5363, 25.5312, 25.516, 25.4876, 25.2907], 'PETKM': [20.177, 20.2048, 20.1842, 20.1927, 20.2041, 20.1901, 20.1983, 20.185, 20.1651, 20.0051], 'PGSUS': [713.7251, 714.0849, 713.9086, 714.3199, 715.2343, 713.5915, 714.6364, 713.7899, 713.3693, 711.2799], 'SAHOL': [66.2567, 66.2915, 66.3135, 66.2827, 66.3036, 66.2985, 66.299, 66.3141, 66.2086, 65.9337], 'SASA': [35.9336, 35.9311, 35.924, 35.8821, 35.8461, 35.8174, 35.8559, 35.7407, 35.8285, 35.752], 'SISE': [48.669, 48.6814, 48.6632, 48.637, 48.6337, 48.6312, 48.6047, 48.6038, 48.5462, 48.3867], 'TAVHL': [117.4198, 117.5253, 117.5736, 117.4848, 117.4604, 117.4219, 117.498, 117.3544, 117.2191, 116.9832], 'TCELL': [61.7821, 61.7723, 61.8717, 61.8591, 61.85, 61.7745, 61.8086, 61.7761, 61.7194, 61.5542], 'THYAO': [250.8937, 250.9139, 251.0985, 251.114, 251.0414, 251.0889, 251.0784, 251.1025, 250.8023, 250.2242], 'TKFEN': [37.2139, 37.2272, 37.2111, 37.1951, 37.2073, 37.1927, 37.1746, 37.0516, 37.0391, 36.9195], 'TTKOM': [27.7504, 27.7745, 27.7874, 27.7752, 27.7685, 27.7598, 27.7195, 27.7315, 27.754, 27.642], 'TUPRS': [140.2143, 140.3899, 140.3812, 140.3322, 140.3512, 140.223, 140.1865, 140.0601, 139.7529, 139.2279], 'VAKBN': [14.9029, 14.8665, 14.8525, 14.8449, 14.8481, 14.842, 14.8158, 14.8086, 14.7983, 14.7897], 'VESTL': [48.827, 48.8205, 48.8674, 48.8206, 48.8259, 48.7787, 48.7895, 48.73, 48.5424, 48.4013], 'YKBNK': [23.6869, 23.6772, 23.7171, 23.6989, 23.7073, 23.6989, 23.8161, 23.8403, 23.8553, 23.6468]}"
result_list2 <- fromJSON(str_dict2)

k <- 1:30 
for(i in 1:length(k)){ #forming a model with
  ind <- which(names(result_list)==short_names[k[i]])
  result_list[[ind]] <- (preds[[k[i]]]+result_list2[[ind]]+result_list[[ind]])/3
}

k <- 1:30
for(i in 1:length(k)){
  ind <- which(names(result_list)==short_names[k[i]])
  result_list[[ind]] <- (result_list2[[ind]]+result_list[[ind]])/2
}

for(i in 1:length(k)){
  ind <- which(names(result_list)==short_names[k[i]])
  print(result_list2[[ind]]-result_list[[ind]])
}

for(i in 1:length(k)){
  ind <- which(names(result_list)==short_names[k[i]])
  result_list[[ind]] <- preds[[k[i]]]
}


qqq <- toJSON(result_list)
qqq <- gsub("\n", " ", qqq, ignore.case=FALSE)
qqq <- gsub("[\n]", "", qqq, ignore.case=FALSE)
qqq <- gsub("[\"]", "'", qqq, ignore.case=FALSE)
qqq <- gsub("[{]", "[", qqq, ignore.case=FALSE)
qqq <- gsub("[}]", "]", qqq, ignore.case=FALSE)
qqq <- gsub("'1':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'2':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'3':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'4':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'5':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'6':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'7':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'8':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'9':", "", qqq, ignore.case=FALSE)
qqq <- gsub("'10':", "", qqq, ignore.case=FALSE)



