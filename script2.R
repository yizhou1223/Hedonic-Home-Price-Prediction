library(tidyverse)
library(sf)
library(corrplot)
library(caret) 
library(AppliedPredictiveModeling)
library(stargazer)
library(ggmap)
library(FNN)
library(spdep)


options(scipen = 999)
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

setwd("F:/SpatialAnalysisforPlanning/midterm")

bostonSales <- st_read("midtermData_Boston_forStudents/Boston_Midterms_Dataset.shp") 
names(bostonSales)
unlist(sapply(bostonSales,class))

boston1 <- bostonSales %>%
  select(-c(2:5,10:17,20:24,27,28,42:60)) %>%
  mutate(LivingArea = as.numeric(gsub(",","",LivingArea))) %>%
  mutate(YR_REMOD = as.numeric(as.character(YR_REMOD))) %>%
  filter(YR_BUILT != 0)

i <- which(boston1$YR_REMOD==0)
boston1$YR_REMOD[i] <- boston1$YR_BUILT[i]
i <- is.na(boston1$YR_REMOD)
boston1$YR_REMOD[i] <- boston1$YR_BUILT[i]

boston1 <- boston1 %>% 
  na.omit() 


#neighborhood
bostonNhoods <- st_read("data/Boston_Neighborhoods.shp")
st_crs(bostonSales) == st_crs(bostonNhoods)
names(bostonNhoods)

bostonNhoods <- bostonNhoods %>% select(Name)
boston1 <- st_join(boston1,bostonNhoods)
#back to regression


#crime
crime <- read.csv("crime.csv")
names(crime)
crime <- crime[which(!is.na(crime$Lat)),]

houseBuffer <- st_buffer(boston1,0.005)%>%
  select(UniqueSale)
plot(houseBuffer$geometry)

crime.point <- st_as_sf(x=crime,coords = c("Long","Lat"))
st_crs(crime.point) <- st_crs(boston1)

houseBuffer <- st_join(houseBuffer,crime.point) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(UniqueSale) %>%
  summarise(crimeCount = n())

boston1 <- boston1 %>%
  left_join(houseBuffer,by="UniqueSale")
boston1 <- boston1 %>%
  select(-crimeCount)
#back to regression



#university
coll.univ <- st_read("data/Colleges_and_Universities.shp")
names(coll.univ)

coll.univ <- coll.univ[which(coll.univ$Longitude!=0),]
meanLong <- mean(coll.univ$Longitude)
meanLat <- mean(coll.univ$Latitude)
meancoord <- data.frame(Long=meanLong,Lat=meanLat)
meanPoint <- st_as_sf(meancoord,coords = c("Long","Lat"))
st_crs(meanPoint) <- st_crs(boston1)

boston1$distUniversity <- as.numeric(st_distance(boston1,meanPoint))
boston1$distUniversity <- 1/(boston1$distUniversity)
#back to regression




#spatial lag
houseXY <- boston1 %>%
  select(Longitude,Latitude) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  as.matrix()

notestXY <- boston1 %>%
  filter(test==0) %>%
  select(Longitude,Latitude) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  as.matrix()

nn = get.knnx(notestXY,houseXY,k=11)
nn

index <- nn$nn.index[,-1]
#price per sqft
boston.notest <- boston1 %>%
  filter(test==0) %>%
  as.data.frame() %>%
  select(-geometry)

lagpricePSQF <- boston.notest$PricePerSq[index]
lagpricePSQF <- matrix(lagpricePSQF, ncol = 10, byrow = FALSE)
boston1$lagprice <- log(rowSums(boston1$LivingArea * lagpricePSQF)/10)




#block group
boston.blockGroup <- st_read("data/blockGroupDataSelect.shp")

house.blockgroup <- st_join(boston1,boston.blockGroup) %>%
  select(c(1,40:50,52)) %>%
  as.data.frame() %>%
  select(-geometry)

boston1 <- left_join(boston1,house.blockgroup,by="UniqueSale")








# where test=0
boston1 <- boston1 %>%
  select(-c(R_TOTAL_RM,X8002_X,N01_BUS,VACANT_CY,crimeCount,
            Style,R_HEAT_TYP,POPDENS_CY,R_KITCH,YR_BUILT,
            N24_BUS,LU,NUM_FLOORS,R_BDRMS,OWN_OCC,LivingArea,STRUCTURE_))

boston.notest <- boston1 %>%
  filter(test==0) %>%
  as.data.frame() %>%
  select(-geometry)


#correlation matrix
M <- cor(boston.notest %>% select(-c(UniqueSale,Latitude,Longitude,test,R_AC,R_BLDG_STY,
                                     R_ROOF_TYP,R_EXT_FIN,R_AC,PricePerSq,Name)))
corrplot(M, method = "number")


reg1 <- lm(log(SalePrice) ~.,data = boston.notest %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,Name)))
summary(reg1)


#cross validation
fitControl <- trainControl(method = "cv", number = 10)
set.seed(825)

lmFit1 <- train(log(SalePrice) ~.,
                data = boston.notest %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,Name)), 
                method = "lm", 
                trControl = fitControl)
lmFit1
lmFit1$resample

#MAPE
mean(abs(exp(reg1$fitted.values) - boston.notest$SalePrice) / boston.notest$SalePrice)

#map residuals
regResiduals <- 
  data.frame(residuals = exp(reg1$residuals),
             SalePrice = boston.notest %>% select(SalePrice),
             Longitude = boston.notest %>% select(Longitude),
             Latitude = boston.notest %>% select(Latitude),
             Name = boston.notest %>% select(Name),
             Legend = "reg")

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(data=regResiduals, 
             aes(Longitude,Latitude, color=factor(ntile(residuals,5))),size=1) +
  
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                      labels=as.character(quantile(regResiduals$residuals,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Residuals \n(Quintile Breaks)") +
  mapTheme()


regResiduals_Summary <-
  regResiduals %>%
  group_by(Name) %>%
  summarize(meanResidual = mean(residuals, na.rm=TRUE),
            sdResidual = sd(residuals, na.rm=TRUE),
            countSales = n()) %>%
  mutate(Legend="reg") %>%
  filter(countSales > 5) %>%
  left_join(bostonNhoods) %>%
  st_sf()

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill=NA, colour="black", size=2) +
  geom_sf(data=regResiduals_Summary, aes(fill=sdResidual), colour="black") +
  mapTheme()

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill=NA, colour="black", size=2) +
  geom_sf(data=regResiduals_Summary, aes(fill=meanResidual), colour="black") +
  mapTheme()



regDF <- cbind(log(boston.notest$SalePrice), reg1$fitted.values)
colnames(regDF) <- c("Observed", "Predicted")
regDF <- as.data.frame(regDF)
ggplot() + 
  geom_point(data=regDF, aes(Observed, Predicted)) +
  stat_smooth(data=regDF, aes(Observed, Observed), method = "lm", se = FALSE, size = 1) + 
  labs(title="Predicted Sales Volume as a function\nof Observed Sales Volume") +
  theme(plot.title = element_text(size = 18,colour = "black"))















stargazer(boston.notest, type="text", title = "Summary Statistics")

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(data=boston.notest, 
             aes(Longitude,Latitude, color=factor(ntile(SalePrice,5))),size=1) +
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#ac96c6","#b856a7","#e10f7c"),
                      labels=as.character(quantile(boston.notest$SalePrice,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Home Sale Price \n(Quintile Breaks)") +
  labs(title="Home Sale Price",
       subtitle="Boston, Massachusetts")+
  mapTheme()

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(data=boston.notest, 
             aes(Longitude,Latitude, color=factor(ntile(distUniversity,5))),size=1) +
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#ac96c6","#b856a7","#e10f7c"),
                      labels=as.character(quantile(boston.notest$distUniversity,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Distance to MIT (inversed) \n(Quintile Breaks)") +
  labs(title="Distance to MIT (inversed)",
       subtitle="Boston, Massachusetts")+
  mapTheme()


ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(data=boston.notest, 
             aes(Longitude,Latitude, color=factor(ntile(MINORITYCY,5))),size=1) +
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#ac96c6","#b856a7","#e10f7c"),
                      labels=as.character(quantile(boston.notest$MINORITYCY,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Percent of Minority\n(Quintile Breaks)") +
  labs(title="Percent of Minority in Block Groups",
       subtitle="Boston, Massachusetts")+
  mapTheme()

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(data=boston.notest, 
             aes(Longitude,Latitude, color=factor(ntile(lagprice,5))),size=1) +
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#ac96c6","#b856a7","#e10f7c"),
                      labels=as.character(quantile(boston.notest$lagprice,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Spatial lagged price\n(Quintile Breaks)") +
  labs(title="Spatial Lag of Home Sale Price",
       subtitle="Boston, Massachusetts")+
  mapTheme()

ggplot()+
  geom_point(data = boston.notest,aes(x=lagprice,y=log(SalePrice)))+
  stat_smooth(data = boston.notest,aes(x=lagprice,y=log(SalePrice)), method = "lm", se = FALSE, size = 1) + 
  labs(title="Observed Sales price as a function\nof Spatial Lagged Sales Price") +
  theme(plot.title = element_text(size = 18,colour = "black"))


#boston.notest<-boston.notest[-which(boston.notest$Name=="Fenway"),]

inTrain <- createDataPartition(
  y = boston.notest$SalePrice, 
  p = .75, list = FALSE)
training <- boston.notest[inTrain,] #the new training set
test <- boston.notest[-inTrain,]  #the new test set

reg <- lm(log(SalePrice) ~.,data = training %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,Name)))
summary(reg)

reg.trainingset <- data.frame(observed=training$SalePrice,
                              predicted=exp(reg$fitted.values))
reg.trainingset <- reg.trainingset %>%
  mutate(error=predicted-observed,
         squareError=error^2,
         absError=abs(error),
         absPctError=absError/observed)
rsquared.training <- 1-(sum(reg.trainingset$squareError)/sum((reg.trainingset$observed-mean(reg.trainingset$observed))^2))
rmse.training <- (mean(reg.trainingset$squareError))^(1/2)
mae.training <- MAE(reg.trainingset$predicted,reg.trainingset$observed)
mape.training <- mean(reg.trainingset$absPctError)

reg.testset <- data.frame(observed=test$SalePrice,
                          predicted=exp(predict(reg,test)))
reg.testset <- reg.testset %>%
  mutate(error=predicted-observed,
         squareError=error^2,
         absError=abs(error),
         absPctError=absError/observed)
rsquared.test <- 1-(sum(reg.testset$squareError)/sum((reg.testset$observed-mean(reg.testset$observed))^2))
rmse.test <- (mean(reg.testset$squareError))^(1/2)
mae.test <- MAE(reg.testset$predicted,reg.testset$observed)
mape.test <- mean(reg.testset$absPctError)


reg.trainingset.summary <- data.frame(dataset=c("training","test"),
                                      Rsquared=c(rsquared.training,rsquared.test),
                                      RMSE=c(rmse.training,rmse.test),
                                      MAE=c(mae.training,mae.test),
                                      MAPE=c(mape.training,mape.test))
reg.trainingset.summary

reg.trainingset.summary1 <- gather(reg.trainingset.summary,statistics,value,-dataset)
ggplot(data = reg.trainingset.summary1,aes(x=dataset,y=value))+
  geom_col(aes(fill=dataset),width = .5) +
  facet_wrap(~statistics,scales = "free_y")+
  labs(title="Summarise of goodness of fit for training and test set")

#cross validation
fitControl <- trainControl(method = "cv", number = 10)
set.seed(825)

lmFit <- train(log(SalePrice) ~.,
               data = training %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq)), 
               method = "lm", 
               trControl = fitControl)
lmFit
lmFit1$resample
mean(lmFit1$resample$Rsquared)
sd(lmFit1$resample$Rsquared)

ggplot(as.data.frame(lmFit$resample), aes(Rsquared)) + 
  geom_histogram(bins=4) +
  labs(x="R-squared",
       y="Count",
       title="Histogram of R-squared by cross-validation")


ggplot() + 
  geom_point(data=reg.testset, aes(observed, error)) +
  stat_smooth(data=reg.testset, aes(observed, error), method = "lm", se = FALSE, size = 1) + 
  labs(title="Regression Residuals as a function\nof Observed Sales Price",
       x="observed home sale price",
       y="regression residual") +
  theme(plot.title = element_text(size = 18,colour = "black"))

ggplot() + 
  geom_point(data=reg.testset, aes(predicted, error)) +
  stat_smooth(data=reg.testset, aes(predicted, error), method = "lm", se = FALSE, size = 1) + 
  labs(title="Regression Residuals as a function\nof Predicted Sales Price",
       x="predicted home sale price",
       y="regression residual") +
  theme(plot.title = element_text(size = 18,colour = "black"))

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(aes(test$Longitude,test$Latitude, color=factor(ntile(reg.testset$error,5))),size=1) +
  scale_colour_manual(values = c("#0f0fe1","#5656a8","#eeeeee","#a85656","#e10f0f"),
                      labels=as.character(quantile(reg.testset$error,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Residual\n(Quintile Breaks)") +
  labs(title="Model Residuals for Test Set")+
  mapTheme()


coords <- cbind(test$Longitude,test$Latitude)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moran.test(reg.testset$error, nb2listw(spatialWeights, style="W"))


ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill="black", colour="white") +
  geom_point(aes(training$Longitude,training$Latitude, color=factor(ntile(reg$fitted.values,5))),size=1) +
  scale_colour_manual(values = c("#edf8fb","#b3cde3","#ac96c6","#b856a7","#e10f7c"),
                      labels=as.character(quantile(boston.notest$SalePrice,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Predicted Value\n(Quintile Breaks)") +
  labs(title="Predicted Sales Price for Training Set")+
  mapTheme()


error.Neighborhood <- data.frame(Name=training$Name,
                                 APE=reg.trainingset$absPctError) %>%
  group_by(Name) %>%
  summarise(MAPE=mean(APE)) %>%
  left_join(bostonNhoods,by="Name") %>%
  st_sf()

ggplot() + 
  geom_sf(data=bostonNhoods, aes(), fill=NA, colour="black", size=2) +
  geom_sf(data=error.Neighborhood, aes(fill=MAPE), colour="black") +
  labs(title="Mean Absolute Percent Error by Neighborhood") +
  mapTheme()
MAPE.neighborhood <- select(as.data.frame(error.Neighborhood),-geometry)
ggplot(MAPE.neighborhood, 
       aes(x=reorder(Name,-MAPE),
           y=MAPE, fill=Name)) + 
  geom_bar(stat="identity") +
  labs(x="neighborhood",
       title="Mean Absolute Percent Error by Neighborhood") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")




#spatial cross validation
pricebyNeighborhood <- boston.notest %>%
  group_by(Name) %>%
  summarise(meanPrice=mean(SalePrice),
            count=n()) %>%
  as.data.frame()
pricebyNeighborhood[order(pricebyNeighborhood$meanPrice,decreasing = TRUE),]

MAPE.neighborhood<-left_join(MAPE.neighborhood,pricebyNeighborhood,by="Name")
ggplot(MAPE.neighborhood, 
       aes(x=reorder(Name,-MAPE),y=MAPE,
           fill=factor(ntile(MAPE.neighborhood$meanPrice,5)))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = rev(heat.colors(5,1)),
                    labels=as.character(quantile(MAPE.neighborhood$meanPrice,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                    name="Average home sales price\n(Quintile Breaks)") +
  labs(x="neighborhood",
       title="MAPE and Average Sales Price by Neighborhood") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

i <- which(boston.notest$Name %in% c("South End","Back Bay"))
training.high <- boston.notest[-i,]
test.high <- boston.notest[i,]

reg.high <- lm(log(SalePrice) ~.,data = training.high %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,Name)))
summary(reg.high)

residual.high <- data.frame(observed=test.high$SalePrice,
                            predicted=exp(predict(reg.high,test.high))) %>%
  transmute(APE=abs(predicted-observed)/observed,
            neighborhood="rich")
mean(residual.high$APE)



i <- which(boston.notest$Name=="Dorchester")
training.middle <- boston.notest[-i,]
test.middle <- boston.notest[i,]

reg.middle <- lm(log(SalePrice) ~.,data = training.middle %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,Name)))
summary(reg.middle)

residual.middle <- data.frame(observed=test.middle$SalePrice,
                              predicted=exp(predict(reg.middle,test.middle))) %>%
  transmute(APE=abs(predicted-observed)/observed,
            neighborhood="medium")
mean(residual.middle$APE)



i <- which(boston.notest$Name=="Mattapan")
training.low <- boston.notest[-i,]
test.low <- boston.notest[i,]

reg.low <- lm(log(SalePrice) ~.,data = training.low %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,Name)))
summary(reg.low)

residual.low <- data.frame(observed=test.low$SalePrice,
                           predicted=exp(predict(reg.low,test.low))) %>%
  transmute(APE=abs(predicted-observed)/observed,
            neighborhood="poor")
mean(residual.low$APE)

spatial.cv <- rbind(residual.high,residual.middle,residual.low)
ggplot(data = spatial.cv,aes(neighborhood,APE))+
  geom_boxplot(aes(fill=neighborhood)) +
  labs(y="MAPE",
       title="MAPE of Spatial Cross-validation")




#predict
boston.predict <- boston1 %>% filter(test==1)
names(boston.predict)
exp(predict(reg1,boston.predict))
prediction <- data.frame(UniqueSale=boston.predict$UniqueSale,
                         PredictedSales=exp(predict(reg1,boston.predict)),
                         TeamName="awesome n adorable")
write.csv(prediction,"awesome_n_adorable.csv",row.names = FALSE)

prediction[92,]$PredictedSales<-exp(predict(reg.r,boston.predict[92,]))

reg.r <- lm(log(SalePrice) ~.,data = boston.notest %>% select(-c(UniqueSale,Latitude,Longitude,test,PricePerSq,
                                                                 MEDHINC_CY,TOTHU_CY,OWNER_CY,BACHDEG_CY,GRADDEG_CY,
                                                                 MINORITYCY,ACSHHBPOV)))
summary(reg.r)
