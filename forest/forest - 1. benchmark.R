library(ggplot2);library(caret);library(randomForest)
train <- read.csv("data/forest/train.csv")
test <- read.csv("data/forest/test.csv")
sample <- read.csv("data/forest/sampleSubmission.csv")

train$Cover_Type <- as.factor(train$Cover_Type)
        # data description: The data is in raw form (not scaled) and contains binary columns of data for 
        # qualitative independent variables such as wilderness areas and soil type.

        # Elevation - Elevation in meters
        # Aspect - Aspect in degrees azimuth
        # Slope - Slope in degrees
        # Horizontal_Distance_To_Hydrology - Horz Dist to nearest surface water features
        # Vertical_Distance_To_Hydrology - Vert Dist to nearest surface water features
        # Horizontal_Distance_To_Roadways - Horz Dist to nearest roadway
        # Hillshade_9am (0 to 255 index) - Hillshade index at 9am, summer solstice
        # Hillshade_Noon (0 to 255 index) - Hillshade index at noon, summer solstice
        # Hillshade_3pm (0 to 255 index) - Hillshade index at 3pm, summer solstice
        # Horizontal_Distance_To_Fire_Points - Horz Dist to nearest wildfire ignition points
        # Wilderness_Area (4 binary columns, 0 = absence or 1 = presence) - Wilderness area designation
        # Soil_Type (40 binary columns, 0 = absence or 1 = presence) - Soil Type designation
        # Cover_Type (7 types, integers 1 to 7) - Forest Cover Type designation

# 1st glance
table(train$Cover_Type)
unique(train$Cover_Type)
pairs(train[,2:11])

# TRAIN RF MODEL  -  BENCHMARK
        rf <- randomForest(Cover_Type~., 
                           data= train[,-1], 
                           ntree=500, 
                           importance=TRUE)
        varImpPlot(rf)
        importance(rf, type=1)
        
        rf_prob <- predict(rf, test[,-1], type="prob")
        rf_pred <- predict(rf,test[,-1])
        submission <- data.frame(Id=test$Id, Cover_Type=rf_pred)
        write.csv(submission, file="submission.csv",row.names = F)
        # benchmark score is 0.6955 no.1273/1694 ntry =100
        # benchmark score is 0.7076 no.1240      ntry =800
        
        
        