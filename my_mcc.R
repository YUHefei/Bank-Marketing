
best.rpart <- function(rpart.obj)
{
  cp <- rpart.obj$cptable[which.min(rpart.obj$cptable[,4]),1]
  prune.rpart(rpart.obj, cp = cp)
}# Selecting the best tree by cv error


get_train_data<-function(data, sample){
  if(sample == 'smote')
    SMOTE(y~., data)
  else if(sample== 'upSample')
    upSample(x = data[, -1], y = data$y)
  else
    data
}

calc_mcc<- function(algorithm,data,k,sampling) {
  mccrs<- c()
  folds <- createFolds(data$y, k = k, list = TRUE, returnTrain = TRUE)
  for (i in 1:k) {
    if(algorithm == 'glm'){
      model <- glm(y~., data =get_train_data(data[folds[[i]],],sampling),family = "binomial")
      predictions <- predict(model,newdata = data[-folds[[i]],],type = "response")
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                           as.integer(as.logical(predictions > "0.5")))
      )
    }
    else if(algorithm == 'rpart_gini'){
      model <-  best.rpart(rpart(y~., data =get_train_data(data[folds[[i]],],sampling),method = "class"))
      predictions <- predict(model,newdata = data[-folds[[i]],])
      predictions <- as.data.frame(predictions)
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                           as.integer(as.logical(predictions$yes > predictions$no))))
      
    }
    else if(algorithm == 'rpart_entropy'){
      model <- best.rpart(rpart(y~., data =get_train_data(data[folds[[i]],],sampling) ,method = "class", parms = list(split = "information"))) 
      predictions <- predict(model,newdata = data[-folds[[i]],])
      predictions <- as.data.frame(predictions)
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                           as.integer(as.logical(predictions$yes > predictions$no)))
      )
    }
    else if(algorithm == 'nnet'){
      model <- nnet(y~., data = get_train_data(data[folds[[i]],],sampling), size = 10, decay=1e-3,  maxit = 1000)
      predictions <- predict(model,newdata = data[-folds[[i]],],type = "raw")
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                           as.integer(as.logical(predictions > 0.5))))
      
    }
    else if(algorithm == 'lda'){
      model <- lda(y~., data = get_train_data(data[folds[[i]],],sampling))
      predictions <- predict(model,newdata = data[-folds[[i]],])
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                           as.integer(as.logical(predictions$class=='yes'))))
      
    }
    else if(algorithm == 'lda_prescale'){
      my_train_data <- get_train_data(data[folds[[i]],],sampling)
      preProcValues <- preProcess(my_train_data, method = c("scale","center"))
      scaled.data <- predict(preProcValues, my_train_data)
      model <- lda(y~., scaled.data)
      preProcValues2 <- preProcess(data[-folds[[i]],], method = c("scale","center"))
      scaled.data2 <- predict(preProcValues, data[-folds[[i]],])
      predictions <- predict(model,newdata = scaled.data2)
      mccrs<-c(mccrs, mccr(as.integer(as.logical(scaled.data2$y=='yes')),
                           as.integer(as.logical(predictions$class=='yes'))))
      
      
    }
    else if(algorithm == 'svm'){
      model <- svm(y~., data = get_train_data(data[folds[[i]],],sampling),gamma = 1,cost = 100, kernel = "polynomial")
      predictions <- predict(model,newdata = data[-folds[[1]],])
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[1]], ]$y=="yes")),
                           as.integer(as.logical(predictions=="yes"))))
      
    }
    else if(algorithm == 'j48'){
      model <- J48(y~., data = get_train_data(data[folds[[i]],],sampling))
      predictions <- predict(model,newdata = data[-folds[[i]],])
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=="yes")),
                           as.integer(as.logical(predictions=="yes"))))
    }
    else if(algorithm == 'bagging'){
      model <- bagging(y~., data = get_train_data(data[folds[[i]],],sampling))
      predictions <- predict(model,newdata = data[-folds[[i]],])
      mccrs<-c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=="yes")),
                           as.integer(as.logical(predictions=="yes"))))
    }
    else if(algorithm == 'random_forest_no_mtry'){
      model <- randomForest(y~., data = na.exclude(get_train_data(data[folds[[i]],],sampling)))
      predictions <- predict(model,newdata = data[-folds[[i]],])
      predictions <- as.data.frame(predictions)
      mccrs = c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                            as.integer(as.logical(predictions=='yes')))
      )
    }
    else if(algorithm == 'random_forest_mtry'){
      model <- randomForest(y~., data = na.exclude(get_train_data(data[folds[[i]],],sampling)),mtry=6)
      predictions <- predict(model,newdata = data[-folds[[i]],])
      predictions <- as.data.frame(predictions)
      mccrs = c(mccrs, mccr(as.integer(as.logical(data[-folds[[i]], ]$y=='yes')),
                            as.integer(as.logical(predictions=='yes')))
      )
      
    }
  }
  mccrs
}


calc_seed_mcc <- function(algorithm,data,k,s,sampling){
  mccrs<- c()
  for (i in (1:s)){
    set.seed(i)
    mccrs<- c(mccrs,calc_mcc(algorithm,data,k,sampling))
  }
  mccrs
}



