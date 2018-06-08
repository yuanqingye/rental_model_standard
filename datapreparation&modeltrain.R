library(readxl)
setwd("~/R_Projects/rental_model_standard")
compare_rent_data_raw = read_xlsx("./data/2017pred_summary.xlsx")
compare_2017_rent_data = compare_rent_data_raw[,c(1,3:10)]
compare_2017_rent_data = data.table(compare_2017_rent_data)
compare_2018_rent_data = compare_rent_data_raw[,c(1,17:25)]
compare_2018_rent_data = data.table(compare_2018_rent_data)
setnames(compare_2018_rent_data,"rate1__1","rate1")
setnames(compare_2018_rent_data,"rate2__1","rate2")

main = function(){
  temp = getMisplacedData()
  mall_names = temp[[1]]
  train_set = temp[[2]]
  test_set = temp[[3]]
  train_mall_names = temp[[4]]
  #SVM part
  ptm <- proc.time()
  svm_result = getSVMResult()
  ptm = proc.time() - ptm
  #random forest part
  ptm <- proc.time()
  rf_result = getRandomForestResult()
  ptm2 = proc.time() - ptm
  #GBM part
  ptm <- proc.time()
  gbm_result = getGBMResult()
  ptm3 = proc.time() - ptm
  #Neural network part
  ptm <- proc.time()
  nn_result = getNeuralNetworkResult()
  ptm4 = proc.time() - ptm
  MSE.rf.MALLS = rf_result[[3]]
  MSE.nn.MALLS = nn_result[[3]]
  MSE.svm.MALLS = svm_result[[3]]
  MSE.gbm.MALLS = gbm_result[[3]]
  MSE.all.MALLS = cbind.data.frame(MSE.rf.MALLS,nn_perc = MSE.nn.MALLS$perc,svm_perc = MSE.svm.MALLS$perc,gbm_perc = MSE.gbm.MALLS$perc)
  temp_median = lapply(lapply(MSE.all.MALLS[,-1],abs),median)
  temp_min = apply(abs(MSE.all.MALLS[,5:8]),1,min)
  temp_mean = lapply(lapply(MSE.all.MALLS[,-1],abs),mean)
  temp_sd = lapply(MSE.all.MALLS[,-1],sd)
  temp_min_index = apply(abs(MSE.all.MALLS[,5:8]),1,which.min)
  MSE.all.MALLS = cbind.data.frame(MSE.all.MALLS,temp_min,temp_min_index)
  rentind = which(names(train_set) %in% c("rent"))
  rf.model = rf_result[[1]]
  rf.train.result= predict(rf.model,train_set[-rentind])
  svm.model = svm_result[[1]]
  svm.train.result = predict(svm.model,train_set[-rentind])
  gbm.model = gbm_result[[1]]
  gbm.train.result = predict(gbm.model,train_set,n.trees = 100000)
  nn.model = nn_result[[1]]
  maxs <- apply(train_set, 2, max)
  mins <- apply(train_set, 2, min)
  train.scaled = as.data.frame(scale(train_set, center = mins, scale = maxs - mins))
  max_rent = maxs["rent"]
  min_rent = mins["rent"]
  nn.train.result = compute(nn.model,train.scaled[,-rentind])
  nn.train.result = nn.train.result$net.result*(max_rent-min_rent)+min_rent
  train_view = cbind.data.frame(mall_names = train_mall_names,train_set[rentind],rf_rent = rf.train.result,nn_rent = nn.train.result,svm_rent = svm.train.result,gbm_rent = gbm.train.result)
  train_view = data.table(train_view)
  train_view_focus = train_view[,lapply(.SD[(.N-5):.N],sum),by = "mall_names"]
  train_view_focus[,`:=`(rf_diff=abs(rf_rent-rent)/rent,nn_diff=abs(nn_rent-rent)/rent,svm_diff=abs(svm_rent-rent)/rent,gbm_diff=abs(gbm_rent-rent)/rent)]
  new_row = data.table(mall_names = "昆明广福路商场")
  train_view_focus = rbind.fill(train_view_focus[1:23,],new_row,train_view_focus[24:nrow(train_view_focus),])
  temp_min_index_train = apply(abs(train_view_focus[,7:10]),1,which.min)
  temp = MSE.all.MALLS[,5:8]
  temp = as.matrix(temp)
  temp_min_index_train = as.numeric(temp_min_index_train)
  temp_decision = temp[cbind(1:nrow(temp), temp_min_index_train)]
  MSE.all.MALLS = cbind.data.frame(MSE.all.MALLS,temp_min_index_train,temp_decision)
  # temp_decision = mapply(`[`,temp,temp_min_index_train)
  train_view_time = cbind.data.frame(mall_names = train_mall_names,age = train_set["AGE"])
  train_view_time = data.table(train_view_time)
  mall_view_time = train_view_time[,.(age = sapply(.SD[.N,"AGE"],sum)),by = "mall_names"]
  MSE.all.MALLS.time = merge(MSE.all.MALLS,mall_view_time,by.x = "mall_name",by.y = "mall_names",all.x = TRUE)
}

predictRentOnAdjacent = function(timespan = 12,dest_date = 201712,mall_filter = NULL){
  first_result = calModelResultOnTimespan(timespan,dest_date)
  first_part = first_result[[1]][!(mall_name %in% first_result[[2]]),]
  second_result = calModelResultOnTimespan(timespan/2,dest_date,first_result[[2]])
  second_part = second_result[[1]][!(mall_name %in% second_result[[2]]),]
  second_part = cbind.data.frame(mall_name = second_part$mall_name,mall_selected = second_part[,-1]*2)
  final_result = rbindlist(list(first_part[!is.na(pred_rent),],second_part),use.names=TRUE,fill = TRUE)
  return(final_result)
}

predictRentOnTimeAgo = function(timespan = 1,dest_date = 201712,passnum = 12,mall_filter = NULL){
  result = calModelResultOnTimespan(timespan,dest_date,mall_filter,FALSE,passnum)
}

calModelResultOnTimespan <- function(timespan,dest_date,isAdjacent = TRUE,passnum = 12,test_set_size = 1,mall_selected = NULL) {
  #Train rf,nn,svm,gbm model and do cross validation for test set(last complete available record)
  cross_result = makeCrossValidation(timespan,dest_date,isAdjacent,passnum,test_set_size,mall_selected)
  rent_data_year =  cross_result[[1]]
  MSE.all.MALLS = cross_result[[2]]
  
  rentind = which(names(dest_rent) %in% c("rent"))
  dest_rent = rent_data_year[[6]]
  dest_mall_names = rent_data_year[[3]]
  un_mature_mall = rent_data_year[[7]]
  
  #using trained model,based on dest set to predict
  dest_view = predict_by_set(cross_result,dest_rent,dest_mall_names,rentind)
  
  # suppose you only have part of the result's
  MSE.all.MALLS.mixed = setDT(MSE.all.MALLS)[dest_view, on="mall_name"]
  if(!is.null(mall_selected)){
    MSE.all.MALLS.mixed = MSE.all.MALLS.mixed[mall_name %in% mall_selected,]
  }
  min_index = apply(abs(MSE.all.MALLS.mixed[,c("perc","nn_perc","svm_perc","gbm_perc")]),1,which.min)
  min_index = as.numeric(min_index)
  matrix = as.matrix(MSE.all.MALLS.mixed[,c("rf_rent","nn_rent","svm_rent","gbm_rent")])
  decision = matrix[cbind(1:nrow(MSE.all.MALLS.mixed), min_index)]
  MSE.all.MALLS.final = cbind.data.frame(MSE.all.MALLS.mixed[,c("mall_name","rf_rent","nn_rent","svm_rent","gbm_rent")],pred_rent = decision)
  result = list(MSE.all.MALLS.final,un_mature_mall)
  return(result)
}

makeCrossValidation = function(timespan = 12,dest_date = 201712,isAdjacent = TRUE,passnum = 12,test_set_size = 1,mall_filter = NULL){
  rent_year_data = getyearModeData(timespan,dest_date,isAdjacent,passnum,test_set_size,mall_filter)
  train_rent = rent_year_data[[4]]
  test_rent = rent_year_data[[5]]
  dest_rent = rent_year_data[[6]]
  train_mall_names = rent_year_data[[1]]
  test_mall_names = rent_year_data[[2]]
  dest_mall_names = rent_year_data[[3]]
  #SVM part
  ptm <- proc.time()
  svm_result = getSVMResult(test_mall_names, train_rent, test_rent)
  ptm = proc.time() - ptm
  #random forest part
  ptm <- proc.time()
  rf_result = getRandomForestResult(test_mall_names, train_rent, test_rent)
  ptm2 = proc.time() - ptm
  #GBM part
  ptm <- proc.time()
  gbm_result = getGBMResult(test_mall_names, train_rent, test_rent)
  ptm3 = proc.time() - ptm
  #Neural network part
  ptm <- proc.time()
  nn_result = getNeuralNetworkResult(test_mall_names, train_rent, test_rent)
  ptm4 = proc.time() - ptm
  MSE.rf.MALLS = rf_result[[3]]
  MSE.nn.MALLS = nn_result[[3]]
  MSE.svm.MALLS = svm_result[[3]]
  MSE.gbm.MALLS = gbm_result[[3]]
  MSE.all.MALLS = cbind.data.frame(
    MSE.rf.MALLS,
    nn_perc = MSE.nn.MALLS$perc,
    svm_perc = MSE.svm.MALLS$perc,
    gbm_perc = MSE.gbm.MALLS$perc
  )
  result = list(rent_year_data,MSE.all.MALLS,svm_result,rf_result,gbm_result,nn_result)
  return(result)
}

getMisplacedData = function(file_location = "~/data/rental_raw_data.csv",test_time = 201710:201712,predict = TRUE){
  big_general_info_original = read.csv(file_location,stringsAsFactors = FALSE)
  big_general_info = big_general_info_original[!is.na(big_general_info_original$rent) & big_general_info_original$rent != 0,]
  # some special cases
  big_general_info[big_general_info$MALL_NAME=="上海汶水商场" & big_general_info$DATE_ID == 201409,"XINYETAI_NUM"] = 0.5
  big_general_info[big_general_info$MALL_NAME=="杭州古墩商场" & big_general_info$DATE_ID == 201410,"XINYETAI_NUM"] = 6/31
  big_general_info = data.table(big_general_info)
  if(predict){
  big_general_info[,rent := c(rent[-1:-3],rep(NA,3)),by = "MALL_NAME"]
  }
  big_general_info = as.data.frame(big_general_info)
  big_general_info$MALL_NAME =  str_replace(big_general_info$MALL_NAME,"北京至尊Mall","北京东四环商场")
  train_df = big_general_info[(big_general_info$DATE_ID < min(test_time)),]
  test_df = big_general_info[big_general_info$DATE_ID %in% test_time,]
  train_mall_names = train_df[,"MALL_NAME"]
  test_mall_names = test_df[,"MALL_NAME"]
  train_rent = train_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID","OPEN_DATE","PRICE","YEAR","city"))]
  test_rent = test_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID","OPEN_DATE","PRICE","YEAR","city"))]
  result = list(test_mall_names,train_rent,test_rent,train_mall_names)
  return(result)
}

getyearModeData = function(timespan = 12,dest_date = 201712,isAdjacent = TRUE,passnum = 12,test_set_size = 1,mall_filter = NULL,file_location = "~/data/rental_raw_data.csv"){
  rent_data_month_raw = read.csv(file_location,stringsAsFactors = FALSE,fileEncoding = "GBK")
  rent_data_month_raw = data.table(rent_data_month_raw)
  rent_data_month_raw$MALL_NAME = enc2utf8(rent_data_month_raw$MALL_NAME)
  cpi = read_xlsx("~/data/cpi.xlsx",col_names = FALSE)
  colnames(cpi) [[1]]= "DATE_ID"
  colnames(cpi) [[8]]= "cpi"
  library(plyr)
  rent_data_month_raw = join(rent_data_month_raw,cpi[,c("DATE_ID","cpi")],by = "DATE_ID",type = "left")
  rent_data_month_raw$cpi = 1+rent_data_month_raw$cpi
  if(!is.null(mall_filter)){
    rent_data_month = rent_data_month_raw[MALL_NAME %in% mall_filter,]
  }
  else{
    rent_data_month = rent_data_month_raw[MALL_NAME != "昆明广福路商场",]
  }
  unuse_col = c("MALL_NAME","MALL_CODE","YEAR","city","OPEN_DATE")
  sum_col = c("CUSTOMER_NUM","SALE","rent")
  prod_col = c("cpi")
  max_col = c("AGE","DATE_ID")
  avg_col = c("BRAND_NUM","RENT_AREA","PRICE","AREA_JIAJU","JIAJU_NUM",             
              "AREA_JIANCAI","JIANCAI_NUM","AREA_RUANZHUANG","RUANZHUANG_NUM","AREA_JINKOU",
              "JINKOU_NUM","AREA_XINYETAI","XINYETAI_NUM","GDP","POPULATION","SALARY")
  future_col = c("rent")
  freeze_col = colnames(rent_data_month)[!(colnames(rent_data_month) %in% c(unuse_col,sum_col,max_col,avg_col,prod_col,future_col))]
  if(isAdjacent){
    rent_data_year = rent_data_month[,c(lapply(.SD[,sum_col,with=FALSE],getYearPara,sum,timespan),lapply(.SD[,avg_col,with=FALSE],getYearPara,mean,timespan),lapply(.SD[,max_col,with = FALSE],getYearPara,max,timespan),lapply(.SD[,prod_col,with=FALSE],getYearPara,prod,timespan),"predprice"=lapply(.SD[,future_col,with = FALSE],getYearReal,sum,timespan),.SD[.N,freeze_col,with=FALSE]),by = "MALL_NAME"]
    un_mature_mall = rent_data_year[,.(record_num = .N),by = MALL_NAME][record_num<=timespan,]$MALL_NAME
    infant_mall = unique(rent_data_month[,.(record_num = .N),by = MALL_NAME][record_num<timespan,]$MALL_NAME)
    setnames(rent_data_year,"rent","current_rent")
    rent_data_year[!(MALL_NAME %in% un_mature_mall),rent:=c(predprice.rent[1:(.N-timespan)],rep(NA,timespan)),by = "MALL_NAME"]
  }
  else{
    rent_data_year = rent_data_month[,c(lapply(.SD[,sum_col,with=FALSE],getYearPara,sum,timespan),lapply(.SD[,avg_col,with=FALSE],getYearPara,mean,timespan),lapply(.SD[,max_col,with = FALSE],getYearPara,max,timespan),lapply(.SD[,prod_col,with=FALSE],getYearPara,prod,timespan),"predprice"=lapply(.SD[,future_col,with = FALSE],getNextYYBasisAgg,sum,timespan,passnum),.SD[.N,freeze_col,with=FALSE]),by = "MALL_NAME"]
    un_mature_mall = rent_data_year[,.(record_num = .N),by = MALL_NAME][record_num<=timespan,]$MALL_NAME
    infant_mall = unique(rent_data_month[,.(record_num = .N),by = MALL_NAME][record_num<passnum,]$MALL_NAME)
    setnames(rent_data_year,"rent","current_rent")
    rent_data_year[!(MALL_NAME %in% un_mature_mall),rent:=c(predprice.rent[1:(.N-passnum)],rep(NA,passnum)),by = "MALL_NAME"]
    }
  # rent_data_year[(MALL_NAME %in% un_mature_mall),predprice:=NA,by = "MALL_NAME"]
  rent_data_year$predprice.rent = NULL
  # base_rent = rent_data_year[,!(names(rent_data_year)%in%c("mall_name","date_id"))]
  dest_rent = rent_data_year[DATE_ID %in% dest_date,]
  rest_rent = rent_data_year[!is.na(rent)&(DATE_ID < min(dest_date)),]
  test_rent = rest_rent[,.SD[(.N-test_set_size+1):.N,],by = "MALL_NAME"]
  train_rent = rest_rent[,.SD[1:(.N-test_set_size),],by = "MALL_NAME"]
  train_mall_names = train_rent$MALL_NAME
  test_mall_names = test_rent$MALL_NAME
  dest_mall_names = dest_rent$MALL_NAME
  #commented 2018/6/8 for training with DATE_ID
  # train_rent = train_rent[,!(names(train_rent)%in%c("MALL_NAME","DATE_ID")),with = FALSE]
  # test_rent = test_rent[,!(names(test_rent)%in%c("MALL_NAME","DATE_ID")),with = FALSE]
  # dest_rent = dest_rent[,!(names(dest_rent)%in%c("MALL_NAME","DATE_ID")),with = FALSE]
  train_rent = train_rent[,!(names(train_rent)%in%c("MALL_NAME")),with = FALSE]
  test_rent = test_rent[,!(names(test_rent)%in%c("MALL_NAME")),with = FALSE]
  dest_rent = dest_rent[,!(names(dest_rent)%in%c("MALL_NAME")),with = FALSE]
  train_rent = data.frame(train_rent)
  test_rent = data.frame(test_rent)
  dest_rent = data.frame(dest_rent)
  result = list(train_mall_names,test_mall_names,dest_mall_names,train_rent,test_rent,dest_rent,un_mature_mall,infant_mall)
  return(result)
}

getRandomForestResult = function(test_mall_names = mall_names,train_rent = train_set,test_rent = test_set){
  library(randomForest)
  rf <- randomForest(rent ~ ., data=train_rent, ntree=500, proximity=TRUE)
  rentind = which(names(test_rent) %in% c("rent"))
  rf.test.next= predict(rf,test_rent[-rentind])
  compare.result.rf = cbind.data.frame(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = rf.test.next)
  compare.result.rf = data.table(compare.result.rf)
  # compare.result.rf[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
  compare.result.rf[,diff:=(real_rent-pred_rent)]
  MSE.rf.MALLS = compare.result.rf[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),rf_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
  MSE.rf <- sum((test_rent$rent - rf.test.next)^2)/length(test_rent$rent)
  result = list(rf,compare.result.rf,MSE.rf.MALLS,MSE.rf)
  return(result)
}

getNeuralNetworkResult = function(test_mall_names = mall_names,train_rent = train_set,test_rent = test_set){
  #neural net part
  source("~/R_Projects/neuralNetwork/Rfile/nn_para.R")
  neuralnet_para_m = neuralnet_para(train_rent,test_rent)
  k = nrow(neuralnet_para_m)
  n <- names(train_rent)
  f <- as.formula(paste("rent ~", paste(n[!n %in% "rent"], collapse = " + ")))
  maxs <- apply(train_rent, 2, max)
  mins <- apply(train_rent, 2, min)
  train.scaled = as.data.frame(scale(train_rent, center = mins, scale = maxs - mins))
  test.scaled = as.data.frame(scale(test_rent,center = mins,scale = maxs - mins))
  max_rent = maxs["rent"]
  min_rent = mins["rent"]
  # first get the proper parameter then apply to method
  nn <- neuralnet(f,data=train.scaled,hidden = rep(neuralnet_para_m$size[k],neuralnet_para_m$depth[k]),act.fct = 'logistic') 
  rentind = which(names(test.scaled) %in% c("rent"))
  pr.nn <- compute(nn,test.scaled[,-rentind])
  pr.nn_ <- pr.nn$net.result*(max_rent-min_rent)+min_rent
  test.r <- (test.scaled$rent)*(max_rent-min_rent)+min_rent
  compare.result.nn = cbind.data.frame(mall_name = test_mall_names,pred_rent = pr.nn_,real_rent = test.r,diff = test.r - pr.nn_)
  compare.result.nn = data.table(compare.result.nn)
  MSE.nn.MALLS = compare.result.nn[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),rf_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
  MSE.nn <- sum((test.r - pr.nn_)^2)/length(test.r)
  result = list(nn,compare.result.nn,MSE.nn.MALLS,MSE.nn)
  return(result)
}

getSVMResult = function(test_mall_names = mall_names,train_rent = train_set,test_rent = test_set){
  setwd("~/R_Projects/SVM")
  source("./Rfile/test_svm.R")
  try_para = svm_para_percent(train_rent,test_rent)
  k = nrow(try_para) 
  svm.model = svm(rent~.,data = train_rent,cost = try_para$C[k-1],gamma = try_para$sigma[k-1],cross = 5)
  svm.test = predict(svm.model,test_rent[,!(colnames(test_rent)%in%c("rent"))])
  compare.result.svm = cbind.data.frame(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = svm.test,diff = test_rent$rent - svm.test)
  compare.result.svm = data.table(compare.result.svm)
  MSE.svm.MALLS = compare.result.svm[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),rf_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
  MSE.svm = sum((svm.test - test_rent[,"rent"])^2)/length(svm.test)
  result = list(svm.model,compare.result.svm,MSE.svm.MALLS,MSE.svm)
}

getGBMResult = function(test_mall_names = mall_names,train_rent = train_set,test_rent = test_set){
  require(gbm)
  # rent.boost = gbm(rent ~ . ,data = train_rent,distribution = "gaussian",n.trees = 10000,nTrain = 100,bag.fraction = 0.8,n.minobsinnode = 10)
  source('~/R_Projects/ensemble_method/Rfile/boosting_para.R')
  # gbm_para_m = boosting_para(train_rent,test_rent) #takes a lot time
  rent.boost = gbm(rent ~ . ,data = train_rent,distribution = "gaussian",n.trees = 100000,interaction.depth = 4)
  n.trees = seq(from=100 ,to=100000, by=100) #num of trees-a vector of 100 values 
  #Generating a Prediction matrix for each Tree
  predmatrix<-predict(rent.boost,test_rent,n.trees = n.trees)
  #Calculating The Mean squared Test Error
  test.error<-with(test_rent,apply((predmatrix-rent)^2,2,mean))
  head(test.error) #contains the Mean squared test error for each of the 100 trees averaged
  compare.result.gbm = cbind.data.frame(mall_name = test_mall_names,real_rent = test_rent$rent,pred_rent = predmatrix[,1000])
  compare.result.gbm = data.table(compare.result.gbm)
  compare.result.gbm[,c("real_rent","pred_rent"):=.(as.numeric(real_rent),as.numeric(pred_rent))]
  compare.result.gbm[,diff:=(real_rent-pred_rent)]
  MSE.gbm.MALLS = compare.result.gbm[,.(pred_rent=sum(pred_rent),real_rent=sum(real_rent),gbm_err = abs(sum(diff)),perc = sum(diff)/sum(real_rent)),by = "mall_name"]
  MSE.gbm = test.error[1000]
  result = list(rent.boost,compare.result.gbm,MSE.gbm.MALLS,MSE.gbm)
  return(result)
}

predict_by_set = function(cross_result,dest_rent,dest_mall_names,rentind){
  train_rent = cross_result[[1]][[4]]
  rf_result = cross_result[[3]]
  svm_result = cross_result[[4]]
  gbm_result = cross_result[[5]]
  nn_result = cross_result[[6]]
  rentind = which(names(dest_rent) %in% c("rent"))
  rf.model = rf_result[[1]]
  rf.dest.result= predict(rf.model,dest_rent[-rentind])
  svm.model = svm_result[[1]]
  svm.dest.result = predict(svm.model,dest_rent[-rentind])
  gbm.model = gbm_result[[1]]
  gbm.dest.result = predict(gbm.model,dest_rent,n.trees = 100000)
  nn.model = nn_result[[1]]
  nn.dest.result = predict.nn(nn.model,train_rent,dest_rent,rentind)
  dest_view = cbind.data.frame(mall_name = dest_mall_names,dest_rent[rentind],rf_rent = rf.dest.result,nn_rent = nn.dest.result,svm_rent = svm.dest.result,gbm_rent = gbm.dest.result)
  dest_view = data.table(dest_view)
  return(dest_view)
}

predict.nn = function(nn.model,train_rent,dest_rent,rentind){
  maxs <- apply(train_rent, 2, max)
  mins <- apply(train_rent, 2, min)
  dest.scaled = as.data.frame(scale(dest_rent, center = mins, scale = maxs - mins))
  max_rent = maxs["rent"]
  min_rent = mins["rent"]
  nn.dest.result = compute(nn.model,dest.scaled[,-rentind])
  nn.dest.result = nn.dest.result$net.result*(max_rent-min_rent)+min_rent
  return(nn.dest.result)
}

seperate_then_cluster = function(cornum = 2,target_time = 201612,isAdjacent =TRUE,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]])){
  # cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]])
  mall_cluster = kmeans(cluster_set[,-1],cornum,nstart = 20)
  mall_cluster$cluster
  kmeans_cluster_result = cbind(mall_name = rent_data_year[[3]],mall_category = mall_cluster$cluster)
  kmeans_cluster_result = data.table(kmeans_cluster_result)
  mall_parts = list()
  result_filter = list()
  for(i in 1:cornum){
    mall_parts[[i]] = kmeans_cluster_result[mall_category == i,]$mall_name
    if(isAdjacent){
      result_filter[[i]] = calModelResultOnTimespan(12,target_time,TRUE,12,1,mall_parts[[i]])
    }
    else{
      result_filter[[i]] = calModelResultOnTimespan(1,target_time,FALSE,12,1,mall_parts[[i]])
    }
  }
  return(result_filter)
}
  
make_2017_comparison = function(result_filter){  
  if(is.data.table(result_filter[[1]])){
    result_combined = rbindlist(result_filter)
  }
  else{
    result_combined = rbindlist(lapply(result_filter,`[[`,1))
  }
  result_combined = data.table(result_combined)
  result_compare = setDT(compare_2017_rent_data)[result_combined[,c("mall_name","pred_rent")],on = "mall_name"]
  setnames(result_compare,"pred_rent","17_pred3")
  result_compare[,err3:=(`17_pred3`-`17_real_rent`)/`17_real_rent`]
  setcolorder(result_compare,c(1:8,ncol(result_compare),9:(ncol(result_compare)-1)))
  return(result_compare)
}

make_2018_comparison = function(result_filter){
  if(is.data.table(result_filter[[1]])){
    result_combined = rbindlist(result_filter)
  }
  else{
  result_combined = rbindlist(lapply(result_filter,`[[`,1))
  }
  result_combined = data.table(result_combined)
  result_compare = setDT(compare_2018_rent_data)[result_combined[,c("mall_name","pred_rent")],on = "mall_name"]
  setnames(result_compare,"pred_rent","18_pred3")
  setnames(result_compare,"17_real_rent__1","17_real_rent")
  result_compare[,rate3:=(`18_pred3`-`17_real_rent`)/`17_real_rent`]
  setcolorder(result_compare,c(1:3,ncol(result_compare)-1,4:8,ncol(result_compare),9:10))
  return(result_compare)
}

`%=%` = function(var,value){
  e <<- new.env()
  varname = deparse(substitute(var))
  assign(varname,value,envir = e)
}

ff = function(n){
  prod(1:n)
}