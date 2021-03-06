big_general_info_original = read.csv("~/data/rental_raw_data.csv",stringsAsFactors = FALSE)
big_general_info_original = data.table(big_general_info_original)
big_general_info = big_general_info_original[!is.na(RENT) & RENT != 0,]
big_general_info[,area_avg := as.integer(median(RENT_AREA,na.rm = TRUE)),by = c("MALL_NAME")]
big_general_info[is.na(RENT_AREA),RENT_AREA := area_avg]
# big_general_info[price == 0, price := rent/RENT_AREA]
big_general_info = as.data.frame(big_general_info)

# some special cases
big_general_info[big_general_info$MALL_NAME=="上海汶水商场" & big_general_info$DATE_ID == 201409,"XINYETAI_NUM"] = 0.5
big_general_info[big_general_info$MALL_NAME=="杭州古墩商场" & big_general_info$DATE_ID == 201410,"XINYETAI_NUM"] = 6/31

big_general_info = big_general_info[,!(names(big_general_info) %in% c("area_avg","GROUP_NUM"))]
big_general_info = data.table(big_general_info)
big_general_info[,RENT := c(RENT[-1:-3],rep(NA,3)),by = "MALL_NAME"]
big_general_info = as.data.frame(big_general_info)
big_general_info$MALL_NAME =  str_replace(big_general_info$MALL_NAME,"北京至尊Mall","北京东四环商场")

train_df = big_general_info[!(big_general_info$DATE_ID %in% 201801:201803),]
# dest_df = big_general_info[big_general_info$DATE_ID %in% c(201801,201802,201803),]
test_df = big_general_info[big_general_info$DATE_ID %in% c(201801,201802,201803),]

train_df = train_df[complete.cases(train_df),]
# dest_df = dest_df[complete.cases(dest_df[,!(names(dest_df) %in% "RENT")]),]
test_df = test_df[complete.cases(test_df),]

test_mall_names = test_df[,"MALL_NAME"]
train_rent = train_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID","OPEN_DATE","PRICE","YEAR"))]
# dest_rent = dest_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID","OPEN_DATE","PRICE","YEAR"))]
test_rent = test_df[,!(names(big_general_info) %in% c("MALL_NAME","MALL_CODE","DATE_ID","OPEN_DATE","PRICE","YEAR"))]

library(readxl)
rent_data_1219 = read_xlsx("~/data/rent_data/new_rent_data.xlsx")
#modify the original data for predict(put rent data 3 month ahead), remove col not used for this model
getModelData = function(rent_data_1219){
  rent_data_1219 = data.table(rent_data_1219)
  rent_data_1219[,rent := c(finalprice[-1:-3],rep(NA,3)),by = "mall_name"]
  assign('rent_data_1219',rent_data_1219,envir = .GlobalEnv)
  colpicked = !(colnames(rent_data_1219)%in%c("year","open_date","finalprice","key1"))
  rent_data_1219 = data.frame(rent_data_1219)
  rent_data_1219 = rent_data_1219[,colpicked]
  return(rent_data_1219)
}

rentModelData = getModelData(rent_data_1219)

getyearModeData = function(){
  rent_data_1219[,.SD[1:(.N-12),],by = "mall_name"]
  rent_data_1219[,seq := 1:.N,by = "mall_name"]
  rent_data_1219[,getYearPara(date_id,min),by = "mall_name"]
  # View(rent_data_1219[,.(startmon = getYearPara(date_id,min)),by = "mall_name"])
  sum_col = c("finalprice","rent_area_wa","customer_num","sale","finalprice_jiaju","area_jiaju",
              "jiaju_num","finalprice_jiancai","area_jiancai","jiancai_num","finalprice_ruanzhuang",
              "area_ruanzhuang","ruanzhuang_num","final_jinkou","area_jinkou","jinkou_num","finalprice_xinyetai",
              "area_xinyetai","xinyetai_num","brand_num","gdp","population","region_area","density")
  avg_col = c("avg_gdp","avg_salary","highway_distance","road_distance","location",             
              "subway_distance","shangquan_distance","shangquan_num")
  rent_data_year = rent_data_1219[,c(lapply(.SD[,sum_col,with=FALSE],getYearPara,sum),lapply(.SD[,avg_col,with=FALSE],getYearPara,mean),lapply(.SD[,"date_id",with = FALSE],getYearPara,max),"predprice"=lapply(.SD[,"finalprice",with = FALSE],getYearReal,sum)),by = "mall_name"]
  # setnames(rent_data_year,"predprice.finalprice","predprice")
  rent_data_year = rent_data_year[,predprice:=c(predprice.finalprice[1:(.N-12)],rep(NA,12)),by = "mall_name"]
  rent_data_year$predprice.finalprice = NULL
  setnames(rent_data_year,"predprice","rent")
  # base_rent = rent_data_year[,!(names(rent_data_year)%in%c("mall_name","date_id"))]
  dest_rent = rent_data_year[date_id == 201711,]
  rest_rent = rent_data_year[!is.na(rent),]
  test_rent = rest_rent[,.SD[.N,],by = "mall_name"]
  train_rent = rest_rent[,.SD[1:(.N-1),],by = "mall_name"]
  test_mall_names = test_rent$mall_name
  dest_mall_names = dest_rent$mall_name
  train_rent = train_rent[,!(names(train_rent)%in%c("mall_name","date_id")),with = FALSE]
  test_rent = test_rent[,!(names(test_rent)%in%c("mall_name","date_id")),with = FALSE]
  dest_rent = dest_rent[,!(names(dest_rent)%in%c("mall_name","date_id")),with = FALSE]
  train_rent = data.frame(train_rent)
  test_rent = data.frame(test_rent)
  dest_rent = data.frame(dest_rent)
}

#seperate the data into train,test and dest set, along with the name of the malls returned
getTrainTestData = function(big_general_info,test_time_index = 201706:201708,dest_time_index = 201709:201711){
  train_df = big_general_info[!(big_general_info$date_id %in% c(test_time_index,dest_time_index)),]
  dest_df = big_general_info[big_general_info$date_id %in% dest_time_index,]
  test_df = big_general_info[big_general_info$date_id %in% test_time_index,]
  
  train_df = train_df[complete.cases(train_df),]
  dest_df = dest_df[complete.cases(dest_df[,!(names(dest_df) %in% "rent")]),]
  test_df = test_df[complete.cases(test_df),]
  
  test_mall_names = test_df[,"mall_name"]
  train_rent = train_df[,!(names(big_general_info) %in% c("mall_name","mall_code","date_id"))]
  dest_rent = dest_df[,!(names(big_general_info) %in% c("mall_name","mall_code","date_id"))]
  test_rent = test_df[,!(names(big_general_info) %in% c("mall_name","mall_code","date_id"))]
  return(list("train_rent"=train_rent,"test_rent"=test_rent,"dest_rent"=dest_rent,"test_mall_names"=test_mall_names))
}

rentModelDataList = getTrainTestData(rentModelData)
train_rent_1219 = rentModelDataList$train_rent
test_rent_1219 = rentModelDataList$test_rent
dest_rent_1219 = rentModelDataList$dest_rent
test_mall_names_1219 = rentModelDataList$test_mall_names

rentModelCor = cor(rbindlist(list(train_rent_1219,test_rent_1219)))
View(rentModelCor)
library(caret)
rentModelHighlyCorrelated <- findCorrelation(rentModelCor, cutoff=0.5,names = TRUE)
print(rentModelHighlyCorrelated)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
rentModel <- train(rent~., data=train_rent_1219, method="neuralnet", preProcess="scale", trControl=control,
                   importance = T)
rentModel = gbm(rent ~ . ,data = train_rent_1219,distribution = "gaussian",n.trees = 100000,interaction.depth = 4)

