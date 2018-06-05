setwd("~/R_Projects/rental_model_standard")
compare_rent_data_raw = read_xlsx("./data/2017pred_summary.xlsx")
compare_2017_rent_data = compare_rent_data_raw[,c(1,3:10)]
compare_2017_rent_data = data.table(compare_2017_rent_data)
dest_env$temp_2017_result = dest_env$result_2017_year_notadjacent[[1]][,.(pred_rent_2017 = sum(pred_rent)),by = mall_name]
dest_env$temp_2017_result = data.table(dest_env$temp_2017_result)
dest_env$rent_2017_compare = setDT(compare_2017_rent_data)[dest_env$temp_2017_result,on = "mall_name"]
setnames(dest_env$rent_2017_compare,"pred_rent_2017","17_pred3")
dest_env$rent_2017_compare[,err3:=(`17_pred3`-`17_real_rent`)/`17_real_rent`]
setcolorder(dest_env$rent_2017_compare,c(1:8,ncol(dest_env$rent_2017_compare),9:(ncol(dest_env$rent_2017_compare)-1)))

View(dest_env$rent_2017_compare[abs(err2)>0.05 & abs(err2)<1])
#苏州园区，天津东丽，成都佳灵

View(cbind(un_mature_mall,apply(test_env$cross_result_2017_halfyear_adjacent[[2]][mall_name %in% un_mature_mall,c(-1:-4)],1,function(x){min(abs(x))}),apply(test_env$cross_result_2017_halfyear_notadjacent[[2]][mall_name %in% un_mature_mall,c(-1:-4)],1,function(x){min(abs(x))})))
View(cbind(test_env$cross_result_2017_halfyear_adjacent[[2]][,1],apply(test_env$cross_result_2017_halfyear_adjacent[[2]][,c(-1:-4)],1,function(x){min(abs(x))}),apply(test_env$cross_result_2017_halfyear_notadjacent[[2]][,c(-1:-4)],1,function(x){min(abs(x))})))


mall_filter = rent_data_month[,.(record_num =.N),by = "MALL_NAME"][record_num>26,]$MALL_NAME
dest_env$result_2017_year_adjacent_filter = calModelResultOnTimespan(12,201612,TRUE,12,1,mall_filter)
setwd("~/R_Projects/rental_model_standard")
compare_rent_data_raw = read_xlsx("./data/2017pred_summary.xlsx")
compare_2017_rent_data = compare_rent_data_raw[,c(1,3:10)]
compare_2017_rent_data = data.table(compare_2017_rent_data)
dest_env$temp_2017_filter_result = dest_env$result_2017_year_adjacent_filter[[1]][,.(pred_rent_2017 = sum(pred_rent)),by = mall_name]
dest_env$temp_2017_filter_result = data.table(dest_env$temp_2017_filter_result)
dest_env$rent_2017_filter_compare = setDT(compare_2017_rent_data)[dest_env$temp_2017_filter_result,on = "mall_name"]
setnames(dest_env$rent_2017_filter_compare,"pred_rent_2017","17_pred3")
dest_env$rent_2017_filter_compare[,err3:=(`17_pred3`-`17_real_rent`)/`17_real_rent`]
setcolorder(dest_env$rent_2017_filter_compare,c(1:8,ncol(dest_env$rent_2017_filter_compare),9:(ncol(dest_env$rent_2017_filter_compare)-1)))

View(dest_env$rent_2017_filter_compare[abs(err2)>0.05 & abs(err2)<1])
#in this model: 无锡锡山，北京至尊mall,成都佳灵商场，宜昌西陵商场，沈阳大东


cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]])
mall_cluster = kmeans(cluster_set[,-1],2,nstart = 20)
mall_cluster$cluster
kmeans_cluster_result = cbind(mall_name = rent_data_year[[3]],mall_category = mall_cluster$cluster)

kmeans_cluster_result = data.table(kmeans_cluster_result)
mall_first_part = kmeans_cluster_result[mall_category == 1,]$mall_name
mall_second_part = kmeans_cluster_result[mall_category == 2,]$mall_name

dest_env$result_2017_year_adjacent_filter1 = calModelResultOnTimespan(12,201612,TRUE,12,1,mall_first_part)
dest_env$result_2017_year_adjacent_filter2 = calModelResultOnTimespan(12,201612,TRUE,12,1,mall_second_part)

setwd("~/R_Projects/rental_model_standard")
compare_rent_data_raw = read_xlsx("./data/2017pred_summary.xlsx")
compare_2017_rent_data = compare_rent_data_raw[,c(1,3:10)]
compare_2017_rent_data = data.table(compare_2017_rent_data)
dest_env$result_2017_year_adjacent_filter_mixed = rbind(dest_env$result_2017_year_adjacent_filter1[[1]][,.(pred_rent_2017 = sum(pred_rent)),by = mall_name],dest_env$result_2017_year_adjacent_filter2[[1]][,.(pred_rent_2017 = sum(pred_rent)),by = mall_name])
dest_env$result_2017_year_adjacent_filter_mixed = data.table(dest_env$result_2017_year_adjacent_filter_mixed)
dest_env$rent_2017_filter_compare_mixed = setDT(compare_2017_rent_data)[dest_env$result_2017_year_adjacent_filter_mixed,on = "mall_name"]
setnames(dest_env$rent_2017_filter_compare_mixed,"pred_rent_2017","17_pred3")
dest_env$rent_2017_filter_compare_mixed[,err3:=(`17_pred3`-`17_real_rent`)/`17_real_rent`]
setcolorder(dest_env$rent_2017_filter_compare_mixed,c(1:8,ncol(dest_env$rent_2017_filter_compare_mixed),9:(ncol(dest_env$rent_2017_filter_compare_mixed)-1)))

View(dest_env$rent_2017_filter_compare_mixed[abs(err2)>0.05 & abs(err2)<1])
#in this model: 苏州园区，北京西四环，北京北五环，天津河东，天津东丽，成都佳灵，沈阳大东
#nearly: 大同东信商场，大庆世博商场，赤峰新城商场

temp_result_filter = list()
# temp_result = list()
temp_result_filter[[3]] = seperate_then_cluster(3)
temp_result[[3]] = make_2017_comparison(temp_result_filter[[3]])

temp_result_filter[[4]] = seperate_then_cluster(4)
temp_result[[4]] = make_2017_comparison(temp_result_filter[[4]])

temp_result_filter[[8]] = seperate_then_cluster(4)
temp_result[[8]] = make_2017_comparison(temp_result_filter[[8]])
# temp_result[[5]] = seperate_then_cluster(5)

View(result_compare[abs(err2)>0.05 & abs(err2)<1])
#in this model: 苏州园区，北京西四环，北京北五环，天津河东，天津东丽，成都佳灵，沈阳大东
#nearly: 大同东信商场，大庆世博商场，赤峰新城商场

pred_result = list()
pred_result_filter = list()
pred_result_filter[[4]] = seperate_then_cluster(4,201712)
pred_result[[4]] = make_2018_comparison(pred_result_filter[[4]])

pred_result_filter[[8]] = seperate_then_cluster(4,201712)
pred_result[[8]] = make_2018_comparison(pred_result_filter[[8]])

pred_result_filter_no_adjacent = list()
pred_result_no_adjacent = list()
pred_result_filter_no_adjacent[[4]] = seperate_then_cluster(4,201701:201712,FALSE)
temp = pred_result_filter_no_adjacent[[4]]
pred_result_filter_no_adjacent[[4]] = lapply(lapply(pred_result_filter_no_adjacent[[4]],`[[`,1),`[`,,.(pred_rent = sum(pred_rent)),by = mall_name)
pred_result_no_adjacent[[4]] = make_2018_comparison(pred_result_filter_no_adjacent[[4]])

temp_result_filter_no_adjacent = list()
temp_result_no_adjacent = list()
temp_result_filter_no_adjacent[[4]] = seperate_then_cluster(4,201601:201612,FALSE)
# temp = temp_result_filter_no_adjacent[[4]]
backup = temp_result_filter_no_adjacent[[4]]
temp_result_filter_no_adjacent[[4]] = lapply(lapply(temp_result_filter_no_adjacent[[4]],`[[`,1),`[`,,.(pred_rent = sum(pred_rent)),by = mall_name)
temp_result_no_adjacent[[4]] = make_2017_comparison(temp_result_filter_no_adjacent[[4]])

cpi = read_xlsx("~/data/cpi.xlsx")


train_rent = getyearModeData()[[4]]

library(caret)
library(doMC)
registerDoMC(cores = 4)

ptm <- proc.time()
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
rfFunresults <- rfe(train_rent[,1:36], train_rent[,37], sizes=c(1:36), rfeControl=control)
ptm2 = proc.time() - ptm
plot(rfFunresults,type=c("g", "o"))

# define the control using a random forest selection function
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
lmFunresults <- rfe(train_rent[,1:36], train_rent[,37], sizes=c(1:36), rfeControl=control)
plot(lmFunresults,type=c("g", "o"))

rent_year_data[[4]]

ptm <- proc.time()
library(gbm)
rent.boost = gbm(rent ~ .-current_rent ,data = train_rent,distribution = "gaussian",n.trees = 100000,interaction.depth = 4)
rent.boost
para_rank1 = summary(rent.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
ptm3 = proc.time() - ptm

# ensure results are repeatable
set.seed(7)
# load the library
library(caret)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
ptm = proc.time()
# train the model
rf_model <- train(rent~., data=train_rent, method="rf", preProcess="scale", trControl=control,
                    importance = T)
# estimate variable importance
importance <- varImp(rf_model, scale=FALSE)
ptm4 = proc.time() - ptm
# summarize importance
print(importance)
# plot importance
plot(importance)
