setwd("~/R_Projects/rental_model_standard")
compare_rent_data_raw = read_xlsx("./data/2017pred_summary.xlsx")
compare_2017_rent_data = compare_rent_data_raw[,c(1,3:10)]
compare_2017_rent_data = data.table(compare_2017_rent_data)
dest_env$result_2017_year_notadjacent = calModelResultOnTimespan(1,201601:201612,FALSE,12)
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

#Need to check which method is more sensitive to time change
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

# temp_result_filter = list()
# temp_result = list()
#old test, no cluster, with no detailed community data
#the default dataset:rent_data_year with 42 matural cities no detailed community data
temp_result_filter[[1]] = seperate_then_cluster(1)
temp_result[[1]] = make_2017_comparison(temp_result_filter[[1]])
rent_data_month_with_new_community_info = rent_data_month_raw[,-(33:41)]
mallnames = unique(rent_data_month_with_new_community_info$MALL_NAME)

redstar_location_copy = redstar_location
redstar_location_copy$mall_name[3] = mallnames[14]
redstar_location_copy$mall_name[39] = mallnames[54]
#because original name is a little different, so I need to exchange the name
temp = calCommunityInfonew(redstar_location_copy)
# temp$mall_name[2] = "北京至尊Mall"
# temp$mall_name[40] = "沈阳欧丽洛雅商场"
rent_data_month_with_new_community_info = merge(rent_data_month_with_new_community_info,temp,by.x = "MALL_NAME",by.y = "mall_name",all.x = TRUE)
# rent_data_month_with_new_community_info = rent_data_month_with_new_community_info[MALL_NAME %in% un_mature_mall,]
# write.xlsx(rent_data_month_with_new_community_info,"~/data/rental_raw_data_new_community.xlsx")
write.csv(rent_data_month_with_new_community_info,"~/data/rental_raw_data_new_community.csv")
#new community data on 42 cities
rent_data_year_new = getyearModeData(12,201612,mall_filter = temp_result[[1]]$mall_name,file_location = "~/data/rental_raw_data_new_community.csv")
temp_result_filter[["newcommunitydata"]] = seperate_then_cluster(1,cluster_set = cbind(rent_data_year_new[[3]], rent_data_year_new[[6]]),file_location = "~/data/rental_raw_data_new_community.csv")
temp_result[["newcommunitydata"]] = make_2017_comparison(temp_result_filter[["newcommunitydata"]])

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
pred_result_filter[[4]] = seperate_then_cluster(4,201612)
pred_result[[4]] = make_2017_comparison(pred_result_filter[[4]])

pred_result_filter[[8]] = seperate_then_cluster(4,201712)
pred_result[[8]] = make_2018_comparison(pred_result_filter[[8]])

#this result come from model with DATE_ID variable for 2017
pred_result_filter[[12]] = seperate_then_cluster(4,201612)
pred_result[[12]] = make_2017_comparison(pred_result_filter[[12]])

#this result come from model with DATE_ID variable for 2018
pred_result_filter[[16]] = seperate_then_cluster(4,201712)
pred_result[[16]] = make_2018_comparison(pred_result_filter[[16]])

pred_result_filter_no_adjacent = list()
pred_result_no_adjacent = list()
# 4,8 is normal case, without the DATE_ID, 12,16 with the DATE_ID 
pred_result_filter_no_adjacent[[4]] = seperate_then_cluster(4,201601:201612,FALSE)
pred_result_filter_no_adjacent[[4]] = lapply(lapply(pred_result_filter_no_adjacent[[4]],`[[`,1),`[`,,.(pred_rent = sum(pred_rent)),by = mall_name)
pred_result_no_adjacent[[4]] = make_2017_comparison(pred_result_filter_no_adjacent[[4]])

pred_result_filter_no_adjacent[[8]] = seperate_then_cluster(4,201701:201712,FALSE)
pred_result_filter_no_adjacent[[8]] = lapply(lapply(pred_result_filter_no_adjacent[[8]],`[[`,1),`[`,,.(pred_rent = sum(pred_rent)),by = mall_name)
pred_result_no_adjacent[[8]] = make_2018_comparison(pred_result_filter_no_adjacent[[8]])

pred_result_filter_no_adjacent[[12]] = seperate_then_cluster(4,201601:201612,FALSE)
pred_result_filter_no_adjacent[[12]] = lapply(lapply(pred_result_filter_no_adjacent[[12]],`[[`,1),`[`,,.(pred_rent = sum(pred_rent)),by = mall_name)
pred_result_no_adjacent[[12]] = make_2017_comparison(pred_result_filter_no_adjacent[[12]])

pred_result_filter_no_adjacent[[16]] = seperate_then_cluster(4,201701:201712,FALSE)
pred_result_filter_no_adjacent[[16]] = lapply(lapply(pred_result_filter_no_adjacent[[16]],`[[`,1),`[`,,.(pred_rent = sum(pred_rent)),by = mall_name)
pred_result_no_adjacent[[16]] = make_2018_comparison(pred_result_filter_no_adjacent[[16]])

# 在不相邻的情况下,时间变量影响小,且整体不上升
# 在相邻情况下,时间变量影响大,且整体上升


#tune the parameter importance
train_rent = getyearModeData()[[4]]
library(caret)
library(doMC)
registerDoMC(cores = 4)

test_importance_data[["adjacent"]] = getyearModeData()
test_importance_data[["notadjacent"]] = getyearModeData(12,201701:201712,TRUE,12,1)

ptm <- proc.time()
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
# train_rent = test_importance_data[["adjacent"]][[4]],result:rfFunresults[["adjacent"]]
train_rent = test_importance_data[["adjacent"]][[4]]
rfFunresults[["adjacent"]] <- rfe(train_rent[,1:37], train_rent[,38], sizes=c(1:36), rfeControl=control)
ptm2 = proc.time() - ptm
plot(rfFunresults[["adjacent"]],type=c("g", "o"))
View(rfFunresults[["adjacent"]]$variables)

train_rent = test_importance_data[["notadjacent"]][[4]]
rfFunresults[["notadjacent"]] <- rfe(train_rent[,1:37], train_rent[,38], sizes=c(1:36), rfeControl=control)
View(rfFunresults[["notadjacent"]]$variables)

train_rent = rent_data_year_new[[4]]
rfFunresults[["newcommunity"]] <- rfe(train_rent[,1:40], train_rent[,41], sizes=c(1:39), rfeControl=control)
View(rfFunresults[["newcommunity"]]$variables)

# define the control specify linear model function
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
lmFunresults <- rfe(train_rent[,1:36], train_rent[,37], sizes=c(1:36), rfeControl=control)
plot(lmFunresults,type=c("g", "o"))

# rent_year_data[[4]]
# rent.boost = list()
train_rent = test_importance_data[["adjacent"]][[4]]
train_rent = test_importance_data[["notadjacent"]][[4]]
train_rent = rent_data_year_new[[4]]
ptm <- proc.time()
library(gbm)
rent.boost[["newcommunity"]] = gbm(rent ~ .-current_rent ,data = train_rent,distribution = "gaussian",n.trees = 100000,interaction.depth = 4)
# rent.boost
para_rank1 = summary(rent.boost[["newcommunity"]]) #Summary gives a table of Variable Importance and a plot of Variable Importance
ptm3 = proc.time() - ptm


train_rent = test_importance_data[["adjacent"]][[4]]
train_rent = test_importance_data[["notadjacent"]][[4]]
train_rent = rent_data_year_new[[4]]
# ensure results are repeatable
set.seed(7)
# load the library
library(caret)
# prepare training scheme_
control <- trainControl(method="repeatedcv", number=10, repeats=3)
ptm = proc.time()
# train the model
rf_model[["newcommunity"]] <- train(rent~., data=train_rent, method="rf", preProcess="scale", trControl=control,
                    importance = T)
# estimate variable importance
importance <- varImp(rf_model[["newcommunity"]], scale=FALSE)
ptm4 = proc.time() - ptm
# summarize importance
print(importance)
# plot importance
plot(importance)

rent_data_0622 = read_xlsx("~/data/rent_data_0622.xlsx")
#using model stored in cross_result to see the result of new store
#clue:First kmeans the un matured mall together with other malls, then do the model
# for infant malls, using original model and knn to seperate it, then do the model
#seperate the training part and test/predict part