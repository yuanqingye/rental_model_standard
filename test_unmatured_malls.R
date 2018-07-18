# FILE_LOCATION = "~/data/rental_raw_data.csv"
FILE_LOCATION = "~/data/rent_data_0622_edit.csv"

#train model with unmatured malls involved and using the old data
#origin rent_data_year with only matured malls is stored in rent_data_year_copy
#using as much data as possible

#check result for 42 malls which has at least 27 records
#dest_date = 201803 is trying to use information as much as possible for clustering
FILE_LOCATION = "~/data/rental_raw_data.csv"
rent_data_year = getyearModeData(dest_date = 201803)
test_include_unmature = list()
result_include_unmature = list()
test_include_unmature[[4]] = cluster_then_train(4,201612,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]][,-ncol(rent_data_year[[6]])]))
result_include_unmature[[4]] = make_2017_comparison(test_include_unmature[[4]])

#IDEA:using 3 months to get a year's rent
#using information as much as possible for clustering
FILE_LOCATION = "~/data/rental_raw_data.csv"
rent_data_year = getyearModeData(dest_date = 201803)
# test_include_unmature = list()
# result_include_unmature = list()
test_include_unmature[[8]] = cluster_then_train(4,201612,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]][,-ncol(rent_data_year[[6]])]),timespan = 3,newtimespan = 12)
result_include_unmature[[8]] = make_2017_comparison(test_include_unmature[[8]])
four_cluster = cluster_the_set(4,cbind(rent_data_year[[3]],rent_data_year[[6]]))

#using fewdata = TRUE to control dest_set = test_set
fewdata = TRUE
FILE_LOCATION = "~/data/rental_raw_data.csv"
rent_data_year = getyearModeData(dest_date = 201803)
four_cluster = cluster_the_set(4,cbind(rent_data_year[[3]],rent_data_year[[6]]))
test_include_unmature[[12]] = cluster_then_train(4,201612,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
result_include_unmature[[12]] = make_2017_comparison(test_include_unmature[[12]])

#using new data to train the model
fewdata = TRUE
FILE_LOCATION = "~/data/rent_data_0622_edit.csv"
test_include_unmature_new = list()
result_include_unmature_new = list()
rent_data_year_new = getyearModeData(dest_date = 201803)
four_cluster_new = cluster_the_set(4,cbind(rent_data_year_new[[3]],rent_data_year_new[[6]]))
test_include_unmature_new[[4]] = cluster_then_train(4,201612,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
result_include_unmature_new[[4]] = make_2017_comparison(test_include_unmature_new[[4]])
View(merge(four_cluster_new,result_include_unmature_new[[4]],by = "mall_name"))

#using the model to predict
fewdata = FALSE
FILE_LOCATION = "~/data/rent_data_0622_edit.csv"
# test_include_unmature_new = list()
# result_include_unmature_new = list()
rent_data_year_new = getyearModeData(dest_date = 201712)
four_cluster_new = cluster_the_set(4,cbind(mall_name = rent_data_year_new[[3]],rent_data_year_new[[6]]))
test_include_unmature_new[[8]] = cluster_then_train(4,201712,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
result_include_unmature_new[[8]] = make_2018_comparison(test_include_unmature_new[[8]])
View(merge(four_cluster_new,result_include_unmature_new[[8]],by = "mall_name")[,c("mall_name","mall_category","rate1","rate2","rate3","eval_rate","target_rate")])
temp[[8]] = merge(four_cluster_new,result_include_unmature_new[[8]],by = "mall_name")[,c("mall_name","mall_category","rate3","eval_rate","target_rate")]
check_predict_effect(temp[[8]])
View(merge(temp[[8]],result_include_unmature_new[[4]][,c("mall_name","err3")],by = "mall_name"))     

test_include_unmature_new[[6]] = cluster_then_train(3,201712,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
result_include_unmature_new[[6]] = make_2018_comparison(test_include_unmature_new[[6]])
three_cluster_new = cluster_the_set(3,cbind(mall_name = rent_data_year_new[[3]],rent_data_year_new[[6]]))
View(merge(three_cluster_new,result_include_unmature_new[[6]],by = "mall_name")[,c("mall_name","mall_category","rate1","rate2","rate3","eval_rate","target_rate")])
temp[[6]] = merge(three_cluster_new,result_include_unmature_new[[6]],by = "mall_name")[,c("mall_name","mall_category","rate1","rate2","rate3","eval_rate","target_rate")]
check_predict_effect(temp[[6]])

test_include_unmature_new[[9]] = cluster_then_train(3,201712,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]))
result_include_unmature_new[[9]] = make_2018_comparison(test_include_unmature_new[[9]])

#verification: using the latest data to verify the effectivness of the model using data to predict rent between 201704 and 201803 
test_include_unmature_new[[12]] = cluster_then_train(4,201703,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
# result_include_unmature_new[[9]] = make_2018_comparison(test_include_unmature_new[[9]])

test_include_unmature_new[[5]] = cluster_then_train(5,201712,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
result_include_unmature_new[[5]] = make_2018_comparison(test_include_unmature_new[[5]])
test_include_unmature[[5]] = cluster_then_train(5,201612,cluster_set = cbind(rent_data_year[[3]],rent_data_year[[6]]),timespan = 3,newtimespan = 12)
result_include_unmature[[5]] = make_2017_comparison(test_include_unmature[[5]])
five_cluster_new = cluster_the_set(5,cbind(mall_name = rent_data_year_new[[3]],rent_data_year_new[[6]]))
temp[[5]] = merge(five_cluster_new,result_include_unmature_new[[5]],by = "mall_name")[,c("mall_name","mall_category","rate1","rate2","rate3","eval_rate","target_rate")]
check_predict_effect(temp[[5]])
View(merge(temp,result_include_unmature[[5]][,c("mall_name","err3")],by = "mall_name"))
# View(merge(five_cluster_new,result_include_unmature_new[[5]],by = "mall_name")[,c("mall_name","mall_category","rate1","rate2","rate3","eval_rate","target_rate")])

FILE_LOCATION = "~/data/rent_data_up_to_201806_edit.xlsx"
fewdata = TRUE
cluster_model_list = list()
cluster_result_list = list()
rent_data_month_2018_06 = read_xlsx(FILE_LOCATION)
rent_data_year_2018_06 = getyearModeData(dest_date = 201806)
temp = cluster_then_train(3,201712,cluster_set = cbind(rent_data_year_2018_06[[3]],rent_data_year_2018_06[[6]]),timespan = 3,newtimespan = 12)
cluster_model_list[["201806"]][["pred"]][[3]] = temp