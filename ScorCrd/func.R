#auc_value(data_train_woe$flag ,data_train_pred)
auc_value <- function(target, prob) {
  prob.rank = rank(prob)
  if(!is.numeric(target)){
    target = as.numeric(as.character(target))
  }
  cnt_1 = sum(target)
  cnt_0 = length(target) - cnt_1
  prob_1 = prob.rank[target == 1]
  u = sum(prob_1) - cnt_1 * (cnt_1 + 1) / 2
  auc = exp(log(u) - log(cnt_1) - log(cnt_0))
  return(auc)
}

#ks_value(data_train_woe$flag ,data_train_pred)
ks_value <- function(target, prob) {
  if(!is.numeric(target)){
    target = as.numeric(as.character(target))
  }
  sum_prob = as.data.frame(table(prob,target))
  sum_prob = as.data.table(sum_prob)
  sum_prob = data.table :: dcast(sum_prob, prob ~ target, value.var = "Freq")
  sum_prob[is.na(sum_prob)] = 0
  sum_prob = data.frame(unclass(sum_prob))
  cum_sum_1 = (cumsum(sum_prob$X1) / sum(sum_prob$X1))
  cum_sum_0 = (cumsum(sum_prob$X0) / sum(sum_prob$X0))
  KS = max(abs(cum_sum_1 - cum_sum_0), na.rm = TRUE)
  return(KS)
}

# lift_value(data_train_woe$flag ,data_train_pred)
lift_value <- function(target, prob) {
  if(!is.numeric(target)){
    target = as.numeric(as.character(target))
  }
  t_prob = data.frame(prob,target)
  t_prob = subset(t_prob, !is.na(prob))
  breaks = cut_equal(prob, g=10)
  prob_bins = split_bins(dat=t_prob,x="prob",breaks)
  sum_prob = as.data.frame(table(prob_bins,target = t_prob$target))
  sum_prob = as.data.table(sum_prob)
  sum_prob = data.table :: dcast(sum_prob, prob_bins ~ target, value.var = "Freq")
  sum_prob[is.na(sum_prob)] = 0
  sum_prob = data.frame(unclass(sum_prob))
  sum_lift = sum_prob[order(sum_prob$prob, decreasing = TRUE),]
  Lift = (cumsum(sum_lift$X1) / ifelse(sum_lift$X0 + sum_lift$X1 > 0 ,cumsum(sum_lift$X0 + sum_lift$X1),1)) /(sum(sum_lift$X1,na.rm = TRUE) / sum(sum_lift$X0 + sum_lift$X1, na.rm = TRUE))
  MAX_Lift = mean(Lift, na.rm = TRUE)
  return(MAX_Lift)
}

# roc_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR", gtitle = "UCICreditCard")
# ks_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR", gtitle = "UCICreditCard")
# lift_plot(train_pred = dat_train, test_pred = dat_test, target = "target", score = "pred_LR", gtitle = "UCICreditCard")
# lift_plot(train_pred = dat_test, test_pred = NULL, target = "target", score = "pred_LR", gtitle = "UCICreditCard TEST")
