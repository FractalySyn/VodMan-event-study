install.packages(readxl); install.packages(tidyverse); install.packages(fBasics)
library(readxl); library(tidyverse); library(fBasics)


# Data --------------------------------------------------------------

chart = function(data, vec1, vec2)
{
  attach(data, warn.conflicts = F)
  matrix = cbind(cumsum(vec1), cumsum(vec2))
  matplot(matrix, type = c("l", "l"), lty = c(1, 1), col = c("red", "blue"))
  legend("topleft", border = "black", col = c("red", "blue"), legend = c(names(data)[1], names(data)[2]), lty = c(1, 1))
  print("correlation =", quote = F)
  cor(vec1, vec2)
}

## Mannesmann
datam = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
chart(datam, mannesmann, dax)

## Vodafone
datav = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
chart(datav, vodafone, footsie)

# Null returns analysis Mannesmann --------------------------------------------------------------

zero_analysis = function(data)
{
  attach(data, warn.conflicts = F)
  vec1 = data[,1]; vec2 = data[,2] ## !!! first and second columns of the dataset must be the firm and the market
  reg = lm(vec1 ~ vec2)
  
  data0 = data[-which(vec1 == 0),]
  vec3 = data0[,1]; vec4 = data0[,2]
  reg0 = lm(vec3 ~ vec4)
  
  data00 = data[which(vec1 == 0),]
  vec5 = data00[,1]; vec6 = data00[,2]
  reg00 = lm(vec5 ~ vec6)
  
  SCR = anova(reg)[2,2]; DF = anova(reg)[2,1]
  scr1 = anova(reg0)[2,2]; df1 = anova(reg0)[2,1]
  scr2 = anova(reg00)[2,2]; df2 = anova(reg00)[2,1]
  
  fisherc = ((SCR - (scr1 + scr2)) / (DF - (df1 + df2))) / ((scr1 + scr2) / (df1 + df2))
  fisher95 = qf(0.95, 2, length(data$dates)-4)
  fisher99 = qf(0.99, 2, length(data$dates)-4)
  result95 = ifelse(fisherc > fisher95, "Heterogeneity", "Homogeneity")
  result99 = ifelse(fisherc > fisher99, "Heterogeneity", "Homogeneity")
  
  list(Model = c(cat(paste("\t\tModel\n")),
                 cat(length(data$dates), "observations on", names(data)[1], "~", names(data)[2], "\n"),
                 cat(sum(vec1 == 0), "null returns, meaning", 100*sum(vec1 == 0)/length(vec1), "percent\n")),
       Chow_Homogeneity_test = dplyr::summarise(data, Fisher_Statistic = fisherc, Fisher_95 = fisher95, CL = result95, 
                                                Fisher_99 = fisher99, CL2 = result99))
  
  
}


## Mannesmann

zero_analysis(datam)
# 110 null returns over 362 obs. i.e. ~30%

# constraint model -> mann = a + b*dax + u --- 362 obs
# non constraint model -> mann = a + bi*dax + u, i=1,2 --- 252 obs and 110 obs respectively

# Fisher statistic (calculated)
# -> F = [(SCRc - (SCR1+SCR2)) / (dfc - (df1+df2))] / [(SCR1 + SCR2) / (df1 + df2)]
# k = 2 (alpha, beta)
# follows a fisher law of k and N-2k df
# => empirical fisher value is 4.78 > theorical values
# => we can reject the null hypothesis of homogeneity at more than 99% confidence level

# Paradox
# H0 rejected -> requires a homoskedasticity test for which Ho = are error variances similar ?
# -> Intuitively no because the second dataset has a null variance <=> heteroskedasticity
# -> The test statistic is V1^2/V2^2 = (SCR1/(n1-k))/(SCR2/(n2-k)) that follows a fisher law of n1-k and n2-k df
# In one case we cannot calculate the fisher (0 division) and replacing it with the limit to 0 gives a infinitely big fisher statistic <=> rejection
# In the other case the fisher statistic will be 0 <=> no rejection
# Paradox : we know the data is heteroskedastic, the test gives two responses and the most reliant is contradicting this fact
# Furthermore accepting heteroskedasticity would mean that the Chow test is biased
# This test isn't relevant in this case of null returns

# Conclusion
# Given the large amount of null returns (30%) and the results of the Chow test we conclude that these zeros certainly come from sampling errors


## Vodafone

zero_analysis(datav)
# 2 null returns over 252 obs i.e. less than 1%

# constraint model -> vod = a + b*dax + u --- 251 obs
# non constraint model -> vod = a + bi*dax + u, i=1,2 --- 249 obs and 2 obs respectively

# => empirical fisher value is 0.126 < theorical values
# -> we cannot reject the null hypothesis of homogeneity 

# Conclusion : we keep the whole dataset



# Retained models --------------------------------------------------------

market_model = function(data, vec1 = data[,1], vec2 = data[,2], plot = F)
{
  vec1 = data[,1]; vec2 = data[,2]
  model = vec1 ~ vec2
  if (plot == T) plot(model)
  reg = lm(model)
  if (plot == T) abline(reg, col = "blue")
  list(summary(reg), coefficients(reg), reg)
}

a=market_model(datam, mannesmann, dax)[[2]]
class(a); length(a); 
t(a); a

## CAPM Mannesmann ~ dax 12/05/98 - 15/10/99
datam = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
datam0 = datam[-which(datam$mannesmann == 0),]
market_model(datam0, mannesmann, dax, plot = T)
# -> beta 0.682696 signficant at more than 99.99% confidence level

## CAPM Vodafone ~ footsie 15/10/98 - 15/10/99
datav = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
market_model(datav, vodafone, footsie, plot = T)
# -> beta 1.247092 signficant at more than 99.99% confidence level

# Event window wrangling ------------------------------------------------------------

## events
event1 = c("1999-11-12", "1999-11-15", "1999-11-16") # 1st official offer
event2 = c("1999-11-17", "1999-11-18") # Mannesmann rejects
event3 = c("1999-12-20", "1999-12-21") # 2nd offer
event4 = c("2000-01-10", "2000-01-11", "2000-01-12", "2000-01-13") # Vodafone wants to elevate the offer (non official)
event5 = c("2000-01-14", "2000-01-17") # EU Commission gets notified
event6 = c("2000-01-28", "2000-01-31", "2000-02-01") # threat merger Vod-Vivendi
event7 = c("2000-02-03", "2000-02-04") # Mannesmann accepts
event8 = c("2000-02-22", "2000-02-23") # Commission rejection
event9 = c("2000-02-29", "2000-03-01") # Vodafone notifies again
event10 = c("2000-03-27", "2000-03-28") # Mannesmann accepts
event11 = c("2000-04-12", "2000-04-13") # EU Commission accepts the merger
event_list = list(event1, event2, event3, event4, event5, event6, event7, event8, event9, event10, event11)

##
null_events = function(event_data, events)
{
  null_returns = which(event_data[,1] == 0)
  null_dates = as.character(event_data$dates[null_returns])
  null_index = null_dates %in% events %>% which()
  
  for (j in 1:length(events)) 
  {
    null_index = c(null_index, null_dates %in% events[[j]] %>% which())
  }
  
  ifelse(length(null_dates) == 0, NA, null_dates[null_index])
}

## Mannessmann
ewm = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
null_events(ewm, event_list_mann)
# there's a null return on 10 jan 2000 -> is it an important date ?

## Vodafone
ewv = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
null_events(ewv, event_list)
# there's no null returns for vodafone on the event window


# Chow temporal stability tests -------------------------------------

temporal_stability = function(data1, data2)
{
  firm_t1 = data1[,1]; market_t1 = data1[,2]
  firm_t2 = data2[,1]; market_t2 = data2[,2]
  firm_T = c(data1[,1], data2[,1]); market_T = c(data1[,2], data2[,2])
  
  reg_t1 = lm(firm_t1 ~ market_t1)
  reg_t2 = lm(firm_t2 ~ market_t2)
  reg_T = lm(firm_T ~ market_T)
  
  scr1 = anova(reg_t1)[2,2]; df1 = anova(reg_t1)[2,1]
  scr2 = anova(reg_t2)[2,2]; df2 = anova(reg_t2)[2,1]
  SCR =  anova(reg_T)[2,2]; DF = anova(reg_T)[2,1]
  
  fisherc = ((SCR - (scr1 + scr2)) / (DF - (df1 + df2))) / ((scr1 + scr2) / (df1 + df2))
  fisher95 = qf(0.95, 2, length(firm_T)-4)
  fisher99 = qf(0.99, 2, length(firm_T)-4)
  result95 = ifelse(fisherc > fisher95, "T instability", "T stability")
  result99 = ifelse(fisherc > fisher99, "T instability", "T stability")
  
  plot(firm_T ~ market_T, xlab = names(data1)[2], ylab = names(data1)[1])
  list(Chow_temp_stability_test = dplyr::summarise(data1, Fisher_Statistic = fisherc, Fisher_95 = fisher95,
                                                   CL = result95, Fisher_99 = fisher99, CL2 = result99))
}

## Mannesmann
ewm = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
ewm0 = ewm[-which(ewm$mannesmann == 0),]
datam = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
datam0 = datam[-which(datam$mannesmann == 0),]

# Chow test
# Fisher statistic (calculated) : F = [(SCRc - (SCR1+SCR2)) / (dfc - (df1+df2))] / [(SCR1 + SCR2) / (df1 + df2)], k = 2 (alpha, beta)
# follows a fisher law of k and T-2k df
temporal_stability(datam0, ewm0)
# test statistic is << than theoretical fishers -> means that the merger and the EU Commission intervention didn't much impact the market
# to be verified with ARs because the beta on event window isn't significant -> the market index (dax) didn't explain mannesmann returns


## Vodafone
ewv = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
datav = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()

# Chow test
# Fisher statistic (calculated) : F = [(SCRc - (SCR1+SCR2)) / (dfc - (df1+df2))] / [(SCR1 + SCR2) / (df1 + df2)], k = 2 (alpha, beta)
# follows a fisher law of k and T-2k df
temporal_stability(datav, ewv)
# test statistic is << than theoretical fishers -> means that the merger and the EU Commission intervention didn't much impact the market
# to be verified with ARs because the beta on event window isn't significant -> the market index (dax) didn't explain mannesmann returns




# Errors analysis Mannesmann --------------------------------------------------------

## function
error_analysis = function(data, vec1, vec2)
{
  attach(data)
  model = vec1 ~ vec2
  reg = lm(model); cf = coef(reg)
  error = data.frame(res = vec1 - (cf[1] + cf[2] * vec2))
  plot(density(error$res))
  JBtest = fBasics::jarqueberaTest(error$res)
  dplyr::summarise(error, mean = mean(res), stdev = sd(res), skewness = skewness(res), excess_kurtosis = kurtosis(res), 
                   JB_statistic = as.numeric(JBtest@test$statistic), JB_pvalue = as.numeric(JBtest@test$p.value))
}

## Mannesmann
# Estimation Window 
datam = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
datam0 = datam[-which(datam$mannesmann == 0),]
error_analysis(datam0, mannesmann, dax)

# Event window
ewm = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
ewm0 = ewm[-which(ewm$mannesmann == 0),]
error_analysis(ewm0, mannesmann, dax)

# Whole data
datam0 = datam[-which(datam$mannesmann == 0),]
ewm0 = ewm[-which(ewm$mannesmann == 0),]
alldatam = as.data.frame(cbind(mannesmann = c(datam0$mannesmann, ewm0$mannesmann), dax = c(datam0$dax, ewm0$dax))); head(alldatam)
error_analysis(alldatam, mannesmann, dax)


## Vodafone
# Estimation Window
datav = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
error_analysis(datav, vodafone, footsie)

# Event window
ewv = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
error_analysis(ewv, vodafone, footsie)

# Whole data
alldatav = as.data.frame(cbind(vodafone = c(datav$vodafone, ewv$vodafone), footsie = c(datav$footsie, ewv$footsie)))
error_analysis(alldatav, vodafon, footsie)



# Event study -------------------------------------------------------------


## Final event list
event1 = c("1999-11-12", "1999-11-15", "1999-11-16") # 1st official offer
event2 = c("1999-11-17", "1999-11-18") # Mannesmann rejects
event3 = c("1999-12-20", "1999-12-21") # 2nd offer
event4 = c("2000-01-10", "2000-01-11", "2000-01-12", "2000-01-13") # Vodafone wants to elevate the offer (non official)
event5 = c("2000-01-14", "2000-01-17") # EU Commission gets notified
event6 = c("2000-01-28", "2000-01-31", "2000-02-01") # threat merger Vod-Vivendi
event7 = c("2000-02-03", "2000-02-04") # Mannesmann accepts
event8 = c("2000-02-22", "2000-02-23") # Commission rejection
event9 = c("2000-02-29", "2000-03-01") # Vodafone notifies again
event10 = c("2000-03-27", "2000-03-28") # Mannesmann accepts
event11 = c("2000-04-12", "2000-04-13") # EU Commission accepts the merger
event_list = list(event1, event2, event3, event4, event5, event6, event7, event8, event9, event10, event11)
event_list_mann = list(event1, event2, event3, event4, event5, event6, event7, event8, event9)
events_all = c(event1, event2, event3, event4, event5, event6, event7, event8, event9, event10, event11)



event_study = function(est_data, event_data, events, model = "capm", bootstrap_multiplier = 100)
{
  # find dates in data
  indexes = events
  for (i in 1:length(events)) 
  {
    indexes[[i]] = match(events[[i]], as.character(event_data$dates))
  }
  
  # calculate observed car
  cf = c(0, 0); expected_return = 0; car = 0
  if (model == "capm")
  {
    cf = market_model(est_data, est_data[,1], est_data[,2])[[2]]
    market = event_data[,2]; firm = event_data[,1]
    for (j in 1:length(indexes))
    {
      expected_return[j] = cf[2] * sum(market[indexes[[j]]]) + cf[1] * length(indexes[[j]])
      car[j] = sum(firm[indexes[[j]]]) - expected_return[j]
    }
  }
  if (model == "cmrm")
  {
    firm = event_data[,1]
    for (j in 1:length(indexes))
    {
      expected_return[j] = mean(est_data[,1]) * length(indexes[[j]])
      car[j] = sum(firm[indexes[[j]]]) - expected_return[j]
    }
  }
  if (model == "b-one")
  {
    market = event_data[,2]; firm = event_data[,1]
    for (j in 1:length(indexes))
    {
      expected_return[j] = sum(market[indexes[[j]]])
      car[j] = sum(firm[indexes[[j]]]) - expected_return[j]
    }
  }
  
  # observed test statistics t and J1  
  expectancy_car = c(); var_AR = list(); stdev_car_j1_test = c(); t_statistic = c(); j1_statistic = c()
  if (model == "capm")
  {
    event_regressors = market_model(event_data, event_data[,1], event_data[,2])[[2]]
    est_regressors = market_model(est_data, est_data[,1], est_data[,2])[[2]]
    stdev_car_t_test = market_model(datam0, mannesmann, dax)[[1]]$sigma
    
    for (k in 1:length(indexes))
    {
      Im = diag(rep(1, length(indexes[[k]]))); column = matrix(rep(1, length(indexes[[k]])), length(indexes[[k]]), 1)
      X_star = matrix(rep(event_regressors), length(indexes[[k]]), 2, byrow = T)
      X = matrix(rep(est_regressors), length(indexes[[k]]), 2, byrow = T)
      
      expectancy_car[k] = car[k] 
      var_AR[[k]] = stdev_car_t_test^2 * Im + stdev_car_t_test^2 * (X_star %*% matlib::Ginv(t(X) %*% X) %*% t(X_star))
      stdev_car_j1_test[k] = sqrt(t(column) %*% var_AR[[k]] %*% column)
      
      t_statistic[k] = expectancy_car[k] / (stdev_car_t_test * sqrt(length(expectancy_car[k])))
      j1_statistic[k] = expectancy_car[k] / stdev_car_j1_test[k]
    }
  }
  if (model == "cmrm")
  {
    stdev_car_t_test = sd(est_data[,1] - mean(est_data[,1]))
    
    for (k in 1:length(indexes))
    {
      expectancy_car[k] = car[k]
      t_statistic[k] = expectancy_car[k] / (stdev_car_t_test * sqrt(length(expectancy_car[k])))
      j1_statistic[k] = NA
    }
  }
  if (model == "b-one")
  {
    stdev_car_t_test = sd(est_data[,1] - est_data[,2])
    
    for (k in 1:length(indexes))
    {
      expectancy_car[k] = car[k]
      t_statistic[k] = expectancy_car[k] / (stdev_car_t_test * sqrt(length(expectancy_car[k])))
      j1_statistic[k] = NA  
    }
  }
  
  # classical parametric tests - student distribution of test statistics
  pvalue_classical_t_test = c(); pvalue_classical_j1_test = c()
  for(l in 1:length(indexes))
  {
    pvalue_classical_t_test[l] = 2 * pt(-abs(t_statistic[l]), length(est_data[,1])-2)
    pvalue_classical_j1_test[l] = ifelse(is.na(j1_statistic[l]), NA, 2 * pt(-abs(j1_statistic[l]), length(est_data[,1])-2))
  }
  
  # empirical distribution of ARs
  ARs = c(); random_ar = list()
  if (model == "capm") ARs = market_model(est_data, est_data[,1], est_data[,2])[[1]]$residuals
  if (model == "cmrm") ARs = est_data[,1] - mean(est_data[,1])
  if (model == "b-one") ARs = est_data[,1] - est_data[,2]
  
  avg_ar = c()
  for(m in 1:bootstrap_multiplier)
  {
    avg_ar[m] = sum(sample(ARs, replace = T)) / length(ARs)
  }
  empirical_ar = sample(avg_ar, bootstrap_multiplier*10, replace = T)
  
  # bootstrap Monte Carlo non parametric test - empirical distribution of the ARs
  pvalue_montecarlo_test = c()
  for (n in 1:length(indexes))
  {
    pvalue_montecarlo_test[n] = length(which(abs(empirical_ar) > abs(car[n]))) / length(empirical_ar)
  }
  
  # bootstrap parametric test Marais - empirical distribution of the test statistics
  bs_est_data = list(); bs_ar = c(); bs_t_statistic = c(); cf = list(); bs_firm = c(); bs_market = c(); bs_sd_residuals = c(); bs_j1_statistic = c()
  if (model == "capm")
  {
    bs_est_data = list(); bs_est_regressors = list(); bs_stdev_ar_j1_test = c()
    for (o in 1:bootstrap_multiplier)
    {
      bs_market_and_residuals = cbind(sample(est_data[,2], replace = T), sample(market_model(est_data, est_data[,1], est_data[,2])[[1]]$residuals, replace = T))
      for (p in 1:(length(est_data[,1])))
      {
        cf = market_model(est_data, est_data[,1], est_data[,2])[[2]]
        bs_firm[p] = cf[1] + cf[2]*bs_market_and_residuals[p,1] + bs_market_and_residuals[p,2]
      }
      bs_est_data[[o]] = data.frame(bs_firm, bs_market_and_residuals[,1], bs_market_and_residuals[,2])
      bs_est_regressors[[o]] = market_model(bs_est_data[[o]], bs_est_data[[o]][,1], bs_est_data[[o]][,2])[[2]]
      bs_sd_residuals[o] = market_model(bs_est_data[[o]], bs_est_data[[o]][,1], bs_est_data[[o]][,2])[[1]]$sigma
      
      bs_ar[o] = sum(sample(market_model(bs_est_data[[o]], bs_est_data[[o]][,1], bs_est_data[[o]][,2])[[1]]$residuals, 20, replace = T)) / 20
      bs_X_star = t(event_regressors)
      bs_X = t(bs_est_regressors[[o]])
      bs_var_AR = bs_sd_residuals[o]^2 + bs_sd_residuals[o]^2 * (bs_X_star %*% solve(t(bs_X) %*% bs_X) %*% t(bs_X_star))
      bs_stdev_ar_j1_test[o] = sqrt(bs_var_AR)
      
      bs_t_statistic[o] = bs_ar[o] / bs_sd_residuals[o]
      bs_j1_statistic[o] = bs_ar[o] / bs_stdev_ar_j1_test[o]
    }
  }
  if (model == "cmrm")
  {
    for (o in 1:bootstrap_multiplier)
    {
      for (p in 1:length(est_data[,1]))
      {
        bs_firm[p] = sum(sample(est_data[,1], replace = T)) / length(est_data[,1])
      }
      bs_firm_mean = mean(bs_firm[p])
      
      bs_ar[o] = sum(sample(bs_firm - bs_firm_mean, 20, replace = T)) / 20
      bs_sd_residuals[o] = sd(bs_firm - bs_firm_mean)
      
      bs_t_statistic[o] = bs_ar[o] / bs_sd_residuals[o]
      bs_j1_statistic[o] = NA
    }
  }
  if (model == "b-one")
  {
    for (o in 1:bootstrap_multiplier)
    {
      for (p in 1:length(est_data[,1]))
      {
        bs_firm[p] = sum(sample(est_data[,1], replace = T)) / length(est_data[,1])
        bs_market[p] = sum(sample(est_data[,2], replace = T)) / length(est_data[,2])
      }
      bs_ar[o] = sum(sample(bs_firm - bs_market, 20, replace = T)) / 20
      bs_sd_residuals[o] = sd(bs_firm - bs_market)
      
      bs_t_statistic[o] = bs_ar[o] / bs_sd_residuals[o]
      bs_j1_statistic[o] = NA
    }
  }
  # p-values
  pvalue_bs_t_test = c(); pvalue_bs_j1_test = c()
  for (q in 1:length(indexes))
  {
    pvalue_bs_t_test[q] = length(which(abs(bs_t_statistic[q]) > abs(t_statistic[q]))) / length(bs_t_statistic[q])
    pvalue_bs_j1_test[q] = ifelse(is.na(bs_j1_statistic[q]), NA, length(which(abs(bs_j1_statistic[q]) > abs(j1_statistic[q]))) / length(bs_j1_statistic[q]))
  }

  plot(density(bs_t_statistic))
  a
  
  ## Results
  
}

event_study(datam0, ewm, event_list_mann, "capm", 2)


# Others ------------------------------------------------------------------


# Pearson's Chi-squared - goodness to fit    // at leat 5 obs for each class
datam = read_excel(path = "C:/Users/Admin/Google Drive/S6/Recherche/Vod-Man/datavodman.xlsx", sheet = "estimationM")
datam0 = datam[-which(datam$mannesmann == 0),]; attach(datam0)
regmd0 = lm(mannesmann~dax); cf = coef(regmd0)
error = data.frame(res = mannesmann - (cf[1] + cf[2] * dax))
res_emp = error$res; avg = mean(res_emp)
res_emp = res_emp %>% sort() %>% as.data.frame()
len = length(res_emp$.); std = sqrt((len/(len-1) * sd(res_emp$.)^2))
eff = c(sum(res_emp < -0.04), sum(res_emp < -0.02 & res_emp >= -0.04), sum(res_emp < -0.01 & res_emp >= -0.02),
        sum(res_emp < 0 & res_emp >= -0.01), sum(res_emp < 0.01 & res_emp >= 0), sum(res_emp < 0.03 & res_emp >= 0.01),
        sum(res_emp < 0.16 & res_emp >= 0.03))
proba = c(pnorm(-0.04, avg, std), pnorm(-0.02, avg, std) - pnorm(-0.04, avg, std), pnorm(-0.01, avg, std) - pnorm(-0.02, avg, std), 
          pnorm(0, avg, std) - pnorm(-0.01, avg, std), pnorm(0.01, avg, std) - pnorm(0, avg, std),
          pnorm(0.03, avg, std) - pnorm(0.01, avg, std), 1 - pnorm(0.03, avg, std))
theo_eff = len * proba
# Test statistic
dist = eff - theo_eff
d2 = sum(dist^2 / theo_eff); d2
# follows a chi-squared of k-r-1 df - k classes, r estimated parameters (mean/sd)
qchisq(0.9, 4)




lis <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))
lis
class(lis[[1]])
lis[[1]][2]






