rm(list=ls())

library(readxl); library(tidyverse); library(fBasics); library(httr)
library(ggplot2); library(ggthemes); library(gridExtra); library(ggridges)


# Data --------------------------------------------------------------

link = "https://github.com/FractalySyn/VodMan-event-study/raw/master/datavodman.xlsx"
download.file(link, "datavodman.xlsx", mode = "wb")

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
datam = read_excel(path = "datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
chart(datam, mannesmann, dax)
ewm = read_excel(path = "datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
chart(ewm, mannesmann, dax)

## Vodafone
datav = read_excel(path = "datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
chart(datav, vodafone, footsie)
ewv = read_excel(path = "datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
chart(ewm, vodafone, footsie)




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
datam = read_excel(path = "datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
zero_analysis(datam)
qf(0.991, 2, 358) # pvalue ~ 0.009 -> confidence level 99.1%
# 110 null returns over 362 obs. i.e. ~30%

# constraint model -> mann = a + b*dax + u --- 362 obs
# non constraint model -> mann = a + bi*dax + u, i=1,2 --- 252 obs and 110 obs respectively

# Fisher statistic (calculated)
# -> F = [(SCRc - (SCR1+SCR2)) / (dfc - (df1+df2))] / [(SCR1 + SCR2) / (df1 + df2)]
# k = 2 (alpha, beta)
# follows a fisher law of k and N-2k df
"=> empirical fisher value is 4.783934 > theoretical values
 => we can reject the null hypothesis of homogeneity at more than 99% confidence level"

# Paradox
# H0 rejected -> requires a homoskedasticity test for which Ho = variances are similar 
# -> Intuitively no because the second dataset has a null variance <=> heteroskedasticity
# -> The test statistic is V1/V2 = (SCR1/(n1-k))/(SCR2/(n2-k)) that follows a fisher law of n1-k and n2-k df
# In one case we cannot calculate the fisher (0 division) and replacing it with the limit to 0 gives a infinitely big fisher statistic <=> rejection
# In the other case the fisher statistic will be 0 <=> no rejection
# Paradox : we know the data is heteroskedastic, the test gives two responses and the most reliant is contradicting this fact
# Furthermore accepting heteroskedasticity would mean that the Chow test is biased
# This test isn't relevant in this case of null returns

# Conclusion
# Given the large amount of null returns (30%) and the results of the Chow test we conclude that these zeros certainly come from sampling errors


## Vodafone
datav = read_excel(path = "datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
zero_analysis(datav)
qf(0.118, 2, 248) # pvalue ~ 0.882 -> confidence level 11.8%
# 2 null returns over 252 obs i.e. less than 1%

# constraint model -> vod = a + b*dax + u --- 251 obs
# non constraint model -> vod = a + bi*dax + u, i=1,2 --- 249 obs and 2 obs respectively

" => empirical fisher value is 0.1257151 < theoretical values
  => we cannot reject the null hypothesis of homogeneity "

# Conclusion : we keep the whole dataset



# Retained models --------------------------------------------------------

market_model = function(data, vec1 = data[,1], vec2 = data[,2], plot = F)
{
  model = data[,1] ~ data[,2]
  reg = lm(model)
  if (plot == T) 
  {
    plot(model, xlab = names(data[,2]))
    abline(reg, col = "blue")
  }
  list(summary(reg), coefficients(reg), reg)
}


## CAPM Mannesmann ~ dax 12/05/98 - 15/10/99
datam = read_excel(path = "datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
datam0 = datam[-which(datam$mannesmann == 0),]
market_model(datam0, mannesmann, dax, plot = T)[[1]]$residuals
# -> beta 0.682696 signficant at more than 99.99% confidence level

## CAPM Vodafone ~ footsie 15/10/98 - 15/10/99
datav = read_excel(path = "datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
market_model(datav, vodafone, footsie, plot = T)
# -> beta 1.247092 signficant at more than 99.99% confidence level

## Plots
p1 = ggplot(datam0, aes(dax, mannesmann)) +
  xlab("Rendements Dax") + ylab("Rendements Mannesmann") + ggtitle("Régression Mannesmann") + 
  geom_point(alpha = 0.7) +
  geom_abline( col = "blue", lwd = 1, lty = 2) +
  theme_wsj() +
  theme(plot.title = element_text(hjust = 0.5, size = 10))
p2 = ggplot(datav, aes(footsie, vodafone)) +
  geom_point(alpha = 0.7) +
  geom_abline( col = "blue", lwd = 1, lty = 2) +
  theme_wsj() +
  xlab("Rendements Footsie") + ylab("Rendements Vodafone") + ggtitle("Régression Vodafone") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10))
grid.arrange(p1, p2, respect = T, ncol = 2)





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
  
  fisherb = (scr1 / df1) / (scr2 / df2)
  fisherb95 = qf(0.95, df1, df2)
  fisherb99 = qf(0.99, df1, df2)
  resultb95 = ifelse(fisherb < fisherb95, "Not biased", "Biased")
  resultb99 = ifelse(fisherb < fisherb99, "Not biased", "Biased")
  
  # plot(firm_T ~ market_T, xlab = names(data1)[2], ylab = names(data1)[1])
  list(Chow_temp_stability_test = dplyr::summarise(data1, Fisher_Statistic = fisherc, 
                                                   Fisher_95 = fisher95,CL = result95, Bias1 = resultb95,
                                                   Fisher_99 = fisher99, CL2 = result99, Bias2 = resultb99,
                                                   df = length(firm_T)-4))
}

## Mannesmann
ewm = read_excel(path = "datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
ewm0 = ewm[-which(ewm$mannesmann == 0),]
datam = read_excel(path = "datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
datam0 = datam[-which(datam$mannesmann == 0),]

# Chow test
# Fisher statistic (calculated) : F = [(SCRc - (SCR1+SCR2)) / (dfc - (df1+df2))] / [(SCR1 + SCR2) / (df1 + df2)], k = 2 (alpha, beta)
# follows a fisher law of k and T-2k df
temporal_stability(datam0, ewm0)
qf(0.825, 2, 325) # p-value 0.175
# test statistic is << than theoretical fishers -> means that the merger and the EU Commission intervention didn't much impact the market
# to be verified with ARs because the beta on event window isn't significant -> the market index (dax) didn't explain mannesmann returns


## Vodafone
ewv = read_excel(path = "datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
datav = read_excel(path = "datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()

# Chow test
# Fisher statistic (calculated) : F = [(SCRc - (SCR1+SCR2)) / (dfc - (df1+df2))] / [(SCR1 + SCR2) / (df1 + df2)], k = 2 (alpha, beta)
# follows a fisher law of k and T-2k df
temporal_stability(datav, ewv)
qf(0.552, 2, 359) # p-value 0.448
# test statistic is << than theoretical fishers -> means that the merger and the EU Commission intervention didn't much impact the market
# to be verified with ARs because the beta on event window isn't significant 




# Errors analysis Mannesmann --------------------------------------------------------

## function
error_analysis = function(data, vec1, vec2)
{
  attach(data, warn.conflicts = F)
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
datam = read_excel(path = "datavodman.xlsx", sheet = "estimationM") %>% as.data.frame()
datam0 = datam[-which(datam$mannesmann == 0),]
error_analysis(datam0, mannesmann, dax)

# Event window
ewm = read_excel(path = "datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
ewm0 = ewm[-which(ewm$mannesmann == 0),]
error_analysis(ewm0, mannesmann, dax)

# Whole data
alldatam = as.data.frame(cbind(mannesmann = c(datam0$mannesmann, ewm0$mannesmann), dax = c(datam0$dax, ewm0$dax))); head(alldatam)
error_analysis(alldatam, mannesmann, dax)


## Vodafone
# Estimation Window
datav = read_excel(path = "datavodman.xlsx", sheet = "estimationV") %>% as.data.frame()
error_analysis(datav, vodafone, footsie)

# Event window
ewv = read_excel(path = "datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
error_analysis(ewv, vodafone, footsie)

# Whole data
alldatav = as.data.frame(cbind(vodafone = c(datav$vodafone, ewv$vodafone), footsie = c(datav$footsie, ewv$footsie)))
error_analysis(alldatav, vodafone, footsie)


## QQ-plots
qq1 = ggplot(data.frame(res = market_model(datam0)[[1]]$residuals), aes(sample = scale(res))) +
  geom_qq() + geom_abline(lwd = 1, col = "firebrick4") +
  theme_economist() + ggtitle("QQ-plot résidus Mannesmann") +
  theme(plot.title = element_text(vjust = 1, hjust = 0.5))
qq2 = ggplot(data.frame(res = market_model(datav)[[1]]$residuals), aes(sample = scale(res))) +
  geom_qq() + geom_abline(lwd = 1, col = "firebrick4") +
  theme_economist() + ggtitle("QQ-plot résidus Vodafone") +
  theme(plot.title = element_text(vjust = 1, hjust = 0.5))
grid.arrange(qq1, qq2, ncol = 2)

## Ridge plots
mod_datam0 = mutate(datam0, period = "estimation") %>% mutate(res = market_model(datam0)[[1]]$residuals) %>% mutate(asset = ("Mannesmann"))
mod_ewm0 = mutate(ewm0, period = "évènement") %>% mutate(res = market_model(ewm0)[[1]]$residuals) %>% mutate(asset = "Mannesmann")
mod_datav = mutate(datav, period = "estimation") %>% mutate(res = market_model(datav)[[1]]$residuals) %>% mutate(asset = "Vodafone")
mod_ewv = mutate(ewv, period = "évènement") %>% mutate(res = market_model(ewv)[[1]]$residuals) %>% mutate(asset = "Vodafone")
alldata = cbind(c(mod_datam0$period, mod_ewm0$period, mod_datav$period, mod_ewv$period),
                c(mod_datam0$res, mod_ewm0$res, mod_datav$res, mod_ewv$res),
                c(mod_datam0$asset, mod_ewm0$asset, mod_datav$asset, mod_ewv$asset))
alldata = tibble(asset = alldata[,3], period = alldata[,1], res = alldata[,2])
mutate(alldata, period = factor(period)) %>%
  mutate(asset = factor(asset)) %>%
  mutate(res = as.numeric(res)) %>%
    ggplot(aes(res, asset, fill = asset)) +
    geom_density_ridges(scale = 2, rel_min_height = 0.01)  +
    ylab("") + xlab("Rentabilités") +
    facet_grid(period ~ .) +
    theme_economist() +
    theme(legend.title = element_blank(), axis.text.y = element_blank(), axis.title.x = element_text(hjust = 0.5, face = "bold", vjust = 0)) +
    xlim(-0.15, 0.22)


# Event window wrangling ------------------------------------------------------------


## events
event1 = c("1999-11-12", "1999-11-15", "1999-11-16") # 1st offer
event2 = c("1999-11-17", "1999-11-18") # Mannesmann rejects
event3 = c("1999-12-20", "1999-12-21", "1999-12-22") # 2nd offer - IPO + Mann-Orange approved
event4 = c("2000-01-10", "2000-01-11", "2000-01-12", "2000-01-13") # Vodafone could elevate the offer
event5 = c("2000-01-14", "2000-01-17") # EU Commission gets notified
event6 = c("2000-02-03", "2000-02-04") # Mannesmann accepts
event7 = c("2000-02-22", "2000-02-23") # Incomplete notif
event8 = c("2000-02-29", "2000-03-01") # Vodafone notifies again
event9 = c("2000-03-27", "2000-03-28") # Mannesmann accepts
event10 = c("2000-04-12", "2000-04-13") # EU Commission accepts the merger
event_list_vod = list(event1, event2, event3, event4, event5, event6, event7, event8, event9, event10)
event_list_mann = list(event1, event2, event3, event4, event5, event6, event7, event8)

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
ewm = read_excel(path = "datavodman.xlsx", sheet = "eventwindowM") %>% as.data.frame()
null_events(ewm, event_list_mann)
# there's a null return on 10 jan 2000 -> is it an important date ?

## Vodafone
ewv = read_excel(path = "datavodman.xlsx", sheet = "eventwindowV") %>% as.data.frame()
null_events(ewv, event_list_vod)
# 22/12 is NA
# there's no null returns for vodafone on the event window





# Event study -------------------------------------------------------------

"Null Dates must be excluded !!!!"
event4m = c("2000-01-11", "2000-01-12", "2000-01-13") # remove 10/01/2000 for Mannesmann
event_list_mann = list(event1, event2, event3, event4m, event5, event6, event7, event8)
event3v = c("1999-12-20", "1999-12-21")
event_list_vod = list(event1, event2, event3v, event4, event5, event6, event7, event8, event9, event10)

bootstrap_X = 1000000

event_study = function(est_data, event_data, events, model = "capm", bootstrap_multiplier)
{
  # find dates in data
  indexes = list()
  for (i in 1:length(events)) 
  {
    indexes[[i]] = match(events[[i]], as.character(event_data$dates))
  }
  
  # calculate observed car
  car = c()
  if (model == "capm")
  {
    cf = market_model(est_data, est_data[,1], est_data[,2])[[2]]
    market = event_data[,2]; firm = event_data[,1]
    for (j in 1:length(indexes))
    {
      expected_return = cf[2] * sum(market[indexes[[j]]]) + cf[1] * length(indexes[[j]])
      car[j] = sum(firm[indexes[[j]]]) - expected_return
    }
  }
  if (model == "cmrm")
  {
    firm = event_data[,1]
    for (j in 1:length(indexes))
    {
      expected_return = mean(est_data[,1]) * length(indexes[[j]])
      car[j] = sum(firm[indexes[[j]]]) - expected_return
    }
  }
  if (model == "b-one")
  {
    market = event_data[,2]; firm = event_data[,1]
    for (j in 1:length(indexes))
    {
      expected_return = sum(market[indexes[[j]]])
      car[j] = sum(firm[indexes[[j]]]) - expected_return
    }
  }
  
  # observed test statistics t and J1  
  t_statistic = c(); j1_statistic = c()
  if (model == "capm")
  {
    event_regressors = market_model(event_data, event_data[,1], event_data[,2])[[2]]
    est_regressors = market_model(est_data, est_data[,1], est_data[,2])[[2]]
    
    for (k in 1:length(indexes)) 
    {
      Im = diag(rep(1, length(indexes[[k]]))); column = matrix(rep(1, length(indexes[[k]])), length(indexes[[k]]), 1)
      X_star = matrix(rep(event_regressors), length(indexes[[k]]), 2, byrow = T)
      X = matrix(rep(est_regressors), length(indexes[[k]]), 2, byrow = T)
      
      expectancy_car = car[k] 
      stdev_car_t_test = market_model(est_data, est_data[,1], est_data[,2])[[1]]$sigma ## one day sigma for V(J1) computation
      
      var_AR = stdev_car_t_test^2 * Im + stdev_car_t_test^2 * (X_star %*% matlib::Ginv(t(X) %*% X) %*% t(X_star))
      stdev_car_j1_test = sqrt(t(column) %*% var_AR %*% column)
      stdev_car_t_test = market_model(est_data, est_data[,1], est_data[,2])[[1]]$sigma * sqrt(length(indexes[[k]]))
      
      t_statistic[k] = expectancy_car / stdev_car_t_test
      j1_statistic[k] = expectancy_car / stdev_car_j1_test
    }
  }
  if (model == "cmrm")
  {
    for (k in 1:length(indexes))
    {
      stdev_car_t_test = sd(est_data[,1] - mean(est_data[,1])) * sqrt(length(indexes[[k]]))
      expectancy_car = car[k]
      t_statistic[k] = expectancy_car / stdev_car_t_test
      j1_statistic[k] = NA
    }
  }
  if (model == "b-one")
  {
    for (k in 1:length(indexes))
    {
      stdev_car_t_test = sd(est_data[,1] - est_data[,2]) * sqrt(length(indexes[[k]]))
      expectancy_car = car[k]
      t_statistic[k] = expectancy_car / stdev_car_t_test
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
  if (model == "capm") ARs = market_model(est_data, est_data[,1], est_data[,2])[[1]]$residuals 
  if (model == "cmrm") ARs = (est_data[,1] - mean(est_data[,1])) 
  if (model == "b-one") ARs = (est_data[,1] - est_data[,2]) 
  
  # bootstrap Monte Carlo non parametric test - empirical distribution of the ARs
  pvalue_montecarlo_test = c(); a = c()
  for (n in 1:length(indexes))
  {
    mc_ar = replicate(bootstrap_multiplier, sum(sample(ARs, length(indexes[[n]]), replace = T))) 
    pvalue_montecarlo_test[n] = mean(abs(mc_ar) > abs(car[n]))
  }
  
  # bootstrap parametric test Marais - empirical distribution of the test statistics
  len = bootstrap_multiplier / length(est_data[,1]) %>% ceiling()
  bs_t_statistic = list(); bs_j1_statistic = list(); bs_firm = c(); bs_market = c()
  if (model == "capm")
  {
    bs_est_data = list(); bs_est_regressors = list()
    for (o in 1:len)
    {
      res = market_model(est_data, est_data[,1], est_data[,2])[[1]]$residuals
      bs_market_and_residuals = cbind(sample(est_data[,2], replace = T), sample(res, replace = T))
      cf = market_model(est_data, est_data[,1], est_data[,2])[[2]]
      for (p in 1:length(est_data[,1]))
      {
        bs_firm[p] = cf[1] + cf[2]*bs_market_and_residuals[p,1] + bs_market_and_residuals[p,2] 
      }
      bs_est_data = data.frame(bs_firm, bs_market_and_residuals[,1], bs_market_and_residuals[,2])
      bs_est_regressors = market_model(bs_est_data, bs_est_data[,1], bs_est_data[,2])[[2]]
      
      bs_sd_residuals = market_model(bs_est_data, bs_est_data[,1], bs_est_data[,2])[[1]]$sigma
      bs_ar = market_model(bs_est_data, bs_est_data[,1], bs_est_data[,2])[[1]]$residuals
      
      bs_X_star = t(event_regressors)
      bs_X = t(bs_est_regressors)
      bs_var_AR = bs_sd_residuals^2 + bs_sd_residuals^2 * (bs_X_star %*% matlib::Ginv(t(bs_X) %*% bs_X) %*% t(bs_X_star))
      bs_stdev_ar_j1_test = sqrt(abs(bs_var_AR))
      
      bs_t_statistic[[o]] = bs_ar / bs_sd_residuals
      bs_j1_statistic[[o]] = bs_ar / as.numeric(bs_stdev_ar_j1_test)
    }
  }
  if (model == "cmrm")
  {
    for (o in 1:len)
    { 
      bs_firm = sample(est_data[,1], length(est_data[,1]), replace = T)
      bs_firm_mean = mean(bs_firm)
      
      bs_ar = bs_firm - bs_firm_mean
      bs_sd_residuals = sd(bs_ar)
      
      bs_t_statistic[[o]] = bs_ar / bs_sd_residuals # 250 t
      bs_j1_statistic[[o]] = NA
    }
  }
  if (model == "b-one")
  {
    for (o in 1:len)
    {
      bs_firm = sample(est_data[,1], length(est_data[,1]), replace = T)
      bs_market = sample(est_data[,2], length(est_data[,1]), replace = T)
      
      bs_ar = bs_firm - bs_market
      bs_sd_residuals = sd(bs_ar)
      
      bs_t_statistic[[o]] = bs_ar / bs_sd_residuals # 250 t
      bs_j1_statistic[[o]] = NA
    }
  }
  
  bs_t_statistic = unlist(bs_t_statistic, use.names = F)
  bs_j1_statistic = unlist(bs_j1_statistic, use.names = F)
  # p-values
  pvalue_bs_t_test = c(); pvalue_bs_j1_test = c()
  for (q in 1:length(indexes))
  {
    pvalue_bs_t_test[q] = length(which(abs(bs_t_statistic) > abs(t_statistic[q]))) / length(bs_t_statistic)
    pvalue_bs_j1_test[q] = ifelse(is.na(bs_j1_statistic[1]), NA, length(which(abs(bs_j1_statistic) > abs(j1_statistic[q]))) / length(bs_j1_statistic))
  }
  
  
  ## Results
  if (model == "capm") results = data.frame(CAR = car, t = t_statistic, j1 = j1_statistic,
                                            t_classical = pvalue_classical_t_test, j1_classical = pvalue_classical_j1_test,
                                            t_bs = pvalue_bs_t_test, j1_bs = pvalue_bs_j1_test,
                                            ar_mc = pvalue_montecarlo_test)
  if (model == "cmrm") results = data.frame(CAR = car, t = t_statistic, 
                                            t_classical = pvalue_classical_t_test, 
                                            t_bs = pvalue_bs_t_test, 
                                            ar_mc = pvalue_montecarlo_test)
  if (model == "b-one") results = data.frame(CAR = car, t = t_statistic, 
                                             t_classical = pvalue_classical_t_test, 
                                             t_bs = pvalue_bs_t_test, 
                                             ar_mc = pvalue_montecarlo_test)
  results
}


# Mannesmann
m1=event_study(datam0, ewm, event_list_mann, "capm", bootstrap_X)
m2=event_study(datam0, ewm, event_list_mann, "cmrm", bootstrap_X)
m3=event_study(datam0, ewm, event_list_mann, "b-one", bootstrap_X)

# Vodafone
v1=event_study(datav, ewv, event_list_vod, "capm", bootstrap_X)
v2=event_study(datav, ewv, event_list_vod, "cmrm", bootstrap_X)
v3=event_study(datav, ewv, event_list_vod, "b-one", bootstrap_X)





## Entire period
ewm0 = ewm[-which(ewm[,1] == 0),]
ewv0 = ewv[-which(ewv[,1] == 0),]
period_vod = list(as.character(ewv0$dates))
period_mann = list(as.character(ewm0$dates))

# Mannesmann
m4=event_study(datam0, ewm0, period_mann, "capm", bootstrap_X)
m5=event_study(datam0, ewm0, period_mann, "cmrm", bootstrap_X)
m6=event_study(datam0, ewm0, period_mann, "b-one", bootstrap_X)

# Vodafone
v4=event_study(datav, ewv0, period_vod, "capm", bootstrap_X)
v5=event_study(datav, ewv0, period_vod, "cmrm", bootstrap_X)
v6=event_study(datav, ewv0, period_vod, "b-one", bootstrap_X)






## Phases
start = "1999-11-12"; endv = "2000-04-13"; endm = "2000-03-10"
phase1 = "2000-01-14"; phase2 = "2000-02-29"

phases_define = function(data, start, end, phase1, phase2)
{
  istart = which(as.character(data$dates) == start); iend = which(as.character(data$dates) == end)
  ip1 = which(as.character(data$dates) == phase1); ip2 = which(as.character(data$dates) == phase2)
  
  ibefore = istart:(ip1-1); iphase1 = ip1:(ip2-1); iphase2 = ip2:iend
  
  list(before = as.character(data$dates[ibefore]),
       first = as.character(data$dates[iphase1]),
       second = as.character(data$dates[iphase2]))
}

mann_phases = phases_define(ewm0, start, endm, phase1, phase2)
vod_phases = phases_define(ewv0, start, endv, phase1, phase2)

# Mannesmann
m7=event_study(datam0, ewm0, mann_phases, "capm", bootstrap_X)
m8=event_study(datam0, ewm0, mann_phases, "cmrm", bootstrap_X)
m9=event_study(datam0, ewm0, mann_phases, "b-one", bootstrap_X)

# Vodafone
v7=event_study(datav, ewv0, vod_phases, "capm", bootstrap_X)
v8=event_study(datav, ewv0, vod_phases, "cmrm", bootstrap_X)
v9=event_study(datav, ewv0, vod_phases, "b-one", bootstrap_X)






## CARs plot
"add vline for phases"
cars = function(est_data1, event_data1, est_data2, event_data2)
{
  cf1 = market_model(est_data1)[[2]]
  expected1 = cf1[1] + cf1[2] * event_data1[,2]
  cars1 = event_data1[,1] - expected1
  
  cf2 = market_model(est_data2)[[2]]
  expected2 = cf2[1] + cf2[2] * event_data2[,2]
  cars2 = event_data2[,1] - expected2
  
  g1 = ggplot(data.frame(cars1 = cumsum(cars1), time = event_data1$dates), aes(time, cars1)) +
    geom_line(lwd = 0.8, col = "firebrick4") + theme_wsj() + xlab("") +
    ggtitle("AR Mannesmann") + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
    geom_vline(xintercept = as.POSIXct(phase1, tz = "UTC"), col = "blue") +
    geom_vline(xintercept = as.POSIXct(phase2, tz = "UTC"), col = "blue") 
  g2 = ggplot(data.frame(cars2 = cumsum(cars2), time = event_data2$dates), aes(time, cars2)) +
    geom_line(lwd = 0.8, col = "firebrick4") + theme_wsj() + xlab("") +
    ggtitle("AR Vodafone") + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
    geom_vline(xintercept = as.POSIXct(phase1, tz = "UTC"), col = "blue") +
    geom_vline(xintercept = as.POSIXct(phase2, tz = "UTC"), col = "blue") 
  
  grid.arrange(g1, g2, ncol = 2)
  
}
cars(datam0, ewm0, datav, ewv)



