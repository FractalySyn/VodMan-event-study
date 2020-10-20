# Chi-squared ------------------------------------------------------------------


# Pearson's Chi-squared - goodness to fit    // at leat 5 obs for each class
datam = read_excel(path = "datavodman.xlsx", sheet = "estimationM")
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
