load("data/NHANESsample.RData")

fileConn <- file("E:/GitHub/EpiShinyLiveD/data/analyticData2.txt")
dput(analytic.data, fileConn)
close(fileConn)

# Not including survey features
boxplot(blood.pressure~race, data = analytic.data, col="grey", 
           ylab="Blood Pressure", 
           xlab ="Race")

# including survey features
analytic.design <- svydesign(strata=~SDMVSTRA, id=~SDMVPSU, weights=~WTMEC2YR, data=analytic.data, nest=TRUE)
svyboxplot(blood.pressure~race, design = analytic.design, col="grey", 
           ylab="Blood Pressure", 
           xlab ="Race")
