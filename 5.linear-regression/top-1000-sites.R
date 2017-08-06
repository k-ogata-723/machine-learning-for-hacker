library(ggplot2)

top.1000.sites <- read.csv('top_1000_sites.tsv',
                            sep = '\t',
                            stringsAsFactors = FALSE)

# top.1000.sitesのPageViewsとUniqueVisitorsを利用してプロットする
scatterPlot <- ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors)) +
  geom_point()

print(scatterPlot)

# scatterPlotのplotがよくわからないので、PageViewだけをplotする
PageViewsPlot <- ggplot(top.1000.sites, aes(x = PageViews)) +
	geom_density()

print(PageViewsPlot)

# PageViewsPlotを対数化する
logPageViewsPlot <- ggplot(top.1000.sites, aes(x = log(PageViews))) + 
	geom_density()

print(logPageViewsPlot)

# 対数化したほうが良さそうなので、PageViewsPlotを対数化する
logScatterPlot <- ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
	geom_point()

# print(logScatterPlot)

# lmでlogScatterPlotの切片を求める
modelLm <- lm(log(PageViews) ~ log(UniqueVisitors),
			data = top.1000.sites)

plot.new()
print(logScatterPlot)
par(new=T)
abline(modelLm, lwd=1, col="blue")

# lm.fitに HasAdvertising と IsEnglish の情報を加える
lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + IsEnglish,
				data = top.1000.sites)

summary(lm.fit)
plot(lm.fit)


## 以下、summaryので出力される中身の例
#Call: 5.2 ウェブのアクセス数を予測する 157
#lm(formula = log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites) 
#
#Residuals:
# Min 1Q Median 3Q Max
#-2.1825 -0.7986 -0.0741 0.6467 5.1549
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|) 
#(Intercept) -2.83441 0.75201 -3.769 0.000173 *** 
#log(UniqueVisitors) の係数は 1.33628 で標準誤差は0.04568である。つまりこの係数は、標準誤差の1.33628 / 0.04568 == 29.25306 倍だけゼロから離れていることになる。もし標準誤差がゼロから 3 以上離れていれば、2 つの 変数が関連していると確信するのは理にかなっている。
#log(UniqueVisitors) 1.33628 0.04568 29.251 < 2e-16 *** 
#---
#Signif. codes: 0‘***’0.001‘**’0.01‘*’0.05‘.’0.1‘ ’1 
#
#Residual standard error: 1.084 on 998 degrees of freedom 
#Multiple R-squared: 0.4616, Adjusted R-squared: 0.4611 
#F-statistic: 855.6 on 1 and 998 DF, p-value: < 2.2e-16

