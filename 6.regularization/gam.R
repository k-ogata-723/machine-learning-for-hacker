library('ggplot2')

set.seed(1)

# -10から10まで、0.0.1間隔でベクトルを作成する
x <- seq(-10, 10, by = 0.01)
# rnormで、xベクトルの数の、平均値0、標準偏差5の乱数を作成している
y <- 1 - x ^ 2 + rnorm(length(x), 0, 5)

ggplot(data.frame(X = x, Y = y), aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(se = FALSE)

# xのデータを使って、新しいデータセットを作り出す
x.squared <- x ^ 2

ggplot(data.frame(XSquared = x.squared, Y = y), aes(x = XSquared, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

#### 以下、非線形のデータを多項式回帰する
set.seed(1)

x <- seq(0, 1, by = 0.01)
# 正弦波を使って、非線形のデータを作成する
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)

ggplot(df, aes(x = X, y = Y)) +
  geom_point()

非線形のデータに、直線を当てはめてみる
summary(lm(Y ~ X, data = df))

Call:
lm(formula = Y ~ X, data = df)

# Residuals:
#      Min       1Q   Median       3Q      Max
# -1.00376 -0.41253 -0.00409  0.40664  0.85874
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.94111    0.09057   10.39   <2e-16 ***
# X           -1.86189    0.15648  -11.90   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.4585 on 99 degrees of freedom
# 明らかにうまく行かないはずなのに、60%の変動が説明できている
# Multiple R-squared:  0.5885,	Adjusted R-squared:  0.5843
# F-statistic: 141.6 on 1 and 99 DF,  p-value: < 2.2e-16

ggplot(df, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
