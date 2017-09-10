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

df <- transform(df, X2 = x ^ 2)
df <- transform(df, X3 = x ^ 3)

summary(lm(Y ~ X + X2 + X3, data = df))

# Call:
# lm(formula = Y ~ X + X2 + X3, data = df)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.32331 -0.08538  0.00652  0.08320  0.20239
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -0.16341    0.04425  -3.693 0.000367 ***
# X            11.67844    0.38513  30.323  < 2e-16 ***
# X2          -33.94179    0.89748 -37.819  < 2e-16 ***
# X3           22.59349    0.58979  38.308  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.1153 on 97 degrees of freedom
# Xの２乗や3乗を追加したことで、r-squaredが58%から97%に上がった
# ただし、この方法方法では、データをどんどん追加していくとデータの相関が強すぎて予測ができなくなっていく
# Multiple R-squared:  0.9745,	Adjusted R-squared:  0.9737
# F-statistic:  1235 on 3 and 97 DF,  p-value: < 2.2e-16

上記のデータを増やした時の相関をなくすために、poly関数で
summary(lm(Y ~ poly(X, degree = 14), data = df))

# poly関数を使って、相関の弱い形でデータの次数を増やす
# ploy関数のdegreeで次数を設定できる
# ただ、次数を増やしすぎるとover fittingになる
poly.fit <- lm(Y ~ poly(X, degree = 7), data = df)
df <- transform(df, PredictY = predict(poly.fit))

ggplot(df, aes(x = X, y = PredictY)) +
  geom_point() +
  geom_line()


# データを学習データとテストデータに分けて、交差検定をおこなう
# まず、交差検定のための正弦波のseedを作成していく

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

print(y)

n <- length(x)

# データをtraingデータとtestデータにランダムな割合に分割するためにindicesを生成
indices <- sort(sample(1:n, round(0.5 * n)))

# 始めからindices個のデータをトレーニングデータとして利用する
training.x <- x[indices]
training.y <- y[indices]

print(training.x)

test.x <- x[-indices]
test.y <- y[-indices]

# テストデータとトレーニングデータのデータフレームを作成する
# rでは、data.frameは色々な方が入れられて、matrixは1種類の型しか入れられないらしい
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

# 以下、RMSE（平均２乗誤差、root mean square error）のコードを作成する
# degree（関数の次元）を増やしていって、テストデータとトレーニングデータの学習曲線を比較
# まず、コストを出す関数を自作
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()

# degreeを1から12まで増やしつつ、計算していく
for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)

  performance <- rbind(performance,
                      data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))

  performance <- rbind(performance,
                      data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                              newdata = test.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

  
