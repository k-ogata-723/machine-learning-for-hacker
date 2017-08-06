library(ggplot2)

heights.weights <- read.csv('01_heights_weights_genders.csv', header = TRUE, sep = ',')

# # lmと使ってシンプルにfittignする
# # geom_pointで点をプロット
# # geom_smoothでfitting\
# heights <- ggplot(heights.weights, aes(x = Height, y = Weight)) +
#   geom_point() +
#   geom_smooth(method = 'lm')
#
# print(heights)

# lmで線形を作成
# Weights = a Heights　のaを求めようとしている
fitted.regression <- lm(Weight ~ Height, data = heights.weights)

# # fitted.regressionの予測仮定をprintする
# # fitted.regressionで切片(intercept)と傾きが得られる
# # predict関数ではfitted.regressionの結果を使って、各Heightsに対するWeightsを得る
# print(predict(fitted.regression))

# 実際のWeightsとpredictしたWeightsの残差を確認する
true.values <- with(heights.weights, Weight)
errors <- true.values - predict(fitted.regression)

# 残差はresiduals関数でも出せる
print(residuals(fitted.regression))

# plot(lmの結果)では、色々な結果が出力される
# その中で、残差の図のみを出力するようにwhichを引数で指定している
plot(fitted.regression, which = 1)

print(fitted.regression)

# 残差に明確な構造が現れる、モデル作成時の悪い兆候の確認
x <- 1:10
y <- x ^ 2

fitted.regression <- lm(y ~ x)

errors <- residuals(fitted.regression)
squared.errors <- errors ^ 2
sum(squared.errors)
# 残差は、値が大き時に大きくなりがちなので、残差の合計ではなく平均を利用する
mse <- mean(squared.errors)
