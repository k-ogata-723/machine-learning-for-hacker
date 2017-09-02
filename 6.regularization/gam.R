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
