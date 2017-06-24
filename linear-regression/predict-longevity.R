library(ggplot2)

ages <- read.csv("longevity.csv")

guess.accuracy <- data.frame()

for (guess in seq(63, 83, by = 1)) {
  prediction.error <- with(ages,
                      mean((AgeAtDeath - guess) ^ 2))
  guess.accuracy <- rbind(guess.accuracy,
                          data.frame(Guess = guess,
                                    Error = prediction.error))
}

# forで回して目的関数を作成
gplo <- ggplot(guess.accuracy, aes(x = Guess, y = Error)) +
　geom_point() +
　geom_line()

# 読み込んだデータをそのままプロット
# fillでグラフないを色付け
# facet_gridでグラフをSmokesが0の時と1のときで分ける
# gplo <- ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
# 　geom_density() +
# 　facet_grid(Smokes ~ .)

plot(gplo)

# 以下、ネットで見つけたggplotのサンプルコード
# http://motw.mods.jp/R/ggplot_tutorial.html
# irisという、rのプリセットのデータを利用している
# library(ggplot2)
# str(iris)
#
# p <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))
# q <- p + geom_point()
# ggsave(file="graph1.png")
#
# plot(q)
