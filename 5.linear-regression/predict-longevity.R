library(ggplot2)

ages <- read.csv("longevity.csv")

constant.guess <- with(ages, mean(AgeAtDeath))
with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^ 2)))

smokers.guess <- with(subset(ages, Smokes == 1),
                      mean(AgeAtDeath))
non.smokers.guess <- with(subset(ages, Smokes == 0),
                      mean(AgeAtDeath))

ages <- transform(ages,
                  NewPrediction = ifelse(Smokes == 0,
                                  non.smokers.guess,
                                  smokers.guess))
with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))

print(ages)

# 読み込んだデータをそのままプロット
# 喫煙者と非喫煙者で、死ぬ年齢の密度をプロット
fillでグラフないを色付け
facet_gridでグラフをSmokesが0の時と1のときで分ける
gplo <- ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
# ヒストグラムを作成
geom_density() +
# ヒストグラムをデータカテゴリごとに表示
facet_grid(Smokes ~ .)

plot(gplo)

## guessを63から83まで回して、目的関数を作成
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
##

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
