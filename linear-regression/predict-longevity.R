library(ggplot2)

ages <- read.csv("longevity.csv")

gplo <- ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
　geom_density() +
　facet_grid(Smokes ~ .)

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
