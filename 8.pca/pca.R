library("lubridate")
library("reshape")
library("ggplot2")

prices <- read.csv('stock_prices.csv')

prices[1, ]
# Date Stock Close
#1 2011-05-25 DTE 51.12
# データの中身の確認用
# print(prices[1, ])

# lubridateライブラリーを使って、日付をエンコードする
prices <- transform(prices, Date = ymd(Date))
# 以下2行では、不要なデータを削除
# subset関数で、不要なもの以外のデータを抽出している
prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

# pricesを扱い形にやすい変換する
# 銘柄ごとに列を分ける
# DataとStockの値をとして持ち、Closeを行の値としてもつ
data.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

# dim関数でdata.stock.matrixの行と列の長さを出す
# dim(data.stock.matrix)
# print(data.stock.matrix)

# cor関数は相関関係を求める関数
# data.stock.matrixの2~最後までの列の相関を求める
# cor関数では、1列目と1列目の相関、1列目と2列目の相関、1列目と3列目の相関....という感じで
# それぞれの行の相関を行列の形で出してくれる
cor.matrix <- cor(data.stock.matrix[,2:ncol(data.stock.matrix)])

# print(cor.matrix)

# as.numericで行列を数値型に変換する
# ちなみに、as.characterを使えば、文字列型に変換できる
correlations <- as.numeric(cor.matrix)

plotCorrelations <- ggplot(data.frame(Correlation = correlations),
          aes(x = Correlation, fill = 1)) +
    geom_density()
    # opts関数でなぜかエラーになるのでコメントアウト
    # + opts(legend.position = 'none')

# cor関数の結果である、相関係数の発生頻度が視覚化される
# 正の密度が高ければ、いい感じのデータを選べている
# print(plotCorrelations)

# 行列のpcaは、princomp関数を使うだけで求められる
pca <- princomp(data.stock.matrix[,2:ncol(data.stock.matrix)])

# print(pca)

# pcaの結果からloadingの列を抽出する
# 固有ベクトルが$loadingに入っている
principal.component <- pca$loading[,1]
loadings <- as.numeric(principal.component)

loadings.plot <- ggplot(data.frame(Loading = loadings),
          aes(x = Loading, fill = 1)) +
      geom_density()
      # + opts(legend.posirion = 'none')

# print(loadings.plot)

# predict関数を使って、pcaを1行に要約する
market.index <- predict(pca)[,1]

# print(market.index)

dji.prices <- read.csv('DJI.csv')
# dji.pricesのDataをymd(Date)で変換する
dji.prices <- transform(dji.prices, Date = ymd(Date))

# print(dji.prices)

# データの期間が長すぎるので必要な期間だけ取り出す
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

# rev関数を使ってデーtを逆の並び順にする
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

# pcaとdjiを合わせたdata.frame型を作成する

comparison <- data.frame(Date = dates, MarketIndex = market.index, DJI = dji)

comparison.plot <- ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# この結果では、DJIとmaeketindexが負の相関を持ってしまう
# print(comparison.plot)

# 以下で、maeketindexを-1にして、相関を正しくしている
# comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)
#
# comparison.ggplot <- ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = FALSE)
#
# # print(comparison.ggplot)

alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

comparison <- transform(comparison, MarketIndex = -scale(MarketIndex))

comparison <- transform(comparison, DJI = scale(DJI))
alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

p <- ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

print(p)
