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

# pricesを扱い形に変換する
# 銘柄ごとに列を分ける
# DataとStockの値をとして持ち、Closeを行の値としてもつ
data.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

# print(data.stock.matrix)

# cor関数は相関関係を求める関数
# data.stock.matrixの2~最後までの列の相関を求める
cor.matrix <- cor(data.stock.matrix[,2:ncol(data.stock.matrix)])

# as.numericで行列を数値型に変換する
# ちなみに、as.characterを使えば、文字列型に変換できる
correlations <- as.numeric(cor.matrix)

plotCorrelations <- ggplot(data.frame(Correlation = correlations),
          aes(x = Correlation, fill = 1)) +
    geom_density()
    # opts関数でなぜかエラーになるのでコメントアウト
    # + opts(legend.position = 'none')

print(plotCorrelations)

# 行列のpcaは、princomp関数を使うだけで求められる
pca <- princomp(data.stock.matrix[,2:ncol(data.stock.matrix)])

print(pca)

# predict関数を使って、pcaを1行に要約する
market.index <- predict(pca)[,1]
print(market.index)

dji.prices <- read.csv('data/DJI.csv')
# dji.pricesのDataをymd(Date)で変換する
dji.prices <- transform(dji.prices, Data = ymd(Date))

# データの期間が長すぎるので必要な期間だけ取り出す
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

# rev関数を使ってデーtをぎゃkの並び順にする
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

# pcaとdjiを合わせたdata.frame型を作成する
comparison <- data.file.rename(Date = dates, MarketIndex = market.index, DJI = dji)
