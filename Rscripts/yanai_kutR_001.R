## yanai_kutR_001.R
##
## Demonstrate how to use R for KUT.R #1
## 2019-12-05 Yuki Yanai


## R を電卓として使う
1 + 1
3 * 4
4 / 5
17 %/% 4; 17 %% 4 # 商と余り: 1行に2つの命令
5^2      #　乗数
sqrt(3)  # 平方根
exp(1)   # ネイピア数（オイラー定数）
log(10)   # 自然対数
log10(10) #底が10の対数
log(10, base = 2) #底が2の対数
pi        #　円周率
## 半径 5.5 の円の面積は?
r <- 5.5
pi * r^2


## ベクトル
a <- c(1, 3, 6, 8, 12)
a

univs <- c("KUT", "University of Kochi", "Kochi University")
univs

univs_jp <- c("高知工科大学", "高知県立大学", "高知大学")
univs_jp

## 数列を作る
1:10
1990:2020

## 1以上100以下の偶数
seq(from = 2, to = 100, by = 2)
## -50以上50以下の奇数
seq(from = -49, to = 50, by = 2)
## 0以上1以下の実数を等間隔で20個作る
seq(from = 0, to = 1, length.out = 20)
## 降順
seq(from = 100, to = 0, by = -5)

## ガウス少年に挑戦：1から100までの整数の合計は
sum(1:100)

## sum() とは?
?sum

## 同じ数を繰り返す
rep(3, 10)
rep(c(3, 5, 9), 4)
rep(c("Mifune", "Himichi", "Kamijo"), each = 2)

## ベクトルの特定の要素にアクセスする
even <- seq(from = 2, to = 100, by = 2)
## 正の偶数で12番目に小さいのは？
even[12]
## 正の偶数で15番目から20番目まで表示
even[15:20]

## 簡単な統計量の計算
x <- 1:100
length(x) # 観測値の数（ベクトルの長さ）
mean(x)   # 平均
median(x) # 中央値
min(x)    # 最小値
max(x)    # 最大値
sd(x)     # 標準偏差
var(x)    # 分散
quantile(x, p = c(0.25, 0.5, 0.75)) # パーセンタイル
fivenum(x) # 五数要約 

## 行列
A <- matrix(1:9, nrow = 3)
A
B <- matrix(1:9, nrow = 3, byrow = TRUE)
B
C <- matrix(1:9, ncol = 3, byrow = TRUE)
C

t(A)   # 行列の転置

## 行列の演算
A + B
A - B
A %*% B  

D <- matrix(c(2, 3, 5,
              7, 11, 13,
              17, 19, 23),
            nrow = 3, byrow = TRUE)
D
solve(D)  # 逆行列

## 行列の要素アクセス, スライシング
A
A[, ]
A[1, ]   # Aの1行目
A[, 2]   # Aの2列目
A[2, 3]  # Aの(2, 3)要素
A[1:2, ]     # Aの1行目と2行目
A[, c(1, 3)] # Aの1列目と3列目
A[-1, ] 　　 # Aの1行目以外
diag(A)  # Aの対角要素


## for ループ
animals <- c('cat', 'lion', 'leopard')
for (i in seq_along(animals)) {
  for (j in 1:4) {
    cat(paste0(animals[i], j), '\t')
  }
  cat('\n')
}

## 条件分岐
for (i in 1:50) {
  if (i %% 5 == 0) { # 5で割り切れる場合
    cat(i, '\n')
  } else {           # その他の場合
    cat('.')
  }
}

## 乱数生成

(x <- runif(100, min = 0, max = 1)) # 一様分布からの乱数生成
hist(x, col = 'royalblue')  # ヒストグラム

(y <- rnorm(100, mean = 100, sd = 10)) # 正規分布からの乱数生成
hist(y, col = 'dodgerblue') # ヒストグラム

speakers <- c('Song', 'Yanai', 'Shimizu', 'Kamijo', 'Himichi', 'Mifune')
sample(speakers, size = 3, replace = FALSE)


## 関数を作る
tri_area <- function(base, height) {
  return(base * height * 0.5)
}
tri_area(base = 10, height = 5)

die <- function(n = 1) {
  return(sample(1:6, size = n, replace = TRUE))
}
die()
die(n = 3)

die_total <- function(n = 1) {
  return(sum(sample(1:6, size = n, replace = TRUE)))
}
die_total()
die_total(n = 3)


## データフレーム
df <- data.frame(x = 1:3,
                 y = 11:13)
plot(y ~ x, data = df)

## 1990年から2010年までのxのデータを日本、合衆国、中国について集めたい
df2 <- expand.grid(year = 1990:2010,
                   country = c("Japan", "USA", "China"))
df2$year
df2$country
unique(df2$country)



## データを読み込む: 厚生労働省の都道府県別生命表
## パッケージのインストールと読み込み
## readxl と dplyr を使う
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
library("readxl")
library("dplyr")

## データ保存用のディレクトリを作る
dir.create("data", showWarning = FALSE)

## データをダウンロードして保存
download.file(url = "https://www.mhlw.go.jp/toukei/saikin/hw/life/tdfk15/dl/tdfk15-09.xls",
              destfile = "data/tdfk15-09.xls")

## Excelデータの読み込み: 最初の6行をとばしてデータを読み込む
myd <- read_excel("data/tdfk15-09.xls", skip = 6, col_names = FALSE)
head(myd)　# データの先頭6行の表示

## 変数名をつける
colnames(myd) <- c('rank', 'pref_m', 'life_m', 'pref_f', 'life_f')
head(myd, n = 8)  # データの先頭8行の表示

## 括弧付きの数字を修正する
myd$life_f[1:2] <- c(87.675, 87.673)

## 括弧のせいで文字列 (chr) 扱いされていたので、数値 (dbl) に直す
myd <- mutate(myd, life_f = as.numeric(life_f))
head(myd)

## 同じ行に異なる都道府県があるのはおかしい！直す！
## 男性のデータ
myd_m <- select(myd, rank:life_m)
head(myd_m)
## 女性のデータ
myd_f <- select(myd, rank, ends_with("f"))
head(myd_f)

## 同じ都道府県が同じ行になるようにして、男女のデータを結合
myd_new <- inner_join(myd_m, myd_f, by = c("pref_m" = "pref_f"))
head(myd_new)

## 変数名（列名）を付け直す
myd_new <- rename(myd_new,
                  rank_m = rank.x,
                  rank_f = rank.y,
                  pref = pref_m)
head(myd_new)

## せっかくなので後で使うために保存する
write.csv(myd_new, file = "data/longevity_pref.csv",
          row.names = FALSE)

## 高知のデータを確認
filter(myd_new, pref == "高　知")

## 男性の平均寿命が最も長いのは？
max(myd_new$life_m)

## 女性の平均寿命が最も短いのは？
min(myd_new$life_f)


## YouTubeを見る
## @kazutan が書いてくれた tubeplayR (https://github.com/kazutan/tubeplayR) 
# #のコードを使う（ただし、Macでしか動かない）。
if(!require("devtools")) install.packages("devtools")
if (!require("tubeplayR")) devtools::install_github("kazutan/tubeplayR")
library("tubeplaryR")

## 再生したiい動画のURLを指定して再生
tubeplay(url = "https://www.youtube.com/watch?v=jl747tReEX0")

############
## ENJOY! 
############
