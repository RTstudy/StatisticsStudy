library(dplyr)

# 問題1を問題の通りにまっすぐ解く --------------------------------------------------------

X <- seq(6)

addCount <- X %>%
            expand.grid(X) %>%
            mutate(sum=Var1+Var2) %>%
            group_by(sum) %>%
            summarise(count=n()) %>%
            mutate(prob=count/sum(.["count"]))
addCount

absCount <- X %>%
            expand.grid(X) %>%
            mutate(sum=abs(Var1-Var2)) %>%
            group_by(sum) %>%
            summarise(count=n()) %>%
            mutate(prob=count/sum(.["count"]))
absCount

# 問題１をもう少し見やすい形に展開する ------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

X <- seq(6)
# 組み合わせの和をmatrix型にする
X_comb <- expand.grid(X,X) %>%
          mutate(sum=Var1+Var2) %>%
          spread(key=Var2,value=sum) %>%
          select(-Var1) %>%
          as.matrix()
# matrix型にした結果を表示
X_comb
# カウントした結果をdata.frameに格納する
X_df <- data.frame(table(X_comb))
X_df
# ヒストグラムをggplot2で描画
X_sum <- expand.grid(X,X) %>%
         mutate(sum=Var1+Var2) %>%
         select(sum)
ggplot(data=X_sum,aes(sum)) +
  geom_histogram(aes(y=..density..))


# さいころ2個の和をシミュレーションしてみる ---------------------------------------------------

library(tidyverse)
library(ggplot2)

# さいころをふる回数n, 実験回数N, 各々のさいころの結果をX,Yとする
n <- 500
N <- 10000
z <- seq(6) #さいころがとりうる値を設定
p <- rep(1/length(z),length(z)) #各値が出る確率を設定

df <- data.frame(sum=seq(2,12))

data <- lapply(seq(N),function(x){
  X <- sample(z,n,replace=TRUE,prob=p)
  Y <- sample(z,n,replace=TRUE,prob=p)
  sum <- X+Y
  sumt <- data.frame(table(sum))
  sumt["sum"] <- as.integer(as.character(sumt[,1]))
  out <- full_join(df,sumt,by="sum")
  return(out["Freq"])
}) %>%
  do.call(cbind,.) %>%
  t() %>%
  as.data.frame()
data[is.na(data)] <- 0
colnames(data) <- as.character(seq(2,12))
median_out <- as.data.frame(apply(data,2,summary))[3,]
mean_out <- as.data.frame(apply(data,2,summary))[4,]

median_out
mean_out

# 各値がでる回数は二項分布に従う
# n回の試行のうちある値Xがnp回出現する確率、と考えれば良い
# 例えば、n=10のとき、7が出現するのは理論上はp=0.167である
# その場合、7が出現する回数はn=10,p=0.167の二項分布に従うことになる
# ちなみにn=1にするとベルヌイ試行になる
data_plot <- data %>%
             gather(key=num, value=count)
ggplot(data=data_plot,aes(x=count,y=..density..,fill=num)) +
  geom_histogram(position = "identity",alpha=0.2) +
  geom_density(aes(color=num,alpha=0.2),show.legend = F)

