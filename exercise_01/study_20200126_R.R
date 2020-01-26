library(dplyr)

# ���1����̒ʂ�ɂ܂��������� --------------------------------------------------------

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

# ���P�������������₷���`�ɓW�J���� ------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

X <- seq(6)
# �g�ݍ��킹�̘a��matrix�^�ɂ���
X_comb <- expand.grid(X,X) %>%
          mutate(sum=Var1+Var2) %>%
          spread(key=Var2,value=sum) %>%
          select(-Var1) %>%
          as.matrix()
# matrix�^�ɂ������ʂ�\��
X_comb
# �J�E���g�������ʂ�data.frame�Ɋi�[����
X_df <- data.frame(table(X_comb))
X_df
# �q�X�g�O������ggplot2�ŕ`��
X_sum <- expand.grid(X,X) %>%
         mutate(sum=Var1+Var2) %>%
         select(sum)
ggplot(data=X_sum,aes(sum)) +
  geom_histogram(aes(y=..density..))


# ��������2�̘a���V�~�����[�V�������Ă݂� ---------------------------------------------------

library(tidyverse)
library(ggplot2)

# ����������ӂ��n, ������N, �e�X�̂�������̌��ʂ�X,Y�Ƃ���
n <- 500
N <- 10000
z <- seq(6) #�������낪�Ƃ肤��l��ݒ�
p <- rep(1/length(z),length(z)) #�e�l���o��m����ݒ�

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

# �e�l���ł�񐔂͓񍀕��z�ɏ]��
# n��̎��s�̂�������lX��np��o������m���A�ƍl����Ηǂ�
# �Ⴆ�΁An=10�̂Ƃ��A7���o������̂͗��_���p=0.167�ł���
# ���̏ꍇ�A7���o������񐔂�n=10,p=0.167�̓񍀕��z�ɏ]�����ƂɂȂ�
# ���Ȃ݂�n=1�ɂ���ƃx���k�C���s�ɂȂ�
data_plot <- data %>%
             gather(key=num, value=count)
ggplot(data=data_plot,aes(x=count,y=..density..,fill=num)) +
  geom_histogram(position = "identity",alpha=0.2) +
  geom_density(aes(color=num,alpha=0.2),show.legend = F)
