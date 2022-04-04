## bootstrap: 남의 도움 없이, 스스로 하다.
head(iris)

## setosa 꽃잎 길이의 95% 신뢰구간을 구한다.
# setosa 를 따로 모은다.
y = subset(iris, Species=='setosa')$Petal.Length
mean(y) # 1.462, 표본평균
sd(y)   # 0.17,  표준편차, 모표준편차인 것처럼 쓰겠다.

n = length(y)

# 95% 신뢰구간의 하한, 상한
lower = mean(y) - 2 * sd(y) / sqrt(n)
upper = mean(y) + 2 * sd(y) / sqrt(n)

c(lower, upper) # 1.41  1.51


## 위 문제를, bootstrap 으로 푼다.
sim_n = 10000
means = c()
for(i in 1:sim_n) {
    # bootstrap에선, 표본크기가 같아야 한다.
    # bootstrap에선, 복원추출이 필수이다.
    sample = sample(y, length(y), replace=T) 
    sample_mean = mean(sample)
    means = c(means, sample_mean)
}

# 95% 신뢰구간
c(quantile(means, .025), quantile(means, .975))
# 2.5%   97.5% 
#1.414   1.510 


## t분포로 구한 신뢰구간
c(mean( + qt(.025, df=n-1) * sd(y) / sqrt(n), mean(y) + qt(.975, df=n-1) * sd(y) / sqrt(n))
# 1.412645 1.511355


### 모표준편차 추정
sim_n = 10000
sds = c()
for(i in 1:sim_n) {
    sample = sample(y, length(y), replace=T)
    sample_sd = sd(sample)
    sds = c(sds, sample_sd)
}
c(quantile(sds, 0.25), quantile(sds, .975))
# 25%        9.75% 
# 0.1566095 0.2124301 


### 가설검정
x = subset(iris, Species=='virginica')$Petal.Length
y = subset(iris, Species=='versicolor')$Petal.Length  
sim_n = 10000
diffs = c()

for(i in 1:sim_n) {
    virginica = sample(x, length(x), replace=T)
    setosa = sample(y, length(y), replace=T)
    mean_diff = mean(virginica) - mean(setosa)
    diffs = c(diffs, mean_diff)
}
c(quantile(diffs, .25), quantile(diffs, .95))
#   25%   95% 
# 1.224 1.460
