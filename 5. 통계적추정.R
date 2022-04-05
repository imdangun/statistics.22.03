# 통계학은 확률론을 현실에 응용하는 학문이다.

### 표본평균
## 키
set.seed(1234)
sim_n = 10000

sample_size = 100
means = c()

for(i in 1:sim_n) {
    sample = rnorm(sample_size, 170, 15) # 100개의 표본을 추출한다.
    means = c(means, mean(sample))
}

hist(means, xlab='키', breaks=50, prob=T)
curve(dnorm(x, 170, 15 / sqrt(sample_size)), 160, 180, add=T, lty=2, lwd=2, col='red')

mean(means) # 170.00, 표본평균들의 평균이 모평균과 비슷하다.

se = 15 / sqrt(sample_size) # 표준오차: 표본평균의 표준편차 = 모표준편차 / sqrt(샘플크기)
means_within_2se = (170 - 2*se < means) & (means < 170 + 2*se)
sum(means_within_2se) # 9514


## 95% 신뢰구간
set.seed(1234)

sim_n = 10000
sample_size = 100
mean = 170 # 모평균
sd = 15    # 모표준편차
se = sd / sqrt(sample_size) # 표준오차
x_bar = c()

for(i in 1:sim_n) {
    sample = rnorm(sample_size, mean, sd)
    x = mean(sample) # 표본평균
    if((x - 2*se < mean) & (mean < x + 2*se)) x_bar = c(x_bar, T)
    else x_bar = c(x_bar, F)
}

mean(x_bar) # 0.95
