# 통계학은 확률론을 현실에 응용하는 학문이다.
# 전체 집단의 특징을 알고 싶지만, 표본만을 구할 수 있기에, 통계학을 사용한다.

# 표집오차(sampling error): 추출시 마다 표본이 달라지고, 그에 따라 표본평균이나 표본비율이 달라진다.

# 점추정(point estimation): 모집단의 평균, 비율 등의 모수치를 하나의 숫자로 찍는 것이다.
# 구간추정(interval estimation): 모수치를 구간으로 찍는 것이다.


## 모평균을 점추정한다.
# 키 평균이 170, 표준편차가 15인 모집단을 대상으로, 키 평균을 점추정한다.
set.seed(1234)
sim_n = 10000

sample_size = 100
means = c()

for(i in 1:sim_n) {
    sample = rnorm(sample_size, 170, 15) # 100개의 표본을 추출한다.
    means = c(means, mean(sample))
}

# 실제, 표본평균들의 분포가 정규분포를 따른다: 중심극한정리
hist(means, xlab='키', breaks=50, prob=T)
# 이론, 표본평균이 따라야 할 이론적 정규분포곡선을 그린다. 실제와 이론이 비슷하다.
curve(dnorm(x, 170, 15 / sqrt(sample_size)), 160, 180, add=T, lty=2, lwd=2, col='red')

mean(means) # 170.00, 불편성(unbiasedness): 표본평균(표본평균들의 평균)이 모평균과 비슷하다.

se = 15 / sqrt(sample_size) # 표준오차(standard error) = 표본평균의 표준편차 = 모표준편차 / sqrt(샘플크기)
means_within_2se = (170 - 2*se < means) & (means < 170 + 2*se)
sum(means_within_2se) # 9514, 표본평균들의 95.14%가 2*se 안에 있다.



## 모평균을 구간추정한다.
# 신뢰도
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
    # 모평균이 표본평균 95% 신뢰구간 안에 있으면
    if((x - 2*se < mean) & (mean < x + 2*se)) x_bar = c(x_bar, T)
    else x_bar = c(x_bar, F)
}

mean(x_bar) # 0.95