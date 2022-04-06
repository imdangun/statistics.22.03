## 이산확률분포: 확률변수가 특정값을 가질 확률이다.
# 로또 경우의 수   
lotto = function(x) choose(6, x) * choose(39, 6 - x) #api, choose(n, r) = nCr
for(x in 0:6) print(lotto(x))
# [1] 3,262,623      번호 0개 맞춘 경우의 수
# [1] 3,454,542      번호 1개 맞춘 경우의 수
# [1] 1233765
# [1] 182780
# [1] 11115
# [1] 234
# [1] 1              번호 6개 맞춘 경우의 수

#
# 로또 확률분포
lotto2 = function(x) choose(6, x) * choose(39, 6 - x) / choose(45, 6)
for(x in 0:6) print(lotto2(x))
#       확률값     확률변수
# [1] 0.4005646       0개
# [1] 0.4241273       1개
# [1] 0.151474        2개
# [1] 0.0224406
# [1] 0.001364631
# [1] 2.872907e-05
# [1] 1.227738e-07 = 0.000000122 = 1/814만



### 연속확률분포: 확률변수가 특정 구간에 있을 확률이다.
## 정규분포(normal distribution)

# 누군가의 소득이 25000달러 이상 35000달러 이하일 확률을 구한다.
# api, pnorm(값, 평균, 표준편차): 음의 무한대에서 값까지의 확률을 리턴한다. probability + normal
pnorm(35000, 30000, 10000) - pnorm(25000, 30000, 10000) # 0.38

pnorm(25000, 30000, 10000)     # 0.30
1 - pnorm(35000, 30000, 10000) # 0.30

pnorm(25000, 30000, 10000) # 0.30
pnorm(-0.5, 0, 1)          # 0.30, 표준정규분포
pnorm(-0.5) # 2, 3번째 파라미터 기본값은 0, 1 이다. 즉, 표준정규분포로 간주한다.

# dnorm() : 확률밀도함수, probability density function
# curve() : 그래프를 그린다.
curve(dnorm(x), -3, 3, xlab='X', ylab='density')



## 중심극한정리(centeral limit theorem): 표본평균의 분포는 정규분포이다.
# 이항분포
sim_n = 10000
y = rbinom(sim_n, 100, 0.5)
hist(y, xlab='X', ylab='mass', main='binom(100, 0.5)', prob=T, breaks=30)
curve(dnorm(x, 50, 5), 25, 75, add=T, lty=2, lwd=1, col='red')

# 키의 분포는 중심극한정리에 해당한다.
sim_n = 1000
n = 30 # 한번 추출 시 30명을 선발
means = vector(length=sim_n)

for(i in 1:sim_n) {
    y = vector(length=n)
    for(j in 1:n) {
        gender = rbinom(1, 1, 0.5) # 개인별 성별을 추출한다.
        if(gender == 0) { # 여성이면
            y[j] = rnorm(1, 160, 5)
        } else { # 남성이면
            y[j] = rnorm(1, 175, 5)
        }
    }
    means[i] = mean(y)
}

hist(means, xlab='표본평균 키', ylab='비율', main='키 분포', prob=T)