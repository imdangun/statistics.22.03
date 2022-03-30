# 시행: 예) 복권 긁기
# 표본공간: 모든 경우
# 사건: 관심 있는 경우
# 배반사건: 동시에 발생 불가한 사건 예)동전 앞면이다 / 동전 뒷면이다.
# 여사건: 관심 사건 외의 사건 예) 로또 모든 숫자가 짝수이다 / 숫자 하나 이상이 홀수이다.
# 수학적 확률 = 사건의 수 / 모든 경우의 수, 표본공간의 모든 경우가 나올 가능성이 같아야 한다.
# 통계적 확률 = 사건의 수 / 시행 횟수, 시행횟수를 늘릴수록 통계적확률과 수학적확률이 비슷해진다.


## 큰수의 법칙(law of large number)
# 표집오차(sampling error): 통계적확률과 수학적확률의 차이값이다.
# 시행횟수를 늘릴수록 표집오차가 0에 가까워진다. 0이 되지는 않는다.

# 베르누이시행(Bernuoulli trial)
# rbinom = Random + BINOMinal, rbinom(난수개수, 시행횟수, 성공확률)
# 시행횟수는 각 난수를 발생시킬 때의 시행횟수이다.
x = rbinom(5, 1, 0.5) 
print(x) # 0 0 1 1 1, 0은 실패, 1은 성공이다.
mean(x)  # 0.6, 통계적확률 = 평균

x = rbinom(100, 1, 0.5)
mean(x) # 0.55, 시행횟수를 늘렸더니 통계적확률이 수학적확률에 가까워졌다.








### Monte Carlo simulation (몬테카를로 시뮬레이션)
# 1, 2, 3, 4, 5 카드 중에서 3장을 뽑아, 320 이상이 되는 확률을 구한다.
simulationCnt = 100000
successCnt = 0
for(i in 1:simulationCnt) {
    # sample(추첨대상, 추첨개수, 복원여부)
    x = sample(1:5, 3, replace=F) # [2, 5, 4]
    if(x[1] >= 4) successCnt = successCnt + 1
    if(x[1] == 3 & (x[2] >=2)) successCnt = successCnt + 1
}
successCnt # 55149(simulationCnt 가 10만 일때)
successCnt / simulationCnt # 0.55149(simulationCnt 가 10만 일때)


## 몬테카를로로 원주율을 구한다.
simCnt = 1000
x = vector(length=simCnt)
y = vector(length=simCnt)
res = 0
for(i in 1:simCnt) {
    # runif(난수 개수, min, max): min max 를 생략하면 0<=x<=1 를 생성한다.
    x[i] = runif(1)
    y[i] = runif(1)
    if(x[i]^2 + y[i]^2 < 1) res = res + 1
}
print(4 * res / simCnt) # 3.12, 원주율을 구했다.

circle = function(x) sqrt(1 - x^2)
plot(x, y, xlab='X', ylab='Y')
curve(circle, from=0, to=1, add=T, col='blue', lwd=2)


## 몬티홀 문제
simCnt = 1000
doors = 1:3
successCnt = 0

for(i in 1:simCnt) {
    car = sample(doors, 1) # 3
    if(car == 1) goat = c(2, 3)
    else if(car == 2) goat = c(1, 3)
    else goat = c(1, 2) # [1, 2]
    
    # 참가자가 문을 고른다.
    pick = sample(doors, 1) # 3
    
    # 고르지 않은 문 중, 염소가 있는 문을 찾는다.
    goatNotPicked = goat[goat != pick] # [1, 2]
    
    # 고르지 않은 문 중, 염소가 있는 문을 연다.
    if(length(goatNotPicked) > 1) open = sample(goatNotPicked, 1) # 2
    else open = goatNotPicked
    
    # 참가자가 닫힌 문중, 고르지 않은 문으로 바꾼다.
    # 이 코드를 넣으면, 성공 확률이 0.68이 된다.
    pick = doors[(doors != pick) & (doors != open)]
    
    if(pick == car) successCnt = successCnt + 1
}

print(successCnt / simCnt) # 0.337

