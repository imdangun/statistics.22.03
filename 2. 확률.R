## 확률 = 관심을 갖는 경우의 수 / 생각할 수 있는 모든 경우의 수

# 시행: 다양한 결과가 나올 수 있는 걸 행하는 것이다. 예) 복권 긁기
# 표본공간: 가능한 모든 경우이다.
# 사건: 관심 있는 경우이다. 단 하나의 경우를 의미하는 게 아니다.
#       사건은 표본공간의 부분집합이다.
# 배반사건: 동시에 일어날 수 없는 사건이다. 예)동전 앞면이다 / 동전 뒷면이다.
# 여사건: 관심 사건 외의 사건이다. 예) 모든 숫자가 짝수이다 / 숫자 하나 이상이 홀수이다.
# 수학적 확률 = 사건의 수 / 모든 경우의 수, 표본공간의 모든 경우가 나올 가능성이 같아야 한다.
# 통계적 확률 = 사건의 수 / 시행 횟수, 시행횟수를 늘릴수록 통계적확률과 수학적확률이 비슷해진다.


## 큰수의 법칙(law of large number): 
#     표본평균은 자료 크기가 클수록, 특정값에 가까워진다.
#     이 특정값이 기대값(expected value) 이다. 
#     통계적 확률은, 큰수의 법칙의 한 경우일 뿐이다. 
#
#     통계적 확률 = 표본평균
#     수학적 확률 = 기대값
#
#     표집오차(sampling error) = 통계적확률 - 수학적확률
#     시행횟수를 극한으로 늘릴수록, 표집오차가 0에 가까워진다. 0이 되지는 않는다.

## 베르누이시행(Bernuoulli trial): 베르누이는 17c 스위스 수학자이다.
#     가능한 결과가 두 개뿐이고, 성공 확률이 정해져 있는 시행이다. 예) 성공/실패, 예/아니오

# api, rbinom(난수개수, 시행횟수, 성공확률): rbinom = Random + BINOMinal
#      시행횟수는 각 난수를 발생시킬 때의 시행횟수이다.
# rbinom(a, 1, p) 은 베르누이 시행, 
# rbinom(a, N, p) 는 이항 시행이다.


# 동전을 던져 앞면이 나올 확률을 구한다.
x = rbinom(5, 1, 0.5) # 베르누이시행이려면, 두번쩨 파라미터 값이 1 이어야 한다.
x # 0 0 1 1 1
mean(x)  # 0.6, 통계적확률 = 표본평균

x = rbinom(100, 1, 0.5)
mean(x) # 0.52, 자료크기(난수개수)를 늘렸더니 통계적확률이 수학적확률에 가까워졌다.



## 몬테카를로 시뮬레이션(방법) (Monte Carlo simulation(method)): 
#       수학공식 없이 통계적 확률을 계산하는 방법이다. 
#       20c 도박의 도시 몬테카를로에서 이름을 땄다.

## 1, 2, 3, 4, 5 카드 중에서 3장을 뽑아, 320 이상이 되는 확률을 구한다.
sim_n = 100000
success_n = 0

for(i in 1:sim_n) {
    # api, sample(추첨대상, 추첨개수, 복원여부)
    x = sample(1:5, 3, replace=F) # x = [2, 5, 4]
    if(x[1] >= 4) success_n = success_n + 1
    if(x[1] == 3 & (x[2] >=2)) success_n = success_n + 1
}

success_n         # 55149
success_n / sim_n # 0.55149


## 몬테카를로 방법으로, 원주율을 구한다.
# 중심점 (0, 0), 반지름 1 인 사분원을 그리고, 쌀을 흩뿌린다.
sim_n = 10000
x = vector(length=sim_n)
y = vector(length=sim_n)
inner_n = 0

for(i in 1:sim_n) {
    # api, runif(난수 개수, min, max): min max 를 생략하면 0<=x<=1 를 생성한다.
    x[i] = runif(1)
    y[i] = runif(1) # 좌표 (x, y) 인 쌀알 하나를 구한다.
    # 쌀 위치에서 원점까지의 거리가, 반지름 1보다 작으면, 원 안에 쌀이 있는 것이다.
    if(sqrt(x[i]^2 + y[i]^2) < 1) inner_n = inner_n + 1
}

# 원의 넓이 = PI * r^2 = PI 이다. 반지름 r이 1이기 때문이다.
# inner_n / sim_n 은, 사분원 안에 떨어진 쌀의 비율이다.
print(4 * (inner_n / sim_n)) # 3.124, 원주율 파이를 구했다.

# plot: 알고리즘 설명은 생략한다.
circle = function(x) sqrt(1 - x^2)
plot(x, y, xlab='X', ylab='Y')
curve(circle, from=0, to=1, add=T, col='blue', lwd=2)


## 몬티홀 문제: 
#      문을 열어 자동차가 보이면 경품으로 받고, 염소가 보이면 꽝이다.
#      진행자가 염소를 보여준 뒤, 선택한 문을 바꾸지 않으면, 차를 받을 확률이 1/3
#      진행자가 염소를 보여준 뒤, 선택한 문을 바꾸면, 차를 받을 확률이 2/3 이다.
sim_n = 1000
doors = 1:3
success_n = 0

for(i in 1:sim_n) {
    car = sample(doors, 1) # 3, 자동차가 세워진 문이다.
    if(car == 1) goat = c(2, 3)
    else if(car == 2) goat = c(1, 3)
    else goat = c(1, 2) # goat = [1, 2]
    
    # 참가자가 문을 고른다.
    pick = sample(doors, 1) # 3
    
    # 고르지 않은 문 중, 염소가 있는 문을 찾는다.
    goatNotPicked = goat[goat != pick] # [1, 2]
    
    # 고르지 않은 문 중, 염소가 있는 문을 연다.
    if(length(goatNotPicked) > 1) open = sample(goatNotPicked, 1) # 2
    else open = goatNotPicked
    
    # 참가자가 닫힌 문중, 고르지 않은 문으로 바꾼다.
    # 이 코드를 넣으면, 성공 확률이 0.68이 된다.
    pick = doors[(doors != pick) & (doors != open)] # 1번
    
    if(pick == car) success_n = success_n + 1
}

print(success_n / sim_n) # 0.337 -> 0.662, 1번 코드를 추가하면 성공 확률이 올라간다.