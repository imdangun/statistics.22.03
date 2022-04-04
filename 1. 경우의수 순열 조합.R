## 경우의수
## factorial
fact = function(n) {
    x = 1
    for(i in 2:n) {
        x = x * i
    }
    return (x)
}

fact(5) # 120


## 순열(permutation):  nPr = n! / (n-r)!
# brute force 로, 5P2 = 5!/(5-2)! 을 구한다.
x = c(1, 2, 3, 4, 5)
cnt = 0

for(i in 1:5) {
    y = x[x != i]
    for(j in 1:4) {
        print(c(i, y[j])) # for{} 에선, print 를 써야 값이 출력된다.
        cnt = cnt + 1
    }
}

cnt # 20

#
perm = function(n, r) {
    return(fact(n) / fact(n-r))
}

perm(5, 2) # 20


## 조합(combination): 순열과 달리 순서를 따지지 않는다.
# nCr = nPr/r! 
# 5C2
x = c(1, 2, 3, 4, 5)
cnt = 0

for(i in 1:4) {
    for(j in (i+1):5){
        print(c(i, j))
        cnt = cnt + 1
    }
}

cnt # 10

# nCr = nPr / r!
comb = function(n, r) {
    return(perm(n, r) / fact(r))
}

comb(5, 2) # 10


## 로또 경우의 수
comb(45, 6) # 8,145,060


## edge case: 가능한 모든 경우를 대비하지 않아, 발생한 예외 경우이다.