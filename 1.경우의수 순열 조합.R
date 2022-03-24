## 경우의수

## factorial
fact = function(n) {
    x = 1
    for(i in 2:n) {
        x = x * i
    }
    return (x)
}
fact(5)

## 순열(permutation)
# nPr = n! / (n-r)!
# 5P2 = 5!/(5-2)! 
x = c(1, 2, 3, 4, 5)
count = 0
for(i in 1:5) {
    x2 = x[x != i]
    for(j in 1:4) {
        print(c(i, x2[j]))
        count = count + 1
    }
}
print(count) # 20

perm = function(n, r) {
    return(fact(n) / fact(n-r))
}
print(perm(5, 2)) # 20

## 조합(combination): 순열과 달리 순서를 따지지 않는다.
# nCr = nPr/r! 
# 5C2
x = c(1, 2, 3, 4, 5)
count = 0
for(i in 1:4) {
    for(j in (i+1):5){
        print(c(i, j))
        count = count + 1
    }
}
print(count) # 10

comb = function(n, r) {
    return(perm(n, r) / fact(r))
}
print(comb(5, 2)) # 10

# 로또 경우의 수
print(comb(45, 6)) # 8,145,060
