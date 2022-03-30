## 조건부확률(conditional probability)
# 조건부확률 >= 주변확률
# 주변확률: 총시행횟수가 분모가 되는 확률이다.
# 베이즈 정리(bayes' theorem): P(B|A) = P(A|B) * P(B) / P(A)

# 주사위를 던져, 홀수 중에 5가 나올 확률을 구한다.
simCnt = 10000
oddCnt = 0
n5Cnt = 0

for(i in 1:simCnt) {
    y = sample(1:6, 1) # 주사위를 던진다.
    if(y %% 2 == 1) oddCnt = oddCnt + 1 # 홀수가 나왔다.
    if(y == 5) n5Cnt = n5Cnt + 1
}
print(n5Cnt / oddCnt) # 0.3282 # 조건부확률: 홀수일 때 5일 확률이다.


## 베이즈 정리
# 1종 오류, false positive, type 1 error 
# 2종 오류, false negative, type 2 error 
# 민감도(sensitivity): true positive 확률
# 특이도(specificity): true negative 확률

#  A인가? 에 대한 답은 positive(yes) , negative(no) 이다.
#    B       |     A        
#  1종오류율    민감도      positive
#  특이도       2종오류율   negative
# 
simCnt = 10000
prevalence = 0.001 # 유병률

sensitivity = 0.99 # 민감도
specificity = 0.99 # 특이도

positiveCnt = 0  # 양성자수
truePositiveCnt = 0  
falsePositiveCnt = 0 # 1종 오류

for(i in 1:simCnt) {
    disease = rbinom(1, 1, prevalence) # 유병률에 따라 감염 여부를 정한다.
    
    if(disease == 1) { # 감염된 경우, A
        # rbinom(a, b, c): c는 난수 1(positive)이 나올 확률이다.
        diagnosis = rbinom(1, 1, sensitivity) # 진단값
        if(diagnosis == 1) { # positive 이면
            positiveCnt = positiveCnt + 1
            truePositiveCnt = truePositiveCnt + 1
        }
    } else { # 감염 안 된 경우, B 
        diagnosis = rbinom(1, 1, 1 - specificity) 
        if(diagnosis == 1) {  
            positiveCnt = positiveCnt + 1
            falsePositiveCnt = falsePositiveCnt + 1 # 1종오류
        }
    }
}

print(positiveCnt)      #104
print(truePositiveCnt)  #  9
print(falsePositiveCnt) # 95

print(falsePositiveCnt / positiveCnt) # 0.91, 1종오류율


### 생일 역설: 30명 학생의 생일이 겹치지 않을 확률
studentCnt = 30
result = 1
for(i in 1:(studentCnt - 1)) {
    result = result * (365 - i) / 365
}
print(result) # 0.29