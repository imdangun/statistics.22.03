## 조건부확률(conditional probability)
# 조건부확률 >= 주변확률
# 주변확률: 총시행횟수가 분모가 되는 확률이다.
# 베이즈 정리(bayes' theorem): P(B|A) = P(A|B) * P(B) / P(A)

# 주사위를 던져, 홀수 중에 5가 나올 확률을 구한다.: 조건부확률
sim_n = 10000
odd_n = 0
five_n = 0

for(i in 1:sim_n) {
    y = sample(1:6, 1) # 주사위를 던진다.
    if(y %% 2 == 1) odd_n = odd_n + 1 # 홀수가 나왔다.
    if(y == 5) five_n = five_n + 1
}

print(five_n / odd_n) # 0.3282 


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
#  1종오류율 + 특이도 = 민감도 + 2종오류율 = 1


##  코로나 검사 결과, 양성 중에서, 진양성은 10% 정도 뿐이다.
sim_n= 10000
prevalence = 0.001 # 유병률

sensitivity = 0.99 # 민감도
specificity = 0.99 # 특이도

positive_n = 0      # 양성수
truePositive_n = 0  # 진양성수
falsePositive_n = 0 # 1종오류수

for(i in 1:sim_n) {
    disease = rbinom(1, 1, prevalence) # 유병률에 따라 감염 여부를 정한다.
    
    if(disease == 1) { # 감염된 경우, A
        diagnosis = rbinom(1, 1, sensitivity) # 진단값(1:positive, 0:negative)
        if(diagnosis == 1) { # positive 이면
            positive_n = positive_n + 1
            truePositive_n = truePositive_n + 1
        }
    } else { # 감염 안 된 경우, B 
        diagnosis = rbinom(1, 1, 1 - specificity) 
        if(diagnosis == 1) {  
            positive_n = positive_n + 1
            falsePositive_n = falsePositive_n + 1 # 1종오류
        }
    }
}

positive_n       # 104
truePositive_n   #   9
falsePositive_n  #  95

falsePositive_n / positive_n # 0.91, 1종오류율


### 생일 역설: 30명 학생의 생일이 겹치지 않을 확률을 구한다.
student_n = 40
result = 1

for(i in 1:(student_n - 1)) {
    result = result * (365 - i) / 365
}

result # 0.29, 생일이 겹치지 않을 확률이다.
       # 0.10, 학생수가 30 -> 40 으로 증가하면, 생일이 겹치지 않을 확률이 떨어진다.