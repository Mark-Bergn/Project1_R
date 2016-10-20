#Project 1,Question 3;

#A = Have Cancer
#Ac = Don't have cancer
#T = Positive Test Result;
#T.A = T|A = 99% accuracy of test
#T.Ac = T|AC = 10% false positive

pr.v<-vector()
for (i in 2:10){
  T.A=0.99
  T.Ac = 0.1
  pr.v[1]<- (0.01)
  pr.v[i]<-(pr.v[i-1]*T.A) / ((pr.v[i-1]*T.A) + (1-pr.v[i-1])*T.Ac)
  
}
pr.v
pr.v[2]
Trial<-c(0:9)
plot(Trial,pr.v,)
#probability of having breast cancer > 95% after 4 positive test results;
?plot
#For a negative Test result in Test 3;

prior.2<-0.49748744

pr.3c<-prior.2*(1-T.A)/((prior.2*(1-T.A)+(1-prior.2)*(1-T.Ac)))

pr.3c#New probabilty of cancer; 


#Given prior = pr.3c
pr.v2<-vector()
for (i in 2:10){
  T.A=0.99
  T.Ac = 0.1
  pr.v2[1]<- (pr.3c)
  pr.v2[i]<-(pr.v2[i-1]*T.A) / ((pr.v2[i-1]*T.A) + (1-pr.v2[i-1])*T.Ac)
  
}
pr.v2

Trial<-c(0:9)
plot(Trial, pr.v2)

#Plot for Scenario 3;
pr.v3<-c(pr.v[1:2],pr.v2)
t<-c(1:12)
plot(t,pr.v3)


