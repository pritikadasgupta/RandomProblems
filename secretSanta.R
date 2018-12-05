# hmmm i'm thinking about a probability question
# we were discussing secret santa
# and we're doing that thing where everyone puts their name in a bowl, and we all draw a name
# pranay asked "what happens if you draw your own name?" and i said "you put it back"
# "it shouldn't normally be a problem unless you were the second-to-last person to draw your own name, because when you put it back, you know the last person will draw your own name"
# and i was trying to think of the probability, given N people, of that scenario - someone drawing their own name on the second-to-last draw
# but that seems kinda hard, even in a specific case (i'm thinking about 4 separate people to start)

myfunction <- function(nPeople,nSimulations){
  options = 1:nPeople
  df = data.frame(matrix(, nrow=nSimulations, ncol=nPeople))
  for (i in 1:nSimulations){
    for (j in 1:nPeople){
      if(j<(nPeople-1)){
        options = options[options != j]#can't pick themselves 
        df[i,j] = sample(options,1)#picks random
        options = append(options,j)#adds themselves back in
        options = options[options != df[i,j]]#takes out what they've picked
      }
      
      if (j>=(nPeople-1)){
        df[i,j] = sample(options,1)#picks random
        if (df[i,j]==(nPeople-1)){
          options = append(options,j)#adds themselves back in
        }
        options = options[options != df[i,j]]#takes out what they've picked
      }
      
    }
    options = 1:nPeople
  }
  # probability = as.numeric(table(unlist(df[,3]))[3])/nSimulations
  probability = length(which(df[,(nPeople-1)] == (nPeople-1)))/nSimulations
  return(probability)
}

#for 100 simulations with 4 ppl
nPeople = 4
nSimulations = 100
myfunction(nPeople,nSimulations)

#for 1000 simulations with 4 ppl
nPeople = 4
nSimulations = 1000
myfunction(nPeople,nSimulations)



# now lets look over lots of sets of simulations and see what it converges to
myfunction2 <- function(nPeople,nSimulations){
  options = 1:nPeople
  myList=c()
  for (i in 1:nSimulations){
    for (j in 1:nPeople){
      if(j<(nPeople-1)){
        options = options[options != j]#can't pick themselves 
        pick = sample(options,1)#picks random
        options = append(options,j)#adds themselves back in
        options = options[options != pick]#takes out what they've picked
      }
    
      if (j>=(nPeople-1)){
        pick = sample(options,1)#picks random
        if (pick==(nPeople-1)){
          options = append(options,j)#adds themselves back in
        }
        options = options[options != pick]#takes out what they've picked
        myList=append(myList,pick)
      }
    }
    options = 1:nPeople
  }

  probability = length(which(myList == (nPeople-1)))/nSimulations
  return(probability)
}


nPeople = 4
n_nSimulations = 1500
probabilities = c()
for (k in 1:n_nSimulations){
  probabilities = append(probabilities,myfunction2(nPeople,k))
}

plot(1:n_nSimulations,probabilities, xlab="number of simulations",ylab="probability")
abline(h = mean(probabilities),col="red")
mean(probabilities)
