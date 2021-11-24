secret_santa <-function(npeople, names){

  # this 'flag' is used to determine if the
  # function stays in or out of the while function
  flag = "bad"

  # first list of names
  namelist1 = matrix(names, ncol = 1, nrow = npeople)
  fam = matrix(ncol = 1, nrow = npeople, NA)

  while (flag == "bad"){

    # names to choose from
    namelist2 = matrix(ncol = 1, nrow = npeople, NA)

    for (i in 1:npeople){
      #pick the first name
      if (i==1){
        xx2 = sample(names, (npeople-i+1), replace=FALSE)
      } else
        xx2 = sample(xx2, (npeople-i+1), replace=FALSE)

      if (i == npeople & xx2[1]==namelist1[i,1]){
        flag = "bad"

      }else if(xx2[1]!= namelist1[i,1]){
        namelist2[i,1] = xx2[1]
        flag = "good"
      } else{
        namelist2[i,1] = xx2[2]
        flag = "good"
      }

      #set up the new matrix with one less name
      used = which(xx2==namelist2[i])
      xx2[used] = "zzzzz"
      xx2 = sort(xx2)[1:(npeople-i)]
    }

    #flag
    #add "has" to the matrix
    has = matrix(ncol=1, nrow = npeople, "has")

    #build the final matrices
    final = cbind(namelist1, has, namelist2)
    #the final results
    #final

  }
  final
}


nam<-c("A", "B", "C", "D")

secret_santa(npeople=4, names=nam)

