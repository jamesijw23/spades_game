print.hand <- function(all_players){
  ncardsN=length(as.matrix(all_players[,1]))
  ncardsE=length(as.matrix(all_players[,2]))
  ncardsS=length(as.matrix(all_players[,3]))
  ncardsW=length(as.matrix(all_players[,4]))
  
#  print(cbind(ncardsN,ncardsE,ncardsS,ncardsW))
  if (ncardsN!=0) {
  
  cat('\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n')
  cat('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n')
  cat('%              Score Board                    \n')
  cat('%   North:', tmp_score[1], 'South:', tmp_score[3], 
      'East:',  tmp_score[2],'West:',  tmp_score[4], '\n')
  cat('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n')
  cat('\n\n\n')
 
  cat('                     N\n')
  cat('               ')
  for (i in 1:13){
    if (i<=ncardsN) {
      cat('*')
    } else {
      cat(' ')
    }
  }
  cat('\n')
  cat('      W                            E\n')
  for (i in 1:13){
    if (i<=ncardsW) {
      cat('*')
    } else {
      cat(' ')
    }
  }
  cat('                ')
  for (i in 1:13){
    if (i<=ncardsE) {
      cat('*')
    } else {
      cat(' ')
    }
  }
  cat('\n')
  cat('                     S\n')

 cat('   ')
 X=as.matrix(all_players[,3])
 for(i in 1:length(X)){
   display_card(X[i,1])
   cat(' ')
 }
 
  
#   print('                      N\n')
#   print('               **************')
#   print('      W                            E\n')
#   print('***************              *************')
#   print('                      S\n')
  }
}


 

