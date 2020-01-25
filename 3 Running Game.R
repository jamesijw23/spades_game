

#####
## Scores
#####
N_S=0
E_S=0
S_S=0
W_S=0

Main_N_S=0
Main_E_S=0
Main_S_S=0
Main_W_S=0


#####
## Organization
#####


## First Hand

count=0
max_score=0
tmp_score=cbind(0,0,0,0);
while(max_score<100){
  deck=distrib_cards(52)
  North=as.matrix(sort(deck[1:13,]))
  South=as.matrix(sort(deck[14:26,]))
  East=as.matrix(sort(deck[27:39,]))
  West=as.matrix(sort(deck[40:52,]))
  ## all_players combines all players into a matrix
  all_players=cbind(North,East,South,West)
  colnames(all_players)<-c("North","East","South","West")
  ## 1:all the cards; 2:who goes first; 
  ## 3:has a spaded played yet; 4:actual hand
  hand_mam=list()
  ## 1: players cards 2: Whose turn 3: if a spade has been played
  hand_mam[1]=list(all_players);hand_mam[2]=list("North");hand_mam[3]=list(0)
  #####
  ## Scores
  #####
  N_S=0
  E_S=0
  S_S=0
  W_S=0
  for( i in 1:13){ 
    print.hand(hand_mam[[1]]) 
    hand_mam=first_hand(hand_mam[[1]],hand_mam[[2]],hand_mam[[3]])
  

    tmp_score=score_function(hand_mam[2],N_S,E_S,S_S,W_S)
    N_S=tmp_score[1,1];E_S=tmp_score[1,2]
    S_S=tmp_score[1,3];W_S=tmp_score[1,4]
#     print("Next Hand")
#     print("Round:");print(cbind(N_S,E_S,S_S,W_S))
#print('all_players:')
#print(all_players)
#print('hand_mam[1]:')
#print(hand_mam[[1]])
  }
  Main_N_S=Main_N_S+N_S;Main_E_S=Main_E_S+E_S
  Main_S_S=Main_S_S+S_S;Main_W_S=Main_W_S+W_S
  all_scores=cbind(Main_N_S,Main_E_S,
                   Main_S_S,Main_W_S)
#   print("Current Score:");print(all_scores)
  max_score=max(all_scores) 
  count=count+1

#  print(count)
}
