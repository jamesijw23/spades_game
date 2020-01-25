library(numbers)
#####
## Choose Cards for computer, just Max
#####
computer_play=function(player,chos_suit,spade_played){ 
  hand=list()
  ## 1) Determine if this is the first hand and if spades have been played
  if(chos_suit==0 & spade_played==0){
    su=runif(1)  
    chos_suit=suite_fun(su)
  } else if(chos_suit==0 & spade_played==1){
    su=runif(1)  
    chos_suit=suite_fun_ws(su)
  }
  
  
  ## 2) Gather the cards that is in the suite played
  tmp1 = find_suit_cards(chos_suit,player)
  
  
  
  ## 3) Determine Computer's (Strategy 1)
  tmp1_sp = as.matrix(player[player>39])
  num_sp = nrow(tmp1_sp)
  
  if(length(tmp1)>0){ ## C has suit
    card=max(tmp1)
  } else if(length(tmp1)==0 & num_sp>0){ ## C doesnt have suit &  has spades
    card = min(tmp1_sp)
  } else if(length(tmp1)==0 & num_sp==0){ ## C doesnt have suit &  no spades
    card=min(player) 
  }
  
  ## Hand without played hand
  new_player<-as.matrix(player[!player == card]) 
  hand[1]=list(new_player)
  hand[2]=list(card)
  return(hand)
}



#####
## Goes through each hand
#####
first_hand=function(all_players,turn1_name,spade_played){
  hand_play=list()
  colnames(all_players)<-c("North","East","South","West") ## Give Players names
  chosen_suit=0; ## ************ this should be 0 ************
  cards_chosen = list();
  ## 1) Who is going to go first
  turn1 = which(colnames(all_players) == turn1_name)
  turns = determine_turn(turn1)
  turn2 = turns$turn2
  turn3 = turns$turn3
  turn4 = turns$turn4
  
  
 
  ## 2) First Person Play
  cat('\n')
  cards_chosen = players_turn(turn1,all_players,chosen_suit,spade_played,cards_chosen)
  ## a. Suite determined for first hand
  suit_decided=find_suite(cards_chosen[[turn1]][2]) 
  
  ## 3) Second Person Play
  cards_chosen = players_turn(turn2,all_players,suit_decided,spade_played,cards_chosen)
  
  
  ## 4) Third Person Play
  cards_chosen = players_turn(turn3,all_players,suit_decided,spade_played,cards_chosen)
  
  ## 5) Fourth Person Play
  cards_chosen = players_turn(turn4,all_players,suit_decided,spade_played,cards_chosen)
  
  
  
  
  
  
  
  ## 6) Get all the cards played by everyone 
  cards_played=cbind(cards_chosen[[1]][2],cards_chosen[[2]][2],
                     cards_chosen[[3]][2],cards_chosen[[4]][2])
  colnames(cards_played) <- colnames(all_players)
  
  
  
  ## 7) Remaining Cards in players hand
  remaining_cards=cbind(cards_chosen[[1]][[1]],cards_chosen[[2]][[1]],
                        cards_chosen[[3]][[1]],cards_chosen[[4]][[1]])
  colnames(remaining_cards) <- colnames(all_players)
  
  ## 8) Determine if Spade had been played
  if(max(as.numeric(cards_played))>39){
    spade_played_o=1
  }else{
    spade_played_o=0
  }
  
  ## 9) Who Won the hand
  
  
  if(suit_decided == 1){ ## Clubs
    appropriate_cards = cards_played[, (cards_played < 14  | cards_played > 38) ]
    location_won = which.max(appropriate_cards)
    winning_card = as.numeric(appropriate_cards[location_won])
  } else if(suit_decided == 2){ ## Diamonds
    appropriate_cards = cards_played[, (cards_played < 27  | cards_played > 38) ]
    location_won = which.max(appropriate_cards)
    winning_card = as.numeric(appropriate_cards[location_won])
  }  else if(suit_decided == 3 | suit_decided == 4 ){ ## Spades or hearts
    appropriate_cards = cards_played
    location_won = which.max(appropriate_cards)
    winning_card = as.numeric(appropriate_cards[location_won])
  } 
  
  
  

  win_player = colnames(all_players)[location_won]
  win_card = display_card_only(winning_card)
  cat(paste0('Who Won: ',win_player,'; with: ', win_card, sep=""))
  

  

  
  ## Change Information
  hand_play[1]=list(remaining_cards)
  hand_play[2]=list(win_player)
  hand_play[3]=list(spade_played_o) 
  
  readline(prompt="Press Enter to Continue")
  return(hand_play)
}





##### 
## Plays the player's turn
#####
players_turn = function(turn,all_players,chosen_suit,spade_played,cards_chosen){
  if (turn!=3) {
    cards_chosen[turn]=list(computer_play(as.matrix(all_players[,turn]),chosen_suit,spade_played))
  } else { 
    cards_chosen[turn]=list(chose_card_user_input(as.matrix(all_players[,turn]),chosen_suit,spade_played))
  } 
  display_who_played(colnames(all_players)[turn], cards_chosen[[turn]][[2]]);
  return(cards_chosen)
}  






##### 
## Finds out whose turn it is based on  who
## won last
#####
determine_turn = function(turn1){
  turn2 = turn1+1;
  turn3 = turn1+2;
  turn4 = turn1+3;
  if (turn2 > 4 ) {turn2=turn2-4;}
  if (turn3 > 4) {turn3=turn3-4;}
  if (turn4 > 4) {turn4=turn4-4;}
  return(list(turn2=turn2,turn3=turn3,turn4=turn4))
}



#####
## Input Card from user (not computer)
#####
chose_card_user_input<-function(cards,suit,spad){
  tmp1=-9999
  hand <-list()
  while(tmp1<0){ ## Determine if input is eligible
    card.name=as.character(readline(prompt="Enter Card Name: "))
    tmp1=get.card(card.name)
    
    
    ## 1) Error Checking 
    
    ## a. Determine if card is in the set of cards
    detim <- tmp1 %in% cards  
    if(detim==F){
      tmp1=-9999
    }
    ## b. Determine if card played is a spade when the spade 
    ## Should not be played
    if (min(cards)<=39) {
      if(spad!=1 & tmp1>39 & suit==0){tmp1=-9995} 
    }
    ## c. Determine if the right Suit is being played 
    card.suite=(floor((tmp1-1)/13)+1) 
    if(card.suite!=suit){
      if (suit %in% (floor((cards-1)/13)+1))
      {tmp1=-9994} 
    }
    
    if(tmp1 == -9999){
      cat("Card is not in hand :( \n")
    } else if( tmp1 == -9998){
      cat("Not Appropriate Card :( \n")
    } else if( tmp1 == -9997){
      cat("Not an appropriate card number :( \n")
    } else if( tmp1 == -9996){
      cat("Not an appropriate card suit :( \n")
    }  else if( tmp1 == -9995){
      cat("Spades have not been Play :( \n")
    } else if( tmp1 == -9994){ 
      cat("Please play the appropriate suit  :( \n")
    } 
    
    
  }
  
  remaing_cards=as.matrix(cards[!cards == tmp1])
  hand[1] <- list(remaing_cards)
  hand[2] <- list(tmp1)
  return(hand)
}


#####
## Tranlate Card into Number (DONE)
#####
get.card=function(X){
  ## 1) Always make card a character
  X=as.character(X)
  
  ## 2) Split card into chucks of characters
  tmp=strsplit(X,"")
  
  ## 3) Check if Card is valid 
  if(length(tmp[[1]])>1 & length(tmp[[1]]) < 4 ){
    if(length(tmp[[1]])==3){
      tmp1 = tmp[[1]][1:2];
      tmp2=tmp[[1]][3];
      tmp1=paste(tmp1[1],tmp1[2],sep="")
    } else if (length(tmp[[1]])==2) { 
      tmp1=tmp[[1]][1];tmp2=tmp[[1]][2]
    }
  } else {
    return (-9998) ## Not Appropriate Card
  }
  
  ## 4) Finding Card's Number 
  if(tmp1==10){
    card_number = 10
  }else if (tmp1 == 'J' | tmp1 == 'j') {
    card_number = 11
  } else if (tmp1 == 'Q' | tmp1 == 'q') {
    card_number = 12
  } else if (tmp1 == 'K' | tmp1 == 'k'){
    card_number = 13
  } else if (tmp1 == 'A' | tmp1 == 'a') {
    card_number =14
  } else if (as.numeric(tmp1)<10 & as.numeric(tmp1)>1){
    card_number = as.numeric(tmp1)
  } else{
    return (-9997) ## Not an appropriate card number
  }
  card_number=(as.numeric(noquote(card_number))-1);
  ### 5) Finding Card's suit 
  if(tmp2=='C'| tmp2=='c'){
    suit_number=0
  } else if(tmp2=='D'| tmp2=='d'){
    suit_number=13
  } else if(tmp2=='H'| tmp2=='h'){
    suit_number=26
  } else if(tmp2=='S'| tmp2=='s'){
    suit_number=39
  } else{
    return (-9996) ## Not an appropriate card suit
  }
  
  ## 6) Find total card value
  card_total_number =suit_number+card_number;
  return(card_total_number)
}


#####
## Score System (DONE)
#####
score_function=function(winner,N_S,E_S,S_S,W_S){
  if(winner=="North") {N_S=N_S+10}
  if(winner=="East") {E_S=E_S+10}
  if(winner=="South") {S_S=S_S+10}
  if(winner=="West") {W_S=W_S+10}
  score_tog=cbind(N_S,E_S,S_S,W_S)
  return(score_tog)
}



#####
## Distribution of Deck (DONE)
#####
distrib_cards=function(val){
  deckk=as.matrix(sample(1:val))
  return(deckk)
}

#####
## find_suit_card Function(DONE)
## Finds the right cards based on suit
#####
find_suit_cards=function(chos_suit,player){
  ## 2) Gather the cards that is in the suite played
  if(chos_suit==1){
    tmp1 = as.matrix(player[player > 0 & player <=13,])
  } else if(chos_suit==2){
    tmp1 = as.matrix(player[player > 13 & player <=26,])
  } else if(chos_suit==3){
    tmp1 = as.matrix(player[player > 26 & player <=39,])
  } else{
    tmp1 = as.matrix(player[player > 39 & player <=52,])
  }
  return(tmp1)
}
#####
## Choose Suite(DONE)
#####
suite_fun=function(s){
  if(s<1/3) {suite=1}
  else if(s>=1/3 && s<=2/3) {suite=2}
  else {suite=3}
  return(suite)
}

suite_fun_ws=function(s){
  if(s<=0.25) {
    suite = 1
  } else if(s > 0.25 & s <= 0.50) {
    suite = 2
  } else if (s > 0.50 & s<=0.75) {
    suite = 3
  } else {
    suite = 4
  }
  return(suite)
}

#####
## Finds which suite was played by first player (DONE)
#####
find_suite=function(card_played){
  if(card_played<1){
    tmp=NA
  } else if(card_played<=13 ){
    tmp=1
  } else if(card_played>=14 && card_played<=26){
    tmp=2
  } else if(card_played>=27 && card_played<=39){
    tmp=3
  } else if(card_played>=40 && card_played<=52){
    tmp=4
  } else{
    tmp=NA
  }
  return(tmp)
}

#####
## Finds Cards Suit and Number based on number(DONE)
#####
display_card <- function(x) {
  suit = floor((x-1)/13);
  card = 2 + ( (x-1) %% 13 );
  if (card < 11) {
    cat(card)
   
  } else if (card==11) {
    cat('J')
   
  } else if (card==12) {
    cat('Q')
  
  } else if (card==13) {
    cat('K')
   
  } else {
    cat('A')
   
  }
  if (suit==0) {
    cat('c')
  
  } else if (suit==1) {
    cat('d')
   
  } else if (suit==2) {
    cat('h')
   
  } else {
    cat('s')
   
  }
 
}


display_card_only <- function(x) {
  suit = floor((x-1)/13);
  card = 2 + ( (x-1) %% 13 );
  if (card < 11) {
    value = card
  } else if (card==11) {
    value = 'J'
  } else if (card==12) {
    value = 'Q'
  } else if (card==13) {
    value = 'K'
  } else {
    value = 'A'
  }
  if (suit==0) {
    suit = 'c'
  } else if (suit==1) {
    suit = 'd'
  } else if (suit==2) {
    suit = 'h'
  } else {
    suit = 's'
  }
  return(paste(value,suit,sep=""))
}


#####
## Prints cards(DONE)
#####
display_who_played<-function(name,card) {
  cat(name)
  cat(' Played: ')
  display_card(card)
  cat('\n')
}

