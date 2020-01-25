##-------------------------------------------------------------------------------
## Creation of the Spades Game and how it works. 
## There 52 cards and we let numbers denote what they are by:
## The order goes 0 to 12 are Clubs
## The order goes 13 to 25 are Diamonds
## The order goes 26 to 38 are Hearts
## The order goes 39 to 51 are Spades
## The Steps of generating the code is to:
## 1) Distribute the deck randomly
## 2) Determines the suite of the card (to include or not spades )
## 3) Finds Cards Suit and Number based on numeric system from 1 to 52
## 4) Displays who played which card 
## 5) Finds cards within hand from a particular suite
## 6) Finds which suite was played by first player in hand
## 7) Add 10 points to winner of hand
## 8) Determine number that card represents 
## 9) Determine if user input is eligible based on current cards
## 10) Determine card for computer based on hand
##-------------------------------------------------------------------------------



##-------------------------------------------------------------------------------
## 1) Distribute the deck randomly

distrib_cards=function(val){
  deckk=as.matrix(sample(1:val))
  return(deckk)
}
## Test
test.cards=distrib_cards(52)
##-------------------------------------------------------------------------------
## 2) Determines the suite of the card (not to include spades)
suite_fun=function(s){
  if(s<1/3) {
    suite=1
  } else if(s>1/3 && s<2/3) {
    suite=2
  } else {
    suite=3
  }
  return(suite)
}
tmp1= runif(1);tmp1
suite_fun(tmp1)



suite_fun_ws=function(s){
  if((s>0 & s<=0.25)) {
    suite = 1
  } else if(s>0.25 & s<=0.50) {
    suite = 2
  } else if (s>0.50 & s<=0.75) {
    suite = 3
  } else {
    suite = 4
  }
  return(suite)
}

##-------------------------------------------------------------------------------
## 3) Finds Cards Suit and Number based on numeric system from 1 to 52
display_card <- function(x) {
  if(x < 1 | x >52){
    cat("Invalid Card")
  } else {
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
}
## Test
for(i in 0:100){
  tmp= display_card(i)
  cat(tmp,"\n")
}
##-------------------------------------------------------------------------------
## 4) Displays who played which card 
display_who_played<-function(name,card) {
  cat(name)
  cat(' Played: ')
  display_card(card)
  cat('\n')
}
display_who_played("South",12)
display_who_played("South",72)
display_who_played("South",0)
##-------------------------------------------------------------------------------
## 5) Finds cards within hand from a particular suite
less_great_fun=function(less,great,stack){
  emp_vec=vector()
  for(i in 1:nrow(stack)){
    tmp=stack[i,]
    if(tmp<=great && tmp>less){emp_vec=rbind(emp_vec,tmp)}
  }
  return(emp_vec)
}
set.seed(1)
deck = distrib_cards(52)
cards = deck[1:13,]
less_great_fun(0,13,as.matrix(cards))


##-------------------------------------------------------------------------------
## 6) Finds which suite was played by first player in hand
find_suite=function(card_played){
  if(card_played<=1){
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
find_suite(10)
find_suite(53)
find_suite(0)
##-------------------------------------------------------------------------------
## 7) Add 10 points to winner of hand
score_function=function(winner,N_S,E_S,S_S,W_S){
  if(winner==1) {N_S=N_S+10}
  if(winner==2) {E_S=E_S+10}
  if(winner==3) {S_S=S_S+10}
  if(winner==4) {W_S=W_S+10}
  score_tog=cbind(N_S,E_S,S_S,W_S)
  return(score_tog)
}
score_function(1,10,10,10,10)
score_function(2,10,10,10,10)
score_function(3,10,10,10,10)
score_function(4,10,10,10,10)
##-------------------------------------------------------------------------------
## 8) Determine number that card represents 
get.card=function(X){
  X=as.character(X)
  tmp=strsplit(X,"")
  if(length(tmp[[1]])>1 & length(tmp[[1]]) < 4 ){
    if(length(tmp[[1]])==3){
      tmp1=tmp[[1]][1:2];
      tmp2=tmp[[1]][3];
      tmp1=paste(tmp1[1],tmp1[2],sep="")
    } else if (length(tmp[[1]])==2) { 
      tmp1=tmp[[1]][1];tmp2=tmp[[1]][2]}
  } else {
    return (NA)
  }
  
 if(tmp1==10){
    tmp1=10
  }else if (tmp1=='J') {
    tmp1="11"
  } else if (tmp1=='Q') {
    tmp1="12"
  } else if (tmp1=='K'){
    tmp1="13"
  } else if (tmp1=='A') {
    tmp1="14"
  } else if (as.numeric(tmp1)<10){
    tmp1=tmp1
  } else{
    tmp1=NA
  }
  
  ## Determine Suit
  if(tmp2=='c'){
    emp=0
  } else if(tmp2=='d'){
    emp=13
  } else if(tmp2=='h'){
    emp=26
  } else if(tmp2=='s'){
    emp=39
  } else{
    emp=NA
  }
  number=(as.numeric(noquote(tmp1))-1);
  if (number>13 | number<1){
    number=NA
  }
  emp1=emp+number;
  return(emp1)
}
data.cards = read.table("all_cards.txt",header=F)
for(i in 1:52){
  cat(get.card(as.character(data.cards[i,1])),"\n")
}
##-------------------------------------------------------------------------------
## 9) Determine if user input is eligible based on current cards
chose_card_user_input<-function(cards,suit,spad){
  tmp1=NA
  hand <-list()
  while(is.na(tmp1)==T){ ## 
    ## 1) Get the card from user and determine the number
    card.name=as.character(readline(prompt="Enter Card Name: "))
    tmp1=get.card(card.name)

    if ((tmp1 %in% cards)==F){ 
      ## 2) Determine if card is in the set of cards
      tmp1=NA
    } else if((floor((tmp1-1)/13)+1)!=suit & suit %in% (floor((cards-1)/13)+1)==T ){
      ## 3) Determine if a card is not in right suite but you still have that suite in hand
      tmp1=NA
    } else if (min(cards) <= 39 & spad!=1 & tmp1>39 & (floor((tmp1-1)/13)+1)!=suit){
      ## 4) Determine if card played is a spade when the spade should not be played
      tmp1=NA
    } else if (suit==0) {
      tmp1=tmp1
    } else{
      tmp1=tmp1
    }
    

    
    
  }
  ccd=as.matrix(cards[!cards == tmp1])
  hand[1] <- list(ccd)
  hand[2] <- list(tmp1)
  return(hand)
}


##-------------------------------------------------------------------------------
## 10) Determine card for computer based on hand
chose_card_no_spade=function(player,chos_suit,spade_played){ 
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
  if(chos_suit==1){
    tmp1=less_great_fun(0,13,player)
  } else if(chos_suit==2){
    tmp1=less_great_fun(13,26,player)
  } else if(chos_suit==3){
    tmp1=less_great_fun(26,39,player)
  } else{
    tmp1=less_great_fun(39,52,player)
  }
  
  
  ## 3) Determine Computer's Card
  tmp1_sp=as.matrix(player[player>39])
  num_sp = nrow(tmp1_sp)
  
  if(sum(tmp1)>0){
    card=max(tmp1)
  }else if(sum(tmp1)==0 & num_sp>0){
    card = min(tmp1_sp)
  } else if(sum(tmp1)==0 & num_sp==0){
    num_nsp=nrow(as.matrix(player));
    nsp_ns=sample(num_nsp,1)
    tmp1_nsp=player[nsp_ns,]
    card=min(tmp1) 
  }
  
  
  new_player<-as.matrix(player[!player == card])
  hand[1]=list(new_player)
  hand[2]=list(card)
  return(hand)
}

