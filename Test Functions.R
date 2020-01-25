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



test_func = function(X){
  
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
    tmp1=  (-9998) ## Not Appropriate Card
  }
  
  ## 4) Finding Card's Number 
  if(tmp1==10){
    card_number = 10
  }else if (tmp1=='J') {
    card_number = 11
  } else if (tmp1=='Q') {
    card_number = 12
  } else if (tmp1=='K'){
    card_number = 13
  } else if (tmp1=='A') {
    card_number =14
  } else if (as.numeric(tmp1)<10 & as.numeric(tmp1)>1){
    card_number = as.numeric(tmp1)
  } else{
    card_number = (-9997) ## Not an appropriate card number
  }
  card_number=(as.numeric(noquote(card_number))-1);
  
  ### 5) Finding Card's suit 
  if(tmp2=='c'){
    suit_number=0
  } else if(tmp2=='d'){
    suit_number=13
  } else if(tmp2=='h'){
    suit_number=26
  } else if(tmp2=='s'){
    suit_number=39
  } else{
    suit_number=NA
  }
  
  ## 6) Find total card value
  card_total_number =suit_number+card_number
  return(card_total_number) 
}



cards_test = c(seq(2,10),'J','Q','K','A')
d_clubs <- paste(cards_test,'c',sep="")
d_diamonds <- paste(cards_test,'d',sep="")
d_hearts <- paste(cards_test,'h',sep="")
d_spades <- paste(cards_test,'s',sep="")
all_cards = rbind(as.matrix(d_clubs),
                  as.matrix(d_diamonds),
                  as.matrix(d_hearts),
                  as.matrix(d_spades))
plot(apply(all_cards,1,test_func))
abline(a=0,b=1)      
