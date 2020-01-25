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
    card_type=chos_suit
  }else if(sum(tmp1)==0 & num_sp>0){
    card = min(tmp1_sp)
    card_type=4
  } else if(sum(tmp1)==0 & num_sp==0){
    num_nsp=nrow(as.matrix(player));
    nsp_ns=sample(num_nsp,1)
    tmp1_nsp=player[nsp_ns,]
    card=min(tmp1)
    card_type=find_suite(card) 
  }
  
  
  new_player<-as.matrix(player[!player == card])
  hand[1]=list(new_player)
  hand[2]=list(card)
  return(hand)
}
