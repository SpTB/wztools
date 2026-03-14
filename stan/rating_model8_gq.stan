//reverse order in pred (0/100 bin first; 50 second) -> doesn't do anything (as expected)
//smaller prec range

data {

  //task skeleton
  int<lower=1> numSubj;
  int<lower=1> numGames;
  int<lower=1> numTrials;
 // array[numSubj, numGames] int<lower=1, upper=2> experience; //1: High exp 2: low exp (novice)

  array[numSubj, numGames, numTrials] int<lower=0, upper=1> is_rating; // (binary) whether a rating trial
  array[numSubj, numGames] int<lower=0, upper=1> reward; // 1:High rewad 0: low reward
  //behav
 // array[numSubj, numGames, numTrials] int<lower=-2, upper=2> choice; // 1:good arm; 2:bad arm, -1: observe good arm; -2: observe bad arm
  //array[numSubj, numGames, numTrials] real<lower=-5, upper=1> rating; // [0, 1] (signed to armA); -5: no rating
  array[numSubj, numGames, numTrials] int<lower=-2, upper=2> choice; // 1:good arm; 2:bad arm, -1: observe good arm; -2: observe bad arm

  //all are (approximately) standardized
  array [numSubj, numGames, numTrials] real dift; //difference
  array [numSubj, numGames, numTrials] real sumt;
//  array [numSubj, numGames, numTrials] real sumrelt; //relative sum of Q values (relative to context)
  array [numSubj, numGames, numTrials] real cbt; //choice dummy, relative to better arm: 1 = armA; -1=armB; 0 = no choice
  array [numSubj, numGames, numTrials] real rbt; //context-dependent (relative) observered reward, sign dependent
  array[numSubj, numGames, numTrials] int<lower=-100, upper=150> outcome; // for arm A

  //fixing

  real fix_d0;
  real fix_dDIF;
  real fix_rDIF;
  real fix_rSUM;
  real fix_rCB;
  real fix_rRB;
  real fix_precH;
  real fix_precL;
 // real fix_bint;
 // real fix_rCON;

 //simulated pars
  array[numSubj] real <lower=0.0, upper=1.0> d0_raw;
  array[numSubj] real <lower=0.0, upper=1.0> d50_raw;
  array[numSubj] real <lower=0.0, upper=1.0> dDIF_raw;
  array[numSubj] real <lower=0.0, upper=1.0> d50DIF_raw;

  array[numSubj] real <lower=0.0, upper=1.0> rDIF_raw;
  array[numSubj] real <lower=0.0, upper=1.0> rSUM_raw;
  array[numSubj] real <lower=0.0, upper=1.0> rCB_raw;
  array[numSubj] real <lower=0.0, upper=1.0> rRB_raw;

  array[numSubj] real <lower=0.0, upper=1.0> precH_raw;
  array[numSubj] real <lower=0.0, upper=1.0> precL_raw;
  array[numSubj] real <lower=0.0, upper=1.0> bint_raw;

}

transformed parameters{
  array[numSubj] real<lower=-10, upper=0> d0;
  array[numSubj] real<lower=-10, upper=0> d50;
  array[numSubj] real<lower=0, upper=5> dDIF;
  array[numSubj] real<lower=0, upper=5> d50DIF;

  array[numSubj] real<lower=0, upper=2> rDIF;
  array[numSubj] real<lower= 0, upper=1> rSUM;
  array[numSubj] real<lower= 0, upper=1> rCB;
  array[numSubj] real<lower= 0, upper=1> rRB;

  array[numSubj] real<lower=10, upper=40> precH;
  array[numSubj] real<lower=10, upper=40> precL;
  array[numSubj] real<lower=-0, upper=.5> bint;

     //fixing
  for (s in 1:numSubj) {

     if (fix_d0    ==-1) d0[s]   = d0_raw[s]   * 10 - 10; else d0[s] = fix_d0;
    d50[s]   = d50_raw[s] * 10 - 10;
    if (fix_dDIF  ==-1) dDIF[s] = dDIF_raw[s] *  5;      else dDIF[s] = fix_dDIF;
    d50DIF[s] = d50DIF_raw[s] *  5;
    if (fix_rDIF  ==-1) rDIF[s] = rDIF_raw[s] *  2;      else rDIF[s] = fix_rDIF;
    if (fix_rSUM  ==-1) rSUM[s] = rSUM_raw[s] * 1;  else rSUM[s] = fix_rSUM;
    if (fix_rCB   ==-1) rCB[s]  = rCB_raw[s]  *  1;      else rCB[s] = fix_rCB;
    if (fix_rRB   ==-1) rRB[s]  = rRB_raw[s]  *  1;      else rRB[s] = fix_rRB;
    if (fix_precH  ==-1)precH[s]  = precH_raw[s]  *  30+10; else precH[s]=fix_precH;
    if (fix_precL  ==-1)precL[s]  = precL_raw[s]  *  30+10;  else precL[s]=fix_precL;
    bint[s]   = bint_raw[s]* .5;
    //if (fix_rCON  ==-1) rCON[s] = rCON_raw[s] *1 -.5;      else rCON[s] = fix_rCON;
  }

  }

generated quantities {
  array[numSubj, numGames, numTrials] real r_pred;

  for (s in 1 : numSubj) {
    for (g in 1 : numGames) {

      real prect = precH[s];
      if (reward[s,g]==0) prect = precL[s];

      for (t in 1:numTrials) {
        //CONFIDENCE RATINGS

        if (is_rating[s,g,t]==1) {

          real rating_direction;
           //binary process 2 (hundo)?
          real diff_reg_bin2;
          diff_reg_bin2 = d0[s] + dDIF[s] * abs(dift[s,g,t]);
          real bin2 = bernoulli_logit_rng(diff_reg_bin2);
          //print("bin2:");
          //print(bin2);
          //need separate loop for this, maybe?

          if (bin2==1){
            if (dift[s,g,t]==-99) {
            r_pred[s,g,t] = -99;
          } else if (dift[s,g,t]>0) {
            r_pred[s,g,t] = 1;
             rating_direction = 1;
          } else {
            r_pred[s,g,t] = 0;
            rating_direction = -1;
          }


          //non-50 cases:
          } else if (bin2 == 0) {
            real diff_reg_bin1 = d50[s] - d50DIF[s] * abs(dift[s,g,t]);
            real bin1 = bernoulli_logit_rng(diff_reg_bin1);
           // print("bin1:");
             //print(bin1);
            if (bin1 == 1) {
             r_pred[s,g,t]=.5;
             rating_direction = 0;
            } else if (bin1==0) {

            //continuous response

              // real context_reg = rCON[s] * reward_trans[s,g] *rating_direction[s,g,t];
               real diff_reg    = rDIF[s]*dift[s,g,t];
               real sum_reg     = rSUM[s]*sumt[s,g,t];
               real choice_reg  = rCB [s]*cbt[s,g,t];
               real reward_reg  = rRB[s] *(choice[s,g,t]>0)*(outcome[s,g,t]>0);
               real bint_reg = choice_reg*reward_reg;
               real r_mean =  diff_reg + sum_reg + choice_reg + reward_reg + bint_reg;

               r_pred[s,g,t] = beta_proportion_rng(inv_logit(r_mean), prect);
               //estimate this only for non-discrete cases
             }
           }

        }

      } //t-loop

    } //g-loop

  } //s-loop

}





