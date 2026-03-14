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
  array[numSubj, numGames, numTrials] real<lower=-5, upper=1> rating; // [0, 1] (signed to armA); -5: no rating
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
 // real fix_rCON;

}

transformed data{
  array[numSubj, numGames, numTrials] int<lower=-1, upper=1> is_discrete_rating; // 0: continuous rating (0,1) 1: binary rating [0 ^ 1]; -1: no rating
  array[numSubj, numGames, numTrials] int<lower=-1, upper=1> is_50_rating; //  1: rating=.5; 0: any other rating; -1: no rating
  array[numSubj, numGames, numTrials] int<lower=-1, upper=1> rating_direction; // 0: continuous rating (0,1) 1: binary rating [0 ^ 1]; -1: no rating
  array[numSubj, numGames] int<lower=-1, upper=1> reward_trans; //-1: low reward; 1: high reward
  for (s in 1:numSubj) {
    for(g in 1:numGames) {
      for(t in 1:numTrials) {

        if (rating[s,g,t]==0) { //min-case
          is_discrete_rating[s,g,t] = 1;
          is_50_rating[s,g,t] = 0;
          } else if (rating[s,g,t]==1) { //max-case
            is_discrete_rating[s,g,t] = 1;
            is_50_rating[s,g,t] = 0;
          } else if (rating[s,g,t]==.5) { //mid-case
            is_discrete_rating[s,g,t] = 0;
             is_50_rating[s,g,t] = 1;
          } else if(rating[s,g,t]==-5) { //no-rating case
            is_discrete_rating[s,g,t] = -1;
            is_50_rating[s,g,t] = -1;
          } else { //(0,1) beta-case
            is_discrete_rating[s,g,t] = 0;
            is_50_rating[s,g,t] = 0;
          }

          if(rating[s,g,t]>0.5) {
            rating_direction[s,g,t] = 1;
          }else if (rating[s,g,t]<0.5) {
            rating_direction[s,g,t] = -1;
          } else {
            rating_direction[s,g,t] = 0;
          }
        } //t-loop

    }//g-loop
  }// s-loop

  for (s in 1:numSubj) {
    for(g in 1:numGames) {
      if(reward[s,g]==0) reward_trans[s,g]=-1; else reward_trans[s,g] = reward[s,g];
    }
  }
}


parameters {

  //beta pars
  real <lower=0> a_d0_raw;
  real <lower=0> a_d50_raw;//intercept (base-rate) how often one does choose 50 on rating scale (indecision case)
  real <lower=0> a_dDIF_raw;
  real <lower=0> a_d50DIF_raw; //value-difference sensitivity to respond '50'

  real <lower=0> a_rDIF_raw;
  real <lower=0> a_rSUM_raw;
  real <lower=0> a_rCB_raw;
  real <lower=0> a_rRB_raw;

  real <lower=0> a_precH_raw;
  real <lower=0> a_precL_raw;

  real <lower=0> a_bint_raw; //absolute-confidence intercept
 // real <lower=0> a_rCON_raw; //context effect

  real <lower=0> b_d0_raw;
  real <lower=0> b_d50_raw;
  real <lower=0> b_dDIF_raw;
  real <lower=0> b_d50DIF_raw;

  real <lower=0> b_rDIF_raw;
  real <lower=0> b_rSUM_raw;
  real <lower=0> b_rCB_raw;
  real <lower=0> b_rRB_raw;

  real <lower=0> b_precH_raw;
  real <lower=0> b_precL_raw;

  real <lower=0> b_bint_raw;
 // real <lower=0> b_rCON_raw;
  //individual pars

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
 // array[numSubj] real <lower=0.0, upper=1.0> rCON_raw;
}

transformed parameters {

//hypers transform
  real <lower=1> a_d0;
  real <lower=1> a_d50;
  real <lower=1> a_dDIF;
  real <lower=1> a_d50DIF;

  real <lower=1> a_rDIF;
  real <lower=1> a_rSUM;
  real <lower=1> a_rCB;
  real <lower=1> a_rRB;

  real <lower=1> a_precH;
  real <lower=1> a_precL;

  real <lower=1> a_bint;
  //real <lower=1> a_rCON;

  real <lower=1> b_d0;
  real <lower=1> b_d50;
  real <lower=1> b_dDIF;
  real <lower=1> b_d50DIF;
  real <lower=1> b_rDIF;
  real <lower=1> b_rSUM;
  real <lower=1> b_rCB;
  real <lower=1> b_rRB;
  real <lower=1> b_precH;
  real <lower=1> b_precL;
  real <lower=1> b_bint;
 // real <lower=1> b_rCON;

//mus
  real<lower=-10, upper=0> mu_d0;
  real<lower=-10, upper=0> mu_d50;
  real<lower=0, upper=5> mu_dDIF;
  real<lower=0, upper=5> mu_d50DIF;

  real<lower=0, upper=2> mu_rDIF;
  real<lower= 0, upper=1> mu_rSUM;
  real<lower= 0, upper=1> mu_rCB;
  real<lower= 0, upper=1> mu_rRB;

  real<lower=10, upper=40> mu_precH;
  real<lower=10, upper=40> mu_precL;

  real<lower= 0, upper=.5> mu_bint;
//  real<lower= -.5, upper=.5> mu_rCON;
//subj

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
 // array[numSubj] real<lower=-.5, upper=.5> rCON;

//hyper
  a_d0 = a_d0_raw+1;
  a_d50 = a_d50_raw+1;
  a_dDIF = a_dDIF_raw+1;
  a_d50DIF = a_d50DIF_raw+1;
  a_rDIF = a_rDIF_raw+1;
  a_rSUM = a_rSUM_raw + 1;
  a_rCB = a_rCB_raw+1;
  a_rRB = a_rRB_raw+1;
  a_precH = a_precH_raw+1;
  a_precL = a_precL_raw+1;
  a_bint = a_bint_raw+1;
 // a_rCON = a_rCON_raw+1;

  b_d0 = b_d0_raw+1;
  b_d50 = b_d50_raw+1;
  b_dDIF = b_dDIF_raw+1;
  b_d50DIF = b_d50DIF_raw+1;
  b_rDIF = b_rDIF_raw+1;
  b_rSUM = b_rSUM_raw + 1;
  b_rCB = b_rCB_raw+1;
  b_rRB = b_rRB_raw+1;
  b_precH = b_precH_raw+1;
  b_precL = b_precL_raw+1;
  b_bint = b_bint_raw+1;
 // b_rCON = b_rCON_raw+1;


  mu_d0   = (a_d0   /(a_d0   + b_d0  )) * 10 - 10;
  mu_d50   = (a_d50   /(a_d50   + b_d50  )) * 10 - 10;
  mu_dDIF = (a_dDIF /(a_dDIF + b_dDIF)) * 5;
  mu_d50DIF = (a_d50DIF /(a_d50DIF + b_d50DIF)) * 5;
  mu_rDIF = (a_rDIF /(a_rDIF + b_rDIF)) * 2;
  mu_rSUM = (a_rSUM /(a_rSUM + b_rSUM)) * 1;
  mu_rCB  = (a_rCB  /(a_rCB  + b_rCB )) * 1;
  mu_rRB  = (a_rRB  /(a_rRB  + b_rRB )) * 1;
  mu_precH  = (a_precH  /(a_precH  + b_precH )) * 30+10;
  mu_precL  = (a_precL  /(a_precL  + b_precL )) * 30+10;
  mu_bint   = (a_bint   /(a_bint   + b_bint  ))* .5 ;
  //mu_rCON   = (a_rCON   /(a_rCON   + b_rCON  ))* 1 -0.5 ;

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


model {

  //draw hypers

  a_d0_raw    ~ gamma(1,1);
  a_d50_raw    ~ gamma(1,1);
  a_dDIF_raw  ~ gamma(1,1);
  a_d50DIF_raw  ~ gamma(1,1);
  a_rDIF_raw  ~ gamma(1,1);
  a_rSUM_raw  ~ gamma(1,1);
  a_rCB_raw   ~ gamma(1,1);
  a_rRB_raw   ~ gamma(1,1);
  a_precH_raw   ~ gamma(1,1);
  a_precL_raw   ~ gamma(1,1);
  a_bint_raw    ~ gamma(1,1);
 // a_rCON    ~ gamma(1,1);

  b_d0_raw    ~ gamma(1,1);
  b_d50_raw    ~ gamma(1,1);
  b_dDIF_raw  ~ gamma(1,1);
  b_d50DIF_raw  ~ gamma(1,1);
  //rat inference parameters
  b_rDIF_raw  ~ gamma(1,.1);//scept...
  b_rSUM_raw  ~ gamma(1,.1);
  b_rCB_raw   ~ gamma(1,.1);
  b_rRB_raw   ~ gamma(1,.1);
  //
  b_precH_raw   ~ gamma(1,1);
  b_precL_raw   ~ gamma(1,1);
  b_bint_raw    ~ gamma(1,.1);
  //b_rCON    ~ gamma(1,1);

  for (s in 1 : numSubj) {

    d0_raw   [s] ~ beta(a_d0  , b_d0  );
    d50_raw   [s] ~ beta(a_d50  , b_d50  );
    dDIF_raw [s] ~ beta(a_dDIF, b_dDIF);
    d50DIF_raw [s] ~ beta(a_d50DIF, b_d50DIF);
    rDIF_raw [s] ~ beta(a_rDIF, b_rDIF);
    rSUM_raw [s] ~ beta(a_rSUM, b_rSUM);
    rCB_raw  [s] ~ beta(a_rCB , b_rCB );
    rRB_raw  [s] ~ beta(a_rRB , b_rRB );
    precH_raw  [s] ~ beta(a_precH , b_precH );
    precL_raw  [s] ~ beta(a_precL , b_precL );
    bint_raw   [s] ~ beta(a_bint  , b_bint  );

     for (g in 1 : numGames) {

      //context reg
      real prect = precH[s];
      if (reward[s,g]==0) prect = precL[s];

      for (t in 1:numTrials) {
        //CONFIDENCE RATINGS
        if (is_rating[s,g,t]==1 && rating[s,g,t]>-5) {
          //binary process 1 ?

          is_50_rating[s,g,t]       ~ bernoulli_logit(d50[s] - d50DIF[s] * abs(dift[s,g,t]));
          is_discrete_rating[s,g,t] ~ bernoulli_logit(d0[s] + dDIF[s] * abs(dift[s,g,t]));

          //continuous response?
          if (is_discrete_rating[s,g,t] == 0) {

           // print("diff: ", diff_std, "sum: ", sumQ_std);

            //real context_reg = rCON[s] * reward_trans[s,g]*rating_direction[s,g,t];
            real diff_reg    = rDIF[s]*dift[s,g,t];
            real sum_reg     = rSUM[s]*sumt[s,g,t];
            real choice_reg  = rCB [s]*cbt[s,g,t];
            real reward_reg  = rRB[s] *(choice[s,g,t]>0)*(outcome[s,g,t]>0);
            real bint_reg = choice_reg*reward_reg;

            real r_mean = diff_reg + sum_reg + choice_reg + reward_reg + bint_reg;// + context_reg;
            //print("rmean: ",r_mean);

            rating[s,g,t] ~ beta_proportion(inv_logit(r_mean), prect);
          }
        }

      } //t-loop

    } //g-loop

  } //s-loop

}







