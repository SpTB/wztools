
functions {
  int sign(real x) {
    return x < 0 ? -1 : x > 0;
  }
}

data {

  //task skeleton
  int<lower=1> numSubj;
  int<lower=1> numGames;
  int<lower=1> numTrials;
  array[numSubj, numGames] int<lower=0, upper=1> reward; // 1:High rewad 0: low reward
  array[numSubj, numGames] int<lower=0, upper=10> exp_cond; //10: High exp 0: low exp (novice)
 // array[numSubj, numGames, numTrials] int<lower=-100, upper=150> outcome; // for arm A
 // array[numSubj, numGames, numTrials] int<lower=-100, upper=150> outcomeB; // for arm B
 // array[numSubj, numGames, numTrials] int<lower=0, upper=2> observation; // 0:no observation (choice trial); 1:observed armA, 2: observed armB
  array[numSubj, numGames, numTrials] int<lower=0, upper=1> is_rating; // (binary) whether a rating trial
  //behav
  //array[numSubj, numGames, numTrials] int<lower=-2, upper=2> choice; // 1:good arm; 2:bad arm, -1: observe good arm; -2: observe bad arm
  array[numSubj, numGames, numTrials] real<lower=-5, upper=1> rating; // [-1, 1] (signed to armA); -5: no rating

  //initials context values
  array [numSubj] real context_high0;
  array [numSubj] real context_low0;

  //fixing
  real fix_alphaI;
  real fix_bRH;
  real fix_alphaC;
  real fix_tau;
  real fix_bTrial;
  real fix_bEXP;


  real fix_obs0;
  real fix_oCON;
  real fix_oDIF;
  real fix_oTrial;
  real fix_oEXP;

 //simulated individual pars
  array[numSubj] real <lower=0.0, upper=1.0> alphaI_raw;
  array[numSubj] real <lower=0.0, upper=1.0> bRH_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> alphaC_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> tau_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> bTrial_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> bEXP_raw ;

  array[numSubj] real <lower=0.0, upper=1.0> obs0_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> oCON_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> oDIF_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> oTrial_raw ;
  array[numSubj] real <lower=0.0, upper=1.0> oEXP_raw ;

}

transformed data{
  //array[numSubj, numGames, numTrials] int<lower=0, upper=1> is_observation; //binary obseration: 1: observation trial; 2: choice trial
  array[numSubj, numGames, numTrials] int<lower=-1, upper=1> is_discrete_rating; // 0: continuous rating (0,1) 1: binary rating [0 ^ 1]; -1: no rating
  array[numSubj, numGames, numTrials] real <lower=-5, upper=1> rating_transformed; //between 0-1
  array[numSubj, numGames] int <lower=0, upper=1> exp_cond_transformed; //binary experience
  for (s in 1:numSubj) {
    for(g in 1:numGames) {
      if (exp_cond[s,g]==10) exp_cond_transformed[s,g] =1; else exp_cond_transformed[s,g] =0;

      for(t in 1:numTrials) {
       // if(observation[s,g,t]==0) is_observation[s,g,t]=0; else is_observation[s,g,t]=1;

        if (rating[s,g,t]==-1) { //min-case
          is_discrete_rating[s,g,t] = 1;
          rating_transformed[s,g,t] = 0;
          } else if (rating[s,g,t]==1) { //max-case
            is_discrete_rating[s,g,t] = 1;
            rating_transformed[s,g,t] = 1;
          } else if(rating[s,g,t]==-5) { //no-rating case
            is_discrete_rating[s,g,t] = -1;
            rating_transformed[s,g,t] = rating[s,g,t];
          } else { //(0,1) beta-case
            is_discrete_rating[s,g,t] = 0;
            rating_transformed[s,g,t] = rating[s,g,t] *.5 + .5;
          }
        } //t-loop

    }//g-loop
  }// s-loop
}



transformed parameters {

//subj (native scale)
  array[numSubj] real <lower=-3, upper=3> alphaI;
 array[numSubj] real <lower=0, upper=1> bRH;
  array[numSubj] real <lower=0, upper=1> alphaC;
  array[numSubj] real <lower=-3, upper=1> tau;
   array[numSubj] real <lower=0.0, upper=3> bTrial;
   array[numSubj] real <lower=0.0, upper=1> bEXP;

   array[numSubj] real <lower=-5, upper=0> obs0;
   array[numSubj] real <lower=-3, upper=0> oCON;
   array[numSubj] real <lower=-3, upper=0> oDIF;
    array[numSubj] real <lower=-3, upper=0> oTrial;
   array[numSubj] real <lower=0.0, upper=3> oEXP;

    //fixing
  for (s in 1:numSubj) {
    if (fix_alphaI==-1) alphaI[s] = alphaI_raw[s] * 6 - 3; else alphaI[s] = fix_alphaI;
   if (fix_bRH   ==-1) bRH[s]    = bRH_raw[s]    *  1; else bRH[s] = fix_bRH;
    if (fix_alphaC==-1) alphaC[s] = alphaC_raw[s]         ; else alphaC[s] = fix_alphaC;
    if (fix_tau   ==-1) tau[s]    = tau_raw[s]    *  4-3;  else tau[s] = fix_tau;
    if (fix_bTrial   ==-1) bTrial[s]    = bTrial_raw[s]    *  3;  else bTrial[s] = fix_bTrial;
    bEXP[s] = bEXP_raw[s] *1;

    if (fix_obs0   ==-1) obs0[s]    = obs0_raw[s]    *5 -5;  else obs0[s] = fix_obs0;
    if (fix_oCON   ==-1) oCON[s]    = oCON_raw[s]    *3 -3;  else oCON[s] = fix_oCON;
    if (fix_oDIF   ==-1) oDIF[s]    = oDIF_raw[s]    *3 -3;  else oDIF[s] = fix_oDIF;
    if (fix_oTrial   ==-1) oTrial[s]    = oTrial_raw[s]    *  3-3;  else oTrial[s] = fix_oTrial;
    oEXP[s] = oEXP_raw[s] *3;
  }
}



generated quantities {

  array [numSubj, numGames, numTrials] int d_pred;
  array [numSubj, numGames, numTrials] int o_pred;
   array [numSubj, numGames, numTrials] real outcome; //simulated outcome
  //array [numSubj, numGames, numTrials] real r_pred; //simulated ratings

  array [numSubj, numGames, numTrials] real dift;
  array [numSubj, numGames, numTrials] real sumt;
  array [numSubj, numGames, numTrials] real cbt;
  array [numSubj, numGames, numTrials] real rbt;

  real p_obsA_low = 0.58; //prob better arm is chosen
  real p_obsA_high = 0.73;
  // array [numSubj, numGames, numTrials] int is_rating;
  // array [numSubj, numGames, numTrials] real outcome;
  //array [numSubj, numGames, numTrials] real is_observation;
  // array [numSubj, numGames] int reward_high;
  for (s in 1:numSubj) {
    for(g in 1:numGames) {
      for (t in 1:numTrials) {
        dift[s,g,t]=-99;
        sumt[s,g,t]=-99;
        cbt[s,g,t] =-99;
        rbt[s,g,t] =-99;
      }
    }
  }

  for (s in 1 : numSubj) {

    //initialize context values
    real context_high = context_high0[s];
    real context_low  = context_low0[s];

    for (g in 1 : numGames) {

      real p_obsA;
      vector[2] ev_rel; //relative ev
      real context_game; //context for current game (high or low)
      //bool specifying reward condition
      //int RH = g <= (numGames/2); //first half of games are high reward
      int RH = reward[s,g];
      //starting values (transfer from previous block)
      if (RH == 1) {
        context_game = context_high;
        } else {
        context_game = context_low;
      };
      //prob of observee being correct
      if (exp_cond_transformed[s,g]==1){
        p_obsA= p_obsA_low;
      } else {
        p_obsA= p_obsA_high;
      }

      ev_rel = rep_vector(0,2);//relative to context

      for (t in 1 : numTrials) {

        real pe; //prediction error
        real taut = tau[s];
        real tau_reg_term = taut + bTrial[s]*t;
        real tau_trial = exp(tau_reg_term)/(1+exp(tau_reg_term));
        //NEW
        //OBSERVATION CHOICE
        real obs_mean = obs0[s] + RH*oCON[s] + abs(ev_rel[1]-ev_rel[2])*oDIF[s] + t*oTrial[s] + exp_cond_transformed[s,g]*oEXP[s];
        o_pred[s,g,t] = bernoulli_logit_rng(obs_mean);

       //CHOICE  XX !! MUST BE BEFORE OUTCOME !!
        if (o_pred[s,g,t] == 0) {
          d_pred[s,g,t] = categorical_rng(softmax(ev_rel * tau_trial)); //choice

        } else {
          d_pred[s,g,t] = bernoulli_rng(p_obsA)-2; //bernoulli_rng(p_obsA)-2; // observed: -1: armA (good); -2: armB (bad)
        }

        //which outcome experienced
        if (abs(d_pred[s,g,t]) == 1) {
          if (reward[s,g]==1) outcome[s,g,t] = normal_rng(30,5);
          if (reward[s,g]==0) outcome[s,g,t] = normal_rng(5,5);
        } else if (abs(d_pred[s,g,t]) == 2) {
          if (reward[s,g]==1) outcome[s,g,t] = normal_rng(20,5);
          if (reward[s,g]==0) outcome[s,g,t] = normal_rng(-5,5);
        }

        real outcome_trial = outcome[s,g,t];

        // calculating the unconstrained learning rate for the current trial
        real exp_reg;
        real reward_reg;
        real choice_reg;
        real int_reg;

        if (exp_cond_transformed[s,g]==1) exp_reg=bEXP[s];
        else exp_reg= -bEXP[s];

        if (outcome_trial>0) reward_reg = bRH[s];
        else reward_reg = -bRH[s];

        // if (choice>0) choice_reg = bO[s];
        // else choice_reg = -bO[s];
        //
        // if (outcome[s,g,t]>0){
        //   if (observation[s,g,t]==0) int_reg = bint[s];
        //   else int_reg = -bint[s];
        // } else { //loss case; reverse
        //   if (observation[s,g,t]==0) int_reg = -bint[s];
        //   else int_reg=bint[s];
        // }

        // unconstrained learning rate for the current trial
        real reg_term = alphaI[s] + exp_reg +  reward_reg;
        //constrained learning rate  (0,1)
        real alpha_trial = exp(reg_term)/(1+exp(reg_term));

        //context update
        real imagined_outcome_rel = ev_rel[abs(abs(d_pred[s, g, t])-3)];
        real known_outcome_rel = outcome_trial- context_game;
        real pe_context =  (imagined_outcome_rel + known_outcome_rel)/2;

        context_game = context_game + alphaC[s]*pe_context;

        //Q-VAL UPDATE
        pe = outcome_trial - context_game - ev_rel[abs(d_pred[s, g, t])];
        ev_rel[abs(d_pred[s, g, t])] += alpha_trial * pe; //taking absolute value because choice variable is signed (minus for observation; e.g. -1 observing armA)

        //CONFIDENCE RATINGS
        //is_rating[s,g,t] = bernoulli_rng(0.33);
        if (is_rating[s,g,t]==1) {
          //binary process?
          real diff = (ev_rel[1] - ev_rel[2])/100 * 14; //Q-value difference with respect to the better option
          real sign_diff = 0;
          if (diff>0)  sign_diff = 1; else  sign_diff=-1;

          //continuous response?
          if (is_discrete_rating[s,g,t] == 0) {

            real diff_bin;
            if (diff>0) diff_bin = 1; else diff_bin=0;

            real sumQ = (ev_rel[1]+ev_rel[2]) * context_game /100 * sign_diff;   //absolute SUM of Q-vals
            real choice_rel=0; // choice relative to the scale
            real r_rel= 0; //relative reward
            // relative to the higher valued option: if dif>0 rating-> armA(1); if dif<0 rating->armB(0))
            // if (diff>0) {
            //     sumQ = (ev_rel[1]+ev_rel[2]) * context_game /100;
            //     r_rel = (outcome_trial-context_game)/100*7;
            //   } else if (diff<0){
            //     sumQ = -((ev_rel[1]+ev_rel[2]) * context_game)/100;
            //     r_rel =-(outcome_trial-context_game)/100*7;
            //   }

              //standardarization

             // real sumQ_std = sumQ/41; //since sd of sumQ ~ 41.1
            //  real r_rel_std = r_rel/10; //approx, since 10 is the sd of payoff
            //
            if (d_pred[s,g,t]==1) {
             choice_rel = 1;
             r_rel = (outcome_trial-context_game)/100 * 7;
            } else if (d_pred[s,g,t]==2) {
              choice_rel = -1;
              r_rel = - ((outcome_trial-context_game)/100 * 7);
            }

            //saving
              dift[s,g,t] = diff;
              sumt[s,g,t] = sumQ;
              cbt[s,g,t] = choice_rel;
              rbt[s,g,t] = r_rel;


           // print("diff: ", diff_std, "sum: ", sumQ_std);
           // real r_mean = rDIF[s]*diff + rSUM[s]*sumQ + rCB[s]*choice_rel + rRB[s]*r_rel;
            //print("rmean: ",r_mean);
            //r_pred[s,g,t] = beta_proportion_rng(inv_logit(r_mean), prec[s])*2-1;
          }
        }

        //update relevant context for next game on final trial
        if (t == numTrials) {
          if (RH == 1) context_high = context_game; else context_low = context_game;
        }

      } //t-loop

    } //g-loop

  } //s-loop

}




