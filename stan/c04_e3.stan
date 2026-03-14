//adding interaction between the learning rate sensitivities (confirm. bias hypo)
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
 // array[numSubj, numGames] int<lower=1, upper=2> experience; //1: High exp 2: low exp (novice)
  array[numSubj, numGames, numTrials] int<lower=-100, upper=150> outcome; // for chosen arm
  array[numSubj, numGames, numTrials] int<lower=-100, upper=150> outcomeB; // for unchosen arm
  array[numSubj, numGames, numTrials] int<lower=0, upper=2> observation; // 0:no observation (choice trial); 1:observed armA, 2: observed armB
  array[numSubj, numGames, numTrials] int<lower=0, upper=1> is_rating; // (binary) whether a rating trial
  //behav
  array[numSubj, numGames, numTrials] int<lower=-2, upper=2> choice; // 1:good arm; 2:bad arm, -1: observe good arm; -2: observe bad arm
  array[numSubj, numGames, numTrials] real<lower=-5, upper=1> rating; // [-1, 1] (signed to armA); -5: no rating

  //initials context values
  array [numSubj] real context_high0;
  array [numSubj] real context_low0;

  //fixing
  real fix_alphaI;
  real fix_bO;
  real fix_bRH;
  real fix_alphaC;
  real fix_tau;


}

transformed data{
  array[numSubj, numGames, numTrials] int<lower=-1, upper=1> is_discrete_rating; // 0: continuous rating (0,1) 1: binary rating [0 ^ 1]; -1: no rating
  array[numSubj, numGames, numTrials] real <lower=-5, upper=1> rating_transformed; //between 0-1
  for (s in 1:numSubj) {
    for(g in 1:numGames) {
      for(t in 1:numTrials) {

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


parameters {

  //beta pars (GAMMA SCALE (0, Inf))
    //choice
  real <lower=0> mu_a_alphaI_raw;
  real <lower=0> mu_a_bO_raw;
  real <lower=0> mu_a_bRH_raw;
  real <lower=0> mu_a_bint_raw; //interaction between bO and bRH //should be positive (see equations)
  real <lower=0> mu_a_alphaC_raw;
  real <lower=0> mu_a_tau_raw;

    //choice
  real <lower=0> mu_b_alphaI_raw;
  real <lower=0> mu_b_bO_raw;
  real <lower=0> mu_b_bRH_raw;
  real <lower=0> mu_b_bint_raw; //interaction between bO and bRH
  real <lower=0> mu_b_alphaC_raw;
  real <lower=0> mu_b_tau_raw;
  //individual pars

  array[numSubj] real <lower=0.0, upper=1.0> alphaI_raw;
  array[numSubj] real <lower=0.0, upper=1.0> bO_raw;
  array[numSubj] real <lower=0.0, upper=1.0> bRH_raw;
  array[numSubj] real <lower=0.0, upper=1.0> bint_raw;
  array[numSubj] real <lower=0.0, upper=1.0> alphaC_raw;
  array[numSubj] real <lower=0.0, upper=1.0> tau_raw;

}

transformed parameters {

//hypers transform (GAMMA + 1 scale)
  real <lower=1> mu_a_alphaI;
  real <lower=1> mu_a_bO;
  real <lower=1> mu_a_bRH;
  real <lower=1> mu_a_bint;
  real <lower=1> mu_a_alphaC;
  real <lower=1> mu_a_tau;

  real <lower=1> mu_b_alphaI;
  real <lower=1> mu_b_bO;
  real <lower=1> mu_b_bRH;
  real <lower=1> mu_b_bint;
  real <lower=1> mu_b_alphaC;
  real <lower=1> mu_b_tau;


//mus
  real<lower=-5, upper=5> mu_alphaI;
  real<lower=-2, upper=0> mu_bO;
  real<lower=0, upper=2> mu_bRH;
  real<lower=0, upper=2> mu_bint;
  real<lower= 0, upper=1> mu_alphaC;
  real<lower=-3, upper=1> mu_tau;


//subj
  array[numSubj] real <lower=-5, upper=5> alphaI;
  array[numSubj] real <lower=-2, upper=0> bO;
  array[numSubj] real <lower=0, upper=2> bRH;
  array[numSubj] real <lower=0, upper=2> bint;
  array[numSubj] real <lower=0, upper=1> alphaC;
  array[numSubj] real <lower=-3, upper=1> tau;



//hyper
  mu_a_alphaI = mu_a_alphaI_raw+1;
  mu_a_bO = mu_a_bO_raw+1;
  mu_a_bRH = mu_a_bRH_raw+1;
  mu_a_bint = mu_a_bint_raw+1;
  mu_a_alphaC = mu_a_alphaC_raw+1;
  mu_a_tau = mu_a_tau_raw+1;

  mu_b_alphaI = mu_b_alphaI_raw+1;
  mu_b_bO = mu_b_bO_raw+1;
  mu_b_bRH = mu_b_bRH_raw+1;
  mu_b_bint = mu_b_bint_raw+1;
  mu_b_alphaC = mu_b_alphaC_raw+1;
  mu_b_tau = mu_b_tau_raw+1;

  mu_alphaI = (mu_a_alphaI /(mu_a_alphaI + mu_b_alphaI)) * 10 - 5;
  mu_bO     = (mu_a_bO     /(mu_a_bO     + mu_b_bO    )) * -2;
  mu_bRH    = (mu_a_bRH    /(mu_a_bRH    + mu_b_bRH   )) *  2;
   mu_bint    = (mu_a_bint    /(mu_a_bint    + mu_b_bint   )) * 2;
  mu_alphaC = (mu_a_alphaC /(mu_a_alphaC + mu_b_alphaC));
  mu_tau   =  (mu_a_tau    /(mu_a_tau    + mu_b_tau   )) * 4 - 3;//.5 + .25;
    //fixing
  for (s in 1:numSubj) {
    if (fix_alphaI==-1) alphaI[s] = alphaI_raw[s] * 10 - 5; else alphaI[s] = fix_alphaI;
    if (fix_bO    ==-1) bO[s]     = bO_raw[s]     *  - 2; else bO[s] = fix_bO;
    if (fix_bRH   ==-1) bRH[s]    = bRH_raw[s]    *  2; else bRH[s] = fix_bRH;
    bint[s]    = bint_raw[s] * 2;
    if (fix_alphaC==-1) alphaC[s] = alphaC_raw[s]         ; else alphaC[s] = fix_alphaC;
    if (fix_tau   ==-1) tau[s]    = tau_raw[s]    *  4 -3;  else tau[s] = fix_tau;


  }
}


model {

  //draw hypers
  mu_a_alphaI_raw ~ gamma(1,1);
  mu_a_bO_raw     ~ gamma(1,.1);
  mu_a_bRH_raw    ~ gamma(1,1);
  mu_a_bint_raw    ~ gamma(1,1);
  mu_a_alphaC_raw ~ gamma(1,1);
  mu_a_tau_raw    ~ gamma(1,1);

  mu_b_alphaI_raw ~ gamma(1,1);
  mu_b_bO_raw     ~ gamma(1,1);
  mu_b_bRH_raw    ~ gamma(1,.1);
  mu_b_bint_raw    ~ gamma(1,.1);
  mu_b_alphaC_raw ~ gamma(1,1);
  mu_b_tau_raw    ~ gamma(1,1);

  for (s in 1 : numSubj) {

    alphaI_raw [s] ~ beta(mu_a_alphaI, mu_b_alphaI);
    bO_raw     [s] ~ beta(mu_a_bO    , mu_b_bO    );
    bRH_raw    [s] ~ beta(mu_a_bRH   , mu_b_bRH   );
    bint_raw    [s] ~ beta(mu_a_bint   , mu_b_bint   );
    alphaC_raw [s] ~ beta(mu_a_alphaC, mu_b_alphaC);
    tau_raw    [s] ~ beta(mu_a_tau   , mu_b_tau   );

    //initialize context values
    real context_high = context_high0[s];
    real context_low  = context_low0[s];

    for (g in 1 : numGames) {

      vector[2] ev_rel; //relative ev
      real context_game; //context for current game (high or low)

      int RH = reward[s,g];
      //starting values (transfer from previous block)
      if (RH == 1) {
        context_game = context_high;
        } else {
        context_game = context_low;
      };
      ev_rel = rep_vector(0,2);//relative to context

      for (t in 1 : numTrials) {

        real taut=tau[s];

        real pe; //prediction error
        real peB; //prediction error for unchosen option


        //rew
        real reward_regA;
        real reward_regB;
        if (outcome[s,g,t]>0) reward_regA = bRH[s];
        else reward_regA = -bRH[s];
        if (outcomeB[s,g,t]>0) reward_regB = bRH[s];
        else reward_regB = -bRH[s];

        //choice
        real choice_reg;
        if (observation[s,g,t]==0) choice_reg = -bO[s];
        else choice_reg = 0;

        // int
        real int_regA=0;
        real int_regB=0;
        if (outcome[s,g,t]>0){
          if (observation[s,g,t]==0) int_regA = bint[s];
          else int_regA = -bint[s];
        } else { //loss case; reverse
          if (observation[s,g,t]==0) int_regA = -bint[s];
          else int_regA=bint[s];
        }
        if (outcomeB[s,g,t]>0){
          if (observation[s,g,t]==0) int_regB = bint[s];
          else int_regB = -bint[s];
        } else { //loss case; reverse
          if (observation[s,g,t]==0) int_regB = -bint[s];
          else int_regB=bint[s];
        }

        // unconstrained learning rate for the current trial
        real reg_term = alphaI[s]   + reward_regA + choice_reg;// + int_regA;
        real reg_termB = alphaI[s]  + reward_regB - choice_reg;// - int_regB;

        //constrained learning rate  (0,1)
        real alpha_trial = exp(reg_term)/(1+exp(reg_term));
        real alphaB_trial = exp(reg_termB)/(1+exp(reg_termB));

       // real trial_reg = -((t-6)/3.45);
        real tau_reg_term = taut;
        real tau_trial = exp(tau_reg_term)/(1+exp(tau_reg_term));
        //CHOICE
        if (choice[s, g, t] > 0) {
          choice[s, g, t] ~ categorical_logit(ev_rel * tau_trial); //choice
        }

        //which outcome
       real outcome_trial = outcome[s,g,t];
       real outcomeB_trial = outcomeB[s,g,t];
       //if (abs(choice[s,g,t]) == 1) {
         //outcome_trial = outcome[s,g,t];
       //} else {
         //outcome_trial = outcomeB[s,g,t];
       //}

        //context update
        real unknown_outcome_rel =  outcomeB_trial - context_game;
        real known_outcome_rel = outcome_trial- context_game;
        real pe_context =  (unknown_outcome_rel + known_outcome_rel)/2;

        context_game = context_game + alphaC[s]*pe_context;

        //Q-VAL UPDATE
        pe = outcome_trial - context_game - ev_rel[abs(choice[s, g, t])];
        peB = outcomeB_trial - context_game - ev_rel[abs(abs(choice[s, g, t])-3)];
        ev_rel[abs(choice[s, g, t])]        += alpha_trial * pe; //taking absolute value because choice variable is signed (minus for observation; e.g. -1 observing armA)
        ev_rel[abs(abs(choice[s, g, t])-3)] += alphaB_trial * peB;
        //CONFIDENCE RATINGS


        //update relevant context for next game on final trial
        if (t == numTrials) {
          if (reward[s, g] == 1) context_high = context_game; else context_low = context_game;
        }

      } //t-loop

    } //g-loop

  } //s-loop

}

generated quantities {

  array [numSubj, numGames, numTrials] int d_pred;
  array [numSubj, numGames, numTrials] real log_lik;
  //array [numSubj, numGames, numTrials] real r_pred; //simulated ratings

  array [numSubj, numGames, numTrials] real dift;
  array [numSubj, numGames, numTrials] real sumt;
  array [numSubj, numGames, numTrials] real cbt;
  array [numSubj, numGames, numTrials] real rbt;


  // array [numSubj, numGames, numTrials] int is_rating;
  // array [numSubj, numGames, numTrials] real outcome;
  // array [numSubj, numGames, numTrials] real is_observation;
  // array [numSubj, numGames] int reward_high;
  for (s in 1:numSubj) {
    for(g in 1:numGames) {
      for (t in 1:numTrials) {
        dift[s,g,t]=-99;
        sumt[s,g,t]=-99;
        cbt[s,g,t] =-99;
        rbt[s,g,t] =-99;
        log_lik[s,g,t] = 0;
      }
    }
  }

  for (s in 1 : numSubj) {

    //initialize context values
    real context_high = context_high0[s];
    real context_low  = context_low0[s];

    for (g in 1 : numGames) {

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
      ev_rel = rep_vector(0,2);//relative to context

      for (t in 1 : numTrials) {

        real taut=tau[s];
        real pe; //prediction error
        real peB; //prediction error for unchosen option
        //rew
        real reward_regA;
        real reward_regB;
        if (outcome[s,g,t]>0) reward_regA = bRH[s];
        else reward_regA = -bRH[s];
        if (outcomeB[s,g,t]>0) reward_regB = bRH[s];
        else reward_regB = -bRH[s];

        //choice
        real choice_reg;
        if (observation[s,g,t]==0) choice_reg = -bO[s];
        else choice_reg = 0;

        // int
        real int_regA=0;
        real int_regB=0;
        if (outcome[s,g,t]>0){
          if (observation[s,g,t]==0) int_regA = bint[s];
          else int_regA = -bint[s];
        } else { //loss case; reverse
          if (observation[s,g,t]==0) int_regA = -bint[s];
          else int_regA=bint[s];
        }
        if (outcomeB[s,g,t]>0){
          if (observation[s,g,t]==0) int_regB = bint[s];
          else int_regB = -bint[s];
        } else { //loss case; reverse
          if (observation[s,g,t]==0) int_regB = -bint[s];
          else int_regB=bint[s];
        }

        // unconstrained learning rate for the current trial
        real reg_term = alphaI[s]   + reward_regA + choice_reg;// + int_regA;
        real reg_termB = alphaI[s]  + reward_regB - choice_reg;// - int_regB;

        //constrained learning rate  (0,1)
        real alpha_trial = exp(reg_term)/(1+exp(reg_term));
        real alphaB_trial = exp(reg_termB)/(1+exp(reg_termB));

       // real trial_reg = -((t-6)/3.45);
        real tau_reg_term = taut;
        real tau_trial = exp(tau_reg_term)/(1+exp(tau_reg_term));

        //CHOICE
        if (observation[s,g,t] == 0) {
          d_pred[s,g,t] = categorical_rng(softmax(ev_rel * tau_trial)); //choice
          log_lik[s,g,t] += categorical_logit_lpmf(choice[s,g,t] | ev_rel * tau_trial);

        } else {
          d_pred[s,g,t] = -observation[s,g,t]; //bernoulli_rng(p_obsA)-2; // observed: -1: armA (good); -2: armB (bad)
        }


        //which outcome experienced
        real outcome_trial = outcome[s,g,t];
        real outcomeB_trial = outcomeB[s,g,t];


        //context update
        real unknown_outcome_rel =  outcomeB_trial - context_game;
        real known_outcome_rel = outcome_trial- context_game;
        real pe_context =  (unknown_outcome_rel + known_outcome_rel)/2;

        context_game = context_game + alphaC[s]*pe_context;

        //Q-VAL UPDATE
        pe = outcome_trial - context_game - ev_rel[abs(choice[s, g, t])];
        peB = outcomeB_trial - context_game - ev_rel[abs(abs(choice[s, g, t])-3)];
        ev_rel[abs(choice[s, g, t])]        += alpha_trial * pe; //taking absolute value because choice variable is signed (minus for observation; e.g. -1 observing armA)
        ev_rel[abs(abs(choice[s, g, t])-3)] += alphaB_trial * peB;
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
            if (choice[s,g,t]==1) {
             choice_rel = 1;
             r_rel = (outcome_trial-context_game)/100 * 7;
            } else if (choice[s,g,t]==2) {
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




