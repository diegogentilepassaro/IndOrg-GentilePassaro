function [beta_EDUC, se_beta_EDUC] = naive_mincerian_reg(wage_var, educ_var, controls)
    EDUC = educ_var;
    nbr_obs = size(EDUC,1);
    CONSTANT = ones(nbr_obs, 1);
    X = horzcat(CONSTANT,controls);
    WAGE = wage_var;
    
    regressors = horzcat(X, EDUC);
    nbr_regressors = size(regressors, 2);
    
    betas = inv(transpose(regressors)*regressors) * (transpose(regressors)*WAGE);
    beta_EDUC = betas(nbr_regressors, 1);
    residuals = wage_var - regressors*betas;
    var_betas = (nbr_obs - nbr_regressors)^(-1) * transpose(residuals)*residuals *...
        inv(transpose(regressors)*regressors);
    var_beta_EDUC = var_betas(nbr_regressors,nbr_regressors);
    se_beta_EDUC = sqrt(var_beta_EDUC);
end