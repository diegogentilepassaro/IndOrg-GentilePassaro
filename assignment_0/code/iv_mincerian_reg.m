function [iv_beta_EDUC, iv_se_beta_EDUC] = iv_mincerian_reg(wage_var, educ_var, instruments, controls)
    EDUC = educ_var;
    nbr_obs = size(EDUC,1);
    CONSTANT = ones(nbr_obs, 1);
    Z = horzcat(CONSTANT,instruments, controls);
    WAGE = wage_var;
    
    predict_EDUC = Z * (inv(transpose(Z)*Z) * (transpose(Z)*EDUC));
    
    regressors = horzcat(CONSTANT, controls, predict_EDUC);
    regressors_for_se = horzcat(CONSTANT, controls, EDUC);
    nbr_regressors = size(regressors, 2);
    
    betas = inv(transpose(regressors)*regressors) * (transpose(regressors)*WAGE);
    iv_beta_EDUC = betas(nbr_regressors, 1);
    residuals = wage_var - regressors_for_se*betas;
    var_betas = (nbr_obs - nbr_regressors)^(-1) * transpose(residuals)*residuals *...
        inv(transpose(regressors)*regressors);
    var_beta_EDUC = var_betas(nbr_regressors,nbr_regressors);
    iv_se_beta_EDUC = sqrt(var_beta_EDUC);
end
