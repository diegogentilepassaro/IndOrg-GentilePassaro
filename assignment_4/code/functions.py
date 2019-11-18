import pandas as pd
import numpy as np
from scipy.stats import norm
from scipy.optimize import minimize

def compute_dummies(df):
    for n in range(3):
        name = 'dummy_' + str(n)
        df[name] = (df['N'] == n)*1
    return(df)

def N_prob(S , n, v_2 , mu, sigma):
    if n == 0:
        prob = 1-norm.cdf((S-mu)/sigma)
    elif n == 1:
        prob = norm.cdf((S-mu)/sigma) - norm.cdf((S*v_2-mu)/sigma)
    else:
        prob0 = 1-norm.cdf((S-mu)/sigma)
        prob1 = norm.cdf((S-mu)/sigma) - norm.cdf((S*v_2-mu)/sigma)
        prob = 1 - prob1 - prob0
    return(prob)
    
def neg_ll(params, df):
    v_2 = params[0]
    mu = params[1]
    sigma = params[2]
    for n in range(3):
        prob_name = 'prob' + str(n)
        df[prob_name] = N_prob(df['pop'], n, v_2, mu, sigma)
        ll_term_name = 'll_term_' + str(n)
        dummy_name = 'dummy_' + str(n)
        df[ll_term_name] = df[dummy_name]*np.log(df[prob_name])
    ll_county = df['ll_term_0']+df['ll_term_1']+df['ll_term_2']
    neg_ll = -np.sum(ll_county)
    return(neg_ll)

def solver(df, neg_ll, init):
    solver_results = minimize(neg_ll, x0 = np.array(init), args = df)
    return(solver_results)
    
def N_prob_het(S , n, v_2 , mu_1, mu_2, sigma):
    if n == 0:
        prob = 1-norm.cdf((S-mu_1)/sigma)
    elif n == 1:
        prob = norm.cdf((S-mu_1)/sigma) - norm.cdf((S*v_2-mu_2)/sigma)
    else:
        prob0 = 1-norm.cdf((S-mu_1)/sigma)
        prob1 = norm.cdf((S-mu_1)/sigma) - norm.cdf((S*v_2-mu_2)/sigma)
        prob = 1 - prob1 - prob0
    return(prob)

def neg_ll_het(params, df):
    v_2 = params[0]
    mu_1 = params[1]
    mu_2 = params[2]
    sigma = params[3]
    for n in range(3):
        prob_name = 'prob' + str(n)
        df[prob_name] = N_prob_het(df['pop'], n, v_2, mu_1, mu_2, sigma)
        ll_term_name = 'll_term_' + str(n)
        dummy_name = 'dummy_' + str(n)
        df[ll_term_name] = df[dummy_name]*np.log(df[prob_name])
    ll_county = df['ll_term_0']+df['ll_term_1']+df['ll_term_2']
    neg_ll_het = -np.sum(ll_county)
    return(neg_ll_het) 
    
def compute_np_cdf_and_v_2(nbr_bins, df):
    df['binned_pop'] = pd.cut(df['pop'], bins = nbr_bins)
    bin_edges = pd.cut(df['pop'], bins = nbr_bins, retbins = True)[1]
    pop_bins = df['binned_pop'].drop_duplicates().sort_values().to_frame()
    pop_bins['counties_per_bin'] = df.groupby(['binned_pop'])['county_id'].transform('count')
    pop_bins['min_bin'] = bin_edges[:nbr_bins:]
    pop_bins['max_bin'] = bin_edges[-nbr_bins:]
    for n in range(3):
        name_n = str(n)
        row = 0
        cond_prob = np.zeros(nbr_bins)
        for s in pop_bins['binned_pop']:
            is_bin = (df['binned_pop']== s)
            is_bin = df[is_bin]
            nbr_counties_in_bin = is_bin.shape[0]
            name_column = 'N' + name_n
            is_bin[name_column] = (is_bin['N'] == n)*1
            N_equal_n = is_bin[name_column]
            cond_prob[row] = np.sum(N_equal_n)/nbr_counties_in_bin
            row = row + 1
        cond_prob_name = 'cond_prob' + name_n
        pop_bins[cond_prob_name] = cond_prob
    pop_bins['G1'] = 1 - pop_bins['cond_prob0']
    pop_bins['G2'] = 1 - pop_bins['cond_prob0'] - pop_bins['cond_prob1']
    cdf = pd.DataFrame(bin_edges)
    cdf.columns = ['max_bin']
    cdf = pd.merge(cdf,
                   pop_bins[['binned_pop', 'counties_per_bin', 'min_bin', 'max_bin', 'G1', 'G2']],
                   on = 'max_bin',
                   how='left')
    cdf['min_bin'][0] = 0
    cdf['G1'][0] = 0
    cdf['G2'][0] = 0
    cdf['m'] = (cdf['G1'] - cdf['G1'].shift(1))/(cdf['max_bin'] - cdf['min_bin'])
    cdf['a'] = cdf['G1'].shift(1) - cdf['m']*cdf['min_bin']
    cdf['m'][0] = 0
    cdf['a'][0] = 0   
    cdf['v_2'] = np.nan
    for i in range(nbr_bins):
        j = i + 1
        result = False
        it = 0
        while (result == False):
            it = it + 1
            result = ((cdf['G2'][j] > cdf['G1'][it-1]) & (cdf['G2'][j] < cdf['G1'][it]))
        cdf.ix[j , 'v_2'] = ((cdf['G2'][j]-cdf['a'][it])/cdf['m'][it])/cdf['max_bin'][j]
    v_2 = np.average(cdf.loc[1:nbr_bins,'v_2'], weights = cdf.loc[1:nbr_bins,'counties_per_bin'])  
#    cdf = cdf[['binned_pop', 'min_bin', 'max_bin', 'G1', 'v_2']]
    return(cdf, v_2)