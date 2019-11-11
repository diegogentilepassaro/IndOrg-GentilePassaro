from functions import *
import matplotlib.pyplot as plt

data = pd.read_csv('../raw_data/6649_data and programs/Data/1988/XMat.out', 
                   sep = '\t',  header = None, engine = 'python')
data.dropna(axis = 'columns', inplace = True)
data.columns = ['county_id','log_pop','log_ret_pc_sales',
              'urb_perc','midwest','log_dist_Benton', 'south',
             'kmart', 'walmart', 'n_small', 'dw_nbr_kmart_other', 
             'dw_nbr_walmart_other', 'opt1', 'opt2', 'opt3']
data = data[['county_id', 'log_pop', 'kmart', 'walmart']]
data['N'] = data['kmart'] + data['walmart']
data['pop'] = np.exp(data['log_pop'])
data = data[['county_id', 'pop', 'N']]

data = compute_dummies(data)

model1 = solver(data.copy(), neg_ll, [0.5, 10, 10])
model1_params_star = model1.x
v_2_star_m1 = model1_params_star[0]
mu_star_m1 = model1_params_star[1]
sigma_star_m1 = model1_params_star[2]
    
model2 = solver(data.copy(), neg_ll_het,  [0.5, 10, 10, 10])
model2_params_star = model2.x
v_2_star_m2 = model2_params_star[0]
mu_1_star_m2 = model2_params_star[1]
mu_2_star_m2 = model2_params_star[2]
sigma_star_m2 = model2_params_star[3]

nbr_bins = 7
cdf, v_2_star_np = compute_np_cdf_and_v_2(7, data.copy())

fig, ax1 = plt.subplots()
ax1.plot(cdf['max_bin'],cdf['G1'], label = '$G1(S^k)$')
ax1.plot(cdf['max_bin'], cdf['v_2'], label = '$v(2)(S^k)$')
ax1.plot(cdf['max_bin'], np.repeat(v_2_star_np, nbr_bins+1), label = r'$\hat{v(2)}$')
ax1.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), shadow=True, ncol=2)
ax1.set_xlabel('S')
ax1.set_ylabel('Probability or v(2)')
ax2 = ax1.twinx()  # instantiate a second axes that shares the same x-axis
ax2.bar(cdf['max_bin'],cdf['counties_per_bin'], color = 'Red', label = 'Counties per bin')
ax2.set_ylabel('Counties per bin')
fig.tight_layout()
plt.savefig('../output/figure_1.pdf')

