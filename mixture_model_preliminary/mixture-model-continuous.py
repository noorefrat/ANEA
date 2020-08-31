import numpy as np
from collections import defaultdict
import pymc3 as pm
import theano
import theano.tensor as tt
theano.config.gcc.cxxflags = "-fbracket-depth=50000000"
import csv
import pickle as pkl

features = []
with open('../features.csv','r') as f:
    for row in csv.reader(f,delimiter=','):
        features.append(row)

ling_data_raw = [l[7:-2] for l in features]
lang_raw = [l[:3] for l in features]
fam_raw = [l[3] for l in features]
area_raw = [l[4] for l in features]
date_raw = [l[-2:] for l in features]
fams = sorted(set([l for l in fam_raw[1:]]))

G = len(fams)
N = len(ling_data_raw)-1

feat_vars = defaultdict(list)
for l in ling_data_raw[1:]:
    for i,f in enumerate(l):
        feat_vars[ling_data_raw[0][i]].append(f)

for k in feat_vars.keys():
    feat_vars[k] = sorted(set(feat_vars[k]))

R = [len(feat_vars[k]) for k in feat_vars.keys()]
X = sum(R)
feat_var_list = [(k,v) for k in feat_vars.keys() for v in feat_vars[k]]
S = [[sum(R[:d]),sum(R[:d])+R[d]] for d in range(len(R))] #the indices for each distribution in the collection

feat_var_id = np.zeros([N,X])
fam_id = np.zeros([N,G])
area_id = np.zeros([N,2])
date_id = np.zeros([N,1])

for i in range(N):
    for j in range(len(feat_vars.keys())):
        feat_var_id[i,feat_var_list.index((ling_data_raw[0][j],ling_data_raw[i+1][j]))] = 1.
    fam_id[i,fams.index(fam_raw[i+1])] = 1.
    if lang_raw[i+1][0] == 'Egyptian':
        area_id[i,1] = 1.
    else:
        area_id[i,0] = 1.
    date = date_raw[i+1]
    #too lazy to deal with BCE/CE
    date_id[i,0] = np.mean([float(d.split()[0]) for d in date]) + 2000

def loglik(phi):
    def llik(feat_var_id):
        lliks = tt.log(phi)*feat_var_id
        return(tt.sum(lliks))
    return(llik)

model = pm.Model()
with model:
    beta_0 = tt.stack([pm.Normal('beta_0_{}'.format(x),0,1) for x in range(X)])
    beta_0_ = tt.stack([beta_0 for n in range(N)])
    beta_time = tt.stack([[pm.Normal('beta_time_{}'.format(x),0,1) for x in range(X)]])
    w_a = tt.stack([[pm.Normal('w_a_{}_{}'.format(k,x),0,1) for x in range(X)] for k in range(2)]) #areal group prior
    w_g = tt.stack([[pm.Normal('w_g_{}_{}'.format(k,x),0,1) for x in range(X)] for k in range(G)]) #genetic group prior
    psi = beta_0_ + tt.dot(date_id,beta_time) + tt.dot(area_id,w_a) + tt.dot(fam_id,w_g)
    phi = tt.concatenate([tt.nnet.softmax(psi[:,S[d][0]:S[d][1]]) for d in range(len(R))],-1)
    target = pm.DensityDist('target',loglik(phi=phi),observed=dict(feat_var_id=feat_var_id))
    inference = pm.ADVI()
    inference.fit(100000, obj_optimizer=pm.adam(learning_rate=.01,beta1=.8),callbacks=[pm.callbacks.CheckParametersConvergence()])
    trace = inference.approx.sample()
    posterior = {k:trace[k] for k in trace.varnames if not k.endswith('__')}
    posterior['ELBO'] = inference.hist
    f = open('mixture-model-continuous-posterior.pkl','wb')
    pkl.dump(posterior,f)
    f.close()
