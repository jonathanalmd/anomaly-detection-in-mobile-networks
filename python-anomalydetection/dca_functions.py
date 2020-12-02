import numpy as np
from scipy import spatial

def Codemsa(msa, theta):
    seqnum = len(msa)
    seqlen = len(msa[0])
    encoded_msa = msa.copy().astype(int)

    #WEIGHT SEQUENCES
    hammdist = spatial.distance.pdist(encoded_msa, 'hamming')
    weight_matrix = spatial.distance.squareform(hammdist < (1.0- theta))
    weights = 1.0 / (np.sum(weight_matrix, axis = 1) + 1.0)
    Meff = np.sum(weights)
    return encoded_msa, weights, Meff

def Sitefreq(encoded_msa, Meff, weights, q, LAMBDA):
    n = encoded_msa.shape[1]
    sitefreq = np.empty((n,q),dtype=float)
    for i in range(n):
        for aa in range(q):
            sitefreq[i,aa] = np.sum([w for k,w in enumerate(weights) if encoded_msa[k,i] == aa])/Meff
    sitefreq = (1-LAMBDA)*sitefreq + LAMBDA/q
    return sitefreq

def Entropy(sitefreq, nP):
    ent = np.zeros((nP),dtype=float)
    for i in range(nP):
        ent[i] = -np.sum(sitefreq[i,:]*np.log(sitefreq[i,:]))
    return ent

def cantor(x, y):
    return (x + y) * (x + y + 1) / 2 + y

def Pairfreq(encoded_msa, Meff, weights, sitefreq, q, LAMBDA):
    n = encoded_msa.shape[1]
    pairfreq = np.zeros((n,q,n,q),dtype=float)
    for i in range(n):
        for j in range(n):
            c = cantor(encoded_msa[:,i],encoded_msa[:,j])
            unique,aaIdx = np.unique(c,True)
            for x,item in enumerate(unique):
                pairfreq[i,encoded_msa[aaIdx[x],i],j,encoded_msa[aaIdx[x],j]] = np.sum([w for k,w in enumerate(weights) if c[k] == item])

    pairfreq /= Meff
    pairfreq = (1-LAMBDA)*pairfreq + LAMBDA/(q*q)

    for i in range(n):
        for am_i in range(q):
            for am_j in range(q):
                if (am_i==am_j):
                    pairfreq[i,am_i,i,am_j] = sitefreq[i,am_i]
                else:
                    pairfreq[i,am_i,i,am_j] = 0.0
    return pairfreq

def Coupling(sitefreq, pairfreq, q):
    nP = sitefreq.shape[0]
    corr_matrix = np.empty(((nP)*(q-1), (nP)*(q-1)),dtype=float)
    for i in range(nP):
        for j in range(nP):
            for am_i in range(q-1):
                for am_j in range(q-1):
                    corr_matrix[i*(q-1)+am_i, j*(q-1)+am_j] = pairfreq[i,am_i,j,am_j] - sitefreq[i,am_i]*sitefreq[j,am_j]

    inv_corr = np.linalg.inv(corr_matrix)
    coupling_matrix = np.exp(np.negative(inv_corr))
    return coupling_matrix

def local_fields(coupling_matrix, sitefreq, q):
    N = sitefreq.shape[0]
    fields = np.empty((N*(q-1)),dtype=float)

    for i in range(N):
        for ai in range(q-1):
            fields[i*(q-1) + ai] = sitefreq[i,ai]/sitefreq[i,q-1]
            for j in range(N):
                for aj in range(q-1):
                    fields[i*(q-1) + ai] /= coupling_matrix[i*(q-1) + ai, j*(q-1) + aj]**sitefreq[j,aj]
    return fields

def local_fields2(i, j, coupling_matrix, sitefreq, q):
    couplings=np.ones([q,q])
    for A in range(q-1):
        for B in range(q-1):
            couplings[A,B] = coupling_matrix[i*(q-1)+A, j*(q-1)+B]
    exp_local_i = np.ones((q))/q
    exp_local_j = np.ones((q))/q
    Pi = sitefreq[i,:]
    Pj = sitefreq[j,:]

    epsilon=.0001
    diff=1.0
    while(diff>epsilon):

         scra1 = np.dot(exp_local_i, np.transpose(couplings))
         scra2 = np.dot(exp_local_j, couplings)

         new1 = np.divide(Pi, scra1)
         new1 = new1/(np.sum(new1))
         new2 = np.divide(Pj, scra2)
         new2 = new2/np.sum(new2)
         abs_diff = [max(abs(new1-exp_local_i)), max(abs(new2-exp_local_j))]

         diff = max(abs_diff)

         exp_local_i = new1
         exp_local_j = new2

    return couplings, exp_local_i, exp_local_j, Pi, Pj

def direct_information(sitefreq, coupling_matrix, nP, q):
    DI = np.zeros((nP,nP),dtype=float)

    ent = np.zeros(nP,dtype=float)
    for i in range(nP):
        ent[i] -= np.sum(sitefreq[i,:]*np.log(sitefreq[i,:]))

    #fields = local_fields(coupling_matrix, sitefreq, q)
    tiny = 0.000001
    for i in range(nP-1):
        for j in range(i+1,nP):
            #h_i = fields[i]
            #h_j = fields[j]
            #eij = coupling_matrix[i*(q-1):(i+1)*(q-1),j*(q-1):(j+1)*(q-1)]
            #Pi = sitefreq[i,:(q-1)]
            #Pj = sitefreq[j,:(q-1)]
            eij, h_i, h_j, Pi, Pj = local_fields2(i, j, coupling_matrix, sitefreq, q)
            x = np.multiply(eij, np.outer(h_i, h_j))
            Pij = x/sum(sum(x))
            Pfac = np.outer(Pi, Pj)
            z = np.outer(Pij, np.log((Pij+tiny)/(Pfac+tiny)))
            DI[i,j] = np.trace(z)
            DI[j,i] = DI[i,j]

#    for i in range(nP):
#        for j in range(nP):
#            h = np.min([ent[i],ent[j]])
#            DI[i,j] /= h

    average_DI = np.average(DI)
    for i in range(nP-1):
        average_i = np.average(DI[i,:])
        for j in range(i+1,nP):
            correction = (np.average(DI[:,j])*average_i)/average_DI
            DI[i,j] = DI[i,j] - correction
            DI[j,i] = DI[i,j]

    for i in range(nP):
        if ent[i] > (np.average(ent)+np.std(ent)):
            DI[i,:] = 0
            DI[:,i] = 0

    return DI
