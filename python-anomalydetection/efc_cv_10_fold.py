import numpy as np
import sys
import matplotlib.pyplot as plt
from dca_functions import *
import time

dataset = "10_4" # os tres datasets sao 10_2, 10_3 e 10_4
# days = sys.argv[1] 
days = "weekdays"


LAMBDA = 0.5
Q = 11
THETA = 0.9



for c in range(1,6):
    train_time = []
    test_time = []
    for cv in range(1):
        start = time.time()

        labels = np.load("labels/labels_{}_cluster_{}_{}.npy".format(dataset,c, days))
        raw_data = np.load("datasets/dataset_{}_cluster_{}_{}.npy".format(dataset,c, days))
        num_samples = raw_data.shape[0]
        n = int(num_samples/10)
        training_data = np.array(list(raw_data)[:cv*n]+list(raw_data[(cv+1)*n:]))

        data, weights, Meff = Codemsa(training_data, THETA)
        sitefreq = Sitefreq(data, Meff, weights, Q, LAMBDA)
        pairfreq = Pairfreq(data, Meff, weights, sitefreq, Q, LAMBDA)
        couplings = Coupling(sitefreq, pairfreq, Q)
        train_data = raw_data[:n,:]
        data, a, b = Codemsa(train_data, THETA)
        energies = np.zeros(data.shape,dtype=float)
        for k in range(data.shape[0]):
            seq = data[k,:]
            for i in range(data.shape[1]):
                for j in range(data.shape[1]):
                    if i != j:
                        energies[k,i] -= np.log(couplings[(Q-1)*i + seq[i], (Q-1)*j + seq[j]])

        end = time.time()
        print(end - start)
        train_time.append(end - start)

        with open('train_time_cluster-{}.txt'.format(c), 'w') as f:
            for item in train_time:
                f.write("%s\n" % item)

        for rep in range(1,31):
            start = time.time()
            cutoff = np.mean(energies,axis=0) + 4*np.std(energies,axis=0)

            test_labels = labels[n*cv:n*(cv+1)]
            test_data = raw_data[n*cv:n*(cv+1),:]
            data, a, b = Codemsa(test_data, THETA)
            energies = np.zeros(data.shape,dtype=float)
            for k in range(data.shape[0]):
                seq = data[k,:]
                for i in range(data.shape[1]):
                    for j in range(data.shape[1]):
                        if i != j:
                            energies[k,i] -= np.log(couplings[(Q-1)*i + seq[i], (Q-1)*j + seq[j]])
                for i in range(data.shape[1]):
                    if energies[k,i] > cutoff[i]:
                        with open("results/anomalies_{}_cluster_{}_{}_4std.dat".format(dataset,c,days),"a") as fl:
                            fl.write("{},{},{}\n".format(energies[k,i],test_labels[k],i))
            end = time.time()
            print(end - start)
            test_time.append(end - start)

        with open('test_time_cluster-{}.txt'.format(c), 'w') as f:
            for item in test_time:
                f.write("%s\n" % item)

            
