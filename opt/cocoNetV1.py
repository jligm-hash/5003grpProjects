import numpy as np
import pandas as pd
import math
import sys

# input the connect ratio
print(sys.argv)

# set the random seed
np.random.seed(100)

setConnectRatio = float(sys.argv[1])
setBlockRatio = float(sys.argv[2])
setAgentNum = int(sys.argv[3])
setAgentTime = int(sys.argv[4])

setBeta = float(sys.argv[5]) # 0.0
setE2i = float(sys.argv[6]) # 4.5
setI2r = float(sys.argv[7]) # 8

argName = ['connectR', 'blockR', 'agentNum', 'maxTime', 'beta', 'e2i', 'i2r']
argStr = [str(i) for i in [setConnectRatio, setBlockRatio, setAgentNum, setAgentTime, setBeta, setE2i, setI2r]]

print('\t'.join(argName))
print('\t'.join(argStr))


# set the number of the agent, attributes of agents
agentNum = setAgentNum

infectedList = np.zeros(agentNum)
# SEIR model
susceptState = np.ones(agentNum)
exposedState = np.zeros(agentNum)
infectState = np.zeros(agentNum)
recoverState = np.zeros(agentNum)

# E2I time, I2R time
timeAtBecomeE = np.zeros(agentNum)
timeAtBecomeI = np.zeros(agentNum)

# initial the SEIR, contact as output 
resTime = []
resSList = []
resEList = []
resIList = []
resRList = []
resContactList = []

# initial some parameter
s2eRatio = setBeta # 0.016 # beta value for S to E

# time for I to remove
time4e2i = np.random.lognormal(mean=setE2i, sigma=1.5, size=agentNum)
time4i2r = np.random.lognormal(mean=setI2r, sigma=2, size=agentNum) # the average recover time


# initial contact matrix, infectious related
# contactNetwork = np.random.normal(size=(agentNum, agentNum)) # use random at the first
contactNetwork = np.random.uniform(size=(agentNum, agentNum))
setBlockRatio = setBlockRatio # 0.8
setConnectRatio = setConnectRatio # 0

blockAindex = int(agentNum*setBlockRatio*(1-setConnectRatio))
blockABindex = int(agentNum*setBlockRatio)
blockBindex = int(agentNum*setBlockRatio+agentNum*(1-setBlockRatio)*setConnectRatio)

# create the symmetry network
networkSym = contactNetwork.copy()
for i in range(networkSym.shape[0]):
    for j in range(i, networkSym.shape[1]):
        if j >= blockABindex and j <= blockBindex and i <= blockAindex:
            networkSym[j][i] = 0
        if j >= blockBindex and i <= blockABindex:
            networkSym[j][i] = 0
        
        if networkSym[j][i] < 0:
            networkSym[j][i] = -networkSym[j][i]
            
        if networkSym[j][i] > 1:
            networkSym[j][i] = 1
            
            
        networkSym[i][j] = networkSym[j][i]

        if i == j :
            networkSym[i][j] = 0
            
# initial the simulation
myTime = 1
contactCount = 0
# at the begining we will assign the number of E patient
susceptState[0] = 0
exposedState[0] = 1
timeAtBecomeE[0] = myTime

# infectState, recoverState

susNumRatio = np.sum(susceptState)
expNumRatio = np.sum(exposedState)
infNumRatio = np.sum(infectState)
recNumRatio = np.sum(recoverState)
susRatio = susNumRatio/agentNum
expRatio = expNumRatio/agentNum
infRatio = infNumRatio/agentNum
recRatio = recNumRatio/agentNum

header = 'Time: '+str(myTime) + ', Contact: ' + str(contactCount) + '\n' + '\t'.join(['susNumRatio', 'expNumRatio', 'infNumRatio', 'recNumRatio'])
prtStr = '\t'.join([str(i) for i in [susNumRatio, expNumRatio, infNumRatio, recNumRatio]])
prtStr2 = '\t'.join([str(i) for i in [susRatio, expRatio, infRatio, recRatio]])

# print('\n'.join([header, prtStr, prtStr2]))

resTime.append(myTime)
resSList.append(susRatio)
resEList.append(expRatio)
resIList.append(infRatio)
resRList.append(recRatio)
resContactList.append(contactCount)

# use while true to get the simulation:
flag4print = 0
while(True):
    # step the simulation when time is to limited or rRatio to 1:
    if recRatio == 1:
        break
    if myTime == setAgentTime:# -1: # -1 for not setting the time
        break
    
    # begin contact, all people contact together                           
    susIndex = np.where(susceptState==1)[0]
    expIndex = np.where(exposedState==1)[0]
    infIndex = np.where(infectState==1)[0]
    recIndex = np.where(recoverState==1)[0]

    myTime = myTime + 1
    contactCount = 0

    # update E to I
    for i in expIndex: # only E will lead to E, I is reomved to be recover
        if myTime - timeAtBecomeE[i] > time4e2i[i]:
            # E to I
            exposedState[i] = 0
            infectState[i] = 1
            timeAtBecomeI[i] = myTime
            # record the expose time and E -> I

    # update I to R
    for i in infIndex: 
        if myTime - timeAtBecomeI[i] > time4i2r[i]:
            # I to R
            infectState[i] = 0
            recoverState[i] = 1

    # update S to E, only I cannot contact
    for i in range(agentNum):
        for j in range(i, agentNum):

            # Infectious cannot cantact
            if infectState[i] == 1 or infectState[i] == 1:
                continue

            # S, E, R can contact
            contactProbability = networkSym[i,j]
            if contactProbability > 1 : # make sure the Pr <= 1
                contactProbability = 1
            # print(contactProbability)

            # contact now
            if np.random.binomial(1,contactProbability):
                contactCount = contactCount + 1 # successful contact
                # print(str(i) + ' and ' + str(j) + ' contacts')

                # only Se, ES will change the state
                if exposedState[i] == 1 and susceptState[j] == 1:
                    if np.random.binomial(1, s2eRatio): # use beta value for S2E
                        susceptState[j] = 0
                        exposedState[j] = 1
                        timeAtBecomeE[j] = myTime
                        continue
                elif exposedState[j] == 1 and susceptState[i] == 1:
                    if np.random.binomial(1, s2eRatio): # use beta value for S2E
                        susceptState[i] = 0
                        exposedState[i] = 1
                        timeAtBecomeE[i] = myTime
                        continue

            else: # not contact
                contactCount = contactCount
                # print(str(i) + ' and ' + str(j) + ' not contacts')
    
    if flag4print == 1:
        print('Total number of contacts is ' + str(contactCount), flush=True) # the contact is important, that is the reason why we will record it

    # call the summary
    susNumRatio = np.sum(susceptState)
    expNumRatio = np.sum(exposedState)
    infNumRatio = np.sum(infectState)
    recNumRatio = np.sum(recoverState)
    susRatio = susNumRatio/agentNum
    expRatio = expNumRatio/agentNum
    infRatio = infNumRatio/agentNum
    recRatio = recNumRatio/agentNum

    header = 'Time: '+str(myTime) + ', Contact: ' + str(contactCount) + '\n' + '\t'.join(['susNumRatio', 'expNumRatio', 'infNumRatio', 'recNumRatio'])
    prtStr = '\t'.join([str(i) for i in [susNumRatio, expNumRatio, infNumRatio, recNumRatio]])
    prtStr2 = '\t'.join([str(i) for i in [susRatio, expRatio, infRatio, recRatio]])
    
    if flag4print == 1:
        print('\n'.join([header, prtStr, prtStr2]), flush=True)

    resTime.append(myTime)
    resSList.append(susRatio)
    resEList.append(expRatio)
    resIList.append(infRatio)
    resRList.append(recRatio)
    resContactList.append(contactCount)
    
# demo the output
totalMaxContact = agentNum*(agentNum-1)/2 # nCr(agentNum, 2)
expectedMaxContactRatio = [i/totalMaxContact for i in resContactList]
totalMaxContact =max(resContactList)
realMaxContactRatio = [i/totalMaxContact for i in resContactList]

# save the output, one is the network, other is the seir ratio
resDf = pd.DataFrame({'time':resTime,
                     'sR':resSList,
                     'eR':resEList,
                     'iR':resIList,
                     'rR':resRList,
                     'conN':resContactList,
                     'conExpR':expectedMaxContactRatio,
                     'conReaR':realMaxContactRatio})

resHeader = []

for i in range(len(argName)):
    resHeader.append(argName[i])
    resHeader.append(argStr[i])
# print(resHeader)
# print('.'.join(resHeader))

# resDf.to_csv('res.coconet.dyn.'+str(setConnectRatio)+'.csv', index=None)
# np.savetxt('res.coconet.net.'+str(setConnectRatio)+'.csv', np.asarray(networkSym), delimiter=",")

resDf.to_csv('res.coconet.dyn.'+'.'.join(resHeader)+'.csv', index=None)
np.savetxt('res.coconet.net.'+'.'.join(resHeader)+'.csv', np.asarray(networkSym), delimiter=",")
