
library(ggplot2)
pwd = '/Users/jiabaoli/Documents/GitHub/spaaGrpProjects/opt/result'
resFile = list.files(path = pwd, pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

dynFile = resFile[grepl('res.coconet.dyn', resFile)]
netFile = resFile[grepl('res.coconet.net', resFile)]

# tmpRes1 = strsplit(dynFile[1], 'res.coconet.dyn.connectR.')
# tmpRes2 = strsplit(tmpRes1[[1]][2], '.blockR.')
# tmpRes3 = strsplit(tmpRes2[[1]][2], '.agentNum.')
# tmpRes4 = strsplit(tmpRes3[[1]][2], '.maxTime.')
# tmpRes5 = strsplit(tmpRes4[[1]][2], '.beta.')
# tmpRes6 = strsplit(tmpRes5[[1]][2], '.e2i.')
# tmpRes7 = strsplit(tmpRes6[[1]][2], '.i2r.')
# tmpRes8 = strsplit(tmpRes7[[1]][2], '.csv')
# 
# connectRatio = as.numeric(tmpRes2[[1]][1])
# blockRatio = as.numeric(tmpRes3[[1]][1])
# agentNum = as.numeric(tmpRes4[[1]][1])
# maxTime = as.numeric(tmpRes5[[1]][1])
# beta = as.numeric(tmpRes6[[1]][1])
# e2i = as.numeric(tmpRes7[[1]][1])
# i2r = as.numeric(tmpRes8[[1]][1])

myStr2Num = function(x){
  tmpRes1 = strsplit(x, 'res.coconet.dyn.connectR.')
  tmpRes2 = strsplit(tmpRes1[[1]][2], '.blockR.')
  tmpRes3 = strsplit(tmpRes2[[1]][2], '.agentNum.')
  tmpRes4 = strsplit(tmpRes3[[1]][2], '.maxTime.')
  tmpRes5 = strsplit(tmpRes4[[1]][2], '.beta.')
  tmpRes6 = strsplit(tmpRes5[[1]][2], '.e2i.')
  tmpRes7 = strsplit(tmpRes6[[1]][2], '.i2r.')
  tmpRes8 = strsplit(tmpRes7[[1]][2], '.csv')
  
  connectRatio = as.numeric(tmpRes2[[1]][1])
  blockRatio = as.numeric(tmpRes3[[1]][1])
  agentNum = as.numeric(tmpRes4[[1]][1])
  maxTime = as.numeric(tmpRes5[[1]][1])
  beta = as.numeric(tmpRes6[[1]][1])
  e2i = as.numeric(tmpRes7[[1]][1])
  i2r = as.numeric(tmpRes8[[1]][1])
  
  return(list('connectRatio' = connectRatio,
              'blockRatio'=blockRatio,
              'agentNum'=agentNum,
              'maxTime'=maxTime,
              'beta'=beta,
              'e2i'=e2i,
              'i2r'=i2r))
}

myStr2Num4net = function(x){
  tmpRes1 = strsplit(x, 'res.coconet.net.connectR.')
  tmpRes2 = strsplit(tmpRes1[[1]][2], '.blockR.')
  tmpRes3 = strsplit(tmpRes2[[1]][2], '.agentNum.')
  tmpRes4 = strsplit(tmpRes3[[1]][2], '.maxTime.')
  tmpRes5 = strsplit(tmpRes4[[1]][2], '.beta.')
  tmpRes6 = strsplit(tmpRes5[[1]][2], '.e2i.')
  tmpRes7 = strsplit(tmpRes6[[1]][2], '.i2r.')
  tmpRes8 = strsplit(tmpRes7[[1]][2], '.csv')
  
  connectRatio = as.numeric(tmpRes2[[1]][1])
  blockRatio = as.numeric(tmpRes3[[1]][1])
  agentNum = as.numeric(tmpRes4[[1]][1])
  maxTime = as.numeric(tmpRes5[[1]][1])
  beta = as.numeric(tmpRes6[[1]][1])
  e2i = as.numeric(tmpRes7[[1]][1])
  i2r = as.numeric(tmpRes8[[1]][1])
  
  return(list('connectRatio' = connectRatio,
              'blockRatio'=blockRatio,
              'agentNum'=agentNum,
              'maxTime'=maxTime,
              'beta'=beta,
              'e2i'=e2i,
              'i2r'=i2r))
}

dynParaList = do.call(rbind, lapply(dynFile, myStr2Num))
netParaList = do.call(rbind, lapply(netFile, myStr2Num4net))

# draw the seir ratio
library(data.table)
dynParaDf = data.table(dynParaList)
netParaDf = data.table(netParaList)

dynParaDf$index = 1:dim(dynParaList)[1]
netParaDf$index = 1:dim(netParaList)[1]

index = dynParaDf[connectRatio == 0 &
                    blockRatio == 0.8  &
                    agentNum == 1000 &
                    maxTime == 500 &
                    beta == 0.001 &
                    e2i == 4.5 &
                    i2r == 4.5]$index

# read one csv
dynDf = fread(file.path(pwd, dynFile[index]))
dynDf2 = dynDf[dynDf$time>1,]
dynDf2$conN = NULL
dynDfMelt = melt(dynDf2, id = 'time')
dynDfMelt[variable=='sR']$variable = 'S'
dynDfMelt[variable=='eR']$variable = 'E'
dynDfMelt[variable=='iR']$variable = 'I'
dynDfMelt[variable=='rR']$variable = 'R'
dynDfMelt[variable=='conExpR']$variable = 'Contact Exp'
dynDfMelt[variable=='conReaR']$variable = 'Contact Rea'

ggplot(dynDfMelt, mapping = aes(x=time, y=value, color=variable)) +
  geom_line(size=1, alpha=0.5) +
  theme_minimal() +
  labs(x = 'Time (Days)',
       y = 'Ratio',
      color = '',
      title = dynFile[index])+
  theme(text = element_text(size = 20),
        plot.title = element_text(size = 10))
# ggsave(filename = paste0(dynFile[index],'.pdf'))


ggplot(dynDf, mapping = aes(x=time))+
  geom_line(mapping = aes(y=sR)) +
  geom_line(mapping = aes(y=eR)) +
  geom_line(mapping = aes(y=iR)) +
  geom_line(mapping = aes(y=rR)) +
  geom_line(mapping = aes(y=conExpR)) +
  geom_line(mapping = aes(y=conReaR)) 

# loop for all the seir curves
# indexList = dynParaDf[(connectRatio == 0 |
#                         connectRatio == 1) &
#                         blockRatio == 0.8 &
#                         agentNum == 1000 &
#                         maxTime == 500]$index
indexList = dynParaDf[  blockRatio == 0.8 &
                        agentNum == 1000 &
                        maxTime == 500 &
                          beta == 0.016 &
                          e2i == 4.5 &
                          i2r == 4.5]$index

for (index in indexList) {
  
  # read one csv
  dynDf = fread(file.path(pwd, dynFile[index]))
  dynDf2 = dynDf[dynDf$time>1,]
  dynDf2$conN = NULL
  dynDfMelt = melt(dynDf2, id = 'time')
  dynDfMelt[variable=='sR']$variable = 'S'
  dynDfMelt[variable=='eR']$variable = 'E'
  dynDfMelt[variable=='iR']$variable = 'I'
  dynDfMelt[variable=='rR']$variable = 'R'
  dynDfMelt[variable=='conExpR']$variable = 'Contact Exp'
  dynDfMelt[variable=='conReaR']$variable = 'Contact Rea'
  
  ggplot(dynDfMelt, mapping = aes(x=time, y=value, color=variable)) +
    geom_line(size=1, alpha=0.5) +
    theme_minimal() +
    labs(x = 'Time (Days)',
         y = 'Ratio',
         color = '',
         title = dynFile[index])+
    theme(text = element_text(size = 20),
          plot.title = element_text(size = 10))
  ggsave(filename = paste0(pwd,'/pdf/',dynFile[index],'.pdf'))
  
}


# draw the matrix
library(pheatmap)
library(viridis)
index = netParaDf[connectRatio == 0 &
                    blockRatio == 0.8  &
                    agentNum == 1000 &
                    maxTime == 500 &
                    beta == 0.016 &
                    e2i == 4.5 &
                    i2r == 4.5]$index

# read one csv
netDf = fread(file.path(pwd, netFile[index]),
              header = F)
pheatmap(netDf,
         cluster_rows = F,
         cluster_cols = F,
         annotation_names_row = F, 
         annotation_names_col = F,
         show_rownames = F, 
         show_colnames = F,
         color = viridis(60))



index = netParaDf[connectRatio == 1 &
                    blockRatio == 0.8  &
                    agentNum == 1000 &
                    maxTime == 500 &
                    beta == 0.016 &
                    e2i == 4.5 &
                    i2r == 4.5]$index

# read one csv
netDf = fread(file.path(pwd, netFile[index]),
              header = F)
pheatmap(netDf,
         cluster_rows = F,
         cluster_cols = F,
         annotation_names_row = F, 
         annotation_names_col = F,
         show_rownames = F, 
         show_colnames = F,
         color = viridis(60))


index = netParaDf[connectRatio == 0.1 &
                    blockRatio == 0.8  &
                    agentNum == 1000 &
                    maxTime == 500 &
                    beta == 0.016 &
                    e2i == 4.5 &
                    i2r == 4.5]$index

# read one csv
netDf = fread(file.path(pwd, netFile[index]),
              header = F)
pheatmap(netDf,
         cluster_rows = F,
         cluster_cols = F,
         annotation_names_row = F, 
         annotation_names_col = F,
         show_rownames = F, 
         show_colnames = F,
         color = viridis(60))


# get the optimization
indexList = dynParaDf$index
dynParaDf$mContactExp = 0
dynParaDf$MiR = 0
for (index in indexList) {
  # read one csv
  dynDf = fread(file.path(pwd, dynFile[index]))
  dynDf2 = dynDf[dynDf$time>1,]
  dynParaDf$mContactExp[index] = min(dynDf2$conExpR)
  dynParaDf$MiR[index] = max(dynDf2$iR)
}

colnames(dynParaDf)
dynParaDf$target = dynParaDf$mContactExp - dynParaDf$MiR

# beta
inpDynParaDf = dynParaDf[agentNum == 1000 &
                        maxTime == 500 &
                        blockRatio == 0.8 &
                        e2i == 4.5 &
                          i2r == 4.5]
ggplot(as.data.frame(inpDynParaDf), mapping = aes(x = unlist(inpDynParaDf$connectRatio),
                                   y = target)) +
  geom_line() +
  facet_wrap(~unlist(inpDynParaDf$beta)) +
  labs(title = 'Beta',
       x = 'Contact ratio',
       y = 'Target = min(Contact Exp ratio)-max(I ratio)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# e2i
inpDynParaDf = dynParaDf[agentNum == 1000 &
                           maxTime == 500 &
                           blockRatio == 0.8 &
                           beta == 0.016 &
                           i2r == 4.5]
ggplot(as.data.frame(inpDynParaDf), mapping = aes(x = unlist(inpDynParaDf$connectRatio),
                                                  y = target)) +
  geom_line() +
  facet_wrap(~unlist(inpDynParaDf$e2i)) +
  labs(title = 'E2I time mean',
       x = 'Contact ratio',
       y = 'Target = min(Contact Exp ratio)-max(I ratio)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# i2r
inpDynParaDf = dynParaDf[agentNum == 1000 &
                           maxTime == 500 &
                           blockRatio == 0.8 &
                           beta == 0.016 &
                           e2i == 4.5]
ggplot(as.data.frame(inpDynParaDf), mapping = aes(x = unlist(inpDynParaDf$connectRatio),
                                                  y = target)) +
  geom_line() +
  facet_wrap(~unlist(inpDynParaDf$i2r)) +
  labs(title = 'I2R time mean',
       x = 'Contact ratio',
       y = 'Target = min(Contact Exp ratio)-max(I ratio)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# block ratio
inpDynParaDf = dynParaDf[agentNum == 1000 &
                           maxTime == 500 &
                           beta == 0.016 &
                           e2i == 4.5 &
                           i2r == 4.5]
ggplot(as.data.frame(inpDynParaDf), mapping = aes(x = unlist(inpDynParaDf$connectRatio),
                                                  y = target)) +
  geom_line() +
  facet_wrap(~unlist(inpDynParaDf$blockRatio)) +
  labs(title = 'Block ratio',
       x = 'Contact ratio',
       y = 'Target = min(Contact Exp ratio)-max(I ratio)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# agent number
inpDynParaDf = dynParaDf[blockRatio == 0.8 &
                           maxTime == 500 &
                           beta == 0.016 &
                           e2i == 4.5 &
                           i2r == 4.5]
ggplot(as.data.frame(inpDynParaDf), mapping = aes(x = unlist(inpDynParaDf$connectRatio),
                                                  y = target)) +
  geom_line() +
  facet_wrap(~unlist(inpDynParaDf$agentNum)) +
  labs(title = 'Agent number',
       x = 'Contact ratio',
       y = 'Target = min(Contact Exp ratio)-max(I ratio)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# general plot for all
inpDynParaDf = dynParaDf
ggplot(inpDynParaDf, mapping = aes(x = as.factor(unlist(inpDynParaDf$connectRatio)),
                                   y = target,
                                   group = as.factor(unlist(inpDynParaDf$connectRatio)),
                                   color = as.factor(unlist(inpDynParaDf$connectRatio)))) +
  geom_boxplot() +
  geom_point() +
  labs(title = 'Combined parameters with equal distribution assumption',
       x = 'Contact ratio',
       y = 'Target = min(Contact Exp ratio)-max(I ratio)') +
  theme_bw() +
  guides(color="none") +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5))
