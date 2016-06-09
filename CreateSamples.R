
library(igraph)
library(MASS)

config.samples_num = 200 # 样本数
config.node_num = 10 # 节点数q
config.arr_num = 20 # 属性信息个数p
#config.arr_sample_num = 1000 #属性信息变量样本个数
config.power = 1 # 生成scale-free network 幂率指数
config.beta = 4 # 
config.rho = 0.8 #
config.p_theta = 0.8 #Gibbs sampling probilities
G = vector('list', config.samples_num) # 图list
M = vector('list', config.samples_num) # 矩阵list
Th = vector('list', config.samples_num) # theta矩阵list

if( config.rho >0 && config.rho <1 && config.p_theta >0 && config.p_theta<1 ){
    for (i in 1:config.samples_num){
        g <- barabasi.game(config.node_num , power = config.power, directed = FALSE) #scale-free network 生成图
        G[[i]] = g
        M[[i]] = as_adjacency_matrix(g)
        Th[[i]] = M[[i]]

        theta = matrix( rep(0, config.node_num*config.node_num), config.node_num, config.node_num )
        for (ir in 1:config.node_num){
            for (ic in ir:config.node_num){
                if( ir == ic){
                    config.rho_now = 1
                }else{
                    config.rho_now = config.rho
                }
                x = rnorm ( config.arr_num )
                x = cbind (1, t(x))
                v_theta = GenerateVectorTheta(config)
                if( ir == ic){
                    theta[ir,ic] = (x %*% v_theta)/2
                }else{
                    theta[ir,ic] = (x %*% v_theta)
                }
            }
        }
        theta = theta + t(theta)
        info = summary(Th[[i]])
        for (io in 1:nrow(info)){
            Th[[i]]@x[io] = theta[info[io,1],info[io,2]]
        }


    }
}else{
    cat("rho is error!\n");
}

GenerateVectorTheta = function( config ){
    len = config.arr_num +1
    v_theta = rep(0, len)
    rand = runif(len, min = 0, max = 1)
    for (j in 1:len){
        if( rand[j]>config.rho_now ){
            v_theta[j] = 0
        }else if( rand[j]<= config.rho_now/2 ){
            v_theta[j] = config.beta
        }else{
            v_theta[j] = (-config.beta)
        }
    }
    return (v_theta)
}

GibbsSampling = function( p_theta,config ){
    
}