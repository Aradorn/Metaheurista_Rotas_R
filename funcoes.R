calc_custo = function (tabela,rota) {
	crota = 0
	qtd_paradas = length(rota)-1

	for(i in 1:qtd_paradas) {
		crota = crota + tabela[rota[i],rota[i+1]]
	}
	crota = crota + tabela[rota[length(rota)], rota[1]] # Volta para Origem
}

heuristica_vizinho = function(tabela) {
	custo_best = 99999
	cid_best = 0
	cid_best = as.vector(cid_best)

	parada_inicial = sample(1:length(tabela[1,]),1) #sorteio
	cidades = 1:length(tabela[1,]) #vetor paradas
	cidades = cidades[-c(parada_inicial)] #retira primeira parada
	rota = parada_inicial

	n = length(cidades)-1
	#print("Rota em Construção")

	while(length(rota) <= (length(tabela[1,])-2)){
		for (i in 1:n) {
			rotat = c(rota,cidades[i])
			if(calc_custo(tabela,rotat) < custo_best) {
				custo_best = as.numeric(calc_custo(tabela,c(rota,cidades[i])))
				cid_best = cidades[i]
			}
		}
		rota = c(rota,cid_best)
		custo_best = 99999
		cidades = cidades[-c(match(cid_best,cidades))]
		n = length(cidades)-1
		cid_best = NULL
	}
	return(rota)
}

swap = function(tabela,Si,T, nvizinho, qtd_swaps){
	custo_best = 99999
	SBv = Si
	qtd_swaps = qtd_swaps
	for(j in 1:nvizinho) {
		tabu = TRUE
		while(tabu == TRUE) {
			v = sample(1:length(Si),qtd_swaps,replace=FALSE)
			for(k in 1:(length(T[,1]-1))){
				for(l in 1:(length(T[1,]-1))){
					if(T[k,l]==v[l]){
						tabu = TRUE
					} else {
						tabu = FALSE
					}
				}
			}
			Sv = Si
	
			for(m in 1:(qtd_swaps/2)){
				for(n in qtd_swaps:(qtd_swaps/2+1)){
					x = Sv[v[m]]
					Sv[v[m]] = Sv[v[n]]
					Sv[v[n]] = x
				}
			}					
				
			custo_vizinho = calc_custo(tabela,Sv)
			if(custo_vizinho < custo_best) {
				SBv = Sv
				custo_best = custo_vizinho
			}
		}
	}
	lista = list(SBv,v)
	return(lista)
}

busca_tabu = function(tabela, Si, BTmax, nvizinho, Lt){

	BTmax = BTmax
	Lt = Lt
	nvizinho = nvizinho
	Si = Si
	Sb = Si
	melhor_iter = 0
	Sv = Si
	v = c(0,0)
	qtd_swaps = 2

	T = matrix(data=0, nrow = Lt, ncol = qtd_swaps, byrow = FALSE, dimname = NULL)
	
	tempo = 0
	tempo = proc.time()
	iter = 0
	tempo = proc.time()
	while((iter - melhor_iter) < BTmax) {
		iter = iter + 1
	if((iter - melhor_iter) == BTmax/2) {
			qtd_swaps = round((0.20*length(Si)),0)
			if(qtd_swaps%%2 != 0){
				qtd_swaps = qtd_swaps+1
			}
			T = matrix(data=0, nrow = Lt, ncol = qtd_swaps, byrow = FALSE, dimname = NULL)
		}
		if ((iter - melhor_iter) == ((BTmax/2)+200)){	
			qtd_swaps = 2
			T = matrix(data=0, nrow = Lt, ncol = qtd_swaps, byrow = FALSE, dimname = NULL)
		}
		lista = swap(tabela, Si, T, nvizinho, qtd_swaps)
		Sv = lista[[1]][1:length(lista[[1]])]
		for(i in (length(T[,1])-1):1){
			T[i+1,] = T[i,]
		}
		T[1,] = lista[[2]][1:2]
		if(calc_custo(tabela,Sv) < (calc_custo(tabela,Sb))){
			Sb = Sv
			melhor_iter = iter
		}
		Si = Sv		
	}
	tempo = proc.time()-tempo
	tempo = round(tempo[3],2)
	custo = calc_custo(tabela,Sb)
	lista = list(custo,tempo,Sb,T)
	return(lista)
}

#	qtd_swaps = round((0.10*length(Si)),0)
#	if(qtd_swaps%%2 != 0){
#		qtd_swaps = qtd_swaps+1}