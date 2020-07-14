import pandas as pd 
import numpy as np

#Read data
dataset = pd.read_excel(r'Data/cidades.xlsx',encoding='utf-8')

#Replace all accents, to become ascii compatible
dataset['city'] = dataset['city'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')
dataset['admin'] = dataset['admin'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')
dataset['capital'] = dataset['capital'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')

#Dictionary with the connections between districts
dist_dict = {
    'Braga' : ['Porto','Viana do Castelo','Vila Real'],
    'Viana do Castelo' : ['Braga'],
    'Porto' : ['Braga','Vila Real','Aveiro','Viseu'],
    'Vila Real' : ['Porto','Braga','Braganca','Viseu'],
    'Braganca' : ['Vila Real','Guarda','Viseu'],
    'Aveiro' : ['Viseu','Porto','Coimbra'],
    'Viseu' : ['Aveiro','Porto','Vila Real','Guarda','Braganca','Coimbra'],
    'Guarda' : ['Braganca','Castelo Branco','Viseu','Coimbra'],
    'Coimbra' : ['Aveiro','Viseu','Leiria','Castelo Branco','Guarda'],
    'Castelo Branco' : ['Guarda','Coimbra','Leiria','Santarem','Portalegre'],
    'Leiria' : ['Coimbra','Castelo Branco','Santarem','Lisboa'],
    'Santarem' : ['Leiria','Lisboa','Setubal','Portalegre','Evora','Castelo Branco'],
    'Portalegre' : ['Castelo Branco','Santarem','Evora'],
    'Lisboa' : ['Leiria','Santarem','Setubal'],
    'Setubal' : ['Evora','Beja','Santarem','Lisboa'],
    'Evora' : ['Setubal','Santarem','Portalegre','Beja'],
    'Beja' : ['Setubal','Evora','Faro'],
    'Faro' : ['Beja']
}

#Dictionary with the connections between districts and famous monuments
mon_dict = {
    'Braga' : ['Bom Jesus do Monte','Santuario da Nossa Senhora do Sameiro','Mosteiro Tibaes','Ponte da Mizarela','Castelo Guimaraes','Paco dos duques'],
    'Evora' : ['Cromleque dos Almendres','Templo de Diana','Convento dos Loios','Mosteiro dos Ossos'],
    'Faro' : ['Arco da Vila','Ruinas de Milreu','Palacio de Estoi','Rosa dos Ventos','Castelo de Silves'],
    'Porto' : ['Cemiterio de Agramonte','Monumento aos Herois da Guerra Peninsular','Palacio da Bolsa','Torre dos Clerigos','Aqueduto de Santa Clara','Igreja de Sao Goncalo'],
    'Lisboa' : ['Cristo Rei','Arco Triunfal da Rua Augusta','Torre de Belem','Padrao dos Descobrimentos','Panteao Nacional','Monumento aos Combatentes do Ultramar','Mosteiro dos Jeronimos','Palacio da Pena','Quinta da Regaleira','Palacio de Monserrate','Palacio Queluz','Convento de Mafra'],
    'Guarda' : ['Se da Guarda','Fortaleza de Almeida','Castelo de Sabugal','Castelo de Trancoso','Castelo de Folgosinho'],
    'Viana do Castelo' : ['Santa Luzia','Santuario de Nossa Senhora da Peneda','Ponte de Ponte Lima','Espigueiros de Soajo','Ponte Medieval de Vilar de Mouros','Palacio da Brejoeira'],
    'Coimbra' : ['Portugal dos Pequeninos','Biblioteca Joanina da Universidade de Coimbra','Mosteiro de Santa Clara-a-Velha'],
    'Santarem' : ['Castelo de Almourol','Castelo de Ourem','Castelo Templario de Tomar','Convento de Cristo','Aqueduto dos Pegoes','Santuario de Fatima'],
    'Viseu' : ['Catedral de Viseu','Mosteiro de Sao Joao de Tarouca','Castelo de Penedono','Ruinas do Castelo de Sernacelhe','Santuario nossa Senhora dos Remedios','Castelo de Lamego','Catedral de Lamego'],
    'Beja' : ['Convento da Nossa Senhora da Conceicao','Castelo de Mertola','Castelo de Noudar','Castelo de Beja','Castelo de Serpa','Pulo do Lobo'],
    'Portalegre' : ['Castelo de Portalegre','Torre de Atalaiao','Castelo de Alegrete','Forte da Graca'],
    'Castelo Branco' : ['Santuario de Nossa Senhora de Almortao','Castelo de Castelo Branco','Castelo de Monsanto','Castelo de Penha','Ruinas Romanas de Centum Cellas','Castelo de Belmonte'],
    'Setubal' : ['Santuario de Nossa Senhora do Cabo','Ruinas Romanas de Mirobriga','Igreja Convento de Jesus Setubal','Convento da Nossa Senhora da Arrabida'],
    'Leiria' : ['Grutas de Santo Antonio','Grutas de Mira de Aire','Grutas de Alvados','Grutas da Moeda','Mosteiro da Batalha','Mosteiro de Alcobaca','Castelo Obidos'],
    'Vila Real' : ['Igreja Matriz Alijo','Palacio Mateus','Igreja Nossa Senhora Guadalupe','Pelourinho de Vila Real','Parque Natural do Alvao','Ponte romana de Trajano'],
    'Braganca' : ['Castelo de Braganca','Gravuras Rupestres de Mazouco','Ponte Medieval de Mirandela','Castelo de Miranda do Douro'],
    'Aveiro' : ['Salinas de Aveiro','Convento de Arouca','Castelo de Santa Maria da Feira','Igreja Paroquial de valega','Palacio do Bucaco']
}

#Wtite complete dataset
full = open('cidades.pl','w+',encoding='utf-8')
full.write('%%cidade(ID,Cidade,Lat,Long,Distrito,Ligacões,Monumentos,temMonumentos,Capital)\n')
for line in dataset.values:
    if (line[1] in mon_dict):
        full.write("cidade(%d,'%s',%f,%f,'%s',%s,%s,'Sim','%s').\n" %(line[0],line[1],line[2],line[3],line[4],dist_dict[line[4]],mon_dict[line[1]],line[5]))
    else:
        full.write("cidade(%d,'%s',%f,%f,'%s',%s,[],'Nao','%s').\n" %(line[0],line[1],line[2],line[3],line[4],dist_dict[line[4]],line[5]))
full.close()


#Calculate distance
def calc_distance(latitude1,longitude1,latitude2,longitude2):
    result = np.sqrt(  (latitude1-latitude2)**2 + (longitude1-longitude2)**2 )
    return result


#Write the base knowledge, with the information of the frontiers
arc  = open('arcos.pl','w+',encoding='utf-8')
arcos = set()
arc.write('%%arco(Cidade1,Cidade2,Distancia)\n')
for line in dataset.values: #iter by lines
    #Calc distance between inner citys in district
    aux = dataset.loc[dataset['admin'] == line[4]] #Subset frame by district
    aux2 = aux.loc[aux['capital'] == 'minor']
    aux = aux.loc[aux['capital'] != 'minor']
    for i in range(0,len(aux)):
        for j in range(0,len(aux2)):
            arcos.add("arco({},{},{}).\n".format(aux.values[i][0],aux2.values[j][0],calc_distance(aux.values[i][2],aux.values[i][3],aux2.values[j][2],aux2.values[j][3])))
            arcos.add("arco({},{},{}).\n".format(aux2.values[j][0],aux.values[i][0],calc_distance(aux.values[i][2],aux.values[i][3],aux2.values[j][2],aux2.values[j][3])))
    #Calc distance between neighbour districts
    for distc in dist_dict[line[4]]:
        #Neighbour district
        aux2 = dataset.loc[dataset['admin'] == distc]
        aux2 = aux2.loc[aux2['capital'] != 'minor']
        aux = aux.loc[aux['capital'] != 'minor']
        for i in range(0,len(aux)):
            for j in range(0,len(aux2)):
                arcos.add("arco({},{},{}).\n".format(aux.values[i][0],aux2.values[j][0],calc_distance(aux.values[i][2],aux.values[i][3],aux2.values[j][2],aux2.values[j][3])))
                arcos.add("arco({},{},{}).\n".format(aux2.values[j][0],aux.values[i][0],calc_distance(aux.values[i][2],aux.values[i][3],aux2.values[j][2],aux2.values[j][3])))

#write to file
for l in arcos:
    arc.write(l) 
arc.close()

        
            
