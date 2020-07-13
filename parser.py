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
    'Braga' : ['Porto','Viana do Castelo', 'Vila Real'],
    'Viana do Castelo' : ['Braga'],
    'Porto' : ['Braga', 'Vila Real', 'Aveiro', 'Viseu'],
    'Vila Real' : ['Porto', 'Braga', 'Braganca', 'Viseu'],
    'Braganca' : ['Vila Real', 'Guarda', 'Viseu'],
    'Aveiro' : ['Viseu','Porto','Coimbra'],
    'Viseu' : ['Aveiro', 'Porto', 'Vila Real', 'Guarda', 'Braganca', 'Coimbra'],
    'Guarda' : ['Braganca', 'Castelo Branco', 'Viseu', 'Coimbra'],
    'Coimbra' : ['Aveiro', 'Viseu', 'Leiria', 'Castelo Branco', 'Guarda'],
    'Castelo Branco' : ['Guarda', 'Coimbra', 'Leiria', 'Santarem', 'Portalegre'],
    'Leiria' : ['Coimbra', 'Castelo Branco', 'Santarem', 'Lisboa'],
    'Santarem' : ['Leiria', 'Lisboa', 'Setubal', 'Portalegre', 'Evora', 'Castelo Branco'],
    'Portalegre' : ['Castelo Branco', 'Santarem', 'Evora'],
    'Lisboa' : ['Leiria', 'Santarem', 'Setubal'],
    'Setubal' : ['Evora', 'Beja', 'Santarem', 'Lisboa'],
    'Evora' : ['Setubal', 'Santarem', 'Portalegre', 'Beja'],
    'Beja' : ['Setubal', 'Evora', 'Faro'],
    'Faro' : ['Beja']
}

#Dictionary with the connections between districts and famous monuments
mon_dict = {
    'Braga' : ['Bom Jesus do Monte' , 'Santuario da Nossa Senhora do Sameiro'],
    'Vila do Conde' : ['Aqueduto de Santa Clara'],
    'Sagres' : ['Rosa dos Ventos'],
    'Povoa de Varzim' : ['Cividade de Terroso'],
    'Evora' : ['Cromleque dos Almendres' , 'Templo de Diana', 'Convento dos Loios', 'Mosteiro dos Ossos'],
    'Faro' : ['Arco da Vila'],
    'Porto' : ['Cemiterio de Agramonte' , 'Monumento aos Herois da Guerra Peninsular','Palacio da Bolsa','Torre dos Clerigos'],
    'Lisboa' : ['Cristo Rei' , 'Arco Triunfal da Rua Augusta' , 'Torre de Belem' , 'Padrao dos Descobrimentos', 'Panteao Nacional', 'Monumento aos Combatentes do Ultramar', 'Mosteiro dos Jeronimos'],
    'Sintra' : ['Palacio da Pena','Quinta da Regaleira','Palacio de Monserrate'],
    'Batalha' : ['Mosteiro da Batalha'],
    'Alcobaca' : ['Mosteiro de Alcobaca'],
    'Guarda' : ['Se da Guarda'],
    'Chaves' : ['Ponte romana de Trajano'],
    'Ovar' : ['Igreja Paroquial de valega'],
    'Vieira do Minho' : ['Ponte da Mizarela'],
    'Mealhada' : ['Palacio do Bucaco'],
    'Tomar' : ['Convento de Cristo'],
    'Moncao' : ['Palacio da Brejoeira'],
    'Viana do Castelo' : ['Santa Luzia'],
    'Guimaraes' : ['Castelo Guimaraes','Paco dos duques'],
    'Obidos' : ['Castelo Obidos'],
    'Mafra' : ['Convento de Mafra'],
    'Coimbra' : ['Portugal dos Pequeninos'],
    'Fatima' : ['Santuario de Fatima'],
    'Almada' : ['Cristo Rei'],
    'Faro' : ['Ruinas de Milreu']
}

#Wtite complete dataset
full = open('cidades.pl','w+',encoding='utf-8')
full.write('%%cidade(ID,Cidade,Lat,Long,Distrito,Ligacões,Monumentos,temMonumentos,Capital)\n')
for line in dataset.values:
    if (line[1] in mon_dict):
        full.write("cidade(%d,'%s',%f,%f,'%s',%s,%s,'Sim','%s').\n" %(line[0],line[1],line[2],line[3],line[4],dist_dict[line[4]],mon_dict[line[1]],line[5]))
    elif (line[4] in mon_dict):
        full.write("cidade(%d,'%s',%f,%f,'%s',%s,%s,'Sim','%s').\n" %(line[0],line[1],line[2],line[3],line[4],dist_dict[line[4]],mon_dict[line[4]],line[5]))
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

        
            
