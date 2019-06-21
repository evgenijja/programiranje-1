
# 3. naloga



def vsi_mozni_skoki(energija):
    seznam_skokov = []
    for i in range(energija + 1):
        seznam_skokov.append(i)
    return seznam_skokov

e = 0
def hitrost_odskakljanja(močvirje):
    močvirje[0] = e
    pot = 0
    stevec_skokov = 0
    while pot < len(močvirje):
        for skok in vsi_mozni_skoki:
            stevec_skokov += 1
            pot = skok + vsi_mozni_skoki(e - skok + močvirje[skok])
    return stevec_skokov
#bom to raje pustila pri miru