# 3. NALOGA

zabojniki = [1, 3, 4, 7, 10]


def candidates(n):
    return [zaboj for zaboj in zabojniki if zaboj <= n]


#this function returns a list with minimal number of boxes which we don't need but it helps
def najmanj_zabojev(n):
    if n == 0:
        return []
    cands = candidates(n)
    if cands == []:
        raise RuntimeError("no solution found")
    largest_candidate = max(cands)
    s = najmanj_zabojev(n - largest_candidate)
    return s + [largest_candidate]

#this function displays the posibilities in a very ugly way
#I think it does calculate all the posibilities but in such a way that it is not possible for me to count them
#I don't have enough time to fi this but the idea would be to somehow change that and then len(moznosti(nostilnost)) would give me the right result
def vse_moznosti(nosilnost):
    if nosilnost == 0:
        return []
    cands = candidates(nosilnost)
    if cands == []:
        raise RuntimeError("no solution found")
    moznosti = []
    for zaboj in candidates(nosilnost):
        vmesna_nosilnost = vse_moznosti(nosilnost - zaboj)
        #print(vmesna_nosilnost)
        moznosti.append(vmesna_nosilnost + [zaboj])
    return moznosti
