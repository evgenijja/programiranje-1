# 3. NALOGA

zabojniki = [1, 3, 4, 7, 10]


def candidates(n):
    return [zaboj for zaboj in zabojniki if zaboj <= n]

def najvecji_zaboj(n):
    if n == 0:
        return []
    cands = candidates(n)
    if cands == []:
        raise RuntimeError("no solution found")
    largest_candidate = max(cands)
    s = najvecji_zaboj(n - largest_candidate)
    return s + [largest_candidate]


