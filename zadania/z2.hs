import Euterpea
melody = line [c (-1) en, c 0 en, c 1 en, c 2 en, c 3 en, c 4 en, c 5 en, c 6 en, c 7 en, c 8 en] 
melody2 = line [c (-1) qn, c 0 qn, c 1 qn, c 2 qn, c 3 qn, c 4 qn, c 5 qn, c 6 qn, c 7 qn, c 8 qn]
melody2rev = line [c 8 qn, c 7 qn, c 6 qn, c 5 qn, c 4 qn, c 3 qn, c 2 qn, c 1 qn, c 0 qn, c (-1) qn]
melody3 = line [c (-1) hn, c 0 hn, c 1 hn, c 2 hn, c 3 hn, c 4 hn, c 5 hn, c 6 hn, c 7 hn, c 8 hn]

melody2t = Modify (Transpose 7) melody2
melody2t2temp = Modify (Tempo 1.8) melody2t
melcomp = line [melody2 :=: melody2t]
melcomp2 = Modify (Tempo 2.0) melcomp
melody2temp = Modify (Tempo 2.0) melody

--muzyka glowna do odtworzenia
music2 = line [melody2temp :=: melody2t2temp :=: melcomp2]

music3 = Modify (Tempo 3.0) music2

