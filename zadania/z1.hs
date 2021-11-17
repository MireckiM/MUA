import Euterpea

melody1 = line [c 3 en,c 3 en,c 3 en, c 3 en, rest hn, c 3 en,c 3 en,c 3 en,c 3 en, rest hn, c 3 en,c 3 en,c 3 en,c 3 en, rest en, c 3 en, c 3 en, rest en, c 3 en, rest qn, c 3 en, c 3 en, rest hn, c 3 en,c 3 en,c 3 en, c 3 en, rest hn, c 3 en,c 3 en,c 3 en,c 3 en, rest hn, c 3 en,c 3 en,c 3 en,c 3 en, rest en, c 3 en, c 3 en, rest en, c 3 en, rest qn, c 3 en, c 3 en, rest hn, c 3 en, c 3 en, c 3 en, c 3 en, c 3 en, rest hn, c 3 en, c 3 en, c 3 en, c 3 en, c 3 en, rest hn, c 3 en, c 3 en, c 3 en, c 3 en, c 3 en, rest en, c 3 en, c 3 en, rest en, c 3 en, rest en, c 3 en, c 3 en, c 3 en, rest qn]

melody1backup = line [rest hn, rest hn, rest hn, rest hn, rest hn, rest hn, rest hn, rest hn, rest hn, rest hn,c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en, rest qn, c 3 en]

melody2 = line [c 3 en :=: c 3 en, c 3 en :=: c 3 en :=: c 3 en, c 3 en :=: c 3 en, c 3 en :=: c 3 en,  c 3 en :=: c 3 en :=: c 3 en,  c 3 en :=: c 3 en :=: c 3 en, c 3 en :=: c 3 en :=: c 3 en :=: c 3 en, rest hn, c 3 en :=: c 3 en, c 3 en, c 3 en :=: c 3 en :=: c 3 en, c 3 en :=: c 3 en, rest qn, c 3 en :=: c 3 en, c 3 en, c 3 en :=: c 3 en :=: c 3 en, c 3 en :=: c 3 en :=: c 3 en :=: c 3 en, rest hn, c 3 en, rest en, c 3 en, c 3 en :=: c 3 en, c 3 en]

melody25 = line [c 3 en :=: c 3 en, c 3 en, c 3 en :=: c 3 en :=: c 3 en, c 3 en :=: c 3 en :=: c 3 en :=: c 3 en, rest hn, c 3 en, c 3 en, c 3 en]

melody3 = Modify (Tempo 0.5) melody2
melody4 = melody1 :=: melody3
melody5 = melody2 :=: melody3
melody6 = Modify (Phrase [Dyn $ Diminuendo 0.8]) melody25
melody7 = Modify (Tempo 2.0) melody1
melody8 = melody7 :=: melody2
end = Modify (Phrase [Tmp $ Ritardando 0.5]) melody6



--muzyka wlasciwa do odsluchu
music = line [melody1 :=: melody1backup, melody2, melody4 :=: melody5, rest hn,melody7 :=: melody5,end]
--combo1 = c 4 qn :+: e 4 qn :=: g 4 qn :+: c 5 qn
--my1 = instrument Violin music
--music4 = Modify (Tempo 4.0) music
