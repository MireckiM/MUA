import Euterpea

akord = chord [ c 3 wn , e 3 wn , g 3 wn ]
aline = line [ c 3 wn , e 3 wn , g 3 wn ]
sekline = c 3 wn :+: e 3 wn :+: g 3 wn
sekaline = aline :=: sekline

s1 = a 5 qn
alls00 = line [c (-1) (1/5), d (-1) (1/5), e (-1) (1/5), f (-1) (1/5), g (-1) (1/5), a (-1) (1/5), b (-1) (1/5)]
alls0 = line [c 0 (1/5), d 0 (1/5), e 0 (1/5), f 0 (1/5), g 0 (1/5), a 0 (1/5), b 0 (1/5)]
alls1 = line [c 1 (1/5), d 1 (1/5), e 1 (1/5), f 1 (1/5), g 1 (1/5), a 1 (1/5), b 1 (1/5)]
alls2 = line [c 2 (1/5), d 2 (1/5), e 2 (1/5), f 2 (1/5), g 2 (1/5), a 2 (1/5), b 2 (1/5)]
alls3 = line [c 3 (1/5), d 3 (1/5), e 3 (1/5), f 3 (1/5), g 3 (1/5), a 3 (1/5), b 3 (1/5)]
alls4 = line [c 4 (1/5), d 4 (1/5), e 4 (1/5), f 4 (1/5), g 4 (1/5), a 4 (1/5), b 4 (1/5)]
alls5 = line [c 5 (1/5), d 5 (1/5), e 5 (1/5), f 5 (1/5), g 5 (1/5), a 5 (1/5), b 5 (1/5)]
alls6 = line [c 6 (1/5), d 6 (1/5), e 6 (1/5), f 6 (1/5), g 6 (1/5), a 6 (1/5), b 6 (1/5)]
alls7 = line [c 7 (1/5), d 7 (1/5), e 7 (1/5), f 7 (1/5), g 7 (1/5), a 7 (1/5), b 7 (1/5)]
alls8 = line [c 8 (1/5)]--, d 8 (1/5), e 8 (1/5), f 8 (1/5), g 8 (1/5), a 8 (1/5), b 8 (1/5)]

alls = alls00 :+: alls0 :+: alls1 :+: alls2 :+: alls3 :+: alls4 :+: alls5 :+: alls6 :+: alls7 :+: alls8

allstemp3 = Modify (Tempo 3.5) alls

allstemp3c = instrument EnglishHorn allstemp3

allsp = instrument Percussion alls2


--DistortionGuitar
--Gunshot
--MelodicDrum
--SynthDrum
--ElectricGrandPiano


allsc = Modify (Tempo 0.4) alls

allsc4 = instrument AcousticGuitarNylon alls4

--resty

r1 = line [c 4 (1/5),rest 1, d 4 (1/5),rest (1/2), e 4 (1/5)]

allt1 = transpose 1 alls1
allst = allt1 :=: alls0


mel = line [ c 4 en , c 4 en , g 4 en , a 4 en , a 4 en , g 4 qn ]

mymel2 = Modify ( Phrase [ Tmp $ Ritardando 0.5]) mel
mymel3 = Modify ( Phrase [ Tmp $ Accelerando 1.5]) mel
my5 = phrase [ Tmp $ Accelerando 1.5] mel

mymel4 = Modify ( Phrase [ Dyn $ Diminuendo 0.8]) mel
mymel5 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) mel

mymel11 = Modify ( Phrase [ Orn $ Trill ]) $ addVolume 100 mel
mymel12 = Modify ( Phrase [ Orn $ InvMordent ]) $ addVolume 50 mel

my6 = invert mel
my7 = retro mel
my8 = invertRetro mel
my9 = retroInvert mel

ip7 a = perc LowAgogo a
ip8 a = perc RideBell a
ciag = [1 , 2 , 2, 1 , 1 , 2 , 1 , 2 , 2, 1 , 2 , 2 , 1 , 1 , 2, 1 , 1 , 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2 ]

funNaCiag d instPer 1 = rest d
funNaCiag d instPer 2 = instPer d

rytm n d pe sn = line $ take n $ map ( funNaCiag d pe ) sn
--i oto nasza muzyka
muzyka = rytm 20 (1/8) ip8 ciag

tresillo a = let p a = perc OpenHiHat a
                 p1 a = line [p a , rest a , rest a ]
                 p2 a = line [p a , rest a ]
             in times 5 $ line [ p1 a , p1 a , p2 a ]

tangolo a b = let p1 a = perc OpenHiHat a
                  p2 b = perc HandClap b
                  r1 a = line [ p1 a , p1 a, p1 a]
                  r2 b = line [ p2 b , p2 b]
              in times 1 $ tempo (3/2) ( r1 a ) :=: ( r2 b)


