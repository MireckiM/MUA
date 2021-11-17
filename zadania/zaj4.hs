import Euterpea
myMel1 = line [c 4 en, e 1 en, g 4 en, a 4 en, a 4 en, a 4 en, rest hn, g 4 qn, g 4 qn, g 4 qn]
my1 = instrument Violin myMel1
my2 = Modify (Phrase [Art $ Legato 0.8]) myMel1
my3 = Modify (Transpose 7) myMel1
my4 = Modify (Phrase [Orn $ Trill]) $ addVolume 100 myMel1
my5 = transpose 7 my3
my6 = invert myMel1
my7 = retro myMel1
my8 = invertRetro myMel1
my9 = retroInvert myMel1

myMel2 = line [myMel1,my9,my8,my2,myMel1]

myComp = line [c 2 en, c 2 en, c 3 en, c 3 en, c 3 en, c 4 en, c 5 en, c 1 en, c 1 en, c 6 en, c 6 en, c 7 en]
myC1 = transpose 7 myComp
compInvert = invert myComp
compRetro = retro myComp
compLeg = Modify (Phrase [Art $ Legato 0.8]) myComp
compTemp = Modify (Phrase [Tmp $ Ritardando 0.5]) myComp
compTemp2 = Modify (Phrase [Tmp $ Accelerando 1.5]) myComp

comp1 = line [compTemp2,compInvert,compLeg,myComp,compRetro,compTemp]
comp1Marimba = instrument Marimba comp1

morRem = line [g 6 en,g 5 en,g 6 en,g 5 en,g 6 en,g 5 en,g 7 en,g 4 en]
