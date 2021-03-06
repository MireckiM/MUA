import Euterpea



----test

--myMel4 = Modify ( Phrase [ Dyn $ Diminuendo 0.8]) myMel1
--myMel5 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) myMel1


--------------------------------------------------------------------------------intro
ch1 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 3) (chord [g 1 (1/2), g 1 (1/2)]))
s1 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 3) (g 4 (1/2)))

introp1 = line [ch1,s1,ch1,s1,ch1,s1,ch1,s1]

lastp1 = Modify (Tempo 0.5) (line [ch1,ch1,ch1,ch1])

ch2 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 3) (chord [f 1 (1/2), f 1 (1/2)]))

introp2 = line [ch2,s1,ch2,s1,ch2,s1,ch2,s1]

lastp2 = Modify (Tempo 0.5) (line [ch2,ch2,ch2,ch2])

ch3 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 3) (chord [c 1 (1/2), c 1 (1/2)]))

introp3 = line[ch3,s1,ch3,s1,ch3,s1,ch3,s1]

lastp3 = Modify (Tempo 0.5) (line [ch3,ch3,ch3,ch3])

ch4 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 1) (chord [c 1 (1/2), c 1 (1/2)]))
ch5 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 0) (chord [c 1 (1/2), c 1 (1/2)]))
s41 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 1) (chord [c 1 (1/4), c 1 (1/4)]))
s42 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 0) (chord [c 1 (1/4), c 2 (1/4)]))
s43 = Modify ( Phrase [ Dyn $ Crescendo 2.0]) (Modify (Transpose 0) (chord [c 1 (1/4), c 1 (1/4)]))
introp4 = line [ch4,s1,ch4,s1,s41,s42,s43]
introp5 = line [ch4,s1,ch4,s1,ch5,s1,ch5,s1]

lastp4 = Modify (Tempo 0.5) (line [ch4,ch4,ch4,ch4])
lastp5 = Modify (Tempo 0.5) (line [ch4,ch4,ch5,ch5])
latest = Modify (Tempo 0.5) (line [ch4,ch4,ch5,Modify (Tempo 0.5) ch5])

lastloop = Modify (Tempo 1.1) (line [lastp1,lastp2,lastp3,lastp5])
latestloop = Modify (Tempo 1.1) (line [lastp1,lastp2,lastp3,latest])

join = line [rest (2/3),s41,s42,s41]

intro = Modify (Tempo 1.1) (introp1 :+: introp2 :+: introp3 :+: introp4 :+: join)
introsh = Modify (Tempo 1.1) (introp1 :+: introp2 :+: introp3 :+: introp5)

introst = Modify (Tempo 2.5) (introp1 :+: introp2 :+: introp3 :+: introp5)

--------------------------------------------------------------------------------perc

allperc = instrument Percussion (line [c 2 (1/5), d 2 (1/5), e 2 (1/5), f 2 (1/5), g 2 (1/5), a 2 (1/5), b 2 (1/5)])

chp1  = line [f 2 (1/4), f 2 (1/4)]
chp2  = line [g 2 (1/4), g 2 (1/4)]
chp12 = line [f 2 (1/4), g 2 (1/4)]
chp3  = Modify (Tempo 2.0) (line [chp1 :=: chp1 ,chp2 :=: chp2])
chp1212 = Modify (Tempo 2.0) (line [chp12,chp12 :=: chp12])


chp1122 = instrument Percussion (Modify (Tempo 2.0) (line [chp1,chp1,chp2,chp2,chp1,chp1,chp2,chp2,chp1,chp1,chp2,chp2,chp1,chp1,chp2,chp2]))
chp121212 = instrument Percussion (Modify (Tempo 2.0) (line [chp12,chp12,chp12,chp12,chp12,chp12,chp12,chp12]))
 
intropp1 = instrument Percussion (line [chp1, chp2, chp1, chp2, chp1, chp2, chp1, chp2])

intropp2 = instrument Percussion (line [chp1,chp1212,chp1,chp1212,chp1,chp1212,chp1,chp1212])

intropp3 = instrument Percussion (line [chp1,chp3,chp1,chp3,g 2 (1/4),g 2 (1/4),f 2 (1/4),f 2 (1/4),g 2 (1/8),g 2 (1/8),f 2 (1/8),f 2 (1/8),f 2 (1/8),f 2 (1/8),g 2 (1/16),g 2 (1/16),g 2 (1/16),f 2 (1/16) :=: f 2 (1/16)])

intropfast1 = Modify (Tempo 4.5) (line [intropp1,intropp1])
intropfast2 = Modify (Tempo 2.5) (line [intropp2,intropp2])

loopp = Modify (Tempo 1.1) (line [chp121212,chp121212,chp121212,chp121212,chp121212,chp121212,chp121212,chp121212])
loopp2 = Modify (Tempo 1.1) (line [chp1122,chp1122,chp1122,chp1122])

looppbeg = Modify (Tempo 1.1) (line [intropp1,intropp1,intropp1,intropp1])
looppmed = Modify (Tempo 1.1) (line [intropp2,intropp2,intropp2,intropp2])
looppfin = Modify (Tempo 1.1) (line [intropp2,intropp2,intropp2,intropp3])


--------------------------------------------------------------------------------back

l1 = Modify (Transpose 3) (line [g 1 (1/4), g 1 (1/4)])
s2 = Modify (Transpose 3) (g 4 (1/2))

lp1 = line [l1,s2,l1,s2,l1,s2,l1,s2]

l2 = Modify (Transpose 3) (line [f 1 (1/4), f 1 (1/4)])

lp2 = line [l2,s2,l2,s2,l2,s2,l2,s2]

l3 = Modify (Transpose 3) (line [c 1 (1/4), c 1 (1/4)])

lp3 = line[l3,s2,l3,s2,l3,s2,l3,s2]

l4 = Modify (Transpose 1) (line [c 1 (1/4), c 1 (1/4)])

lp4 = line[l4,s2,l4,s2,s41,s42,s43]

l5 = Modify (Transpose 0) (line [c 1 (1/4), c 1 (1/4)])

lp5 = line[l4,s2,l4,s2,l5,s2,l5,s2]

back = line [lp1 :+: lp2 :+: lp3 :+: lp4 :+: join]
backsh = line [lp1 :+: lp2 :+: lp3 :+: lp5]
loop1 = Modify (Tempo 1.1) back
loopsh = Modify (Tempo 1.1) backsh

s3 = Modify (Transpose 3) (line [g 4 (1/4),g 3 (1/4)])

ln = Modify (Tempo 1.1) (line [l1,s3,l1,s3,l1,s3,l1,s3,l2,s3,l2,s3,l2,s3,l2,s3,l3,s3,l3,s3,l3,s3,l3,s3,l4,s3,l4,s3,s41,s42,s43])
lnsh = Modify (Tempo 1.1) (line [l1,s3,l1,s3,l1,s3,l1,s3,l2,s3,l2,s3,l2,s3,l2,s3,l3,s3,l3,s3,l3,s3,l3,s3,l4,s3,l4,s3,l5,s3,l5,s3])

lnguitar = instrument DistortionGuitar ln
lnshguitar = instrument DistortionGuitar lnsh
lnn = instrument FX7Echoes ln

lncall = instrument Lead3Calliope ln

lntooth = instrument Lead2Sawtooth ln

lnmix = lncall :=: lntooth

lntest = Modify (Tempo 2.0) lnmix

--------------------------------------------------------------------------------ambient

bb1 = Modify (Transpose 3) (line [f 1 2 :=: g 2 2])
bb2 = Modify (Transpose 3) (line [g 1 2 :=: f 2 2])
bb3 = Modify (Transpose 3) (line [g 1 2 :=: c 2 2])
bb4 = Modify (Transpose 3) (line [f 1 2 :=: c 2 2])
bb5 = Modify (Transpose 3) (line [f 1 2 :=: c 1 2])
s42b = Modify (Transpose 0) (chord [c 1 (3/2), c 2 (3/2)])

ambient = instrument Lead3Calliope ((Modify (Tempo 1.1) (line [bb1,bb1,bb2,bb2,bb3,bb3,bb4,s42b])))
ambientsh = instrument Lead4Chiff ((Modify (Tempo 1.1) (line [bb1,bb1,bb2,bb2,bb3,bb3,bb4,bb5])))
ambient2 = instrument Lead4Chiff (Modify (Tempo 1.1) (line [bb1,bb1,bb2,bb2,bb3,bb3,bb4,s42b]))
--ambientv = Modify ( Phrase [ Orn $ Trill ]) $ addVolume 80 ambient

ambientintro = Modify (Tempo 1.1) (Modify (Tempo 0.25) (instrument Lead4Chiff (line [ch1,ch1,ch2,ch2,ch3,ch3,ch4,ch5])))
ambientintro2 = Modify (Tempo 1.1) (Modify (Tempo 0.125) (instrument Lead4Chiff (line [ch1,ch2,ch3,Modify (Tempo 2.0) ch4 ,Modify (Tempo 2.0) ch5])))
ambient3 = instrument FX7Echoes (Modify (Tempo 1.1) (line [bb1,bb1,bb2,bb2,bb3,bb3,bb4,s42b]))

--------------------------------------------------------------------------------bridge

--brib = Modify (Tempo 1.1) (line [ch1,ch1,ch2,ch2,ch3,ch3,ch4])
bri = Modify (Tempo 1.1) (line [ch1,ch1,ch2,ch2,ch3,ch3,ch4,ch5])
brii = Modify (Tempo 1.1) (line [rest (1/4) :+: Modify (Transpose 3) (g 4 (1/4))]) 
bri2 = Modify (Tempo 1.1) (line [ch1 :=: brii,ch1 :=: brii,ch2 :=: brii,ch2 :=: brii,ch3 :=: brii,ch3 :=: brii,ch4 :=: brii,ch5 :=: brii])
bri2g = instrument DistortionGuitar bri2
brimain = Modify (Tempo 1.1) (retro bri) :+: Modify (Tempo 1.2) bri :+: Modify (Tempo 1.3) (retro bri) :+: Modify (Tempo 1.4) bri2 :+: Modify (Tempo 1.5) bri2

brimain0 = line [rest(1/12) :+: (instrument Percussion (f 2 (1/16))) :+: rest(1/4) :+: (instrument Percussion (f 2 (1/16)) :+: rest(1/4) :+: (instrument Percussion (f 2 (1/16)))) :+: rest(1/4) :+: (instrument Percussion (f 2 (1/16))) :+: rest(1/12) :+: (instrument Percussion (f 2 (1/16)))]
--------------------------------------------------------------------------------main


--mainmusic = (Modify (Tempo 2.0) intro) :+: (Modify (Tempo 2.0) (intro :=: ambient :=: loop1 :=: ln) ):+: (Modify (Tempo 2.0) (intro :=: loop1 :=: ln :=: lnguitar :=: ambient)) :+: (Modify (Tempo 2.0) (intro :=: loop1 :=: ln :=: lnguitar :=: lnn) :+: bridgemain :=: bridgemet)

mainmusicnew = (Modify (Tempo 0.8) intro) :+: (Modify (Tempo 0.9) (introsh :=: ambientintro2 :=: lnsh :=: looppbeg)) :+: (introsh :=: ambientintro :=: lnsh :=: looppfin)  :+: brimain0 :+: rest (1/12) :+: c 1 1 :+: c 1 (1/2) :+: c 1 (1/3) :+: brimain :+:  Modify (Tempo 1.5) (lastloop :=: lnn :=: lnsh :=: ambientintro :=: looppmed) :+: Modify (Tempo 1.3) ((instrument FX7Echoes (line [latestloop])) :=: latestloop :=: ambientintro2)

-- Modify (Tempo 1.6) (ambientintro :=: looppfin :=: lastloop) 

-- Glowna kompozycja
sumain = Modify (Tempo 1.5) mainmusicnew




--------------------------------------------------------------------------------
--gumain = Modify (Tempo 1.1) (instrument PercussiveOrgan sumain)

---------------------------------------------------------------------------------voc

-- rest (1/3) :+: c 1 1 :+: c 1 (1/2) :+: c 1 (1/3):+: brimain

--br1 = line [e 3 (10/36),f 3 (10/36),e 3 (10/36),f 3 (12/36),e 3 (1/2),g 3 1]
--br2 = line [e 3 (10/36),f 3 (10/36),e 3 (10/36),g 3 (1/3),g 2 (1/2),e 3 1]
--br3 = line [d 2 (10/36),e 3 (10/36),f 3 (20/36),f 3 (22/36),f 3 (26/36),f 2 (22/36),e 3 (20/36),e 1 (10/36),d 2 (1/2)]
--
--br4 = line [e 3 (10/36),f 3 (10/36),e 3 (10/36),f 4 (12/36),e 4 (1/2),g 4 1]
--br5 = line [e 3 (10/36),f 3 (10/36),e 4 (10/36),g 4 (1/3),g 3 (1/2),e 4 1]
--br6 = line [d 1 (10/36),e 2 (10/36),f 2 (20/36),f 2 (22/36),f 2 (26/36),f 1 (22/36),e 2 (20/36),e 1 (10/36),d 2 (1/2)]
--
--br7 = line [e 3 (10/36),f 3 (10/36),e 3 (10/36),f 3 (12/36),e 2 (1/2),g 3 1]
--
--brx = invert br1
--

--bridgemain = Modify (Tempo 1.3) (instrument Lead3Calliope (line [br1,br2,br3,br4,br5,br6]))

--
--br3d = Modify (Tempo 2.0) br3
--bridgemain = Modify (Tempo 2.5) ((br1 :=: (instrument DistortionGuitar (e 3 (106/36)))) :+: (br2 :=: (instrument DistortionGuitar (e 3 (86/36)))) :+: (br3 :=: (instrument DistortionGuitar (d 2 (176/36)))) :+: rest (1/2) :+: (br4 :=: (instrument DistortionGuitar (e 3 (106/36)))) :+: br5 :+: br6)
--brms = bridgemain :=: bri
--brmsm = instrument OverdrivenGuitar brms
--
--bridgemet = instrument DistortionGuitar bridgemain
--brch = instrument Percussion bridgemain




