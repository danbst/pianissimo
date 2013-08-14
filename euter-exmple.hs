{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}

import Euterpea

deriving instance Functor Primitive
deriving instance Functor Music

main = do putStrLn "Cumparsita by Rodriegez"
          play cumparsita
          putStrLn "Rachmaninoff prelude C# minor"
          play rachmaninoff_prelude

cumparsita = tempo (66/60) $ timesM 2 $
    line [ line [ d 4 qn, c 5 qn, a 4 qn, fs 4 qn ]
         , common1
         , line [ d 4 qn, d 5 qn, bf 4 qn, g 4 qn ]
         , common1
         ]
    where common1 = line [enr, d 4 en, ds 4 en, d 4 en, df 4 qn, d 4 en]
    
    
rachmaninoff_prelude = tempo (20/60) $ intro :+: (leftHand :=: rightHand :=: bassChords)
    where
      octaveChord note duration octaves = chord $ map (\o -> note o duration) octaves
      rachChord notes = chord $ map ($ en) realNotes
            where realNotes = (fmap (trans 12) . head notes) : notes
      intro = line [ octaveChord a qn [1, 2, 3]
                   , octaveChord gs qn [1, 2, 3]
                   , octaveChord cs wn [1, 2, 3]
                   ]
      common1 = line [
          enr
        , rachChord [ cs 4, gs 4 ]
        , rachChord [ e 4, b 4 ]
        , rachChord [ ds 4, as 4 ]
        , enr
        , rachChord [ d 4, a 4 ]
        , enr
        , rachChord [ bs 3, fs 4 ]
        ]
      common2 = line [
          hnr
        , octaveChord a qn [1, 2, 3]
        , octaveChord gs qn [1, 2, 3]
        ]
      common3 = line [
          enr
        , rachChord [ e 3, gs 3 ]
        , rachChord [ gs 3, b 3 ]
        , rachChord [ g 3, as 3 ]
        , enr
        , rachChord [ fs 3, bs 3 ]
        , enr
        , rachChord [ ds 3, fs 3 ]
        ]
      rightHand = line [
          common1
        , common1  
        ]
      bassChords = line [
          common2
        , octaveChord cs hn [1, 2, 3] :=: common2
        , octaveChord cs hn [1, 2, 3]
        ]
      leftHand = line [
          common3
        , common3
        ]

bahia_bianca = partA
  where 
    partA = leftHand :=: rightHand
      where
        rightHand = 
          -- 1
          line [
            snr
          , cs 4 sn
          , a 5 sn
          , gs 5 sn
          , fs 5 sn
          , e 5 en
          , b 4 sn
          ] :+: 
          -- 2
          chord [
            d 5 en
          , b 4 en
          , fs 4 en
          ] :+: chord [
            cs 5 qn
          , a 4 en
          , fs 4 en
          ] :+: snr :+: fs 4 sn :+:
          -- 3
          chord [
            b 4 en
          , gs 4 en
          , fs 4 en
          , d 4 en
          ] :+: chord [
            a 4 qn
          , fs 3 qn
          , cs 4 qn
          ] :+: snr :+: cs 4 sn :+:
          -- 4
          chord [
            gs 4 en
          , es 4 en
          , b 3 en
          ] :+: chord [
            fs 4 qn
          , cs 4 qn
          , a 3 qn
          ] :+: enr
        leftHand = 
          fs 2 en :+: chord [
            fs 4 en
          , cs 4 en
          , a 3 en
          ] :+: gs 2 en :+: chord [
            es 4 en
          , cs 4 en
          , b 3 en
          , gs 4 en
          ] :+: a 2 en :+: chord [
            cs 4 en
          , a 3 en
          , fs 3 en
          ] :+: chord [
            fs 4 en
          , cs 4 en
          , a 3 en
          ] :+: snr :+: fs 3 sn :+: 
          b 3 en :+: a 3 en :+: cs 2 en :+: snr :+:
          cs 3 sn :+: gs 3 en :+: fs 3 en :+:
          d 3 en :+: cs 3 en
          
unknown = line [
    c 4 sn, d 4 sn, e 4 hn
  , g 4 en, c 5 sn, d 5 sn, e 5 en, d 5 en, c 5 en
  , a 4 den, af 4 sn
  , e 5 qn, g 4 en, e 4 en, g 4 en, e 5 en, ef 5 en
  , fs 4 (hn + en), a 4 en, g 4 en, f 4 en, e 4 hn
  , c 5 en, g 5 sn, a 5 sn
  ]
  