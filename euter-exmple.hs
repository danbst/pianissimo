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
      rachChord notes = chord (map (\n -> n en) notes) :=: (fmap (trans 12) (head notes $ en))
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