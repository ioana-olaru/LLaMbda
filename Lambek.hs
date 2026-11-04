{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes
import LExp

-- DEC_RINV = decide right inversion: true if gamma -> B 
dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma (DivL a b) = dec_rinv (a: gamma) b       -- target type is left division
dec_rinv gamma (DivR b a) = dec_rinv (gamma ++ [a]) b   -- target type is right division
dec_rinv gamma (Atm p) = 
         or [dec_lfoc gammaL focus gammaR p
            | i <- [0 .. length gamma -1],
              let (gammaL, focus : gammaR) = splitAt i gamma    -- target type is atomic 
        ]

-- DEC_LFOC = decide left focus: true if gammaL [A] gammaR -> p
dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL (Atm q) gammaR p = null gammaL && null gammaR && q == p         -- focus is atomic
dec_lfoc gammaL (DivL a b) gammaR p = 
        or [dec_rinv argument a && dec_lfoc remaining b gammaR p
        | i <- [0 .. length gammaL -1],
          let (remaining, argument) = splitAt i gammaL
        ]                                                                       -- focus is left division
dec_lfoc gammaL (DivR b a) gammaR p = 
        or [dec_rinv argument a && dec_lfoc gammaL b remaining p
        | i <- [1 .. length gammaR],
          let (argument, remaining) = splitAt i gammaR
        ]                                                                       -- focus is right division


-- TYPESFOR: lists all types in lex assigned to word w
typesFor :: Lexicon sem -> String -> [LTp]
typesFor lex w =
    [grammaticalType
     | (word, grammaticalType, _) <- items lex,
       word == w]


-- ISGRAMMATICAL: determines if a string ws is grammatically correct in lex 
isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws = 
        or [dec_rinv gamma (stp lex)
        | gamma <- mapM(typesFor lex) ws]


-- DER_RINV = derived right inversion: returns lists of lambda expressions as output, rather than a single boolean decision
der_rinv :: [(Var,LTp)] -> LTp -> [LExp]
der_rinv gamma (DivL a b) = 
    let x = fresh (map(V . fst) gamma)
    in map (L x) (der_rinv ((x, a) : gamma) b)                          -- target type is left division
der_rinv gamma (DivR b a) = 
        let x = fresh (map(V . fst) gamma)
        in map (L x) (der_rinv (gamma ++ [(x,a)]) b)                    -- target type is right division
der_rinv gamma (Atm p) = 
         concat [ [ foldl A (V x) args 
                | args <- der_lfoc gammaL focus gammaR p]
            | i <- [0 .. length gamma -1],
              let (gammaL, (x, focus) : gammaR) = splitAt i gamma
        ]                                                               -- target type is atomic


-- DER_LFOC = derived left focus 
der_lfoc :: [(Var,LTp)] -> LTp -> [(Var,LTp)] -> Atm -> [[LExp]]
der_lfoc gammaL (Atm q) gammaR p =
    if null gammaL && null gammaR && q == p
    then [[]]
    else []                                                             -- focus is atomic
der_lfoc gammaL (DivL a b) gammaR p = 
        [e : es 
        | i <- [0 .. length gammaL -1],
          let (remaining, argument) = splitAt i gammaL,
          e <- der_rinv argument a,
          es <- der_lfoc remaining b gammaR p
        ]                                                               -- focus is left division
der_lfoc gammaL (DivR b a) gammaR p = 
        [e : es 
        | i <- [1 .. length gammaR],
          let (argument, remaining) = splitAt i gammaR,
          e <- der_rinv argument a,
          es <-  der_lfoc gammaL b remaining p
        ]                                                               -- focus is right division 

-- ENTRIESFOR: lists all types of the word and associated semantic value
entriesFor :: Lexicon sem -> String -> [(LTp,sem)]
entriesFor lex w =
    [(tp, sem)
     | (word, tp, sem) <- items lex,
       word == w]

