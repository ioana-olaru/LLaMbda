{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma (DivL a b) = dec_rinv (a: gamma) b
dec_rinv gamma (DivR b a) = dec_rinv (gamma ++ [a]) b
dec_rinv gamma (Atm p) = 
         or [dec_lfoc gammaL focus gammaR p
            | i <- [0 .. length gamma -1],
              let (gammaL, focus : gammaR) = splitAt i gamma
        ]


dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL (Atm q) gammaR p = null gammaL && null gammaR && q == p
dec_lfoc gammaL (DivL a b) gammaR p = 
        or [dec_rinv argument a && dec_lfoc remaining b gammaR p
        | i <- [0 .. length gammaL -1],
          let (remaining, argument) = splitAt i gammaL
        ]
dec_lfoc gammaL (DivR b a) gammaR p = 
        or [dec_rinv argument a && dec_lfoc gammaL b remaining p
        | i <- [1 .. length gammaR],
          let (argument, remaining) = splitAt i gammaR
        ]

typesFor :: Lexicon sem -> String -> [LTp]
typesFor lex w =
    [grammaticalType
     | (word, grammaticalType, _) <- items lex,
       word == w]

isGrammatical :: Lexicon sem -> [String] -> Bool
isGrammatical lex ws = 
        or [dec_rinv gamma (stp lex)
        | gamma <- mapM(typesFor lex) ws]

