{-# OPTIONS_GHC -w #-}
module HypoParser where

import HypoLexer
import Exception
import Relaciones
import ManejoTiempo
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,94) ([4096,959,61696,59,128,0,0,0,0,49152,3,0,0,14353,0,0,0,0,0,2,14369,0,1024,0,0,0,0,1024,0,64,0,64,0,4,16384,32768,0,61696,59,0,0,0,0,0,0,0,0,0,0,0,0,0,64,64,0,0,4096,896,0,0,33040,3,0,0,0,0,0,0,2,0,0,0,1024,0,0,2,4,0,512,512,0,0,4,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Relacion","Rel","DistanciaTiempo","ExtensionTiempo","DireccionTiempoRel","DireccionTiempoMod","OperadorExpansion","ExtensionTiempoAbsoluto","CantidadTemporal","'('","')'","','","o","aprox","igual","antes","despues","mas","menos","de","poco","mucho","cantidad","unidad","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (13) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (19) = happyShift action_13
action_0 (20) = happyShift action_14
action_0 (21) = happyShift action_15
action_0 (22) = happyShift action_16
action_0 (24) = happyShift action_17
action_0 (25) = happyShift action_18
action_0 (26) = happyShift action_19
action_0 (4) = happyGoto action_20
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (13) = happyShift action_10
action_1 (17) = happyShift action_11
action_1 (18) = happyShift action_12
action_1 (19) = happyShift action_13
action_1 (20) = happyShift action_14
action_1 (21) = happyShift action_15
action_1 (22) = happyShift action_16
action_1 (24) = happyShift action_17
action_1 (25) = happyShift action_18
action_1 (26) = happyShift action_19
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 (7) = happyGoto action_5
action_1 (8) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 (11) = happyGoto action_8
action_1 (12) = happyGoto action_9
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_21
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 _ = happyReduce_3

action_5 (19) = happyShift action_35
action_5 (20) = happyShift action_36
action_5 (21) = happyShift action_15
action_5 (22) = happyShift action_16
action_5 (9) = happyGoto action_33
action_5 (10) = happyGoto action_34
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_6

action_7 (13) = happyShift action_10
action_7 (17) = happyShift action_32
action_7 (24) = happyShift action_17
action_7 (25) = happyShift action_18
action_7 (26) = happyShift action_19
action_7 (11) = happyGoto action_31
action_7 (12) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_8

action_9 _ = happyReduce_17

action_10 (26) = happyShift action_30
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (13) = happyShift action_10
action_11 (18) = happyShift action_29
action_11 (24) = happyShift action_17
action_11 (25) = happyShift action_18
action_11 (26) = happyShift action_19
action_11 (12) = happyGoto action_28
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (27) = happyShift action_27
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_11

action_14 _ = happyReduce_12

action_15 (23) = happyShift action_26
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (23) = happyShift action_25
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (27) = happyShift action_24
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (27) = happyShift action_23
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (27) = happyShift action_22
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (16) = happyShift action_21
action_20 (28) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (13) = happyShift action_10
action_21 (17) = happyShift action_11
action_21 (18) = happyShift action_12
action_21 (19) = happyShift action_13
action_21 (20) = happyShift action_14
action_21 (21) = happyShift action_15
action_21 (22) = happyShift action_16
action_21 (24) = happyShift action_17
action_21 (25) = happyShift action_18
action_21 (26) = happyShift action_19
action_21 (5) = happyGoto action_40
action_21 (6) = happyGoto action_4
action_21 (7) = happyGoto action_5
action_21 (8) = happyGoto action_6
action_21 (10) = happyGoto action_7
action_21 (11) = happyGoto action_8
action_21 (12) = happyGoto action_9
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_20

action_23 _ = happyReduce_22

action_24 _ = happyReduce_21

action_25 _ = happyReduce_16

action_26 _ = happyReduce_15

action_27 _ = happyReduce_4

action_28 _ = happyReduce_18

action_29 (27) = happyShift action_39
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (15) = happyShift action_38
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_9

action_32 (13) = happyShift action_10
action_32 (24) = happyShift action_17
action_32 (25) = happyShift action_18
action_32 (26) = happyShift action_19
action_32 (12) = happyGoto action_28
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_7

action_34 (13) = happyShift action_10
action_34 (17) = happyShift action_32
action_34 (24) = happyShift action_17
action_34 (25) = happyShift action_18
action_34 (26) = happyShift action_19
action_34 (11) = happyGoto action_37
action_34 (12) = happyGoto action_9
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_13

action_36 _ = happyReduce_14

action_37 _ = happyReduce_10

action_38 (26) = happyShift action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_5

action_40 _ = happyReduce_1

action_41 (15) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (26) = happyShift action_43
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (15) = happyShift action_44
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (26) = happyShift action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (14) = happyShift action_46
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (27) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_19

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (uneRelacion happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyTerminal (UNIDAD happy_var_2))
	_
	 =  HappyAbsSyn5
		 (al_mismo_tiempo (strToUnidad happy_var_2)
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyTerminal (UNIDAD happy_var_3))
	_
	_
	 =  HappyAbsSyn5
		 (aproximadamente (al_mismo_tiempo (strToUnidad happy_var_3))
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (mezclaRelacion happy_var_1 (happy_var_2 happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn8
		 (antes segs
	)

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn8
		 (despues segs
	)

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn9
		 (antesM
	)

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn9
		 (despuesM
	)

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 _
	_
	 =  HappyAbsSyn10
		 (mas_de
	)

happyReduce_16 = happySpecReduce_2  10 happyReduction_16
happyReduction_16 _
	_
	 =  HappyAbsSyn10
		 (menos_de
	)

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (aproximadamente happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 10 12 happyReduction_19
happyReduction_19 ((HappyTerminal (UNIDAD happy_var_10)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (CANTIDAD happy_var_8)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (CANTIDAD happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (CANTIDAD happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (CANTIDAD happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (cantidadBorrosa (happy_var_2,happy_var_4,happy_var_6,happy_var_8) (strToUnidad happy_var_10)
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  12 happyReduction_20
happyReduction_20 (HappyTerminal (UNIDAD happy_var_2))
	(HappyTerminal (CANTIDAD happy_var_1))
	 =  HappyAbsSyn12
		 (cantidad happy_var_1 (strToUnidad happy_var_2)
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  12 happyReduction_21
happyReduction_21 (HappyTerminal (UNIDAD happy_var_2))
	_
	 =  HappyAbsSyn12
		 (poco (strToUnidad happy_var_2)
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  12 happyReduction_22
happyReduction_22 (HappyTerminal (UNIDAD happy_var_2))
	_
	 =  HappyAbsSyn12
		 (mucho (strToUnidad happy_var_2)
	)
happyReduction_22 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 28 28 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LB -> cont 13;
	RB -> cont 14;
	COMA -> cont 15;
	O -> cont 16;
	APROXIMADAMENTE -> cont 17;
	IGUAL -> cont 18;
	ANTES -> cont 19;
	DESPUES -> cont 20;
	MAS -> cont 21;
	MENOS -> cont 22;
	DE -> cont 23;
	POCO -> cont 24;
	MUCHO -> cont 25;
	CANTIDAD happy_dollar_dollar -> cont 26;
	UNIDAD happy_dollar_dollar -> cont 27;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 28 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Ex a -> (a -> Ex b) -> Ex b
happyThen = (thenE)
happyReturn :: () => a -> Ex a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Ex a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Ex a
happyError' = (\(tokens, _) -> parseError tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Rel = Relacion
type CantidadTemporal = Relacion
type ExtensionTiempoAbsoluto = Relacion
type OperadorExpansion = Modificador
type DireccionTiempoRel = Relacion
type DireccionTiempoMod = Modificador
type ExtensionTiempo = Relacion
type DistanciaTiempo = Relacion

segs = strToUnidad "seconds"


lexer :: String -> [Token]
lexer = alexScanTokens

--prueba = getContents >>= print . parser . lexer

parse :: String -> Ex Relacion
parse = parser . lexer 


-- Errores
parseError :: [Token] -> Ex a
parseError _ = failE "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
