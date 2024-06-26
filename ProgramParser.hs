{-# OPTIONS_GHC -w #-}
module ProgramParser where

import ProgramLexer
import Unificacion
import Prolog
import PROLogic
import ManejadorRedes
import NumerosBorrosos
import Exception
import ManejoTiempo
import HypoParser(parse)
import Relaciones
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18
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
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,119) ([0,3584,0,448,4096,0,0,0,256,0,256,0,64,0,1,0,0,0,0,0,0,1792,0,224,0,28,2048,0,28672,0,0,0,0,8192,0,1024,0,0,0,5120,0,2,0,0,0,0,1024,0,288,0,0,0,224,0,0,32768,3,28672,0,8,0,32,0,0,0,0,512,0,64,0,2,0,80,0,0,49152,0,10368,0,0,32768,4,0,0,0,0,64,0,512,0,0,0,0,0,0,128,0,5120,0,2,0,17,0,4,2048,0,2048,0,4,0,0,4096,0,0,1,4096,0,8,0,1,0,16,0,1,1024,0,16,0,256,0,2,0,0,2048,0,2048,0,32,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Inicio","ProgramaAmp","ClausulaAmp","ClausulaRed","Origen","Restricciones","Restriccion","Nodo","NombreNodo","ValorNodo","Clausula","Atomos","Atomo","Terms","Termino","'.'","','","';'","'('","')'","\":-\"","'='","sconst","nconst","var","%eof"]
        bit_start = st Prelude.* 29
        bit_end = (st Prelude.+ 1) Prelude.* 29
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..28]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (26) = happyShift action_7
action_0 (27) = happyShift action_8
action_0 (28) = happyShift action_9
action_0 (4) = happyGoto action_10
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (14) = happyGoto action_4
action_0 (16) = happyGoto action_5
action_0 (18) = happyGoto action_6
action_0 _ = happyReduce_5

action_1 (26) = happyShift action_7
action_1 (27) = happyShift action_8
action_1 (28) = happyShift action_9
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (14) = happyGoto action_4
action_1 (16) = happyGoto action_5
action_1 (18) = happyGoto action_6
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_15
action_2 _ = happyReduce_1

action_3 _ = happyReduce_4

action_4 (21) = happyShift action_14
action_4 _ = happyReduce_7

action_5 (24) = happyShift action_13
action_5 _ = happyReduce_23

action_6 (25) = happyShift action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (22) = happyShift action_11
action_7 (25) = happyReduce_33
action_7 _ = happyReduce_27

action_8 _ = happyReduce_34

action_9 _ = happyReduce_32

action_10 (29) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (26) = happyShift action_25
action_11 (27) = happyShift action_8
action_11 (28) = happyShift action_9
action_11 (17) = happyGoto action_26
action_11 (18) = happyGoto action_27
action_11 _ = happyReduce_31

action_12 (26) = happyShift action_25
action_12 (27) = happyShift action_8
action_12 (28) = happyShift action_9
action_12 (18) = happyGoto action_24
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (26) = happyShift action_7
action_13 (27) = happyShift action_8
action_13 (28) = happyShift action_9
action_13 (15) = happyGoto action_22
action_13 (16) = happyGoto action_23
action_13 (18) = happyGoto action_6
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (22) = happyShift action_21
action_14 (7) = happyGoto action_17
action_14 (8) = happyGoto action_18
action_14 (9) = happyGoto action_19
action_14 (10) = happyGoto action_20
action_14 _ = happyReduce_13

action_15 (26) = happyShift action_7
action_15 (27) = happyShift action_8
action_15 (28) = happyShift action_9
action_15 (6) = happyGoto action_16
action_15 (14) = happyGoto action_4
action_15 (16) = happyGoto action_5
action_15 (18) = happyGoto action_6
action_15 _ = happyReduce_3

action_16 _ = happyReduce_2

action_17 _ = happyReduce_6

action_18 (20) = happyShift action_37
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (20) = happyShift action_36
action_19 _ = happyReduce_9

action_20 _ = happyReduce_12

action_21 (26) = happyShift action_34
action_21 (28) = happyShift action_35
action_21 (11) = happyGoto action_32
action_21 (12) = happyGoto action_33
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (20) = happyShift action_31
action_22 _ = happyReduce_22

action_23 _ = happyReduce_25

action_24 _ = happyReduce_28

action_25 (22) = happyShift action_30
action_25 _ = happyReduce_33

action_26 (20) = happyShift action_28
action_26 (23) = happyShift action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_30

action_28 (26) = happyShift action_25
action_28 (27) = happyShift action_8
action_28 (28) = happyShift action_9
action_28 (18) = happyGoto action_45
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (25) = happyReduce_35
action_29 _ = happyReduce_26

action_30 (26) = happyShift action_25
action_30 (27) = happyShift action_8
action_30 (28) = happyShift action_9
action_30 (17) = happyGoto action_44
action_30 (18) = happyGoto action_27
action_30 _ = happyReduce_31

action_31 (26) = happyShift action_7
action_31 (27) = happyShift action_8
action_31 (28) = happyShift action_9
action_31 (16) = happyGoto action_43
action_31 (18) = happyGoto action_6
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (20) = happyShift action_42
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (25) = happyShift action_41
action_33 _ = happyReduce_17

action_34 _ = happyReduce_19

action_35 _ = happyReduce_18

action_36 (22) = happyShift action_39
action_36 (10) = happyGoto action_40
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (22) = happyShift action_39
action_37 (9) = happyGoto action_38
action_37 (10) = happyGoto action_20
action_37 _ = happyReduce_13

action_38 (20) = happyShift action_36
action_38 _ = happyReduce_8

action_39 (26) = happyShift action_34
action_39 (28) = happyShift action_35
action_39 (11) = happyGoto action_52
action_39 (12) = happyGoto action_33
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_11

action_41 (26) = happyShift action_50
action_41 (27) = happyShift action_51
action_41 (13) = happyGoto action_49
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (22) = happyShift action_48
action_42 (26) = happyShift action_34
action_42 (28) = happyShift action_35
action_42 (11) = happyGoto action_47
action_42 (12) = happyGoto action_33
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_24

action_44 (20) = happyShift action_28
action_44 (23) = happyShift action_46
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_29

action_46 _ = happyReduce_35

action_47 (20) = happyShift action_55
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (26) = happyShift action_54
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_16

action_50 _ = happyReduce_20

action_51 _ = happyReduce_21

action_52 (20) = happyShift action_53
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (26) = happyShift action_34
action_53 (28) = happyShift action_35
action_53 (11) = happyGoto action_47
action_53 (12) = happyGoto action_33
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (20) = happyShift action_58
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (22) = happyShift action_56
action_55 (26) = happyShift action_57
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (27) = happyShift action_61
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (23) = happyShift action_60
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (26) = happyShift action_59
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (20) = happyShift action_63
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_15

action_61 (20) = happyShift action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (27) = happyShift action_65
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (26) = happyShift action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (20) = happyShift action_67
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (20) = happyShift action_66
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (27) = happyShift action_69
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (26) = happyShift action_68
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (23) = happyShift action_71
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (20) = happyShift action_70
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (27) = happyShift action_73
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (23) = happyShift action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_10

action_73 (23) = happyShift action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (26) = happyShift action_75
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (23) = happyShift action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_14

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (creaProgramaAmp (reverse happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_3 : happy_var_1
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  5 happyReduction_5
happyReduction_5  =  HappyAbsSyn5
		 ([]
	)

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (creaClausulaAmp happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (creaClausulaAmp happy_var_1 universal
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (creaClausulaRed happy_var_1 (reverse happy_var_3)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (creaClausulaRed origenUniversal (reverse happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 13 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_11)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_9)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (creaOrigen happy_var_2 (snd happy_var_5,snd happy_var_7,snd happy_var_9,snd happy_var_11)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  9 happyReduction_13
happyReduction_13  =  HappyAbsSyn9
		 ([]
	)

happyReduce_14 = happyReduce 16 10 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyTerminal (SConst happy_var_15)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (NConst happy_var_13)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (NConst happy_var_11)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (NConst happy_var_9)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (NConst happy_var_7)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_2,happy_var_4,(snd happy_var_7,snd happy_var_9,snd happy_var_11,snd happy_var_13),strToUnidad (snd happy_var_15))
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 7 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyTerminal (SConst happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (case (procesaRelacion happy_var_2 (snd happy_var_6) happy_var_4) of
                                                         (Ok r) -> r
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (creaNodoVal happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (creaNodo happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyTerminal (Var happy_var_1))
	 =  HappyAbsSyn12
		 (snd happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn12
		 (snd happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn13
		 (snd happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyTerminal (NConst happy_var_1))
	 =  HappyAbsSyn13
		 (show (snd happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 ((happy_var_1, reverse happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 ((happy_var_1, [])
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  15 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_3 : happy_var_1
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 16 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (A (snd happy_var_1) (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn16
		 (A (snd happy_var_1) []
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (I happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  17 happyReduction_31
happyReduction_31  =  HappyAbsSyn17
		 ([]
	)

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 (HappyTerminal (Var happy_var_1))
	 =  HappyAbsSyn18
		 (V (snd happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn18
		 (T (snd happy_var_1) []
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyTerminal (NConst happy_var_1))
	 =  HappyAbsSyn18
		 (T (show (snd happy_var_1)) []
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 18 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (T (snd happy_var_1) (reverse happy_var_3)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	EoR _ -> cont 19;
	Sep _ -> cont 20;
	Sem _ -> cont 21;
	LB _ -> cont 22;
	RB _ -> cont 23;
	Imp _ -> cont 24;
	Eq _ -> cont 25;
	SConst happy_dollar_dollar -> cont 26;
	NConst happy_dollar_dollar -> cont 27;
	Var happy_dollar_dollar -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
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


type Inicio = ProgramaAmp
type Restricciones = [Restriccion]
type Restriccion = (Nodo,Nodo,NumeroBorroso,UnidadTiempo)
type NombreNodo = String
type ValorNodo = String
type Atomos = [Atomo]
type Terms  = [Termino]

procesaRelacion :: Nodo -> String -> Nodo -> Ex (Nodo,Nodo,NumeroBorroso,UnidadTiempo)
procesaRelacion n1 rel n2 =
    case relacion of
        (Ok r) -> Ok (n1,n2,getDist r,getUT r)
        (Failed err) -> Failed err
    where
        relacion = HypoParser.parse rel

lexer :: String -> [Token]
lexer = alexScanTokens

--prueba file = readFile file >>= print . parser . lexer

parse :: String -> Ex ProgramaAmp
parse = parser . lexer 

-- Errores
parseError :: [Token] -> Ex a
parseError [] = failE "No token found."
parseError (t:tks) = failE ("Parse error: at line " ++ (show line) ++ ", column " ++ (show column) ++ ".")
    where 
        AlexPn _ line column = token_posn t;
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
