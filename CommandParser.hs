{-# OPTIONS_GHC -w #-}
module CommandParser where

import CommandLexer
import Unificacion
import Prolog
import PROLogic
import ManejadorRedes
import NumerosBorrosos
import ImpComandos
import Exception
import ManejoTiempo
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
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
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,123) ([0,4096,0,4096,0,2048,0,0,0,0,0,2048,0,0,0,0,0,6144,0,0,0,4224,0,0,0,0,0,4240,0,0,0,0,0,28672,0,16,0,96,0,0,0,1024,0,256,0,0,0,0,0,28672,0,28672,0,28672,0,256,0,0,0,0,0,32,0,32,0,0,0,20480,0,0,0,0,0,256,0,544,0,0,0,28672,0,0,0,28672,0,32,0,1024,0,0,0,0,0,256,0,256,0,32,0,20480,0,0,0,12288,0,20736,0,544,0,0,0,0,0,32,0,4096,0,0,0,0,0,0,0,32,0,20480,0,32,0,256,0,8192,0,4096,0,32,0,32,0,8192,0,4096,0,32,0,32,0,8192,0,4096,0,512,0,32,0,8192,0,512,0,0,0,512,0,4096,0,512,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Consulta","Opciones","Opcion","Argumentos","Argumento","ObjetivoAmp","Objetivo","Atomo","Terms","Termino","ClausulaRed","Origen","Restricciones","Restriccion","Nodo","NombreNodo","ValorNodo","'.'","','","';'","':'","'('","')'","'='","comopt","sconst","nconst","var","%eof"]
        bit_start = st Prelude.* 32
        bit_end = (st Prelude.+ 1) Prelude.* 32
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..31]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (29) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (28) = happyShift action_7
action_2 (5) = happyGoto action_8
action_2 (6) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (32) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (28) = happyShift action_7
action_4 (5) = happyGoto action_5
action_4 (6) = happyGoto action_6
action_4 _ = happyReduce_5

action_5 (28) = happyShift action_7
action_5 (29) = happyShift action_12
action_5 (6) = happyGoto action_9
action_5 (7) = happyGoto action_13
action_5 (8) = happyGoto action_11
action_5 _ = happyReduce_9

action_6 _ = happyReduce_4

action_7 _ = happyReduce_6

action_8 (28) = happyShift action_7
action_8 (29) = happyShift action_12
action_8 (6) = happyGoto action_9
action_8 (7) = happyGoto action_10
action_8 (8) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_3

action_10 (24) = happyShift action_16
action_10 (29) = happyShift action_12
action_10 (8) = happyGoto action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_8

action_12 _ = happyReduce_10

action_13 (21) = happyShift action_15
action_13 (24) = happyShift action_16
action_13 (29) = happyShift action_12
action_13 (8) = happyGoto action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_7

action_15 _ = happyReduce_2

action_16 (29) = happyShift action_21
action_16 (30) = happyShift action_22
action_16 (31) = happyShift action_23
action_16 (9) = happyGoto action_17
action_16 (10) = happyGoto action_18
action_16 (11) = happyGoto action_19
action_16 (13) = happyGoto action_20
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_28
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (22) = happyShift action_26
action_18 (23) = happyShift action_27
action_18 _ = happyReduce_12

action_19 _ = happyReduce_14

action_20 (27) = happyShift action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (25) = happyShift action_24
action_21 (27) = happyReduce_22
action_21 _ = happyReduce_16

action_22 _ = happyReduce_23

action_23 _ = happyReduce_21

action_24 (29) = happyShift action_36
action_24 (30) = happyShift action_22
action_24 (31) = happyShift action_23
action_24 (12) = happyGoto action_37
action_24 (13) = happyGoto action_38
action_24 _ = happyReduce_20

action_25 (29) = happyShift action_36
action_25 (30) = happyShift action_22
action_25 (31) = happyShift action_23
action_25 (13) = happyGoto action_35
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (29) = happyShift action_21
action_26 (30) = happyShift action_22
action_26 (31) = happyShift action_23
action_26 (11) = happyGoto action_34
action_26 (13) = happyGoto action_20
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (25) = happyShift action_33
action_27 (14) = happyGoto action_29
action_27 (15) = happyGoto action_30
action_27 (16) = happyGoto action_31
action_27 (17) = happyGoto action_32
action_27 _ = happyReduce_30

action_28 _ = happyReduce_1

action_29 _ = happyReduce_11

action_30 (22) = happyShift action_47
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (22) = happyShift action_46
action_31 _ = happyReduce_26

action_32 _ = happyReduce_29

action_33 (29) = happyShift action_44
action_33 (31) = happyShift action_45
action_33 (18) = happyGoto action_42
action_33 (19) = happyGoto action_43
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_13

action_35 _ = happyReduce_17

action_36 (25) = happyShift action_41
action_36 _ = happyReduce_22

action_37 (22) = happyShift action_39
action_37 (26) = happyShift action_40
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_19

action_39 (29) = happyShift action_36
action_39 (30) = happyShift action_22
action_39 (31) = happyShift action_23
action_39 (13) = happyGoto action_54
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (27) = happyReduce_24
action_40 _ = happyReduce_15

action_41 (29) = happyShift action_36
action_41 (30) = happyShift action_22
action_41 (31) = happyShift action_23
action_41 (12) = happyGoto action_53
action_41 (13) = happyGoto action_38
action_41 _ = happyReduce_20

action_42 (22) = happyShift action_52
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (27) = happyShift action_51
action_43 _ = happyReduce_33

action_44 _ = happyReduce_35

action_45 _ = happyReduce_34

action_46 (25) = happyShift action_49
action_46 (17) = happyGoto action_50
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (25) = happyShift action_49
action_47 (16) = happyGoto action_48
action_47 (17) = happyGoto action_32
action_47 _ = happyReduce_30

action_48 (22) = happyShift action_46
action_48 _ = happyReduce_25

action_49 (29) = happyShift action_44
action_49 (31) = happyShift action_45
action_49 (18) = happyGoto action_61
action_49 (19) = happyGoto action_43
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_28

action_51 (29) = happyShift action_59
action_51 (30) = happyShift action_60
action_51 (20) = happyGoto action_58
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (25) = happyShift action_57
action_52 (29) = happyShift action_44
action_52 (31) = happyShift action_45
action_52 (18) = happyGoto action_56
action_52 (19) = happyGoto action_43
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (22) = happyShift action_39
action_53 (26) = happyShift action_55
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_18

action_55 _ = happyReduce_24

action_56 (22) = happyShift action_64
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (29) = happyShift action_63
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_32

action_59 _ = happyReduce_36

action_60 _ = happyReduce_37

action_61 (22) = happyShift action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (29) = happyShift action_44
action_62 (31) = happyShift action_45
action_62 (18) = happyGoto action_56
action_62 (19) = happyGoto action_43
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (22) = happyShift action_66
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (25) = happyShift action_65
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (30) = happyShift action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (29) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (22) = happyShift action_70
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (22) = happyShift action_69
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (30) = happyShift action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (29) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (22) = happyShift action_74
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (22) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (30) = happyShift action_76
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (29) = happyShift action_75
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (26) = happyShift action_78
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (22) = happyShift action_77
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (30) = happyShift action_80
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (26) = happyShift action_79
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_27

action_80 (26) = happyShift action_81
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (29) = happyShift action_82
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (26) = happyShift action_83
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_31

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (SConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (CO happy_var_1 (reverse happy_var_2) (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (SConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (C happy_var_1 (reverse happy_var_2) (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
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

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyTerminal (ComOpt happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  7 happyReduction_9
happyReduction_9  =  HappyAbsSyn7
		 ([]
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (creaObjetivoAmp (reverse happy_var_1) happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (creaObjetivoAmp (reverse happy_var_1) universal
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 11 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (A happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn11
		 (A happy_var_1 []
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (I happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  12 happyReduction_20
happyReduction_20  =  HappyAbsSyn12
		 ([]
	)

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyTerminal (Var happy_var_1))
	 =  HappyAbsSyn13
		 (V happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn13
		 (T happy_var_1 []
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 (HappyTerminal (NConst happy_var_1))
	 =  HappyAbsSyn13
		 (T (show happy_var_1) []
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (SConst happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (T happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (creaClausulaRed happy_var_1 (reverse happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 (creaClausulaRed origenUniversal (reverse happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 13 15 happyReduction_27
happyReduction_27 (_ `HappyStk`
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
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (creaOrigen happy_var_2 (happy_var_5,happy_var_7,happy_var_9,happy_var_11)
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_3 : happy_var_1
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  16 happyReduction_30
happyReduction_30  =  HappyAbsSyn16
		 ([]
	)

happyReduce_31 = happyReduce 16 17 happyReduction_31
happyReduction_31 (_ `HappyStk`
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
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_2,happy_var_4,(happy_var_7,happy_var_9,happy_var_11,happy_var_13),strToUnidad happy_var_15)
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  18 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (creaNodoVal happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (creaNodo happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyTerminal (Var happy_var_1))
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  20 happyReduction_36
happyReduction_36 (HappyTerminal (SConst happy_var_1))
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyTerminal (NConst happy_var_1))
	 =  HappyAbsSyn20
		 (show (happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	EoR -> cont 21;
	Sep -> cont 22;
	Sem -> cont 23;
	Points -> cont 24;
	LB -> cont 25;
	RB -> cont 26;
	Eq -> cont 27;
	ComOpt happy_dollar_dollar -> cont 28;
	SConst happy_dollar_dollar -> cont 29;
	NConst happy_dollar_dollar -> cont 30;
	Var happy_dollar_dollar -> cont 31;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 32 tk tks = happyError' (tks, explist)
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


type Restricciones = [Restriccion]
type Restriccion = (Nodo,Nodo,NumeroBorroso,UnidadTiempo)
type NombreNodo = String
type ValorNodo = String
type Atomos = [Atomo]
type Terms  = [Termino]

lexer :: String -> [Token]
lexer = alexScanTokens

--prueba = getContents >>= print . parser . lexer

parse :: String -> Ex Consulta
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
