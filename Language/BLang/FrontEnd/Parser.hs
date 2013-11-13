{-# OPTIONS_GHC -w #-}
module Language.BLang.FrontEnd.Parser (module ParsedAST, parse) where
import qualified Language.BLang.FrontEnd.ParsedAST as ParsedAST (AST(..)) 
import Language.BLang.FrontEnd.Lexer  as Lex (Token(..), Literal(..), lexer)

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Lex.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (())

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Lex.Token)
	-> HappyState (Lex.Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Lex.Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2 :: () => Int -> ({-HappyReduction (IO) = -}
	   Int 
	-> (Lex.Token)
	-> HappyState (Lex.Token) (HappyStk HappyAbsSyn -> (IO) HappyAbsSyn)
	-> [HappyState (Lex.Token) (HappyStk HappyAbsSyn -> (IO) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (IO) HappyAbsSyn)

happyReduce_1 :: () => ({-HappyReduction (IO) = -}
	   Int 
	-> (Lex.Token)
	-> HappyState (Lex.Token) (HappyStk HappyAbsSyn -> (IO) HappyAbsSyn)
	-> [HappyState (Lex.Token) (HappyStk HappyAbsSyn -> (IO) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (IO) HappyAbsSyn)

action_0 (4) = happyGoto action_2
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (40) = happyAccept
action_2 _ = happyFail

happyReduce_1 = happyMonadReduce 0 4 happyReduction_1
happyReduction_1 (happyRest) tk
	 = happyThen (( undefined)
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyNewToken action sts stk
	= Lex.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Lex.EOF -> action 40 40 tk (HappyState action) sts stk;
	Lex.LiteralToken happy_dollar_dollar -> cont 5;
	Lex.ID "int" -> cont 6;
	Lex.ID "float" -> cont 7;
	Lex.ID "void" -> cont 8;
	Lex.ID "if" -> cont 9;
	Lex.ID "else" -> cont 10;
	Lex.ID "while" -> cont 11;
	Lex.ID "for" -> cont 12;
	Lex.ID "typedef" -> cont 13;
	Lex.ID "return" -> cont 14;
	Lex.ID happy_dollar_dollar -> cont 15;
	Lex.SymAssign -> cont 16;
	Lex.SymLogic "&&" -> cont 17;
	Lex.SymLogic "||" -> cont 18;
	Lex.SymLogic "!" -> cont 19;
	Lex.SymRelational "<" -> cont 20;
	Lex.SymRelational ">" -> cont 21;
	Lex.SymRelational "<=" -> cont 22;
	Lex.SymRelational ">=" -> cont 23;
	Lex.SymArithmetic "+" -> cont 24;
	Lex.SymArithmetic "-" -> cont 25;
	Lex.SymArithmetic "*" -> cont 26;
	Lex.SymArithmetic "/" -> cont 27;
	Lex.LiteralToken (IntLiteral happy_dollar_dollar) -> cont 28;
	Lex.LiteralToken (FloatLiteral happy_dollar_dollar) -> cont 29;
	Lex.LiteralToken (StringLiteral happy_dollar_dollar) -> cont 30;
	Lex.SymSeparator "(" -> cont 31;
	Lex.SymSeparator ")" -> cont 32;
	Lex.SymSeparator "{" -> cont 33;
	Lex.SymSeparator "}" -> cont 34;
	Lex.SymSeparator "[" -> cont 35;
	Lex.SymSeparator "]" -> cont 36;
	Lex.SymSeparator "," -> cont 37;
	Lex.SymSeparator ";" -> cont 38;
	Lex.SymSeparator "." -> cont 39;
	_ -> happyError' tk
	})

happyError_ 40 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => IO a -> (a -> IO b) -> IO b
happyThen = (>>=)
happyReturn :: () => a -> IO a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> IO a
happyReturn1 = happyReturn
happyError' :: () => (Lex.Token) -> IO a
happyError' tk = parseError tk

parse = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError _ = error "XD"
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<inbyggd>" #-}
{-# LINE 1 "<kommandorad>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates\\GenericTemplate.hs" #-}
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
