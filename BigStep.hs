import Estado


data AExp = Num Int
      |Var String
      |Som AExp AExp
      |Sub AExp AExp
      |Mul AExp AExp
   deriving(Show)

data BExp = TRUE
      | FALSE
      | Not BExp
      | And BExp BExp
      | Or  BExp BExp
      | Ig  AExp AExp
   deriving(Show)

data CExp = While BExp CExp
    | If BExp CExp CExp
    | Seq CExp CExp
    | Atrib AExp AExp
    | Skip
    | Repeat BExp CExp
    | Swap AExp AExp
   deriving(Show)                



abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s)  = let  (n1,s1) = abigStep (e1, s)
                               (n2,s2) = abigStep (e2, s)
                          in (n1+n2,s)
abigStep (Sub e1 e2,s)  = let  (n1,s1) = abigStep (e1, s)
                               (n2,s2) = abigStep (e2, s)
                          in (n1-n2,s)
abigStep (Mul e1 e2,s)  = let  (n1,s1) = abigStep (e1, s)
                               (n2,s2) = abigStep (e2, s)
                          in (n1*n2,s)


bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s)  = (True,s)
bbigStep (FALSE,s) = (False,s)
bbigStep (Not b,s) = case bbigStep (b,s) of
       (True,_) -> (False, s)
       (False,_) -> (True, s)
  
bbigStep (Ig e1 e2,s )  = let  (n1,s1) = abigStep (e1, s)
                               (n2,s2) = abigStep (e2, s)
                            in (n1 == n2,s)

--TODO curto circuito no and e no or

-- bbigStep (And b1 b2,s ) = let  (n1,s1) = bbigStep (b1, s)
--                                (n2,s2) = bbigStep (b2, s)
--                             in (n1 && n2,s)

-- bbigStep (Or b1 b2,s )  = let  (n1,s1) = bbigStep (b1, s)
--                                (n2,s2) = bbigStep (b2, s)
--                             in (n1 || n2,s)

bbigStep (And b1 b2,s ) = case bbigStep (b1,s) of
                            (True,_)  -> bbigStep(b2, s)
                            (False,_) -> (False, s)

bbigStep (Or b1 b2,s )  = case bbigStep (b1,s) of
                            (True,_)  -> (True, s)
                            (False,_) -> bbigStep(b2, s)




cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s) = (Skip,s)

cbigStep (If b c1 c2,s) = case bbigStep (b,s) of
                            (True,_)  -> cbigStep(c1, s)
                            (False,_) -> cbigStep(c2, s)


cbigStep (Seq c1 c2,s)  = case cbigStep(c1, s) of
                            (Skip, s) -> cbigStep(c2, s)
                            (c1, s) -> cbigStep(c1, s)

cbigStep (Atrib (Var x) e,s) =  let (n1, s1) = abigStep(e, s)
                                in (Skip, (mudaVar s x n1))
                                
cbigStep (While b c, s) = case bbigStep (b,s) of
                            (True,_)  -> cbigStep(Seq c (While b c), s)
                            (False,_) -> (Skip, s)

cbigStep (Repeat b c, s) = case bbigStep (b,s) of
                            (True,_)  -> (Skip, s)
                            (False,_) -> cbigStep(Seq c (Repeat b c), s)

cbigStep (Swap (Var x) (Var y), s) = let n1 = (procuraVar s x)
                                         n2 = (procuraVar s y)
                                     in ((mudaVar s y n1), (mudaVar s x n2))




--TESTES

meuEstado :: Estado
meuEstado = [("x",26), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))

teste3 :: AExp
teste3 = (Sub (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


testec1 :: CExp
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
		(Atrib (Var "y") (Var "z")))

--testec2 :: CExp

fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

testeb1 :: BExp
testeb1 = (Or (And TRUE FALSE) (Or TRUE FALSE))

testeb2 :: BExp
testeb2 = (And (And TRUE FALSE) (Or TRUE FALSE))

testeb3 :: BExp
testeb3 = (And (Ig (Num 0) (Var "z")) (And (And TRUE FALSE) (Or TRUE FALSE)))




--No final do teste c2 o valor de x tem q ser o dobro, o de y e z tem q ser o inicial de x
testec2 :: CExp
testec2 = (Seq 
              (Seq (Atrib (Var "z") (Var "x")) 
                    (If (Ig (Num 1) (Var "z")) 
                          (Atrib (Var "x") (Var "y")) 
                          (Atrib (Var "x") (Som (Var "x")(Var "z")) ) 
                    )
              ) 
              (Atrib (Var "y") (Var "z"))
          )

