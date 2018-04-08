type id = string 
datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm 
             | AssignStm of id * exp 
             | PrintStm of exp list
     and exp = IdExp of id 
             | NumExp of int 
             | OpExp of exp * binop * exp 
             | EseqExp of stm * exp 

val prog = 
  CompoundStm(
      AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
      CompoundStm(
        AssignStm("b",
            EseqExp(
                PrintStm[IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
                OpExp(NumExp 10, Times, IdExp "a"))),
        PrintStm[IdExp "b"]))

fun maxargs (PrintStm es) = Int.max (length es, List.foldl Int.max 0 (map maxargs_exp es))
    | maxargs (CompoundStm (fst, snd)) = Int.max (maxargs fst, maxargs snd)
    | maxargs (AssignStm (id, exp)) = maxargs_exp exp
and maxargs_exp (OpExp (left,_,right)) = Int.max (maxargs_exp left, maxargs_exp right)
    | maxargs_exp (EseqExp (s, e)) = Int.max (maxargs s, maxargs_exp e)
    | maxargs_exp _ = 0
