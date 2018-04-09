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

fun maxargs (PrintStm es) = 
        Int.max (length es, List.foldl Int.max 0 (map maxargs_exp es))
    | maxargs (CompoundStm (fst, snd)) = Int.max (maxargs fst, maxargs snd)
    | maxargs (AssignStm (id, exp)) = maxargs_exp exp
and maxargs_exp (OpExp (left,_,right)) = Int.max (maxargs_exp left, maxargs_exp right)
    | maxargs_exp (EseqExp (s, e)) = Int.max (maxargs s, maxargs_exp e)
    | maxargs_exp _ = 0

type env = (id * int) list
exception InvalidVariableException

fun lookup (nil, _) = raise InvalidVariableException
  | lookup ((identifier, value)::rst, variable) = 
      if  identifier = variable then value else lookup (rst, variable) 

fun update (env, k, v) = (k, v)::env

fun interp s = 
    let fun interpStm ((CompoundStm (fst, snd)), env) 
                = interpStm (snd, interpStm (fst, env))
            | interpStm ((AssignStm (identifier, expr)), env)
                = let val (value, env2) = interpExp (expr, env)
                  in update(env, identifier, value)
                  end 
            | interpStm (PrintStm exps, env)
                = let fun printStatement (exp, env) = 
                      let val (var, env2) = interpExp (exp, env)
                      in print (Int.toString var);
                         print " ";
                         env2 
                      end 
                    val tx = List.foldr printStatement env exps 
                        in print "\n"; tx 
                  end 
    and interpExp (IdExp i, t)
          = (lookup (t,i), t)
        | interpExp (NumExp n, t)
          = (n,t)
        | interpExp (OpExp (e1,bop,e2),t)
          = let val (n1,t1) = interpExp (e1,t)
                val (n2,t2) = interpExp (e2,t)
            in ((interpBop bop) (n1,n2), t2)
            end
        | interpExp (EseqExp (s,e),t)
          = let val t1 = interpStm (s,t)
            in interpExp (e,t1)
            end
      and interpBop Plus  = (op + )
        | interpBop Minus = (op - )
        | interpBop Times = (op * )
        | interpBop Div   = (op div )
  in interpStm (s,[])
end
