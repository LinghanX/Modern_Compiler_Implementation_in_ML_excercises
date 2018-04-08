type id = string

datatype binop = Add | Sub | Multi | Div

datatype statement = CompoundStm of statement * statement
                   | AssignStm of id * exp
                   | PrintStm of exp list 
    and exp = IdExp of id
            | NumExp of int
            | OpExp of exp * binop * exp
            | EseqExp of statement * exp
