(* beginning of interpreter *)
type push_value = Int of int | String of string | Name of string |
Bool of bool | Error | Unit

type op_value = Pop | Add | Sub | Mul | Div | Rem | Neg | Swap | Cat | And | Or | Not | Bind

type bind_value = push_value * push_value 

type stack_value = P of push_value | O of op_value | B of bind_value | Quit

(*Determines the operator on the line*)
let detOp (s: string) : stack_value = 
  match s with
  | "pop" -> O Pop
  | "add" -> O Add
  | "sub" -> O Sub
  | "mul" -> O Mul
  | "div" -> O Div
  | "rem" -> O Rem
  | "neg" -> O Neg
  | "swap"-> O Swap
  | "cat" -> O Cat
  | "And" -> O And
  | "or"  -> O Or
  | "not" -> O Not
  | "bind"-> O Bind
  | "quit"-> Quit
  | _ -> Quit

  
(*Determines if the error being pushed is :error: or :unit:*)
let detEr v : push_value =
  match v with
  | ":error:" -> Error
  | ":unit:" -> Unit
  | _ -> Error

(*Removes the double quotations from strings*)
let detString v : push_value =
   let lst = String.split_on_char '"' v in
     match lst with
     | hd::nk::tl -> String nk
     | hd::[] -> Error
     | [] -> Error
    
(*Puts strings that contain spaces into one string*)
let rec combineStrings (s: string) (tl: string list) : string =
  match tl with
  | hd::[] -> s^hd
  | hd::tl -> s^hd^" "^(combineStrings s tl)
  | [] -> s

(*Makes sure that the value is an int and if not returns Error*)
let detInt tl : push_value =
  match tl with
  | [] -> Error
  | hd::nk::tl -> Error
  | hd::[] -> (try 
                Int (int_of_string hd)
              with Failure _ -> Error)

(*Checks if a character is a digit*)
let isDigit dig : bool =
  match dig with
  | '0' .. '9' -> true
  | _ -> false

(*Makes sure that the name doesnt begin with a number*)
let checkName st : push_value = 
  let fstChar = st.[0] in
  if(isDigit fstChar) then Error else Name st
    
(*Determines the type of value that is being pushed*)
let detPush s tl : push_value =
  match s with
  | "push" -> (match tl with
               | hd::tl -> (detEr hd)
               | [] -> Error)
  | "pushi" -> detInt tl
  | "pushs" -> (detString (combineStrings "" tl))
  | "pushn" -> (match tl with
                 | hd::nk::tl -> Error
                 | hd::tl -> (checkName hd)
                 | [] -> Error)
  | "pushb" -> (match tl with
                 | hd::tl -> (match hd with
                              | ":true:" -> Bool true
                              | ":false:" -> Bool false
                              | _ -> Error)
                 | [] -> Error)
  | _ -> Error

(*Determines the type of the line*)
let determineType (l: string list) : stack_value =
  match l with 
  | hd::[] -> (detOp hd)
  | hd::tl -> P (detPush hd tl) 
  | _ -> Quit
       
(*Performs add operation*)
let add val1 val2 : push_value list =
  match val1 with
  | Int x -> (match val2 with
            | Int y -> (Int (x + y))::[]
            | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

(*Performs sub operation*)
let sub val1 val2 : push_value list = 
  match val1 with
  | Int x -> (match val2 with
            | Int y -> (Int (y - x))::[]
            | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

(*Performs mul operation*)
let mul val1 val2 : push_value list = 
  match val1 with
  | Int x -> (match val2 with
            | Int y -> (Int (x * y))::[]
            | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

(*Performs div operation*)
let div val1 val2 : push_value list = 
  match val1 with
  | Int x -> (match val2 with
            | Int y -> (match x with
                        | 0 -> Error::val1::val2::[]
                        | _ -> (Int (y / x))::[])
            | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

(*Performs rem operation*)
let rem val1 val2 : push_value list = 
  match val1 with
  | Int x -> (match val2 with
            | Int y -> (match x with
                        | 0 -> Error::val1::val2::[]
                        | _ -> (Int (y mod x))::[])
            | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

(*Performs neg operation*)
let neg val1 : push_value list= 
  match val1 with
  | Int x -> (Int (-x))::[]
  | _ -> Error::val1::[]

let cat val1 val2 : push_value list =
  match val1 with
  | String s1 -> (match val2 with
                  | String s2 -> String (s1^s2)::[]
                  | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

let myOr val1 val2 : push_value list =
  match val1 with
  | Bool b1 ->( match val2 with
                | Bool b2 -> Bool (b1||b2)::[]
                | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

let myAnd val1 val2 : push_value list = 
  match val1 with
  | Bool b1 -> (match val2 with
                | Bool b2 -> Bool (b1&&b2)::[]
                | _ -> Error::val1::val2::[])
  | _ -> Error::val1::val2::[]

let myNot val1 : push_value list =
  match val1 with
  | Bool b -> (match b with
                | true -> (Bool false)::[]
                | false -> (Bool true)::[])
  | _ -> Error::val1::[]

(*Performs whatever operation is being given*)
let performCalc fileVals oper : push_value list=
  match oper with
  | Pop ->(match fileVals with
          | hd::tl -> tl
          | [] -> Error::[])
  | Add ->(match fileVals with
          | hd::nk::tl -> (add hd nk)@tl
          | hd::[] -> Error::hd::[]
          | [] -> Error::[])
  | Sub ->(match fileVals with
          | hd::nk::tl -> (sub hd nk)@tl
          | hd::[] -> Error::hd::[]
          | [] -> Error::[])
  | Mul ->(match fileVals with
          | hd::nk::tl -> (mul hd nk)@tl
          | hd::[] -> Error::hd::[]
          | [] -> Error::[])
  | Div ->(match fileVals with
          | hd::nk::tl -> (div hd nk)@tl
          | hd::[] -> Error::hd::[]
          | [] -> Error::[])
  | Rem ->(match fileVals with
          | hd::nk::tl -> (rem hd nk)@tl
          | hd::[] -> Error::hd::[]
          | [] -> Error::[])
  | Neg ->(match fileVals with
          | hd::tl -> (neg hd)@tl
          | [] -> Error::[])
  | Swap ->(match fileVals with
          | hd::nk::tl -> (nk::hd::[])@tl
          | hd::[] -> Error::hd::[]
          | [] -> Error::[])
  | Cat -> (match fileVals with
            | hd::nk::tl -> (cat hd nk)@tl
            | hd::[] -> Error::hd::[]
            | [] -> Error::[])
  | Or -> (match fileVals with
           | hd::nk::tl -> (myOr hd nk)@tl
           | hd::[] -> Error::hd::[]
           | [] -> Error::[])
  | And -> (match fileVals with
           | hd::nk::tl -> (myAnd hd nk)@tl
           | hd::[] -> Error::hd::[]
           | [] -> Error::[])
  | Not -> (match fileVals with
           | hd::tl -> (myNot hd)@tl
           | [] -> Error::[])

(*Used to create a stack of fileVals to be printed onto the outfile*)
let rec calculate (stack: stack_value list) (newList: push_value list) =
  match stack with
  | hd::tl -> (match hd with
              | P pval -> calculate (tl) (pval::[]@newList)
              | O oval -> calculate (tl) (performCalc newList oval)
              | Quit -> newList)
  | _ -> newList
  

let interpreter ((input : string), (output : string)) : unit =

  let ic = open_in input in
  let oc = open_out output in
  let rec loop_read stack =
    (* We use try with to catch the End_of_file exception. *)
    try
        (* Read a line from ic. Build a new list with l::acc
           and pass to next recursive call. *)
        let l = input_line ic in 
          let line = String.split_on_char ' ' l in
            let stackVal = determineType line in loop_read (stackVal::stack)
            
    with
      (* At the end of file, we will reverse the string since
         the list is building by keeping add new element at the
         head of old list. *)
    | End_of_file -> List.rev stack in

    

    let fileWrite prtVal =
      match prtVal with
      | Int i -> Printf.fprintf oc "%d\n" i
      | String s -> Printf.fprintf oc "%s\n" s
      | Name n -> Printf.fprintf oc "%s\n" n
      | Bool b -> (match b with
                  | true -> Printf.fprintf oc "%s\n" ":true:"
                  | false -> Printf.fprintf oc "%s\n" ":false:")
      | Error -> Printf.fprintf oc "%s\n" ":error:"
      | Unit -> Printf.fprintf oc "%s\n" ":unit:" in

      let rec printToFile (stk: push_value list) =
        match stk with
        | hd::tl -> fileWrite hd; printToFile (tl)
        | [] -> Printf.fprintf oc "" in

    let stack = loop_read [] in

    let printerStack = calculate stack [] in

    printToFile printerStack; close_out oc

    
    




    




  
  




  
