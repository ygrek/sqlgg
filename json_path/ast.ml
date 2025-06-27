type array_index = 
  | Index of int
  | Wildcard
  | Last
  | LastOffset of int

type range = {
  start_idx: array_index;
  end_idx: array_index;
}

type path_leg =
  | Member of string          
  | ArrayAccess of array_index 
  | ArrayRange of range        
  | MemberWildcard            
  | DoubleWildcard            

type t = {
  scope: string;              
  legs: path_leg list;
}

module Syntax = struct
  let root = { scope = "$"; legs = [] }

  let (/) path leg = { path with legs = path.legs @ [leg] }
  let (//) path legs = { path with legs = path.legs @ legs }

  let (~.) name = Member name

  let idx n = ArrayAccess (Index n)                    (* idx 0 *)
  let any = ArrayAccess Wildcard                       (* any *)
  let last = ArrayAccess Last                          (* last *)
  let wildcard = MemberWildcard                        (* wildcard *)
  let range start end_ = ArrayRange { 
    start_idx = Index start; 
    end_idx = Index end_ 
  }
  
end
