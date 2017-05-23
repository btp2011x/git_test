type 'a dtree = Node of 'a * 'a dtree list

let treething = Node (('a',3), [Node (('b',1), [Node (('c',4), [])]); Node (('d',1), [])])

let rec flatten acc t = match t with
	| Node (a,[]) -> acc @ [a]
	| Node (b,c) -> List.fold_left flatten (acc @ [b]) c

let leftone = flatten [] treething

let rec flatten' acc t = match t with
	| Node (a,[]) -> [a]
	| Node (b,c) -> List.fold_right (fun v a -> (flatten' [] v) @ a) c [b]

let rightone = flatten' [] treething
