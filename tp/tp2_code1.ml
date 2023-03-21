let rec map fonction liste =
    match liste with 
    | [] -> []
    | elt1:reste -> (fonction elt1)::(map fonction reste) ;;

let rec filter fonction liste =
    match liste with
    | [] -> []
    | elt1::reste -> 
        if(fonction elt1) 
        then elt1::(filter fonction reste) 
        else filter fonction reste;;

let rec fold_left fonction init liste =
    match liste with
    | [] -> init
    | elt1::reste -> fold_left fonction (fonction init elt1) reste;;

let rec fold_right fonction liste init =
    match liste with
    | [] -> init
    | elt1::reste -> fonction elt1 (fold_right fonction init reste);;

val concat : 'a list -> 'a list

let rec concat_rt liste_de_listes acc =
    match liste_de_listes with
    | [] -> acc
    | liste1::reste -> concat_rt reste (acc @ liste1)