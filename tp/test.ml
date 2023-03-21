let rec fusionne liste1 liste2 fct_comparaison = 
  match liste1, liste2 with 
    | [], element -> element
    | element, [] -> element
    | tete1::queue1, tete2::queue2 -> 
        if fct_comparaison tete1 tete1
        then tete1::(fusionne queue1 liste2 fct_comparaison) 
        else tete2::(fusionne liste1 queue2 fct_comparaison) ;;

let liste1 = [9;8;5;6;7;3] ;;
let liste2 = [20;1;2;30;7] ;;

fusionne liste1 liste2 (<) ;;