(* ----- Tri par insertion avec fonction de comparaison. *)

let rec insere_dans_liste_triee element liste fct_comparaison =
	match liste with 
	| [] -> [element]
	| elt1::reste -> 
		if fct_comparaison element elt1 
		then element::liste 
		else elt1::(insere_dans_liste_triee element reste fct_comparaison) ;;

let rec tri_insertion liste fct_comparaison =
	match liste with
	| [] -> []
	| elt1::reste -> insere_dans_liste_triee elt1 (tri_insertion reste fct_comparaison) fct_comparaison ;;

let cmp_croissant el1 el2 =
    el1 < el2

let cmp_decroissant el1 el2 =
    el1 > el2

let liste_a_trier = [9;0;5;9;6] ;;

tri_insertion liste_a_trier cmp_croissant ;;
tri_insertion liste_a_trier cmp_decroissant ;;