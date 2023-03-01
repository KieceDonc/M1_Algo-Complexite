let rec coupe_en_2 liste = 
  match liste with 
    | [] -> [], []
    | [element] -> liste, []
    | elt1::elt2::reste -> 
        let partie1, partie2 = coupe_en_2 reste in 
            elt1::partie1, elt2::partie2 ;;

