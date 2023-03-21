let rec tri_rapide liste fct_comparaison = 
    let rec partition liste pivot petits egaux grands = 
        match liste with 
        | [] -> List.concat [tri_fusion petits fct_comparaison; egaux; tri_fusion grands fct_comparaison]
        | elt1:reste ->
        if (fct_comparaison elt1 pivot) 
        then partition reste pivot (elt1::petits) egaux grands
        else if(fct_comparaison pivot el1) 
            then partition reste pivot petits egaux (elt1:grands)
            else partition reste pivot petits (elt1::egaux) grands
    in
    match liste with 
    | [] -> []
    | [_] -> [_]
    | pivot::reste -> partition liste pivot [] [] [] ;; 