let g = {
    sommets =  [|"A"; "B"; "C"; "D"|] ;
    aretes = [|
        [(1, 85.); (2, 217.); (4, 173.)];
        [(0, 85.); (5, 80.)] ;
    |]
} ;;

étape 1 : init -> tableau distances (def infinity), tableau predecesseurs (def -1), maj tableau à l'indice du sommet de départ (0 et 0)
étape 2 : recursion ->

let dijkstra graphe sommet_depart =
    (* Initialisation *)
    let nb_sommets = Array.length graphe.sommets in
    let distances = Array.make nb_sommets infinity in
    let predecesseurs = Array.make nb_sommets (-1) in

    distances.(sommet_depart) <- 0. ;
    predecesseurs.(sommet_depart) <- sommet_depart ;

    let dijkstra_aux sommets_restant =
        match sommets_restant with
        | [] -> distances, predecesseurs
        | elt1::reste -> (
            let sommet_dist_in = List.fold_left(fun min i -> if distance.(min) < distance.(i) then min else i) elt1 reste in (* -> fold_left *)
            list.iter( function (voisin, poids) ->
                let nouvelle_dist = distances.(sommet_dist_min) +. poids in
                if nouvelle_dist < distances.(voisin)
                then (distances.(voisin) <- nouvelle_dist; predecesseurs.(voisin) <- sommet_dist_in)
            ) graphe.aretes.(sommet_dist_min)(* Mettre à jour les voisins de sommet_dist_min si besoin. -> List.iter *)
            dijsktra_aux(List.filter(function sommet -> sommet <> sommet_dist_min) sommets_restant) (* Appel récursif en précisant que sommet_dist_min a été traité *)
        )(* Etapes intermediaires *)
    in
    dijkstra_aux (sequence 1 nb_sommets) ;;

(function x-> x + 1)
(fun x y -> x + Y)