(*
(* Produit matriciel avec les opérations de Strassen *)
let multipleMatrixStrassen firstMatrix secondMatrix =

(* Produit matriciel ordinaire *)
let multipleMatrix firstMatrix secondMatrix = 
*)

(* Transforme une matrice avec une taille étant une puissance de 2. Complète les nouvelles valeurs avec un 0 *)
let transformMatrix matrix = 
  let inputLength = Array.length matrix in
  let finalSize = Float.to_int(2. ** Float.ceil(Float.log2(float_of_int inputLength))) in (*Recherche la puissance de 2 supérieur la plus proche*)
  
  let finalMatrix = Array.make_matrix finalSize finalSize 0 in

  for x = 0 to finalSize - 1 do
    for y = 0 to finalSize - 1 do
      if x < inputLength && y < inputLength then
        finalMatrix.(x).(y) <- Random.int 10
      else
        finalMatrix.(x).(y) <- 0
    done
  done;

  finalMatrix

(* Crée une matrice contenant des valeurs de 0 à 9 avec une taille donnée *)
let createMatrix size =
  
  let matrix = Array.make_matrix size size 0 in
  
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      matrix.(x).(y) <- Random.int 10
    done
  done;

  matrix

let myMatrix = createMatrix(3);;
let myTransformedMatrix = transformMatrix(myMatrix);;