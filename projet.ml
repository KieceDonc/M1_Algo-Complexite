
(* Produit matriciel avec les opérations de Strassen *)
let rec strassenMultipleMatrix a b = 0;;


(* Produit matriciel ordinaire *)
let multipleMatrix firstMatrix secondMatrix = 
  let rowLengthFirstMatrix = Array.length firstMatrix in
  let colsLengthSecondMatrix = Array.length secondMatrix.(0) in
  let colsLengthFirstMatrix = Array.length firstMatrix.(0) in 
  let result = Array.make_matrix rowLengthFirstMatrix colsLengthSecondMatrix 0 in
  for i = 0 to rowLengthFirstMatrix - 1 do
    for j = 0 to colsLengthSecondMatrix - 1 do
      for k = 0 to colsLengthFirstMatrix - 1 do
        result.(i).(j) <- result.(i).(j) + firstMatrix.(i).(k) * secondMatrix.(k).(j)
      done;
    done;
  done;
  result

(* Addition de deux matrices *)
let rec addMatrix firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  let result = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- firstMatrix.(x).(y) + secondMatrix.(x).(y)
    done;
  done;
  result

(* Soustraction de deux matrices *)
let rec subtractMatrix firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  let result = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- firstMatrix.(x).(y) - secondMatrix.(x).(y)
    done;
  done;
  result

(* Fonction permettant d'extraire une sous-matrice d'une matrice donnée *)
let subMatrix matrix topLeftx topLefty length =
  let sub = Array.make_matrix length length matrix.(0).(0) in
  for x = 0 to length - 1 do
    for y = 0 to length - 1 do
      sub.(x).(y) <- matrix.(topLefty+x).(topLeftx+y)
    done
  done;
  sub

(* Transforme une matrice avec une taille étant une puissance de 2. Complète les nouvelles valeurs avec un 0 *)
let transformMatrix matrix = 
  let inputSize = Array.length matrix in
  let finalSize = int_of_float(2. ** ceil(log(float_of_int inputSize) /. log(2.0))) in (*Recherche la puissance de 2 supérieur la plus proche*)
  
  let finalMatrix = Array.make_matrix finalSize finalSize 0 in

  for x = 0 to finalSize - 1 do
    for y = 0 to finalSize - 1 do
      if x < inputSize && y < inputSize then
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

let myFirstMatrix = createMatrix(3);;
let myFirstTransformedMatrix = transformMatrix(myFirstMatrix);;
let mySecondMatrix = createMatrix(3);;
let mySecondTransformedMatrix = transformMatrix(mySecondMatrix);;

let myMultipledMatrix = multipleMatrix myFirstTransformedMatrix mySecondTransformedMatrix;;

let mySubMatrix = subMatrix myFirstMatrix 0 0 2;;