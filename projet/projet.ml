let printMatrix matrix =
  let matrixSize = Array.length matrix in
  for x = 0 to matrixSize - 1 do
    print_string "-------";
  done;
  print_string "\n";
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      print_int matrix.(x).(y);
      print_string "\t";
    done;
    print_string "\n";
  done;;

(* Crée une matrice contenant des valeurs de 0 à 9 avec une taille donnée *)
let createMatrix size =
  let matrix = Array.make_matrix size size 0 in
  
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      matrix.(x).(y) <- Random.int 10
    done
  done;

  matrix

(* Transforme une matrice avec une taille étant une puissance de 2. Complète les nouvelles valeurs avec un 0 *)
let transformMatrix matrix = 
  let inputSize = Array.length matrix in
  let finalSize = int_of_float(2. ** ceil(log(float_of_int inputSize) /. log(2.0))) in (*Recherche la puissance de 2 supérieur la plus proche*)
  
  let finalMatrix = Array.make_matrix finalSize finalSize 0 in

  for x = 0 to finalSize - 1 do
    for y = 0 to finalSize - 1 do
      if x < inputSize && y < inputSize then
        finalMatrix.(x).(y) <- matrix.(x).(y)
      else
        finalMatrix.(x).(y) <- 0
    done
  done;

  finalMatrix

  (* Fonction permettant d'extraire une sous-matrice d'une matrice donnée *)
let subMatrix matrix topLeftx topLefty length =
  let sub = Array.make_matrix length length matrix.(0).(0) in
  for x = 0 to length - 1 do
    for y = 0 to length - 1 do
      sub.(x).(y) <- matrix.(topLefty+x).(topLeftx+y)
    done
  done;
  sub

(* Fusionne les quatre sous-matrices en une matrice *)
let mergeMatrices a11 a12 a21 a22 =
  let matrixSize = Array.length a11 in
  let result = Array.make_matrix (2 * matrixSize) (2 * matrixSize) 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- a11.(x).(y);
      result.(x).(y + matrixSize) <- a12.(x).(y);
      result.(x + matrixSize).(y) <- a21.(x).(y);
      result.(x + matrixSize).(y + matrixSize) <- a22.(x).(y)
    done;
  done;
  result

(* Divise une matrice en quatre sous-matrices *)
let splitMatrix matrix =
  let matrixSize = Array.length matrix in
  let subMatrixSize = matrixSize / 2 in
  let a11 = subMatrix matrix 0 0 subMatrixSize in
  let a12 = subMatrix matrix subMatrixSize 0 subMatrixSize in
  let a21 = subMatrix matrix 0 subMatrixSize subMatrixSize in
  let a22 = subMatrix matrix subMatrixSize subMatrixSize subMatrixSize in
  a11, a12, a21, a22

(* Produit matriciel ordinaire *)
let multipleMatrices firstMatrix secondMatrix = 
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
let rec addMatrices firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  let result = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- firstMatrix.(x).(y) + secondMatrix.(x).(y)
    done;
  done;
  result

(* Soustraction de deux matrices *)
let rec subtractMatrices firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  let result = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- firstMatrix.(x).(y) - secondMatrix.(x).(y)
    done;
  done;
  result

(* Produit matriciel avec les opérations de Strassen *)
let rec strassenMultipleMatrices firstMatrix secondMatrix =
  let n = Array.length firstMatrix in
  if n = 1 then
    let result = Array.make_matrix 1 1 0 in
    result.(0).(0) <- firstMatrix.(0).(0) * secondMatrix.(0).(0);
    result
  else
    let a11, a12, a21, a22 = splitMatrix firstMatrix in
    let b11, b12, b21, b22 = splitMatrix secondMatrix in
    let s1 = strassenMultipleMatrices (addMatrices a11 a22) (addMatrices b11 b22) in
    let s2 = strassenMultipleMatrices (addMatrices a21 a22) b11 in
    let s3 = strassenMultipleMatrices a11 (subtractMatrices b12 b22) in
    let s4 = strassenMultipleMatrices a22 (subtractMatrices b21 b11) in
    let s5 = strassenMultipleMatrices (addMatrices a11 a12) b22 in
    let s6 = strassenMultipleMatrices (subtractMatrices a21 a11) (addMatrices b11 b12) in
    let s7 = strassenMultipleMatrices (subtractMatrices a12 a22) (addMatrices b21 b22) in
    let c11 = addMatrices (subtractMatrices (addMatrices s1 s4) s5) s7 in
    let c12 = addMatrices s3 s5 in
    let c21 = addMatrices s2 s4 in
    let c22 = addMatrices (subtractMatrices (addMatrices s1 s3) s2) s6 in
    mergeMatrices c11 c12 c21 c22

let myFirstMatrix = createMatrix(3);;
printMatrix myFirstMatrix;;
let myFirstTransformedMatrix = transformMatrix(myFirstMatrix);;
printMatrix myFirstTransformedMatrix;;

let mySecondMatrix = createMatrix(3);;
printMatrix mySecondMatrix;;
let mySecondTransformedMatrix = transformMatrix(mySecondMatrix);;
printMatrix mySecondTransformedMatrix;;

let myMultipledMatrix = multipleMatrices myFirstTransformedMatrix mySecondTransformedMatrix;;
printMatrix myMultipledMatrix;;

let myStrassenMultipledMatrix = strassenMultipleMatrices myFirstTransformedMatrix mySecondTransformedMatrix;;
printMatrix myStrassenMultipledMatrix;;