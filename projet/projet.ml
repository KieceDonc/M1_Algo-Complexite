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

(* Fonction pour renvoyer la transposée d'une matrice *)
let transposeMatrix matrix =
  let matrixSize = Array.length matrix in
  let transposedMatrix = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      transposedMatrix.(x).(y) <- matrix.(y).(x)
    done;
  done;
  transposedMatrix

(* Crée la matrice identité *)
let createIdentityMatrix matrixSize =
  let identityMatrix = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    identityMatrix.(x).(x) <- 1;
  done;
  identityMatrix;;

(* Crée une matrice contenant des valeurs de 0 à 9 avec une taille donnée *)
let createMatrix size =
  let matrix = Array.make_matrix size size 0 in
  
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      matrix.(x).(y) <- Random.int 10
    done
  done;

  matrix

(* 
  Crée une matrice de la forme A = B Ct  
                                   C D 

  Ct étant la transposé de C
*)
let createSpecialMatrice matrixSize =
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

(* Divise une matrice en quatre sous-matrices *)
let splitMatrix matrix =
  let matrixSize = Array.length matrix in
  let subMatrixSize = matrixSize / 2 in
  let a11 = subMatrix matrix 0 0 subMatrixSize in
  let a12 = subMatrix matrix subMatrixSize 0 subMatrixSize in
  let a21 = subMatrix matrix 0 subMatrixSize subMatrixSize in
  let a22 = subMatrix matrix subMatrixSize subMatrixSize subMatrixSize in
  a11, a12, a21, a22

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

let multipleMatricesByConstant matrix constant = 
  let matrixSize = Array.length matrix in
  let result = Array.make_matrix matrixSize matrixSize 0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- (matrix.(x).(y) * constant)
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
  let matrixSize = Array.length firstMatrix in
  if matrixSize = 1 then
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

(* Retourne l'inverse d'une matrice *)
(* https://sd.blackball.lv/library/Introduction_to_Algorithms_Third_Edition_(2009).pdf *)
(* Page 830 *)
(* https://www.sciencedirect.com/science/article/pii/S0377042708006237#b5 *)
let rec inverseMatrix matrix =
  let matrixSize = Array.length matrix in
  if matrixSize = 1 then
    let result = Array.make_matrix 1 1 0 in
    result.(0).(0) <- (1/matrix.(0).(0));
    result
  else
    let a11, a12, a21, a22 = splitMatrix matrix in
    let r1 = transposeMatrix a11 in
    let r2 = strassenMultipleMatrices a21 r1 in
    let r3 = strassenMultipleMatrices r1 a12 in
    let r4 = strassenMultipleMatrices a21 r3 in
    let r5 = subtractMatrices r4 a22 in
    let r6 = transposeMatrix r5 in
    let x12 = strassenMultipleMatrices r3 r6 in
    let x21 = strassenMultipleMatrices r6 r2 in
    let r7 = strassenMultipleMatrices r3 x21 in
    let x11 = subtractMatrices r1 r7 in
    let x22 = multipleMatricesByConstant r6 (-1) in
    printMatrix x12;
    mergeMatrices x11 x12 x21 x22
  
let myFirstMatrix = transformMatrix(createMatrix(4));;
printMatrix myFirstMatrix;;

let mySecondMatrix = transformMatrix(createMatrix(4));;
printMatrix mySecondMatrix;;

let myInvertMatrix = inverseMatrix myFirstMatrix;;