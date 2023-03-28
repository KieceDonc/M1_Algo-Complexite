(*
(* Produit matriciel avec les opérations de Strassen *)
let rec strassenMultipleMatrices firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  if matrixSize = 1 then
    let result = Array.make_matrix 1 1 0.0 in
    result.(0).(0) <- firstMatrix.(0).(0) *. secondMatrix.(0).(0);
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
    let result = Array.make_matrix 1 1 0.0 in
    if matrix.(0).(0) = 0 then
      result.(0).(0) <- 0.0
    else
      result.(0).(0) <- 1.0 /. float_of_int(matrix.(0).(0));
    result
  else
    let a11, a12, a21, a22 = splitMatrix matrix in
    let r1 = inverseMatrix a11 in
    let r2 = strassenMultipleMatrices a21 r1 in
    let r3 = strassenMultipleMatrices r1 a12 in
    let r4 = strassenMultipleMatrices a21 r3 in
    let r5 = subtractMatrices r4 a22 in
    let r6 = inverseMatrix r5 in
    let x12 = strassenMultipleMatrices r3 r6 in
    let x21 = strassenMultipleMatrices r6 r2 in
    let r7 = strassenMultipleMatrices r3 x21 in
    let x11 = subtractMatrices r1 r7 in
    let x22 = multipleMatrixByConstant r6 (-1.) in
    mergeMatrices x11 x12 x21 x22
  
let myFirstMatrix = transformMatrix(createMatrix(4));;

let mySecondMatrix = transformMatrix(createMatrix(4));;

let myFirstSpecialMatrix = createSpecialMatrix 4;;
printMatrix myFirstSpecialMatrix;;

let myInvertMatrix = inverseMatrix myFirstSpecialMatrix;;
printMatrix myInvertMatrix;;
*)