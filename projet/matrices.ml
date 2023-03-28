(* Fusionne les quatre sous-matrices en une matrice *)
let mergeMatrices a11 a12 a21 a22 =
  let matrixSize = Array.length a11 in
  let result = Array.make_matrix (2 * matrixSize) (2 * matrixSize) 0.0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- a11.(x).(y);
      result.(x).(y + matrixSize) <- a12.(x).(y);
      result.(x + matrixSize).(y) <- a21.(x).(y);
      result.(x + matrixSize).(y + matrixSize) <- a22.(x).(y);
    done;
  done;
  result

(* Produit matriciel ordinaire *)
let multipleMatrices firstMatrix secondMatrix = 
  let rowLengthFirstMatrix = Array.length firstMatrix in
  let colsLengthSecondMatrix = Array.length secondMatrix.(0) in
  let colsLengthFirstMatrix = Array.length firstMatrix.(0) in 
  let result = Array.make_matrix rowLengthFirstMatrix colsLengthSecondMatrix 0.0 in
  for i = 0 to rowLengthFirstMatrix - 1 do
    for j = 0 to colsLengthSecondMatrix - 1 do
      for k = 0 to colsLengthFirstMatrix - 1 do
        result.(i).(j) <- result.(i).(j) +. firstMatrix.(i).(k) *. secondMatrix.(k).(j)
      done;
    done;
  done;
  result

(* Addition de deux matrices *)
let add firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  let result = Array.make_matrix matrixSize matrixSize 0.0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- firstMatrix.(x).(y) +. secondMatrix.(x).(y)
    done;
  done;
  result

(* Soustraction de deux matrices *)
let rec subtract firstMatrix secondMatrix =
  let matrixSize = Array.length firstMatrix in
  let result = Array.make_matrix matrixSize matrixSize 0.0 in
  for x = 0 to matrixSize - 1 do
    for y = 0 to matrixSize - 1 do
      result.(x).(y) <- firstMatrix.(x).(y) -. secondMatrix.(x).(y)
    done;
  done;
  result
