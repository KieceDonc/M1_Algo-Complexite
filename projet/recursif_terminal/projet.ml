open Matrix
open Matrices

let matrixSize = 32;;

let myFirstMatrix = Matrix.transform(Matrix.create(matrixSize));;

let mySecondMatrix = Matrix.transform(Matrix.create(matrixSize));;

let t1 = Sys.time();; 
Printf.printf "Début multiplication strassen\n";;
let resultStrassen = Matrices.strassenMultiple myFirstMatrix mySecondMatrix;;
let t2 = Sys.time();;
Printf.printf "Fin multiplication strassen\nTemps d'exécution : %.2f secondes\n" (t2 -. t1);;

Printf.printf "\n"
let t1 = Sys.time();;
Printf.printf "Début multiplication normal\n";;
let resultNormal = Matrices.multiple myFirstMatrix mySecondMatrix;;
let t2 = Sys.time();;
Printf.printf "Fin multiplication normal\nTemps d'exécution : %.2f secondes\n" (t2 -. t1);;

let mySpecialMatrix = Matrix.createSpecial matrixSize;;
Matrix.print mySpecialMatrix;;

let myInverseMatrix = Matrices.inverse mySpecialMatrix;;
Matrix.print myInverseMatrix;;

let approxIdentityMatrix = Matrices.multiple mySpecialMatrix myInverseMatrix;;
Matrix.print approxIdentityMatrix;;

let identityMatrix = Matrix.createIdentity matrixSize;;

let diffMatrix = Matrices.subtract approxIdentityMatrix identityMatrix;;
let totalDiff = Array.fold_left
  (fun acc (x,y) -> acc +. abs_float (diffMatrix.(x).(y)))
  0.0
  (Array.init (matrixSize * matrixSize) (fun x -> (x / matrixSize, x mod matrixSize)));;
print_float totalDiff;;
Printf.printf "\n";;

let totalDiff = Array.fold_left
  (fun acc (x,y) -> acc +. diffMatrix.(x).(y))
  0.0
  (Array.init (matrixSize * matrixSize) (fun x -> (x / matrixSize, x mod matrixSize)));;
print_float totalDiff;;
Printf.printf "\n";;