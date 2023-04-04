open Matrix
open Matrices

let myFirstMatrix = Matrix.transform(Matrix.create(2));;

let mySecondMatrix = Matrix.transform(Matrix.create(2));;

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

let specialMatrixSize = 4;;
let mySpecialMatrix = Matrix.createSpecial specialMatrixSize;;
Matrix.print mySpecialMatrix;;

let myInverseMatrix = Matrices.inverse mySpecialMatrix;;
Matrix.print myInverseMatrix;;

let approxIdentityMatrix = Matrices.multiple mySpecialMatrix myInverseMatrix;;
Matrix.print approxIdentityMatrix;;

let identityMatrix = Matrix.createIdentity specialMatrixSize;;

let diffMatrix = Matrices.subtract approxIdentityMatrix identityMatrix;;
let totalDiff = Array.fold_left
  (fun acc (x,y) -> acc +. abs_float (diffMatrix.(x).(y)))
  0.0
  (Array.init (specialMatrixSize * specialMatrixSize) (fun x -> (x / specialMatrixSize, x mod specialMatrixSize)));;
print_float totalDiff;;