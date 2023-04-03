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

let mySpecialMatrix = Matrix.createSpecial 4;;
Matrix.print mySpecialMatrix;;

let myInvertMatrix = Matrices.inverse mySpecialMatrix;;
Matrix.print myInvertMatrix;;

let approxIdentityMatrix = Matrices.multiple mySpecialMatrix myInvertMatrix;;
Matrix.print approxIdentityMatrix;;

let multiple = Matrices.multiple myFirstMatrix mySecondMatrix;;
Matrix.print multiple;;

let strassen = Matrices.strassenMultiple myFirstMatrix mySecondMatrix;;
Matrix.print strassen;;