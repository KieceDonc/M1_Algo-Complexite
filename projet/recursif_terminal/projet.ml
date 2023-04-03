open Matrix
open Matrices

let identityMatrix = Matrix.createIdentity 4;;
Matrix.print identityMatrix;;

let matrix = Matrix.create 4;;
Matrix.print matrix;;

let transposedMatrix = Matrix.transpose matrix;;
Matrix.print transposedMatrix;;

let transformedMatrix = Matrix.transform (Matrix.create 5);;
Matrix.print transformedMatrix;;

let specialMatrix = Matrix.createSpecial 4;;
Matrix.print specialMatrix;;

let subMatrix = Matrix.sub specialMatrix 1 1 2;;
Matrix.print subMatrix;;

let constant = Matrix.multipleByConstant subMatrix 4.;;
Matrix.print constant;;