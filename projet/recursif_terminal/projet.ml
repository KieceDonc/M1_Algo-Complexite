open Matrix
open Matrices

let identityMatrix = Matrix.createIdentity 4;;
Matrix.print identityMatrix;;

let matrix = Matrix.create 4;;
Matrix.print matrix;;

let transposedMatrix = Matrix.transpose matrix;;
Matrix.print transposedMatrix;;
