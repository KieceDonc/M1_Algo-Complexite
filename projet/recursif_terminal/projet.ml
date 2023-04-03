open Matrix
open Matrices

let firstMatrix = Matrix.create 4;;
Matrix.print firstMatrix;;

let secondMatrix = Matrix.create 4;;
Matrix.print secondMatrix;;

let thirdMatrix = Matrices.subtract firstMatrix secondMatrix;;
Matrix.print thirdMatrix;;