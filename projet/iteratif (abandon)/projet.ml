open Matrix
open Matrices

let mySpecialMatrix = Matrix.createSpecial 512;;
Matrix.print mySpecialMatrix;;

let myInvertMatrix = Matrices.inverse mySpecialMatrix;;
Matrix.print myInvertMatrix;;

let test = Matrices.multiple mySpecialMatrix myInvertMatrix;;
Matrix.print test