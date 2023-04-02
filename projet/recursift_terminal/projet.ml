open Matrix
open Matrices

let identityMatrix = Matrix.createIdentity 4;;
Matrix.print identityMatrix;;

let matrix = Matrix.create 4;;
Matrix.print matrix;;

let transposedMatrix = Matrix.transpose matrix;;
Matrix.print transposedMatrix;;

let array = Array.fold_left (fun acc x -> acc.(x) <- Random.int 10; acc) (Array.make 10 0) (Array.init 10 (fun x -> x)) in

Array.iter (Printf.printf "%d ") new_arr;;