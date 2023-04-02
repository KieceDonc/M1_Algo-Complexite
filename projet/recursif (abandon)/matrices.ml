  module Matrices = struct
    open Matrix
    
    (* Fusionne les quatre sous-matrices en une matrice *)
    let merge a11 a12 a21 a22 =
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
    let multiple firstMatrix secondMatrix = 
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

    (* Produit matriciel avec les opérations de Strassen *)
    let rec strassenMultiple firstMatrix secondMatrix =
      let matrixSize = Array.length firstMatrix in
      if matrixSize = 1 then
        let result = Array.make_matrix 1 1 0.0 in
        result.(0).(0) <- firstMatrix.(0).(0) *. secondMatrix.(0).(0);
        result
      else
        let a11, a12, a21, a22 = Matrix.split firstMatrix in
        let b11, b12, b21, b22 = Matrix.split secondMatrix in
        let s1 = strassenMultiple (add a11 a22) (add b11 b22) in
        let s2 = strassenMultiple (add a21 a22) b11 in
        let s3 = strassenMultiple a11 (subtract b12 b22) in
        let s4 = strassenMultiple a22 (subtract b21 b11) in
        let s5 = strassenMultiple (add a11 a12) b22 in
        let s6 = strassenMultiple (subtract a21 a11) (add b11 b12) in
        let s7 = strassenMultiple (subtract a12 a22) (add b21 b22) in
        let c11 = add (subtract (add s1 s4) s5) s7 in
        let c12 = add s3 s5 in
        let c21 = add s2 s4 in
        let c22 = add (subtract (add s1 s3) s2) s6 in
        merge c11 c12 c21 c22

    (* Retourne l'inverse d'une matrice *)
    (* https://sd.blackball.lv/library/Introduction_to_Algorithms_Third_Edition_(2009).pdf *)
    (* Page 830 *)
    (* https://www.sciencedirect.com/science/article/pii/S0377042708006237#b5 *)
    let rec inverse matrix =
      let matrixSize = Array.length matrix in
      if matrixSize = 1 then
        let result = Array.make_matrix 1 1 0.0 in
        if matrix.(0).(0) = 0.0 then
          result.(0).(0) <- 0.0
        else
          result.(0).(0) <- 1.0 /. matrix.(0).(0);
        result
      else
        let a11, a12, a21, a22 = Matrix.split matrix in
        let r1 = inverse a11 in
        let r2 = strassenMultiple a21 r1 in
        let r3 = strassenMultiple r1 a12 in
        let r4 = strassenMultiple a21 r3 in
        let r5 = subtract r4 a22 in
        let r6 = inverse r5 in
        let x12 = strassenMultiple r3 r6 in
        let x21 = strassenMultiple r6 r2 in
        let r7 = strassenMultiple r3 x21 in
        let x11 = subtract r1 r7 in
        let x22 = Matrix.multipleByConstant r6 (-1.) in
        merge x11 x12 x21 x22

  end