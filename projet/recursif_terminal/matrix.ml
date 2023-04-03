module Matrix = struct
    let print matrix =
        let matrixSize = Array.length matrix in
        for x = 0 to matrixSize - 1 do
            print_string "-------";
        done;
        print_string "\n";
        for x = 0 to matrixSize - 1 do
            for y = 0 to matrixSize - 1 do
            print_float matrix.(x).(y);
            print_string "\t";
            done;
            print_string "\n";
        done;;

    (* Crée la matrice identité *)
    let createIdentity matrixSize =
        Array.fold_left
          (fun acc x -> acc.(x) <- Array.make matrixSize 0.0; acc.(x).(x) <- 1.0; acc)
          (Array.make_matrix matrixSize matrixSize 0.0)
          (Array.init matrixSize (fun x -> x))
    
    (* Crée une matrice contenant des valeurs de 0 à 9 avec une taille donnée *)
    let create matrixSize =
        Array.fold_left
            (fun acc x -> acc.(x) <- Array.init matrixSize (fun _ -> float_of_int (Random.int 10)); acc)
            (Array.make_matrix matrixSize matrixSize 0.0)
            (Array.init matrixSize (fun x -> x))

    (* Fonction pour renvoyer la transposée d'une matrice *)
    let transpose matrix = 
        let matrixSize = Array.length matrix in
        let finaMatrix = Array.make_matrix matrixSize matrixSize 0.0 in
        let rec transpose x y = 
          finaMatrix.(y).(x) <- matrix.(x).(y);
          if x = matrixSize - 1 && y = matrixSize - 1 then
            finaMatrix
          else if y = matrixSize - 1 then
            transpose (x+1) 0
          else
            transpose x (y+1)
        in
        transpose 0 0

    (* Transforme une matrice avec une taille étant une puissance de 2. Complète les nouvelles valeurs avec un 0 *)
    let transform matrix = 
        let inputSize = Array.length matrix in
        let finalSize = int_of_float(2. ** ceil(log(float_of_int inputSize) /. log(2.0))) in (*Recherche la puissance de 2 supérieur la plus proche*)
        
        let finalMatrix = Array.make_matrix finalSize finalSize 0.0 in

        for x = 0 to finalSize - 1 do
            for y = 0 to finalSize - 1 do
            if x < inputSize && y < inputSize then
                finalMatrix.(x).(y) <- matrix.(x).(y)
            else
                finalMatrix.(x).(y) <- 0.0
            done;
        done;

        finalMatrix

    (* 
    Crée une matrice de la forme A = B Ct  
                                    C D 
                                    C D 

                                     C D 

    Ct étant la transposé de C
    *)
    let createSpecial matrixSize =
        let subMatrixSize = matrixSize / 2 in
        let result = Array.make_matrix matrixSize matrixSize 0.0 in
        let b = create subMatrixSize in
        let c = create subMatrixSize in
        let d = create subMatrixSize in
        let ct = transpose c in
        for x = 0 to subMatrixSize - 1 do
            for y = 0 to subMatrixSize - 1 do
            result.(x).(y) <- b.(x).(y);
            result.(x).(y + subMatrixSize) <- ct.(x).(y);
            result.(x + subMatrixSize).(y) <- c.(x).(y);
            result.(x + subMatrixSize).(y + subMatrixSize) <- d.(x).(y)
            done;
        done;
        transform result

    (* Fonction permettant d'extraire une sous-matrice d'une matrice donnée *)
    let sub matrix topLeftx topLefty length =
        let sub = Array.make_matrix length length 0.0 in
        for x = 0 to length - 1 do
            for y = 0 to length - 1 do
                sub.(x).(y) <- matrix.(topLefty+x).(topLeftx+y)
            done;
        done;
        sub

    (* Divise une matrice en quatre sous-matrices *)
    let split matrix =
        let matrixSize = Array.length matrix in
        let subMatrixSize = matrixSize / 2 in
        let a11 = sub matrix 0 0 subMatrixSize in
        let a12 = sub matrix subMatrixSize 0 subMatrixSize in
        let a21 = sub matrix 0 subMatrixSize subMatrixSize in
        let a22 = sub matrix subMatrixSize subMatrixSize subMatrixSize in
        a11, a12, a21, a22

    let multipleByConstant matrix constant = 
        let matrixSize = Array.length matrix in
        let result = Array.make_matrix matrixSize matrixSize 0.0 in
        for x = 0 to matrixSize - 1 do
            for y = 0 to matrixSize - 1 do
                result.(x).(y) <- (matrix.(x).(y) *. constant)
            done;
        done;
        result
end