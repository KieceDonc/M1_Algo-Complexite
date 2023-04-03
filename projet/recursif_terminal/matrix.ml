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
        Array.fold_left
            (fun acc (x,y) -> acc.(x).(y) <- matrix.(y).(x); acc)
            (Array.make_matrix matrixSize matrixSize 0.0)
            (Array.init (matrixSize * matrixSize) (fun x -> (x / matrixSize, x mod matrixSize)))

    (* Transforme une matrice avec une taille étant une puissance de 2. Complète les nouvelles valeurs avec un 0 *)
    let transform matrix = 
        let inputSize = Array.length matrix in
        let finalSize = int_of_float(2. ** ceil(log(float_of_int inputSize) /. log(2.0))) in (*Recherche la puissance de 2 supérieur la plus proche*)
        Array.fold_left
            (fun acc (x,y) -> acc.(x).(y) <- matrix.(x).(y); acc)
            (Array.make_matrix finalSize finalSize 0.0)
            (Array.init (inputSize * inputSize) (fun x -> (x / inputSize, x mod inputSize)))
        
    (* 
    Crée une matrice de la forme A = B Ct  
                                    C D 
                                    C D 

                                     C D 

    Ct étant la transposé de C
    *)
    let createSpecial matrixSize =
        let matrix = create matrixSize in
        let subMatrixSize = matrixSize / 2 in
        Array.fold_left
            (fun acc (x,y) -> acc.(x + subMatrixSize).(y) <- matrix.(x).(y + subMatrixSize); acc)
            matrix
            (Array.init (subMatrixSize * subMatrixSize) (fun x -> (x / subMatrixSize, x mod subMatrixSize)))

    (* Fonction permettant d'extraire une sous-matrice d'une matrice donnée *)
    let sub matrix topLeftx topLefty length =
        Array.fold_left
            (fun acc (x,y) -> acc.(x).(y) <- matrix.(topLeftx + x).(topLefty + y); acc)
            (Array.make_matrix length length 0.0)
            (Array.init (length * length) (fun x -> (x / length, x mod length)))

    (* Divise une matrice en quatre sous-matrices *)
    let split matrix =
        let matrixSize = Array.length matrix in
        let subMatrixSize = matrixSize / 2 in
        let a11 = sub matrix 0 0 subMatrixSize in
        let a12 = sub matrix 0 subMatrixSize subMatrixSize in
        let a21 = sub matrix subMatrixSize 0 subMatrixSize in
        let a22 = sub matrix subMatrixSize subMatrixSize subMatrixSize in
        a11, a12, a21, a22

    (* Multiplie une matrice par une constante *)
    let multipleByConstant matrix constant = 
        let matrixSize = Array.length matrix in
        Array.fold_left
            (fun acc (x,y) -> acc.(x).(y) <- acc.(x).(y) *. constant; acc)
            (Array.make_matrix matrixSize  matrixSize 0.0)
            (Array.init (matrixSize * matrixSize) (fun x -> (x / matrixSize, x mod matrixSize)))
end