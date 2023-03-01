let createMatrix size =
  let finalSize = Float.to_int(2. ** Float.ceil(Float.log2(float_of_int size))) in (*Recherche la puissance de 2 supérieur la plus proche*)
  
  let finalMatrix = Array.make_matrix finalSize finalSize 0 in
  
  for x = 0 to finalSize - 1 do
    for y = 0 to finalSize - 1 do
      finalMatrix.(x).(y) <- Random.int 9 
    done
  done;

  finalMatrix

let myMatrix = createMatrix(3);;