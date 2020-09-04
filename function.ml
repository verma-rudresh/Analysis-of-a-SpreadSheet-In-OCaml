(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let add_const (a, x1,y1, x2,y2 ,c , dx,dy) =  for i = 0 to abs(x2-x1) do 
  for j = 0 to (y2-y1) do
      a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) +. c; 
  done;
done;;

(* to add ranged values, make sure x1 <= x2 and y1 <= y2 also that x2-x1 = x4-x3  *)
let add_range (a,   x1,y1,x2,y2  ,x3,y3,x4,y4  ,dx,dy) =  for i = 0 to abs(x2-x1) do 
              for j = 0 to abs(y2-y1) do
                  a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) +. a.(x3+i).(y3+j); 
              done;
          done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let add_range_cell (a, x1,y1, x2,y2 ,tx,ty , dx,dy) =  for i = 0 to abs(x2-x1) do 
      for j = 0 to abs(y2-y1) do
          a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) +. a.(tx).(ty); 
      done;
  done;;

(* to mult ranged values, make sure x1 <= x2 and y1 <= y2 also that x2-x1 = x4-x3  *)
let mult_range (a,   x1,y1,x2,y2  ,x3,y3,x4,y4  ,dx,dy) =  for i = 0 to abs(x2-x1) do 
              for j = 0 to abs(y2-y1) do
                  a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) *. a.(x3+i).(y3+j); 
              done;
          done;;
          

(* mult a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let mult_range_cell (a, x1,y1, x2,y2 ,tx,ty , dx,dy) =  for i = 0 to abs(x2-x1) do 
          for j = 0 to abs(y2-y1) do
              a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) *. a.(tx).(ty); 
          done;
      done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let mult_const (a, x1,y1, x2,y2 ,c , dx,dy) =  for i = 0 to abs(x2-x1) do 
  for j = 0 to abs(y2-y1) do
      a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) *. c; 
  done;
done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let sub_const (a, x1,y1, x2,y2 ,c , dx,dy) =  for i = 0 to abs(x2-x1) do 
  for j = 0 to abs(y2-y1) do
      a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) -. c; 
  done;
done;;

(* to add ranged values, make sure x1 <= x2 and y1 <= y2 also that x2-x1 = x4-x3  *)
let sub_range (a,   x1,y1,x2,y2  ,x3,y3,x4,y4  ,dx,dy) =  for i = 0 to abs(x2-x1) do 
              for j = 0 to abs(y2-y1) do
                  a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) -. a.(x3+i).(y3+j); 
              done;
          done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let sub_range_cell (a, x1,y1, x2,y2 ,tx,ty , dx,dy) =  for i = 0 to abs(x2-x1) do 
      for j = 0 to abs(y2-y1) do
          a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) -. a.(tx).(ty); 
      done;
  done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let sub_const_otherway (a, x1,y1, x2,y2 ,c , dx,dy) =  for i = 0 to abs(x2-x1) do 
  for j = 0 to abs(y2-y1) do
      a.(dx+i).(dy+j) <- c -. a.(x1+i).(y1+j); 
  done;
done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let sub_range_cell_otherway (a, x1,y1, x2,y2 ,tx,ty , dx,dy) =  for i = 0 to abs(x2-x1) do 
      for j = 0 to abs(y2-y1) do
          a.(dx+i).(dy+j) <- a.(tx).(ty) -. a.(x1+i).(y1+j); 
      done;
  done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let div_const (a, x1,y1, x2,y2 ,c , dx,dy) =  for i = 0 to abs(x2-x1) do 
  for j = 0 to abs(y2-y1) do
      a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) /. c; 
  done;
done;;

(* to add ranged values, make sure x1 <= x2 and y1 <= y2 also that x2-x1 = x4-x3  *)
let div_range (a,   x1,y1,x2,y2  ,x3,y3,x4,y4  ,dx,dy) =  for i = 0 to abs(x2-x1) do 
              for j = 0 to abs(y2-y1) do
                  a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) /. a.(x3+i).(y3+j); 
              done;
          done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let div_range_cell (a, x1,y1, x2,y2 ,tx,ty , dx,dy) =  for i = 0 to abs(x2-x1) do 
      for j = 0 to abs(y2-y1) do
          a.(dx+i).(dy+j) <- a.(x1+i).(y1+j) /. a.(tx).(ty); 
      done;
  done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let div_const_otherway (a, x1,y1, x2,y2 ,c , dx,dy) =  for i = 0 to abs(x2-x1) do 
  for j = 0 to abs(y2-y1) do
      a.(dx+i).(dy+j) <- c /. a.(x1+i).(y1+j); 
  done;
done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let div_range_cell_otherway (a, x1,y1, x2,y2 ,tx,ty , dx,dy) =  for i = 0 to abs(x2-x1) do 
      for j = 0 to abs(y2-y1) do
          a.(dx+i).(dy+j) <- a.(tx).(ty) /. a.(x1+i).(y1+j); 
      done;
  done;;

let row_min (a, x1,y1,x2,y2,  dx,dy) =  
    for i = 0 to (x2-x1) do
        a.(dx+i).(dy) <- max_float ;
        for j = 0 to (y2-y1) do 
                a.(dx+i).(dy) <- min a.(x1+i).(y1+j) a.(dx+i).(dy) ;
         done ;
         a.(dx+i).(dy) <- a.(dx+i).(dy) /. (float_of_int(y2-y1+1))  ;
    done ;;

let row_max (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (x2-x1) do
      a.(dx+i).(dy) <- min_float ;
      for j = 0 to (y2-y1) do 
              a.(dx+i).(dy) <- max a.(x1+i).(y1+j) a.(dx+i).(dy) ;
        done ;
        a.(dx+i).(dy) <- a.(dx+i).(dy) /. (float_of_int(y2-y1+1))  ;
  done ;;

let col_min (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (y2-y1) do
      a.(dx).(dy+i) <- max_float ;
      for j = 0 to (x2-x1) do 
              a.(dx).(dy+i) <- min a.(x1+j).(y1+i) a.(dx).(dy+i) ;
        done
  done ;;

let col_max (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (y2-y1) do
      a.(dx).(dy+i) <- min_float ;
      for j = 0 to (x2-x1) do 
              a.(dx).(dy+i) <- max a.(x1+j).(y1+i) a.(dx).(dy+i) ;
        done
  done ;;


(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let max_range (a, x1,y1, x2,y2 , dx,dy) =   a.(dx).(dy) <- min_float ;
for i = 0 to abs(x2-x1) do 
for j = 0 to abs(y2-y1) do
  a.(dx).(dy) <- max a.(x1+i).(y1+j) a.(dx).(dy) ; 
done;
done;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let min_range (a, x1,y1, x2,y2 , dx,dy) =   a.(dx).(dy) <- max_float ;
for i = 0 to abs(x2-x1) do 
for j = 0 to abs(y2-y1) do
  a.(dx).(dy) <- min a.(x1+i).(y1+j) a.(dx).(dy); 
done;
done;;

let row_avg (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (x2-x1) do
      a.(dx+i).(dy) <- 0. ;
      for j = 0 to (y2-y1) do 
              a.(dx+i).(dy) <- a.(x1+i).(y1+j) +. a.(dx+i).(dy) ;
       done ;
       a.(dx+i).(dy) <- a.(dx+i).(dy) /. (float_of_int(y2-y1+1))  ;
  done ;;


let col_avg (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (y2-y1) do
      a.(dx).(dy+i) <- 0. ;
      for j = 0 to (x2-x1) do 
              a.(dx).(dy+i) <- a.(x1+j).(y1+i) +. a.(dx).(dy+i) ;
        done;
        a.(dx).(dy+i) <- a.(dx).(dy+i) /. (float_of_int(x2-x1+1))  ;
  done ;;


(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let avg_range (a, x1,y1, x2,y2 , dx,dy) =   a.(dx).(dy) <- 0.0 ;
for i = 0 to abs(x2-x1) do 
for j = 0 to abs(y2-y1) do
  a.(dx).(dy) <- a.(x1+i).(y1+j) +. a.(dx).(dy); 
done;
done;
a.(dx).(dy) <- (a.(dx).(dy)) /. (float_of_int ((x2-x1+1)*(y2-y1+1))) ;;

let sum_range (a, x1,y1, x2,y2 , dx,dy) =   a.(dx).(dy) <- 0.0 ;
for i = 0 to abs(x2-x1) do 
for j = 0 to abs(y2-y1) do
  a.(dx).(dy) <- a.(x1+i).(y1+j) +. a.(dx).(dy); 
done;
done;;

let col_sum (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (y2-y1) do
      a.(dx).(dy+i) <- 0. ;
      for j = 0 to (x2-x1) do 
              a.(dx).(dy+i) <- a.(x1+j).(y1+i) +. a.(dx).(dy+i) ;
       done
  done ;;


let row_sum (a, x1,y1,x2,y2,  dx,dy) =  
    for i = 0 to (x2-x1) do
        a.(dx+i).(dy) <- 0. ;
        for j = 0 to (y2-y1) do 
                a.(dx+i).(dy) <- a.(x1+i).(y1+j) +. a.(dx+i).(dy) ;
         done
    done ;;

let row_count (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (x2-x1) do
      a.(dx+i).(dy) <- 0. ;
      for j = 0 to (y2-y1) do
              if((Float.is_nan a.(x1+i).(y1+j)) = false) then
                  a.(dx+i).(dy) <- 1. +. a.(dx+i).(dy) ;
        done
  done ;;


let col_count (a, x1,y1,x2,y2,  dx,dy) =  
  for i = 0 to (y2-y1) do
      a.(dx).(dy+i) <- 0. ;
      for j = 0 to (x2-x1) do
              if((Float.is_nan a.(x1+j).(y1+i)) = false) then
                  a.(dx).(dy+i) <- 1. +. a.(dx).(dy+i) ;
        done
  done ;;

(* add a constant to a range of cells *)  (*assuming that x1 <= x2 and y1 <= y2*)
let count_range (a, x1,y1, x2,y2 , dx,dy) =   a.(dx).(dy) <- 0.0 ;
for i = 0 to abs(x2-x1) do 
for j = 0 to abs(y2-y1) do
  if((Float.is_nan a.(x1+i).(y1+j)) = false) then
      a.(dx).(dy) <- 1. +. a.(dx).(dy);
done;
done;;