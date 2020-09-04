/* File parser.mly */
%{
  open Printf
  open Function
  (*let arr = Array.make_matrix (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) (nan) ;; *)
  let arr = Array.make_matrix (6) (6) (nan) ;;   (*store the spreadsheet*)
  let range1 = Array.make 4 0 ;;                (*store range*)
  let range2 = Array.make 4 0 ;;
  let index = Array.make 2 0 ;;                 (*store the destination index*)
  let cell_index = Array.make 2 0 ;;            (*store the index of cell used as an operand*)
  let const = Array.make 1 0. ;;                  (*store the constant used as an operand*)
  let oper = Array.make 1 0 ;;                    (*store the operator to be applied*)

(*let m = int_of_string Sys.argv.(1);; 
let n = int_of_string Sys.argv.(2);; *)

let m = 6 ;;
let n = 6 ;;

let rec read_list lx idx cur_idx = match lx with 
                            | [] -> None ;
                            | e::l -> (
                                    if not(e = "") then arr.(idx).(cur_idx) <- float_of_string e;
                                    read_list l idx (cur_idx+1) 
                                )
                            ;;


let read_file =
  try
    let in_stream = open_in "sheet.csv" in
        for i=0 to (m-1) do
          let line = input_line in_stream in
          let split = Str.split (Str.regexp ",") in
          let values = split line in
            read_list values i 0;
        done;
        close_in in_stream; 
  with e ->
    Printf.printf "File not found!";
    raise e ;;

    read_file ;;

  let print_array a = for i = 0 to Array.length a - 1 do 
                        Printf.printf "%g " a.(i) ;
                    done ;;
   
   let print_array_str a = for i = 0 to Array.length a - 1 do 
                        Printf.printf "%s " a.(i) ;
                    done ;;
    
  let print_2d_array a = for i = 0 to Array.length a - 1 do 
                        print_array a.(i);
                        print_newline ();
                    done ;;
    


%}

%token ADD, RANGE, INDEX, ASSIGN, SEMICOLON, EOL, CONST, DIV, SUBT, MULT, EOF
%token SUM, AVG, MIN, MAX
%token COUNT, ROWCOUNT, COLCOUNT, ROWAVG, COLAVG, ROWSUM, COLSUM, ROWMAX, COLMAX, ROWMIN, COLMIN
%type <float> CONST
%type <int*int> INDEX
%type <int*int*int*int> RANGE

%start main             /* the entry point */
%type <unit> main
%%
main:
        main expr     {}
      | expr   {}
    /* | line EOF   {}
    | line EOL   {}
    | EOF   {}
;
line :
    line expr SEMICOLON {}
; */
expr:
    head ASSIGN funct SEMICOLON   {print_2d_array arr; }
;

head:
    INDEX     { 
        let (v1,v2) = $1 in 
            index.(0) <- v1 ; index.(1) <- v2;
        Printf.printf"set index now %d %d \n" index.(0) index.(1) ;
    }
;

funct:
    ADD args        
    {  
        Printf.printf("calc now add\n");
        match oper.(0) with 
        | 1 -> add_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  range2.(0),range2.(1),range2.(2),range2.(3)  ,index.(0),index.(1));
        | 2 -> add_const (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 3 -> add_const (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 4 -> add_range_cell (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        | 5 -> add_range_cell (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        ;
    }  
  | SUBT args       
  { 
        Printf.printf("calc now sub\n");
        match oper.(0) with 
        | 1 -> sub_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  range2.(0),range2.(1),range2.(2),range2.(3)  ,index.(0),index.(1));
        | 2 -> sub_const_otherway (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 3 -> sub_const (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 4 -> sub_range_cell_otherway (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        | 5 -> sub_range_cell (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        ; 
  } 
  | MULT args       
  {     
        Printf.printf("calc now mult\n");
        match oper.(0) with 
        | 1 -> mult_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  range2.(0),range2.(1),range2.(2),range2.(3)  ,index.(0),index.(1));
        | 2 -> mult_const (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 3 -> mult_const (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 4 -> mult_range_cell (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        | 5 -> mult_range_cell (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        ; 
   } 
  | DIV args        
  {     Printf.printf("calc now sub\n");
        match oper.(0) with 
        | 1 -> div_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  range2.(0),range2.(1),range2.(2),range2.(3)  ,index.(0),index.(1));
        | 2 -> div_const_otherway (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 3 -> div_const (arr , range1.(0),range1.(1),range1.(2),range1.(3),  const.(0)  ,index.(0),index.(1));
        | 4 -> div_range_cell_otherway (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        | 5 -> div_range_cell (arr , range1.(0),range1.(1),range1.(2),range1.(3),  cell_index.(0),cell_index.(1)  ,index.(0),index.(1));
        ; 
  } 
  | COUNT args     
  {     
        Printf.printf("calc now count\n");
        match oper.(0) with 
        | 6 -> count_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }  
  | ROWCOUNT args  
  {     
        Printf.printf("calc now count\n");
        match oper.(0) with 
        | 6 -> row_count (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  | COLCOUNT args  
  {     
        Printf.printf("calc now count\n");
        match oper.(0) with 
        | 6 -> col_count (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  |  SUM args     
  {     
        Printf.printf("calc now sum\n");
        match oper.(0) with 
        | 6 -> sum_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }  
  | ROWSUM args  
  {     
        Printf.printf("calc now sum\n");
        match oper.(0) with 
        | 6 -> row_sum (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  | COLSUM args  
  {     
        Printf.printf("calc now sum\n");
        match oper.(0) with 
        | 6 -> col_sum (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }   
  |  AVG args     
  {     
        Printf.printf("calc now avg\n");
        match oper.(0) with 
        | 6 -> avg_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }  
  | ROWAVG args  
  {     
        Printf.printf("calc now avg\n");
        match oper.(0) with 
        | 6 -> row_avg (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  | COLAVG args  
  {     
        Printf.printf("calc now avg\n");
        match oper.(0) with 
        | 6 -> col_avg (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  |  MIN args     
  {     
        Printf.printf("calc now min\n");
        match oper.(0) with 
        | 6 -> min_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }  
  | ROWMIN args  
  {     
        Printf.printf("calc now min\n");
        match oper.(0) with 
        | 6 -> row_min (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  | COLMIN args  
  {     
        Printf.printf("calc now min\n");
        match oper.(0) with 
        | 6 -> col_min (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  |  MAX args     
  {     
        Printf.printf("calc now max\n");
        match oper.(0) with 
        | 6 -> max_range (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }  
  | ROWMAX args  
  {     
        Printf.printf("calc now max\n");
        match oper.(0) with 
        | 6 -> row_max (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   } 
  | COLMAX args  
  {     
        Printf.printf("calc now max\n");
        match oper.(0) with 
        | 6 -> col_max (arr , range1.(0),range1.(1),range1.(2),range1.(3),  index.(0),index.(1));
        ; 
   }
;
/* Store the values in respective arrays */
args:
    /* 1 */
    RANGE RANGE       {     
        oper.(0) <- 1;
        let (x1,x2,x3,x4) = $1 in 
            range1.(0) <- x1; 
            range1.(1) <- x2;
            range1.(2) <- x3;
            range1.(3) <- x4;
        let (y1,y2,y3,y4) = $2 in 
            range2.(0) <- y1; 
            range2.(1) <- y2;
            range2.(2) <- y3;
            range2.(3) <- y4;

        Printf.printf("R R \n")  
    }  

    /* 2 */
  | CONST RANGE       { 
      oper.(0) <- 2;
        let (x1,x2,x3,x4) = $2 in 
            range1.(0) <- x1; 
            range1.(1) <- x2;
            range1.(2) <- x3;
            range1.(3) <- x4;
        let y1 = $1 in 
            const.(0) <- y1; 

      Printf.printf("C R \n") 
    }


    /* 3 */
  | RANGE CONST       { 
      oper.(0) <- 3;
        let (x1,x2,x3,x4) = $1 in 
            range1.(0) <- x1; 
            range1.(1) <- x2;
            range1.(2) <- x3;
            range1.(3) <- x4;
        let y1 = $2 in 
            const.(0) <- y1; 

      Printf.printf("R C \n") 
    } 


    /* 4 */
  | INDEX RANGE       { 
      oper.(0) <- 4;
        let (x1,x2,x3,x4) = $2 in 
            range1.(0) <- x1; 
            range1.(1) <- x2;
            range1.(2) <- x3;
            range1.(3) <- x4;
        let (y1,y2) = $1 in 
            cell_index.(0) <- y1; 
            cell_index.(1) <- y2; 
      Printf.printf("I R \n") 
    } 


    /* 5 */
  | RANGE INDEX       { 
      oper.(0) <- 5;
        let (x1,x2,x3,x4) = $1 in 
            range1.(0) <- x1; 
            range1.(1) <- x2;
            range1.(2) <- x3;
            range1.(3) <- x4;
        let (y1,y2) = $2 in 
            cell_index.(0) <- y1; 
            cell_index.(1) <- y2;
      Printf.printf("R I \n") 
    }


    /* 6 */
  | RANGE {
      oper.(0) <- 6;
        let (x1,x2,x3,x4) = $1 in 
            range1.(0) <- x1; 
            range1.(1) <- x2;
            range1.(2) <- x3;
            range1.(3) <- x4;
      Printf.printf("R \n")
  }
;
