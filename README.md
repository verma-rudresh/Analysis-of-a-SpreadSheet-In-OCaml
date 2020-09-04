# Analysis-of-a-SpreadSheet-In-OCaml
lexer.mll : 
    this is a basic tokenizer . Ignore all the blank spaces and newline characters.
    Also for index token I used a int*int datatype. for ranges I used int*int*int*int to store two indices.
    Rest is just basic programming.

calc.ml :
    main file that calls the lexer as well as the parser.

function.ml :
    this file contains all the functions for the backend.
     
               array   range             destination
 => add_const (a,      x1,y1, x2,y2 , c , dx,dy) -> this function is used to add a constant value to the range specified.
                                                 uses a basic nested for loop to add values and store finally at the destination cell.
   
                array   range          range          destination
 => add_range (a,       x1,y1,x2,y2  ,x3,y3,x4,y4  ,  dx,dy) -> this function is used to add two ranges specified.
                                                 uses a basic nested for loop to add values and store finally at the destination cell.

 => add_range_cell (a,   x1,y1,x2,y2  ,tx,ty  ,dx,dy) -> this function is used to add range specified and value at a cell.
                                                 uses a basic nested for loop to add values and store finally at the destination cell.

##### Note := mult,div,subt also follows the same as above just the operator is different  

 => functions with suffix as _otherway were created to handle non-commutativity of the div and sub operations. Rest is same as above.

That was all for binary functions.
Now for unary operations, they follow :

 => abcd_row => this function evaluates the propery by iterating over the rows in the outer loop.
 then in the nested loop it iterates over the columns and keeps the storing the values in the required cell at the end of the loop.

 => abcd_col => this function evaluates the propery by iterating over the columns in the outer loop.
                    then in the nested loop it iterates over the rows and keeps the storing the values in the required cell at the end of the loop.

 => abcd_range => this function evaluates the propery by iterating over the entire range in two nested loops.
                    keeps the answer value in the required cell at the end of the loops.

Now all below functions follow the same structure with just few small changes.

 => count -> to track empty cells , I used the float value 'nan' . So use the function is_nan to check if cell is empty and update the counter as required.

 => min/max -> for min, I initialize the min value as Float.max_float then keep taking min over all cells.
                  for max, I initialize the max value as Float.min_float then keep taking max over all cells.
 => sum -> sum is straight forward. initialize with 0.0 and keep adding cells.

 => avg -> first part is same as sum. just at last divide by no.of cells.
    

parser.mly :
    few helper functions :
        read_list -> this function read a list of strings and then 
                        stores the values inside a float array.
        
        read_file -> this function is used to read the csv file.

        print_array -> used to print an array of floats.

        print_2d_array -> used to print 2d array using print array.

    Grammar :
        main -> main expr | expr

        where each expr is from of " head := FUNC ARGS ; "

        HEAD keeps the destination cell's location in from int*int.

        FUNC keeps the function to be applied.
