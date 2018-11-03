ClearAll;

(* Initialisation *)

boardinit = Table[0, 20, 10]; (* board empty *)
board = boardinit; (* board in the game *)

(*Creating pieces*)

yellowp = ReplaceAll[1 -> Yellow][{{1, 1}, {1, 1}} ];
redp = ReplaceAll[1 -> Red][{{1, 1, 0}, {0, 1, 1}}];
greenp = ReplaceAll[1 ->  Green][{{1, 0}, {1, 1}, {0, 1}}];
cyanp = ReplaceAll[1 -> Cyan][{{1}, {1}, {1}, {1}}];
bluep = ReplaceAll[1 -> Blue][{{1, 1}, {1, 0}, {1, 0}}];
orangep = ReplaceAll[1 -> Orange][{{1, 0}, {1, 0}, {1, 1}}];
purplep = ReplaceAll[1 -> Purple][{{1, 0}, {1, 1}, {1, 0}}];
lpiece = {{yellowp, Dimensions[yellowp]}, {redp, 
    Dimensions[redp]}, {greenp, Dimensions[greenp]}, {cyanp, 
    Dimensions[cyanp]}, {bluep, Dimensions[bluep]}, {orangep, 
    Dimensions[orangep]}, {purplep, Dimensions[purplep]}};


(* Functions *)

newboard[board_, t_, 
  p_] := {nboard = board; pie = p[[1]]; dim = p[[2]]; 
   nboard[[t ;; t + dim[[1]] - 1, r ;; r + dim[[2]] - 1]] = pie; 
   nboard}[[1]]

collision[piece_, a_] := 
 a <= 20 && board[[a + piece[[2, 1]], r]] === 0 && 
  board[[a + piece[[2, 1]], r + 1]] === 0

R[x_] := Rotate[x, angle](*This function does not actually work*)


(* Representations *)

board = boardinit;
r = 5; (*Placing the piece in the middle of the board at the \
beginning*)
angle = 0; 


DynamicModule[{a = 0, piece = RandomChoice[lpiece, 1][[1]]},
 EventHandler[Dynamic[Row[{
     MatrixPlot[board, ImageSize -> 200],
     If[a == 20 - piece[[2, 1]] + 1,
       {board = newboard[boardinit, a, piece], boardinit = board, 
        a = 1, r = 5},
       If[collision[piece, a],
        If[
         a == 0, {board = newboard[boardinit, 1, piece], 
          a++}, {board = newboard[boardinit, a, piece], a++}],
        {board = newboard[boardinit, a, piece], boardinit = board, 
         a = 1, r = 5, piece = RandomChoice[lpiece, 1][[1]]}]];
     }], TrackedSymbols :> {}, 
   UpdateInterval -> 0.3],(*slowing down the DynamicModule*)
  {"LeftArrowKeyDown" :> (r -= 1),(*Moving the piece left*)
   "RightArrowKeyDown" :> (r += 1)(*Moving the piece right*)
   }]]
