(* ::Package:: *)

(* Welcome to Mathematica Tetris Project !
This package creates a simple playable Tetris game
Just click "Run Package" and see magic appear !

What's missing:
Bug fixes for blue piece (go left with blue piece and will collide alone...
Bug fixes and various improvements
*)

(* This function is not really useful and will be removed in future versions *)
ClearAll;

(* Initialisation *)
boardinit=Table[0, 20, 10]; (* board empty *)
board=boardinit; (* board in the game *)

(* All the possible pieces of the game 
A piece has the following structure :
piece[[y, x]] (x for direction -- and y for | )*)
yellowPiece={{1, 1},{1, 1}} /. 1 -> Yellow;
redPiece={{1, 1, 0},{0, 1, 1}} /. 1 -> Red;
greenPiece={{1, 0}, {1, 1}, {0, 1}} /. 1 -> Green;
cyanPiece={{1},{1},{1},{1}} /. 1 -> Cyan;
bluePiece={{1,1},{1,0},{1,0}} /. 1 -> Blue;
orangePiece={{1,0},{1,0},{1,1}} /. 1 -> Orange;
purplePiece={{1,0},{1,1},{1,0}} /. 1 -> Purple;

points=0;

(* Here is a list (2 dimens array) of all the pieces, with their dimensions *)
lpiece={
	{yellowPiece, Dimensions[yellowPiece]},
	{redPiece, Dimensions[redPiece]},
	{greenPiece, Dimensions[greenPiece]},
	{cyanPiece, Dimensions[cyanPiece]},
	{bluePiece, Dimensions[bluePiece]},
	{orangePiece, Dimensions[orangePiece]},
	{purplePiece, Dimensions[purplePiece]}
};


(* Functions *)

(* Returns an array with all the dimensions, layer by layer of the piece
The return type is {{amount, decalX}, ...} *)
get3DConfig[piece_]:= (
	rList = {};
	Table[
		counter = 0;
		starter = 0;
		Table[
			If[piece[[1, i, j]] =!= 0, counter++; If[counter == 1, starter = j]],
			{j, 1, Length[piece[[1, i]]]}
		];
		AppendTo[rList, {counter, starter}],
		{i, 1, Length[piece[[1]]]}
	];
	Return[rList]
)

(* Creates a new board with the given piece
This method has been recoded because of empty locations : void must not replace pieces *)
newboard[board_, t_, p_]:={
	nboard = board;
	pieceShape = get3DConfig[p];
	nboard = ReplacePart[nboard, 
		{i_ /; i >= t && i < t + p[[2, 1]],
		j_ /; j >= r && j < r + p[[2, 2]]}
		:> If[p[[1, i - t + 1, j - r + 1]] =!= 0, p[[1, i - t + 1, j - r + 1]], nboard[[i, j]]]];
	nboard
}[[1]]

(* Checks if there is collision with other pieces or sides of the board
Retruns False if collision, True instead *)
collision[piece_, a_]:= (
	(* Returns all the locations where we have to check for collision
	This method can be recalled if needed, in order to update the locations *)
	collisionCheckLocations[]:=(
		rList = {};
		Table[Table[
			If[a - 1 + i < 20 && i == Length[piece[[1]]] && piece[[1, i, j]] =!= 0, AppendTo[rList, {i + 1, j}], (
				If[piece[[1, i, j]] == 0 && i =!= 1 && piece[[1, i - 1, j]] =!= 0, AppendTo[rList, {i, j}]];
			)],
		{j, 1, Length[piece[[1, i]]]}], {i, 1, Length[piece[[1]]]}];
		Return[rList]
	);
	
	collisionsChecks = collisionCheckLocations[];
	hasCollision = False;
	Table[
		If[
			!hasCollision && a - 1 + collisionsChecks[[i, 1]] > 0 &&
			board[[a - 1 + collisionsChecks[[i, 1]], r - 1 + collisionsChecks[[i, 2]]]] =!= 0,
			hasCollision = True; Return[]
		],
	{i, 1, Length[collisionsChecks]}];
	
	Return[!hasCollision]
)

(* Rotates a piece *)
rotatePiece[piece_] := Return[{Reverse[piece[[1]], {2}]\[Transpose], Reverse[piece[[2]]]}]

(* Checks if the current piece can move in the provided direction.
The direction can be symbolised with every number, positive or negative *)
moveCurrentPiece[piece_, direction_, alt_]:=(
	If[r > 1 && Sign[direction] == -1, (
		valid = True;
		Table[(
			If[board[[alt + i, r - 1]] =!= 0, valid = False]
		), {i, 0, Length[piece] - 1}];
		If[valid, r--]
	)];
	(* r is dicreased of 1 because the location is count two times in r and Length[piece[[...]]] *)
	If[r - 1 + Length[piece[[1, 1]]] < Length[board[[1]]] && Sign[direction] == 1, (
		valid = True;
		Table[(
			If[board[[alt + i, r + Length[piece[[1, 1]]]]] =!= 0, valid = False]
		), {i, 0, Length[piece] - 1}];
		If[valid, r++]
	)]
)

checkForRowRemoval[a_, piece_]:=(
	(* This function will remove the filled rows *)
	Table[(
		If[Length[Select[boardinit[[a - 1  + i]], # == 0 &]] == 0, (points++;boardinit=Delete[boardinit, a - 1  + i];PrependTo[boardinit, Table[0, 10]])];
	), {i, 1, Length[piece[[1]]]}]
)

checkForGameOver[a_]:=(
	If[a == 1, Print["Game Over ! You had ", points, " points"];gameSpeed = 0];
)


(* Representations *)

board=boardinit;
r=5; (*Placing the piece in the middle of the board at the beginning*)
gameSpeed = 0.3;


DynamicModule[
	{
		a=1,
		piece=RandomChoice[lpiece,1][[1]]
	},
	EventHandler[
		Dynamic[
			Row[
				{
					MatrixPlot[
						board, 
						ImageSize -> 200
					],
					If[gameSpeed =!= 0, (
						If[a == 20 - piece[[2,1]] + 1,
							{
								board=newboard[boardinit,a,piece],
								boardinit=board,
								checkForRowRemoval[a, piece],
								a=1,
								r=5,
								piece=RandomChoice[lpiece, 1][[1]]
							},
							If[collision[piece, a],
								If[a==0,
									{
										board=newboard[boardinit, 1, piece],
										a++
									},
									{
										board=newboard[boardinit, a, piece],
										a++
									}
								],
								{
									(* The piece performs its last movement *)
									board=newboard[boardinit, a, piece],
									boardinit=board,
									checkForRowRemoval[a, piece],
									checkForGameOver[a],
									a=1,
									r=5,
									piece=RandomChoice[lpiece, 1][[1]]
								}
							]
						];
					)]
				}
			],
			TrackedSymbols :> {},
			UpdateInterval -> gameSpeed
		],(*slowing down the DynamicModule*)
		{
			"LeftArrowKeyDown" :> (moveCurrentPiece[piece, -1, a]),(*Moving the piece left*)
			"RightArrowKeyDown" :> (moveCurrentPiece[piece, 1, a]), (*Moving the piece right*)
			"UpArrowKeyDown" :> (piece = rotatePiece[piece])
		}
	]
]
