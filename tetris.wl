(* ::Package:: *)

(* Welcome to Mathematica Tetris Project !
This package creates a simple playable Tetris game
Just click "Run Package" and see magic appear ! *)

(* This function is not really useful and will be removed in future versions *)
ClearAll;

(* Initialisation *)
boardinit=Table[0, 20, 10]; (* board empty *)
board=boardinit; (* board in the game *)

(* All the possible pieces of the game *)
yellowPiece={{1, 1},{1, 1}} /. 1 -> Yellow;
redPiece={{1, 1, 0},{0, 1, 1}} /. 1 -> Red;
greenPiece={{1, 0}, {1, 1}, {0, 1}} /. 1 -> Green;
cyanPiece={{1},{1},{1},{1}} /. 1 -> Cyan;
bluePiece={{1,1},{1,0},{1,0}} /. 1 -> Blue;
orangePiece={{1,0},{1,0},{1,1}} /. 1 -> Orange;
purplePiece={{1,0},{1,1},{1,0}} /. 1 -> Purple;

(* Here is a list (2 dimens array) of all the pieces *)
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
	(* Returns all the location where we have to check for collision
	This method can be recalled if needed, in order to update the locations *)
	collisionCheckLocations[]:=(
		rList = {};
		Table[Table[
			If[piece[[1, i, j]] == 0 && i =!= 1, AppendTo[rList, {i, j}]; If[MemberQ[rList, {i - 1, j}], rList = Complement[rList, {i - 1, j}]]];
			If[a + i < 20 && i == Length[piece[[1]]] && piece[[1, i, j]] =!= 0, AppendTo[rList, {i + 1, j}]],
		{j, 1, Length[piece[[1, i]]]}], {i, 1, Length[piece[[1]]]}];
		Return[rList]
	);
	
	collisionsChecks = collisionCheckLocations[];
	hasCollision = False;
	Table[
		If[
			!hasCollision && a + collisionsChecks[[i, 1]] > 0 &&
			board[[a + collisionsChecks[[i, 1]], r - 1 + collisionsChecks[[i, 2]]]] =!= 0,
			hasCollision = True; Return[]
		],
	{i, 1, Length[collisionsChecks]}];
	
	Return[!hasCollision]

)
(* Rotates a piece *)
rotatePiece[piece_] := Rotate[piece, angle]

(* Checks if the current piece can move in the provided direction.
The direction can be symbolised with every number, positive or negative *)
moveCurrentPiece[piece_, direction_]:=(
	If[r > 1 && Sign[direction] == -1, r--];
	(* r is dicreased of 1 because the location is count two times in r and Length[piece[[...]]] *)
	If[r - 1 + Length[piece[[1, 1]]] < Length[board[[1]]] && Sign[direction] == 1, r++]
)


(* Representations *)

board=boardinit;
r=5; (*Placing the piece in the middle of the board at the beginning*)
angle=0; 


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
					If[a == 20 - piece[[2,1]] + 1,
						{
							board=newboard[boardinit,a,piece],
							boardinit=board,
							a=1,
							r=5
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
								board=newboard[boardinit, 1, piece],
								a++;
								board=newboard[boardinit, a, piece],
								boardinit=board,
								a=1,
								r=5,
								piece=RandomChoice[lpiece, 1][[1]]
							}
						]
					];
				}
			],
			TrackedSymbols :> {},
			UpdateInterval -> 0.3
		],(*slowing down the DynamicModule*)
		{
			"LeftArrowKeyDown" :> (moveCurrentPiece[piece, -1]),(*Moving the piece left*)
			"RightArrowKeyDown" :> (moveCurrentPiece[piece, 1])(*Moving the piece right*)
		}
	]
]
