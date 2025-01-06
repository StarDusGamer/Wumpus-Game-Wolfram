(* Declaring Variables *)
map = Table["0", {4}, {6}]
map[[1, 1]] = "P"

restrictedPositions = {{1, 1}, {1, 2}, {2, 1}}
pitXs = {}
pitYs = {}

While[Length[pitXs] < 4,
	pitX = RandomInteger[{1, 6}];
	If[MemberQ[pitXs, pitX], Continue[]];
	pitXs = Append[pitXs, pitX]
]

While[Length[pitYs] < 4,
	pitY = RandomInteger[{1, 4}];
	position = {pitXs[[Length[pitYs] + 1]], pitY};
	If[MemberQ[pitYs, pitY] || MemberQ[restrictedPositions, position],
		Continue[]
	];
	pitYs = Append[pitYs, pitY]
]

isValidSpawn[x_, y_] := Module[{},
	If[MemberQ[restrictedPositions, {x, y}], Return[False]];
	If[MemberQ[Transpose[{pitXs, pitYs}], {x, y}], Return[False]];
	Return[True]
]

spawnWumpus[] := Module[{x, y},
	While[True,
		x = RandomInteger[{1, 6}];
		y = RandomInteger[{1, 4}];
		If[isValidSpawn[x, y],
			Return[{x, y}]
		]
	]
]

wumpus = spawnWumpus[]

(* Declaring Functions *)
spaceDead[x_, y_] := Module[{},
	With[{index = Position[pitYs, y]},
		If[index =!= {} && pitXs[[index[[1, 1]]]] == x,
			Return[True]
		]
	];
	If[{x, y} == wumpus,
		Print["You Have Been Eaten!!!"];
		Return[True]
	];
	Return[False]
]

moveLeft[x_, y_] := Module[{},
	If[x-1 > 0,
		map[[y, x]] = "0";
		map[[y, x-1]] = "P";
		Return[True]
	];

	Print["Can't Move Left"];
	Return[False]
]

moveRight[x_, y_] := Module[{},
	If[x+1 <= 6,
		map[[y, x]] = "0";
		map[[y, x+1]] = "P";
		Return[True]
	];

	Print["Can't Move Right"];
	Return[False]
]

moveDown[x_, y_] := Module[{},
	If[y+1 <= 4,
		map[[y, x]] = "0";
		map[[y+1, x]] = "P";
		Return[True]
	];

	Print["Can't Move Down"];
	Return[False]
]

moveUp[x_, y_] := Module[{},
	If[y-1 > 0,
		map[[y, x]] = "0";
		map[[y-1, x]] = "P";
		Return[True]
	];

	Print["Can't Move Up"];
	Return[False]
]

hasBreeze[x_, y_] := Module[{i = 1, neighbors, validNeighbors, isBreeze = False},
	neighbors = {
		{x - 1, y},
		{x + 1, y},
		{x, y - 1},
		{x, y + 1}
	};
	validNeighbors = Select[neighbors, 
		#[[1]] > 0 && #[[1]] <= 6 && #[[2]] > 0 && #[[2]] <= 4 &
	];
	Do[
		With[{index = Position[pitYs, validNeighbors[[i, 2]]]},
			If[index =!= {} && pitXs[[index[[1, 1]]]] == validNeighbors[[i, 1]],
        			isBreeze = True;
        			Break[]      
			]
    		],
		{i, Length[validNeighbors]}
	];
	
	Print[isBreeze];
	Return[isBreeze]
]

shoot[x_, y_] := Module[{input},
	input = InputString["Choose a Direction to Shoot (l, r, u, d)\n"];
	Switch[input,
		"l", If[wumpus[[2]] == y && wumpus[[1]] < x, Return[True]],
		"r", If[wumpus[[2]] == y && wumpus[[1]] > x, Return[True]],
		"u", If[wumpus[[2]] < y && wumpus[[1]] == x, Return[True]],
		"d", If[wumpus[[2]] > y && wumpus[[1]] == x, Return[True]]
	];
	Return[False]
]

While[True,
	i = 1;
	Do[
		Print[Row[map[[i, All]]]],
		{i, 1, 4}
	];

	input = InputString["Choose a Direction (l, r, u, d)\nOr type s to shoot\n"];

	If[!MemberQ[{"l", "r", "u", "d", "s"}, input],
		Print["Not A Valid Input, Please Try Again"];
		Continue[]
	];

	position = Position[map, "P"][[1]];
	Print[position];

	Switch[input,
		"l", moveLeft[position[[2]], position[[1]]],
		"r", moveRight[position[[2]], position[[1]]],
		"u", moveUp[position[[2]], position[[1]]],
		"d", moveDown[position[[2]], position[[1]]],
		"s", If[shoot[position[[2]], position[[1]]],
			Print["THE WUMPUS HAS BEEN KILLED!\n"];
			Break[],
			Print["You Have Missed, THE WUMPUS ESCAPES :(\n"];
			Break[]
		]
	];
	
	position = Position[map, "P"][[1]];

	If[spaceDead[position[[2]], position[[1]]],
		Print["You Have Died\nTerminating...."];
		Break[]
	];

	If[hasBreeze[position[[2]], position[[1]]], Print["You Feel a Breeze"]]
]

map[[wumpus[[2]], wumpus[[1]]]] = "W"
Do[
	map[[pitYs[[i]], pitXs[[i]]]] = "_",
	{i, Length[pitXs]}
]
Do[
	Print[Row[map[[i, All]]]],
	{i, 1, 4}
]
