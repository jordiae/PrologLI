#file = sudoku
#file = nurses
file = banner6_def


$(file): $(file).pl
	swipl -O -q -g main --stand_alone=true -o $(file) -c $(file).pl
