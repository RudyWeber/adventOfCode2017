/* This code is ugly. You've been warned. */

type direction =
  | NORTH
  | SOUTH
  | EAST
  | WEST;

let input = 265149;

let ceiledSquareRoot = input |> float_of_int |> sqrt |> ceil |> int_of_float;

let dim = ceiledSquareRoot mod 2 === 0 ? ceiledSquareRoot + 1 : ceiledSquareRoot;

let matrix_part1 = Array.make_matrix(dim, dim, 0);

let matrix_part2 = Array.make_matrix(dim, dim, 0);

let origin = dim / 2;

let sumOfAdjacentValues = (matrix, x, y) => {
  let sum = ref(0);

  for (i in x - 1 to x + 1) {
    for (j in y - 1 to y + 1) {
      try {
        sum := sum^ + matrix[i][j];
      } {
        | e => ()
      };
    };
  };
  
  sum
};

let result = ref(0);

let fillMatrix = (dim, part2, matrix) => {
  let rec aux = (x, y, direction, l, second) => {
    if (x < 0 || x >= dim || y < 0 || y >= dim) {
      ()
    } else {
      switch direction {
      | NORTH =>
        let limit = y - l;
        for (j in 1 to l) {
          if (y - j < dim) {
            let value = part2 ? (sumOfAdjacentValues(matrix, x, y - j))^ : matrix[x][y] + j;
            result := result^ === 0 && value >= input ? value : result^;
            matrix[x][y - j] = value
          }
        };
        aux(x, limit, WEST, second ? l + 1 : l, ! second)
      | SOUTH =>
        let limit = y + l;
        for (j in 1 to l) {
          if (y + 1 >= 0) {
            let value = part2 ? (sumOfAdjacentValues(matrix, x, y + j))^ : matrix[x][y] + j;
            result := result^ === 0 && value >= input ? value : result^;
            matrix[x][y + j] = value
          }
        };
        aux(x, limit, EAST, second ? l + 1 : l, ! second)
      | EAST =>
        let limit = x + l;
        for (i in 1 to l) {
          if (x + i < dim) {
            let value = part2 ? (sumOfAdjacentValues(matrix, x + i, y))^ : matrix[x][y] + i;
            result := result^ === 0 && value >= input ? value : result^;
            matrix[x + i][y] = value
          }
        };
        aux(limit, y, NORTH, second ? l + 1 : l, ! second)
      | WEST =>
        let limit = x - l;
        for (i in 1 to l) {
          if (x - i >= 0) {
            let value = part2 ? (sumOfAdjacentValues(matrix, x - i, y))^ : matrix[x][y] + i;
            result := result^ === 0 && value >= input ? value : result^;
            matrix[x - i][y] = value
          }
        };
        aux(limit, y, SOUTH, second ? l + 1 : l, ! second)
      }
    };
  };
  matrix[origin][origin] = 1;
  aux(origin, origin, EAST, 1, false)
};

let manhattanDistance = (x1, y1, x2, y2) => abs(x1 - x2) + abs(y1 - y2);

let findDistance = (number, matrix) => {
  let dist = ref(0);
  for (i in 0 to Array.length(matrix) - 1) {
    for (j in 0 to Array.length(matrix[i]) - 1) {
      if (matrix[i][j] == number) {
        dist := manhattanDistance(origin, origin, i, j)
      }
    }
  };
  dist
};

matrix_part1 |> fillMatrix(dim, false);
 (matrix_part1 |> findDistance(265149))^ |> string_of_int |> print_endline;

 matrix_part2 |> fillMatrix(dim, true);
 
 result^ |> string_of_int |> print_endline;