let file = "./bin/day2input";

let ic = open_in(file);

let formatInput = (ic) => {
  let rec aux = () =>
    try {
      let line = input_line(ic);
      let splitedLine = Str.split(Str.regexp("\t"), line) |> List.map((n) => int_of_string(n));
      [splitedLine, ...aux()]
    } {
    | e =>
      switch e {
      | End_of_file =>
        close_in(ic);
        []
      | e =>
        close_in_noerr(ic);
        raise(e)
      }
    };
  aux()
};

let formattedInput = formatInput(ic);

let maxList = (list) =>
  list |> List.fold_left((max, current) => current > max ? current : max, min_int);

let minList = (list) =>
  list |> List.fold_left((min, current) => current < min ? current : min, max_int);

let adventDay2_1 = () =>
  formattedInput |> List.fold_left((sum, line) => sum + maxList(line) - minList(line), 0);

let getDivider = (n, list) => list |> List.find((divider) => n mod divider == 0);

let adventDay2_2 = () => {
  let sorted = formattedInput |> List.map((line) => line |> List.sort((a, b) => a > b ? (-1) : 1));
  sorted
  |> List.fold_left(
       (sum, numbers) => {
         let rec aux = (numbers) =>
           switch numbers {
           | [current, ...rest] =>
             try {
               let divider = getDivider(current, rest);
               sum + current / divider
             } {
             | Not_found => aux(rest)
             }
           | [] => raise(Not_found)
           };
         aux(numbers)
       },
       0
     )
};

adventDay2_1() |> string_of_int |> print_endline;

adventDay2_2() |> string_of_int |> print_endline;