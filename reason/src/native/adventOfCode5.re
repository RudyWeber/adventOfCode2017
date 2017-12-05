let file = "./bin/day5input";

let ic = open_in(file);

let formatInput = (ic) => {
  let rec aux = () =>
    try {
      let number = input_line(ic);
      [number |> int_of_string, ...aux()]
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

let formattedInput = ic |> formatInput |> Array.of_list;

let formattedInputLength = Array.length(formattedInput);

let rec exit_1 = (index, steps, input) =>
  if (index < 0 || index >= formattedInputLength) {
    steps
  } else {
    let offset = input[index];
    input[index] = input[index] + 1;
    exit_1(index + offset, steps + 1, input)
  };

let rec exit_2 = (index, steps, input) =>
  if (index < 0 || index >= formattedInputLength) {
    steps
  } else {
    let offset = input[index];
    input[index] = input[index] + (offset >= 3 ? (-1) : 1);
    exit_2(index + offset, steps + 1, input)
  };

formattedInput |> exit_2(0, 0) |> string_of_int |> print_endline;