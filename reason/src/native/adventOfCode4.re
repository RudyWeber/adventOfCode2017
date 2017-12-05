let file = "./bin/day4input";

let ic = open_in(file);

let formatInput = (ic) => {
  let rec aux = () =>
    try {
      let line = input_line(ic);
      [line |> Str.split(Str.regexp(" ")), ...aux()]
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

let formattedInput = ic |> formatInput;

let areTheSame = (s1, s2) => s1 == s2;

let areAnagrams = (s1, s2) =>
  if (String.length(s1) != String.length(s2)) {
    false
  } else {
    let length = String.length(s1);
    let dict = Hashtbl.create(String.length(s1));
    for (i in 0 to length - 1) {
      try {
        let nbOcc = Hashtbl.find(dict, s1.[i]);
        Hashtbl.replace(dict, s1.[i], nbOcc + 1)
      } {
      | _ => Hashtbl.add(dict, s1.[i], 1)
      };
      try {
        let nbOcc = Hashtbl.find(dict, s2.[i]);
        Hashtbl.replace(dict, s2.[i], nbOcc - 1)
      } {
      | _ => Hashtbl.add(dict, s2.[i], (-1))
      }
    };
    Hashtbl.fold((_, value, sum) => sum && value == 0, dict, true)
  };

let rec isValid = (predicate, input) =>
  switch input {
  | [first, ...rest] =>
    rest
    |> List.fold_left((found, word) => found && ! predicate(first, word), true)
    && isValid(predicate, rest)
  | [] => true
  };

let noDups = (input) => isValid(areTheSame, input);

let noAnagrams = (input) => isValid(areAnagrams, input);

let countNoDupsStrings = (input) =>
  input |> List.fold_left((sum, line) => sum + (noDups(line) ? 1 : 0), 0);

let countNoAnagramsStrings = (input) =>
  input |> List.fold_left((sum, line) => sum + (noAnagrams(line) ? 1 : 0), 0);

formattedInput |> countNoDupsStrings |> string_of_int |> print_endline;

formattedInput |> countNoAnagramsStrings |> string_of_int |> print_endline;