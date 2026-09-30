{ pkgs, lib, config, inputs, ... }:

{
  dotenv.enable = true;
  packages = [ pkgs.git ];
  languages.go.enable = true;

  scripts.add-day.exec = ''
    [[ $1 =~ ^[0-9]+$ ]] || { echo "Usage: add-day DAY_NUMBER" >&2; exit 1; }

    num=$(printf "%02d" "$1")
    data="src/day$num/input.txt"
    solution="src/day$num/main.go"

    mkdir -p src/day$num
    if [[ ! -f "$data" ]]; then
        echo "Fetching data for day $1"
        curl -fsS --cookie "session=$AOC_SESSION" "https://adventofcode.com/2018/day/$1/input" -o "$data"
    else
        echo "Data for day $1 already present!"
    fi

    if [[ ! -f "$solution" ]]; then
      cp main.go.template "$solution"
    else
      echo "Solution for day $1 already present!"
    fi
  '';
}
