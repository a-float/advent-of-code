{ pkgs, lib, config, inputs, ... }:

{
  dotenv.enable = true;
  packages = [ pkgs.git ];
  languages.go.enable = true;

  scripts.add-day.exec = ''
    [[ $1 =~ ^[0-9]+$ ]] || { echo "Usage: add-day DAY_NUMBER" >&2; exit 1; }

    num=$(printf "%02d" "$1")
    data="data/day$num.txt"
    solution="days/day$num.go"

    if [[ ! -f "$data" ]]; then
        echo "Fetching data for day $1"
        curl -fsS --cookie "session=$AOC_SESSION" "https://adventofcode.com/2018/day/$1/input" -o "$data"
    else
        echo "Data for day $1 already present!"
    fi

    [[ -f "$solution" ]] || touch "$solution"
  '';
}
