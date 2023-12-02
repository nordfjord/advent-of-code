defmodule Part1 do
  def parse_line(line) do
    # Example: "Game 11: 3 blue, 4 red; 5 green, 6 red; 2 green"
    # Split on ":"
    [game, rest] = String.split(line, ": ")
    game = String.replace(game, "Game ", "") |> String.to_integer()

    nums =
      rest
      |> String.split("; ")
      |> Enum.map(fn x ->
        x
        |> String.split(", ")
        |> Enum.map(fn x -> x |> String.split(" ") end)
        |> Enum.reduce(%{red: 0, green: 0, blue: 0}, fn [num, color], acc ->
          case color do
            "red" -> %{acc | red: acc.red + String.to_integer(num)}
            "green" -> %{acc | green: acc.green + String.to_integer(num)}
            "blue" -> %{acc | blue: acc.blue + String.to_integer(num)}
          end
        end)
      end)

    {game, nums}
  end

  def solve(lines) do
    max = %{red: 12, green: 13, blue: 14}

    lines
    |> Enum.map(&parse_line/1)
    |> Enum.filter(fn {_game, balls} ->
      balls
      |> Enum.all?(fn nums ->
        nums.red <= max.red && nums.green <= max.green && nums.blue <= max.blue
      end)
    end)
    |> Enum.map(fn {game, _nums} ->
      game
    end)
    |> Enum.sum()
  end
end

defmodule Part2 do
  def solve(lines) do
    lines
    |> Enum.map(&Part1.parse_line/1)
    |> Enum.map(fn {_game, shown} ->
      maxes =
        shown
        |> Enum.reduce(%{red: 0, green: 0, blue: 0}, fn nums, acc ->
          %{
            red: max(acc.red, nums.red),
            green: max(acc.green, nums.green),
            blue: max(acc.blue, nums.blue)
          }
        end)

      maxes.red * maxes.green * maxes.blue
    end)
    |> Enum.sum()
  end
end

# Read all lines from stdin
lines =
  IO.read(:stdio, :eof)
  |> String.split("\n")

IO.write("Part 1: ")
Part1.solve(lines) |> IO.inspect()

IO.write("Part 2: ")
Part2.solve(lines) |> IO.inspect()
