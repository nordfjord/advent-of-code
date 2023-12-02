defmodule Game do
  def empty() do
    %{red: 0, green: 0, blue: 0}
  end

  def concat(a, b) do
    %{red: a.red + b.red, green: a.green + b.green, blue: a.blue + b.blue}
  end

  def maximum(a, b) do
    %{red: max(a.red, b.red), green: max(a.green, b.green), blue: max(a.blue, b.blue)}
  end

  def power(x), do: x.red * x.green * x.blue

  def create(red, green, blue) do
    %{red: red, green: green, blue: blue}
  end

  def lte(a, b) do
    a.red <= b.red && a.green <= b.green && a.blue <= b.blue
  end
end

defmodule Part1 do
  def parse_ball(ball) do
    [num, color] = String.split(ball, " ")
    {String.to_integer(num), color}
  end

  defp to_colors([], acc), do: acc

  defp to_colors([{num, color} | balls], acc) do
    to_colors(
      balls,
      case color do
        "red" -> Game.concat(acc, Game.create(num, 0, 0))
        "green" -> Game.concat(acc, Game.create(0, num, 0))
        "blue" -> Game.concat(acc, Game.create(0, 0, num))
      end
    )
  end

  defp to_colors(balls), do: to_colors(balls, Game.empty())

  def parse_balls(balls) do
    balls
    |> String.split(", ")
    |> Enum.map(&parse_ball/1)
    |> to_colors()
  end

  def parse_games(games) do
    games
    |> String.split("; ")
    |> Enum.map(&parse_balls/1)
  end

  def parse_game(game) do
    game |> String.replace("Game ", "") |> String.to_integer()
  end

  def parse_line(line) do
    [game, games] = String.split(line, ": ")
    {parse_game(game), parse_games(games)}
  end

  def snd({_, x}), do: x
  def fst({x, _}), do: x

  def solve(lines) do
    max = %{red: 12, green: 13, blue: 14}

    lines
    |> Enum.map(&parse_line/1)
    |> Enum.filter(fn balls ->
      balls
      |> snd()
      |> Enum.all?(fn nums -> Game.lte(nums, max) end)
    end)
    |> Enum.map(&fst/1)
    |> Enum.sum()
  end
end

defmodule Part2 do
  defp find_max([], maxes), do: maxes

  defp find_max([nums | rest], maxes) do
    find_max(rest, Game.maximum(maxes, nums))
  end

  def find_max(list), do: find_max(list, Game.empty())

  def power(nums), do: nums.red * nums.green * nums.blue

  def solve(lines) do
    lines
    |> Enum.map(&Part1.parse_line/1)
    |> Enum.map(&Part1.snd/1)
    |> Enum.map(&find_max/1)
    |> Enum.map(&power/1)
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
