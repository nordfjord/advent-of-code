defmodule Part1 do
  defp solve([], sum), do: sum

  defp solve([{winning, mine} | cards], sum) do
    overlap = MapSet.intersection(winning, mine) |> MapSet.size()

    if overlap > 0 do
      solve(cards, sum + 2 ** (overlap - 1))
    else
      solve(cards, sum)
    end
  end

  def solve(cards) do
    solve(cards, 0)
  end
end

defmodule Part2 do
  defp solve([], _, _, sum), do: Map.values(sum) |> Enum.sum()

  defp solve([{winning, mine} | cards], i, len, sum) do
    overlap = MapSet.intersection(winning, mine) |> MapSet.size()
    sum = Map.update(sum, i, 1, &(&1 + 1))

    sum =
      if overlap > 0 do
        0..(overlap - 1)
        |> Enum.map(fn j -> i + 1 + j end)
        |> Enum.filter(fn j -> j < len end)
        |> Enum.reduce(sum, fn j, acc ->
          curr = Map.get(acc, i, 0)
          Map.update(acc, j, curr, &(&1 + curr))
        end)
      else
        sum
      end

    solve(cards, i + 1, len, sum)
  end

  def solve(lines), do: solve(lines, 0, Enum.count(lines), Map.new())
end

defmodule Parse do
  def parse_numbers(line) do
    line
    |> String.trim()
    |> String.split(~r/\s+/)
    |> Enum.map(&String.to_integer/1)
  end
end

# Read all lines from stdin
lines =
  IO.read(:stdio, :eof)
  |> String.split("\n")
  |> Enum.map(fn line ->
    [left, my_cards] = String.split(line, " | ")
    [_game_id, winning_cards] = String.split(left, ": ")

    my_cards = Parse.parse_numbers(my_cards)
    winning_cards = Parse.parse_numbers(winning_cards)

    {MapSet.new(winning_cards), MapSet.new(my_cards)}
  end)

IO.write("Part 1: ")
Part1.solve(lines) |> IO.inspect()

IO.write("Part 2: ")
Part2.solve(lines) |> IO.inspect()
