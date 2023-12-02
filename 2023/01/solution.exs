defmodule Day1.Main do
  defp parse_char(c, {-1, _last}) when ?0 <= c and c <= ?9 do
    n = c - ?0
    {n, n}
  end

  defp parse_char(c, {first, _}) when ?0 <= c and c <= ?9 do
    n = c - ?0
    {first, n}
  end

  defp parse_char(_, {first, last}), do: {first, last}

  def part1(lines) do
    lines
    |> Enum.map(fn str ->
      {first, last} =
        str
        |> String.to_charlist()
        |> Enum.reduce({-1, -1}, &parse_char/2)

      first * 10 + last
    end)
    |> Enum.sum()
  end

  @mappings %{
    ~c"one" => 1,
    ~c"two" => 2,
    ~c"three" => 3,
    ~c"four" => 4,
    ~c"five" => 5,
    ~c"six" => 6,
    ~c"seven" => 7,
    ~c"eight" => 8,
    ~c"nine" => 9
  }

  defp starts_with(_, []), do: true
  defp starts_with([], _), do: false
  defp starts_with([h1 | t1], [h2 | t2]) when h1 == h2, do: starts_with(t1, t2)
  defp starts_with(_, _), do: false

  defp parse_line(first, last, []) do
    first * 10 + last
  end

  defp parse_line(-1, _last, [c | rest]) when ?0 <= c and c <= ?9 do
    n = c - ?0
    parse_line(n, n, rest)
  end

  defp parse_line(first, _last, [c | rest]) when ?0 <= c and c <= ?9 do
    n = c - ?0
    parse_line(first, n, rest)
  end

  defp parse_line(first, last, [_ | rest]) do
    word = @mappings |> Enum.find(fn {k, _} -> k end)

    case {first, word} do
      {_, nil} -> parse_line(first, last, rest)
      {-1, {_, v}} -> parse_line(v, v, rest)
      {first, {_, v}} -> parse_line(first, v, rest)
    end
  end

  def parse_line(str) do
    parse_line(-1, -1, str |> String.to_charlist())
  end

  def part2(lines) do
    lines
    |> Enum.map(&parse_line/1)
    |> Enum.sum()
  end
end

lines = IO.read(:stdio, :eof) |> String.split("\n")

IO.write("Part 1:")
lines |> Day1.Main.part1() |> IO.inspect()

IO.write("Part 2:")
lines |> Day1.Main.part2() |> IO.inspect()
