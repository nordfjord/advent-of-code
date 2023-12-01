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

  def part1(file) do
    {:ok, file} = File.read(file)

    file
    |> String.split("\n")
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

  def parse_line(str) do
    parse_line_recursive(-1, -1, str |> String.to_charlist())
  end

  defp parse_line_recursive(first, last, []) do
    first * 10 + last
  end

  defp parse_line_recursive(-1, _last, [c | rest]) when ?0 <= c and c <= ?9 do
    n = c - ?0
    parse_line_recursive(n, n, rest)
  end

  defp parse_line_recursive(first, _last, [c | rest]) when ?0 <= c and c <= ?9 do
    n = c - ?0
    parse_line_recursive(first, n, rest)
  end

  defp parse_line_recursive(first, last, str) do
    [_ | rest] = str

    case {first, @mappings |> Enum.find(fn {k, _} -> starts_with(str, k) end)} do
      {_, nil} -> parse_line_recursive(first, last, rest)
      {-1, {_, v}} -> parse_line_recursive(v, v, rest)
      {first, {_, v}} -> parse_line_recursive(first, v, rest)
    end
  end

  def part2(file) do
    {:ok, file} = File.read(file)

    file
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> Enum.sum()
  end
end
