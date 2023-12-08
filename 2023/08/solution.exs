defmodule Parse do
  def read_stdin() do
    IO.read(:stdio, :eof)
  end

  def parse() do
    str = read_stdin()
    [directions, rest] = str |> String.split("\n\n")

    graph =
      Enum.reduce(
        String.split(rest, "\n"),
        %{},
        fn line, graph ->
          [src, lr] = String.split(line, " = ")

          [l, r] =
            lr |> String.replace("(", "") |> String.replace(")", "") |> String.split(", ")

          Map.put(graph, src, {l, r})
        end
      )

    {directions, graph}
  end
end

{directions, graph} = Parse.parse()

defmodule Part1 do
  def solve(directions, graph, curr, i) do
    idx = rem(i, String.length(directions))
    dir = String.at(directions, idx)
    {l, r} = graph[curr]

    next =
      case dir do
        "L" -> l
        "R" -> r
      end

    case next do
      "ZZZ" -> i + 1
      _ -> solve(directions, graph, next, i + 1)
    end
  end
end

defmodule Part2 do
  def find_cycle(directions, graph, curr, i, visited) do
    dir = String.at(directions, rem(i, String.length(directions)))
    {l, r} = graph[curr]

    next =
      case dir do
        "L" -> l
        "R" -> r
        nil -> raise "nil direction"
      end

    case visited do
      {j, c} when c == next ->
        j

      _ ->
        if String.ends_with?(next, "Z") do
          find_cycle(directions, graph, next, i + 1, {i + 1, next})
        else
          find_cycle(directions, graph, next, i + 1, visited)
        end
    end
  end

  defp gcd(a, b) when b == 0, do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))
  def lcm(a, b), do: (a * b) |> Integer.floor_div(gcd(a, b))

  def lcm(list) when is_list(list) do
    Enum.reduce(list, 1, &lcm(&1, &2))
  end

  def solve(directions, graph) do
    starting_points =
      graph |> Map.keys() |> Enum.filter(&String.ends_with?(&1, "A"))

    starting_points
    |> Enum.map(&find_cycle(directions, graph, &1, 0, nil))
    |> lcm()
  end
end

IO.write("Part 1: ")
IO.inspect(Part1.solve(directions, graph, "AAA", 0))

IO.write("Part 2: ")
IO.inspect(Part2.solve(directions, graph))
