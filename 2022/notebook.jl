### A Pluto.jl notebook ###
# v0.19.16

using Markdown
using InteractiveUtils

# ╔═╡ fb57906c-f966-4c91-a317-acf0a9124570
using PGFPlotsX

# ╔═╡ b5bca982-ac99-4cb5-aa7d-37c876508091
using Plots; pgfplotsx()

# ╔═╡ 97255b29-c5ed-4761-9e81-1a7e78953382
using Base.Iterators

# ╔═╡ f9aa330e-4a7f-4fec-87ac-c80a38bad5a3
using MLStyle

# ╔═╡ 91787e5e-09be-4658-a3ee-15ce4a03848a
using FunctionalCollections

# ╔═╡ d5b18c72-4f3c-4980-9f0b-7ae1b215938b
using DataFrames

# ╔═╡ 0b99a294-3b23-4941-922b-aedde02bbe52
md"""
# Advent of Code: 2022
"""

# ╔═╡ 5b472ec1-268c-4edd-be69-e66f4576362f
md"""
## Prelude
"""

# ╔═╡ 8f0ad058-2cfb-49e1-b0a4-c3bbef5d5412
md"""
### Imports
"""

# ╔═╡ 2661cbe1-4dff-4d86-9991-15fea3b0545d
md"""
### Utilities
"""

# ╔═╡ c6acdd00-cbe0-4765-af97-9ea872482237
data(day; parser=identity, reader=readlines) =
	[parser(line) for line in reader("inputs/$(day).in")]

# ╔═╡ 86344ca7-e4b6-432b-9600-d5b9b715ae36
struct Mismatch
	actual::Any
	expected::Any
end

# ╔═╡ 47f7e480-6ab8-4f54-aa69-27fa7d2cbbb3
struct Unchecked
	value::Any
end

# ╔═╡ c4bd968d-4b61-4acd-95aa-37400cdd120c
struct Match
	value::Any
end

# ╔═╡ 7194866c-d84e-4930-9b86-929b2ada03ae
test(actual, expected) = @match (actual, expected) begin
	(actual, nothing)								=> Unchecked(actual)
	(actual, expected) && if actual == expected end => Match(actual)
	(actual, expected) 								=> Mismatch(actual, expected)
end

# ╔═╡ 5b0bcc21-47ad-435d-9e3d-9504e1019b57
check(input, (part, expected)) = Dict(part => test(part(input), expected))

# ╔═╡ 67aa1198-045f-44e8-a020-67aacd3e2da6
check(input, part1, part2) = check(input, part1) ∪ check(input, part2)

# ╔═╡ 352d3b6f-f659-4cfb-a54d-97d3e9ef9ff7
@active Re{r :: Regex}(x) begin
    res = match(r, x)
    if res !== nothing
        Some(res)
    else
        nothing
    end
end

# ╔═╡ ec27b519-53a1-4816-b8a8-a8f111e6c641
md"""
## Day 1: Calorie Counting

<https://adventofcode.com/2022/day/1>
"""

# ╔═╡ e04313c5-2a5a-4a5c-8f6c-59df0a37b2a2
in1 = data(
	1, 
	reader = fname -> split(read(fname, String), "\n\n"), 
	parser = x -> map(y -> parse(Int, y), split(x))
)

# ╔═╡ e7f8e486-3da3-408e-b56a-b00f0c006b32
day1_1(food_per_elf) = maximum(sum(food) for food in food_per_elf)

# ╔═╡ 006e69ba-9d97-4733-b861-9007b9418c81
day1_2(food_per_elf) = 
	sum(take(sort([sum(food) for food in food_per_elf], rev=true), 3))

# ╔═╡ bde4c07c-202f-406f-bb7c-23673cb3daa8
check(in1, (day1_1, 69883), (day1_2, 207576))

# ╔═╡ 2f607612-736e-4fc0-be11-6e4bb93db8f9
md"""
## Day 2: Rock Paper Scissors

<https://adventofcode.com/2022/day/2>
"""

# ╔═╡ 6dae27af-1942-4a20-9fc0-009ef740120b
@data Shape begin
    Rock
    Paper
    Scissors
end

# ╔═╡ a012ed55-4e1a-4fc7-8841-3dcea12b7314
@data Result begin
	Win
	Lose
	Draw
end

# ╔═╡ 412480a2-db4c-4cee-985a-401a624d3e7d
shape(x) = @match x begin
	"A" || "X" => Rock
	"B" || "Y" => Paper
	"C" || "Z" => Scissors
end

# ╔═╡ e6d66b8e-1050-4fa3-a649-cc3eb6444996
result(x) = Dict("X" => Lose, "Y" => Draw, "Z" => Win)[x]

# ╔═╡ 4f727ffb-1905-4d0f-821a-d1b05315d24d
shape_precedence = Dict(Scissors => Rock, Paper => Scissors, Rock => Paper)

# ╔═╡ a156c598-ccbe-48dc-99a7-c15daad2fc99
wins_against(s::Shape) = shape_precedence[s]

# ╔═╡ 5fcaeed3-4efd-46a2-9463-8ae7680ed812
loses_against(s::Shape) = Dict(value => key for (key, value) in shape_precedence)[s]

# ╔═╡ e4d817e1-4f8a-442a-9e7f-452deb711d0e
shape_value(s::Shape) = Dict(Rock => 1, Paper => 2, Scissors => 3)[s]

# ╔═╡ 2d280950-b546-4e10-870d-2d54cf55dc8b
opposing_shape(s::Shape, result::Result) = @match (s, result) begin
	(x, Win)     => wins_against(x)
	(x, Lose)    => loses_against(x)
	(x, Draw)    => x
end

# ╔═╡ 183a5ac7-e66c-44d5-a42d-d5539b14b2b7
score_round(a::Shape, b::Shape) = 
	if loses_against(a) == b
		shape_value(b)
	elseif wins_against(a) == b
		shape_value(b) + 6
	else
		shape_value(b) + 3
	end

# ╔═╡ 6d5eb00c-fc9a-4977-b576-4451456b1ca8
in2 = data(2, parser = x -> split(x))

# ╔═╡ 2b543d00-d48e-4e1d-86b1-6d59d904ae0c
day2_1(rounds) = 
	sum(score_round(shape(a), shape(b)) 
		for (a, b) in rounds)

# ╔═╡ c1a2b2ad-2ad0-4a51-acee-ce81e9e1b8ba
day2_2(rounds) = 
	sum(score_round(shape(a), opposing_shape(shape(a), result(b))) 
		for (a, b) in rounds)

# ╔═╡ 3172a98f-41e4-45ea-ade6-4f0db8cf5f30
check(in2, (day2_1, 12855), (day2_2, 13726))

# ╔═╡ 9021f259-58cd-4162-9699-c11ea34f4020
md"""
## Day 3: Rucksack Reorganization

<https://adventofcode.com/2022/day/3>
"""

# ╔═╡ f6f10ad1-a205-4247-92b0-049e9615883f
in3 = data(3)

# ╔═╡ 7af6b9c9-f921-4d68-a591-e8d00b2ca85f
priority(item) = @match item begin
  'a':'z' => Int(item) - 96
  'A':'Z' => Int(item) - 38
end

# ╔═╡ 782cbf83-134c-4781-a3a9-044e45af41e0
day3_1(rucksacks) =
	sum(begin c1 = r[1:div(length(r), 2)]
		   	  c2 = r[div(length(r), 2)+1:length(r)] 
			priority(only(c1 ∩ c2))
		end for r in rucksacks)

# ╔═╡ 9e21d6a4-b088-4afc-bbec-a74f156b2268
day3_2(rucksacks) =
	sum(priority(only(a ∩ b ∩ c)) for (a, b, c) in partition(rucksacks, 3))

# ╔═╡ 4bce2a82-8ec8-456b-9de2-fa8eadc8aa1c
check(in3, (day3_1, 7727), (day3_2, 2609))

# ╔═╡ 0e1fde5a-74b7-44a0-a148-40bc07d99a4c
md"""
## Day 4: Camp Cleanup

<https://adventofcode.com/2022/day/4>
"""

# ╔═╡ 66002dbb-80bf-4fc2-92fc-0d6dd06529d6
in4 = data(
	4, 
	parser = line -> 
		partition(map(x -> parse(Int, x), match(r"(\d+)-(\d+),(\d+)-(\d+)", line)), 2)
)

# ╔═╡ 6edfe026-d046-4489-aa91-bb7d7c6f40a0
day4_1(pairs) = 
	sum(1 for ((a, b), (c, d)) in pairs if issubset(a:b, c:d) || issubset(c:d, a:b))

# ╔═╡ aee2c54b-3558-4d55-bd8d-67ea8eaa0713
day4_2(pairs) = 
	sum(1 for ((a, b), (c, d)) in pairs if !isdisjoint(a:b, c:d))

# ╔═╡ 29f52842-fb37-4e22-9e65-9058ca654bc8
check(in4, (day4_1, 459), (day4_2, Nothing))

# ╔═╡ 6bc2dce2-5202-499d-830b-a9685f0ac96d
md"""
## Day 5: Supply Stacks

<https://adventofcode.com/2022/day/5>
"""

# ╔═╡ c9012459-412d-4a8a-81b6-ba83f8a12e74
in5 = data(5)

# ╔═╡ bb4f206a-ab41-4316-907c-652caeafd9a8
struct StackMove
	quantity::Int
	from::Int
	to::Int
end

# ╔═╡ 9a5e2a75-e57b-4ff3-bdd7-ab920674ce94
function parse_stacks(input)
	matcher = line -> 
		map(x -> (x[1], div(x.offset - 1, 4) + 1), eachmatch(r"\[([A-Z])\]", line))
		
	stacks = Dict()
	for (value, index) in mapreduce(matcher, vcat, input)
		stacks[index] = append!(get(stacks, index, []), value)
	end
	stacks
end

# ╔═╡ 69673877-00ae-41ba-964a-0bd5f5da4f77
function parse_moves(input)
	matcher = function(line)
		match_group = match(r"move (\d+) from (\d+) to (\d+)", line)
		if !isnothing(match_group)
			StackMove(
				parse(Int, match_group[1]),
				parse(Int, match_group[2]),
				parse(Int, match_group[3])
			)
		end
	end
	
	filter(!isnothing, map(matcher, input))
end

# ╔═╡ 8fdba246-f0d1-4d20-97a6-81dccccbe7e3
function move_crates(stacks, from, to, quantity)
	from_stack = stacks[from]
	to_stack = stacks[to]

	merge(
		stacks,
		Dict(
			from => collect(drop(from_stack, quantity)),
			to => vcat(collect(take(from_stack, quantity)), to_stack)
		)
	)
end

# ╔═╡ 884c1bce-b34e-4ee8-bf74-24f848936fbd
function day5_1(input)
	stacks = reduce(
		function(stacks, move)
			updated_stacks = stacks
			for i in 1:move.quantity
				updated_stacks = move_crates(updated_stacks, move.from, move.to, 1)
			end
			updated_stacks
		end, 
		parse_moves(input); 
		init=parse_stacks(input)
	)

	join(first(stacks[i]) for i in 1:length(keys(stacks)))
end

# ╔═╡ bed6191b-e976-4dfb-80f6-d13a19055976
function day5_2(input)
	stacks = reduce(
		function(stacks, move)
			move_crates(stacks, move.from, move.to, move.quantity)
		end, 
		parse_moves(input); 
		init=parse_stacks(input)
	)

	join(first(stacks[i]) for i in 1:length(keys(stacks)))
end

# ╔═╡ 3e576b6c-70c1-4b13-83aa-a3a7d399d590
check(in5, (day5_1, "RNZLFZSJH"), (day5_2, "CNSFCGJSM"))

# ╔═╡ b22cdad7-aaca-41eb-84b9-b2e0a5fe5eb6
md"""
## Day 6: Tuning Trouble

<https://adventofcode.com/2022/day/6>
"""

# ╔═╡ 423eaaa6-b4ab-47ab-9a57-81207093f1a8
in6 = data(6, reader = fname -> read(fname, String))

# ╔═╡ 352e1139-811b-4cfb-9202-21c008065bfb
start_is_marker(str, marker_size) = 
	length(union(take(str, marker_size))) == marker_size

# ╔═╡ f486f9e7-75a2-4da8-bab7-46683730b8d3
find_first_marker(string, marker_size) = begin
	idx = marker_size
	while !start_is_marker(string, marker_size)
		string = drop(string, 1)
		idx += 1
	end
	idx
end

# ╔═╡ e62da71f-db6d-4e49-8bac-58ae2c48e937
day6_1(input) = find_first_marker(input, 4)

# ╔═╡ f6736f45-7fb8-46bd-8c13-300d9f760666
day6_2(input) = find_first_marker(input, 14)

# ╔═╡ 3b909b09-ad81-4908-a2e5-ba25d7ed7d60
check(in6, (day6_1, 1640), (day6_2, 3613))

# ╔═╡ 540ce384-9cf8-444a-9ac5-4160acf98911
md"""
## Day 7: No Space Left On Device

<https://adventofcode.com/2022/day/7>
"""

# ╔═╡ 86907fdb-47ec-40af-8d9f-904ecceeedda
begin
	@data CommandLine begin
		ChangeDirectory(String)
		ListDirectory()
		FileEntry(String, Int)
		DirectoryEntry(String)
	end
	
	@as_record ChangeDirectory
	@as_record ListDirectory
	@as_record FileEntry
	@as_record DirectoryEntry
end

# ╔═╡ 64b9d1fd-e15d-4bcf-82fe-25950dfa1055
parse_line(line) = @match line begin
	Re{r"\$ cd (.*)"}(m) => ChangeDirectory(m[1])
	Re{r"\$ ls"}(m)      => ListDirectory()
	Re{r"(\d+) (.*)"}(m) => FileEntry(m[2], parse(Int, m[1]))
    Re{r"dir (.*)"}(m)   => DirectoryEntry(m[1])
end


# ╔═╡ 65588be6-082b-4d19-a538-5b59e20e8aa4
in7 = data(7, parser=parse_line)

# ╔═╡ b4082bd6-ab2b-433a-92b9-5512fa1c41c1
empty_directory = Dict(:files => Set(), :size => 0)

# ╔═╡ 52e0cd46-4c68-49ce-b0a3-852e7e95c740
change_directory(working_directory, arg) = @match arg begin
	"/"   => "/"
	".."  => replace(working_directory, r"(?<=\/)[^\/]+\/$" => "")
	other => working_directory * other * "/"
end

# ╔═╡ 1beaf20b-6b9b-4391-bd9e-649de726bea6
md"""
Rather than a tree, the "filesystem" is represented as a flat dictionary, with directory paths as keys and directory info as values, e.g.:

```
{
  "/": {size: 0, files: ()}
  "/foo": {size: 100, files: ('baz.txt')}
  "/foo/bar": {size: 0, files: ()}
}
```

The `files` refers to the file entries in the directory itself, while `size` is their total size (non-recursive).
"""

# ╔═╡ 459fc841-2d51-43bc-9770-3701c58f54dd
parse_filesystem(command_lines) = reduce(
	function((cwd, fs), cmd_line)
		current_directory = get(fs, cwd, empty_directory)

		@match cmd_line begin
			ChangeDirectory(arg) =>
				let new_cwd = change_directory(cwd, arg)
					
					(
						new_cwd,
						merge(
							fs, 
							Dict(new_cwd => get(fs, new_cwd, empty_directory))
						)
					)
				end
			
			FileEntry(name, size) =>
				let new_files = current_directory[:files] ∪ [name],
					new_size = current_directory[:size] + size
					
					(
						cwd, 
						merge(
							fs, 
							Dict(cwd => Dict(:files => new_files, :size => new_size))
						)
					)
				end
			
			_ => (cwd, fs)
		end
	end,
	command_lines;
	init=("/", Dict())
)

# ╔═╡ c76e090f-66d0-4757-8ddf-67de4c4d84e4
md"""
The `directory_size` function will calculate the recursive size of a directory. That is, the total size of all files under the directory tree, and not just immediate descendants. This works by taking the sum of all `sizes` for keys starting with the
directory name (e.g. `/` or `/foo/`).
"""

# ╔═╡ 936fa9f0-df05-4558-902a-790a38b1ce03
directory_size(fs, search_path) =
	sum(fs[path][:size] for path in keys(fs) if startswith(path, search_path))

# ╔═╡ 84972b80-c4c0-44a1-b9d9-c9ce50f48d08
day7_1(command_lines) = 
	let (_, fs) = parse_filesystem(command_lines)		
		sum(directory_size(fs, dir) for dir in keys(fs) 
				if directory_size(fs, dir) <= 100_000)
	end

# ╔═╡ 254e5f4d-4ce8-4863-aa70-c7c0eb5c0549
day7_2(command_lines) =
	let (_, fs) = parse_filesystem(command_lines),
		space_needed = 30_000_000,
	    space_unused = 70_000_000 - directory_size(fs, "/")
		space_to_free = space_needed - space_unused

		minimum(directory_size(fs, dir) for dir in keys(fs) 
					if directory_size(fs, dir) >= space_to_free)
	end

# ╔═╡ f3a557d8-2e27-4311-aa7b-279f74d1823f
check(in7, (day7_1, 1367870), (day7_2, 549173))

# ╔═╡ cd6636bc-90fe-48a2-bc43-8fa32320e8a6
md"""
## Day 8: Treetop Tree House

<https://adventofcode.com/2022/day/8>

We can first convert the input into a NxM matrix for easy access to columns:
"""

# ╔═╡ 61d3be91-8768-4aeb-934c-762b8f866d25
in8 = hcat(data(8, parser=line -> [parse(Int, ch) for ch in line])...)'

# ╔═╡ 44cc07f6-fd74-43b2-a48f-0f7a0f8a6027
testin8 = hcat(data("8.test", parser=line -> [parse(Int, ch) for ch in line])...)'

# ╔═╡ 68642794-8a02-487b-a66a-2df520b88860
perspectives(matrix, (y, x); view=:outer) =
	let row = matrix[y, :], col = matrix[:, x]
		[
			view === :inner ? reverse(row[1:x-1]) : row[1:x-1],
			row[x+1:end],
			view === :inner ? reverse(col[1:y-1]) : col[1:y-1],
			col[y+1:end]
		]
	end

# ╔═╡ c4d59b37-943b-4d78-a32d-b340c7e1ed46
md"""
A tree is visible if "all of the other trees between it and an edge of the grid are shorter than it."

We can define a function that tests if a tree is directly on the perimeter, or if the trees in front of it—when viewed from west/east/north/south—are all shorter.
"""

# ╔═╡ ac5165db-6b1c-417d-bf13-f16a563486e3
tree_is_visible(matrix, (y, x)) =
	let tree_height = matrix[y, x],
		shorter = t -> t < tree_height,
		(n, m) = size(matrix),
		on_perimeter = y == 1 || x == 1 || y == n || x == m

		on_perimeter || any(p -> all(shorter, p), perspectives(matrix, (y, x)))
	end

# ╔═╡ 280c6399-766e-4d0f-b4b0-04b798091c46
md"""
The following visualizes the tree visibility for the entire matrix. `1`s are visible trees and `0`s are not visible. 
"""

# ╔═╡ 2eafa736-de41-4273-aa14-c6943633ac8e
[tree_is_visible(in8, i) for i in Tuple.(CartesianIndices(in8))]

# ╔═╡ b377aaee-d8b6-44eb-a3b3-90077e42fb6c
scenic_score(matrix, (y, x)) =
	let tree_height = matrix[y, x],
		shorter = t -> t < tree_height

		reduce(
			*, 
			[min(length(collect(takewhile(shorter, p))) + 1, length(p)) 
				for p in perspectives(matrix, (y, x); view=:inner)]
		)
	end

# ╔═╡ 86cc8843-2933-4e94-9464-0cdce10364d6
day8_1(matrix) =
	sum(1 for i in Tuple.(CartesianIndices(matrix)) if tree_is_visible(matrix, i))

# ╔═╡ 427a9574-3429-4c65-9d0c-6c5cbcc8fcb1
day8_2(matrix) =
	maximum(scenic_score(matrix, i) for i in Tuple.(CartesianIndices(matrix)))

# ╔═╡ 5142136c-371e-43d3-bb0d-9ddc2bde3e19
check(in8, (day8_1, 1782), (day8_2, nothing))

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
FunctionalCollections = "de31a74c-ac4f-5751-b3fd-e18cd04993ca"
MLStyle = "d8e11817-5142-5d16-987a-aa16d5891078"
PGFPlotsX = "8314cec4-20b6-5062-9cdb-752b83310925"
Plots = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"

[compat]
DataFrames = "~1.4.4"
FunctionalCollections = "~0.5.0"
MLStyle = "~0.4.16"
PGFPlotsX = "~1.5.1"
Plots = "~1.36.6"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.8.3"
manifest_format = "2.0"
project_hash = "f2588a532196c3f04cc4cc6d36eec8c28b51e00a"

[[deps.ArgCheck]]
git-tree-sha1 = "a3a402a35a2f7e0b87828ccabbd5ebfbebe356b4"
uuid = "dce04be8-c92d-5529-be00-80e4d2c0e197"
version = "2.3.0"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.1"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[deps.BitFlags]]
git-tree-sha1 = "43b1a4a8f797c1cddadf60499a8a077d4af2cd2d"
uuid = "d1d4a3ce-64b1-5f1a-9ba4-7e7e69966f35"
version = "0.1.7"

[[deps.Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "19a35467a82e236ff51bc17a3a44b69ef35185a2"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.8+0"

[[deps.Cairo_jll]]
deps = ["Artifacts", "Bzip2_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "4b859a208b2397a7a623a03449e4636bdb17bcf2"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.16.1+1"

[[deps.ChainRulesCore]]
deps = ["Compat", "LinearAlgebra", "SparseArrays"]
git-tree-sha1 = "e7ff6cadf743c098e08fca25c91103ee4303c9bb"
uuid = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
version = "1.15.6"

[[deps.ChangesOfVariables]]
deps = ["ChainRulesCore", "LinearAlgebra", "Test"]
git-tree-sha1 = "38f7a08f19d8810338d4f5085211c7dfa5d5bdd8"
uuid = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
version = "0.1.4"

[[deps.CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "ded953804d019afa9a3f98981d99b33e3db7b6da"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.0"

[[deps.ColorSchemes]]
deps = ["ColorTypes", "ColorVectorSpace", "Colors", "FixedPointNumbers", "Random", "SnoopPrecompile"]
git-tree-sha1 = "aa3edc8f8dea6cbfa176ee12f7c2fc82f0608ed3"
uuid = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
version = "3.20.0"

[[deps.ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "eb7f0f8307f71fac7c606984ea5fb2817275d6e4"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.4"

[[deps.ColorVectorSpace]]
deps = ["ColorTypes", "FixedPointNumbers", "LinearAlgebra", "SpecialFunctions", "Statistics", "TensorCore"]
git-tree-sha1 = "d08c20eef1f2cbc6e60fd3612ac4340b89fea322"
uuid = "c3611d14-8923-5661-9e6a-0046d554d3a4"
version = "0.9.9"

[[deps.Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "417b0ed7b8b838aa6ca0a87aadf1bb9eb111ce40"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.12.8"

[[deps.Compat]]
deps = ["Dates", "LinearAlgebra", "UUIDs"]
git-tree-sha1 = "00a2cccc7f098ff3b66806862d275ca3db9e6e5a"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "4.5.0"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "0.5.2+0"

[[deps.Contour]]
git-tree-sha1 = "d05d9e7b7aedff4e5b51a029dced05cfb6125781"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.6.2"

[[deps.Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[deps.DataAPI]]
git-tree-sha1 = "e08915633fcb3ea83bf9d6126292e5bc5c739922"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.13.0"

[[deps.DataFrames]]
deps = ["Compat", "DataAPI", "Future", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrettyTables", "Printf", "REPL", "Random", "Reexport", "SnoopPrecompile", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "d4f69885afa5e6149d0cab3818491565cf41446d"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.4.4"

[[deps.DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "d1fff3a548102f48987a52a2e0d114fa97d730f0"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.13"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[deps.DefaultApplication]]
deps = ["InteractiveUtils"]
git-tree-sha1 = "c0dfa5a35710a193d83f03124356eef3386688fc"
uuid = "3f0dd361-4fe0-5fc6-8523-80b14ec94d85"
version = "1.1.0"

[[deps.DelimitedFiles]]
deps = ["Mmap"]
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"

[[deps.DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "c36550cb29cbe373e95b3f40486b9a4148f89ffd"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.9.2"

[[deps.Downloads]]
deps = ["ArgTools", "FileWatching", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"
version = "1.6.0"

[[deps.Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "bad72f730e9e91c08d9427d5e8db95478a3c323d"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.4.8+0"

[[deps.FFMPEG]]
deps = ["FFMPEG_jll"]
git-tree-sha1 = "b57e3acbe22f8484b4b5ff66a7499717fe1a9cc8"
uuid = "c87230d0-a227-11e9-1b43-d7ebe4e7570a"
version = "0.4.1"

[[deps.FFMPEG_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "PCRE2_jll", "Pkg", "Zlib_jll", "libaom_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "74faea50c1d007c85837327f6775bea60b5492dd"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "4.4.2+2"

[[deps.FileWatching]]
uuid = "7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"

[[deps.FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "335bfdceacc84c5cdf16aadc768aa5ddfc5383cc"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.4"

[[deps.Fontconfig_jll]]
deps = ["Artifacts", "Bzip2_jll", "Expat_jll", "FreeType2_jll", "JLLWrappers", "Libdl", "Libuuid_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "21efd19106a55620a188615da6d3d06cd7f6ee03"
uuid = "a3f928ae-7b40-5064-980b-68af3947d34b"
version = "2.13.93+0"

[[deps.Formatting]]
deps = ["Printf"]
git-tree-sha1 = "8339d61043228fdd3eb658d86c926cb282ae72a8"
uuid = "59287772-0a20-5a39-b81b-1366585eb4c0"
version = "0.4.2"

[[deps.FreeType2_jll]]
deps = ["Artifacts", "Bzip2_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "87eb71354d8ec1a96d4a7636bd57a7347dde3ef9"
uuid = "d7e528f0-a631-5988-bf34-fe36492bcfd7"
version = "2.10.4+0"

[[deps.FriBidi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "aa31987c2ba8704e23c6c8ba8a4f769d5d7e4f91"
uuid = "559328eb-81f9-559d-9380-de523a88c83c"
version = "1.0.10+0"

[[deps.FunctionalCollections]]
deps = ["Test"]
git-tree-sha1 = "04cb9cfaa6ba5311973994fe3496ddec19b6292a"
uuid = "de31a74c-ac4f-5751-b3fd-e18cd04993ca"
version = "0.5.0"

[[deps.Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

[[deps.GLFW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libglvnd_jll", "Pkg", "Xorg_libXcursor_jll", "Xorg_libXi_jll", "Xorg_libXinerama_jll", "Xorg_libXrandr_jll"]
git-tree-sha1 = "d972031d28c8c8d9d7b41a536ad7bb0c2579caca"
uuid = "0656b61e-2033-5cc2-a64a-77c0f6c09b89"
version = "3.3.8+0"

[[deps.GR]]
deps = ["Artifacts", "Base64", "DelimitedFiles", "Downloads", "GR_jll", "HTTP", "JSON", "Libdl", "LinearAlgebra", "Pkg", "Preferences", "Printf", "Random", "Serialization", "Sockets", "TOML", "Tar", "Test", "UUIDs", "p7zip_jll"]
git-tree-sha1 = "051072ff2accc6e0e87b708ddee39b18aa04a0bc"
uuid = "28b8d3ca-fb5f-59d9-8090-bfdbd6d07a71"
version = "0.71.1"

[[deps.GR_jll]]
deps = ["Artifacts", "Bzip2_jll", "Cairo_jll", "FFMPEG_jll", "Fontconfig_jll", "GLFW_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libtiff_jll", "Pixman_jll", "Pkg", "Qt5Base_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "501a4bf76fd679e7fcd678725d5072177392e756"
uuid = "d2c73de3-f751-5644-a686-071e5b155ba9"
version = "0.71.1+0"

[[deps.Gettext_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "9b02998aba7bf074d14de89f9d37ca24a1a0b046"
uuid = "78b55507-aeef-58d4-861c-77aaff3498b1"
version = "0.21.0+0"

[[deps.Glib_jll]]
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE2_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "d3b3624125c1474292d0d8ed0f65554ac37ddb23"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.74.0+2"

[[deps.Graphite2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "344bf40dcab1073aca04aa0df4fb092f920e4011"
uuid = "3b182d85-2403-5c21-9c21-1e1f0cc25472"
version = "1.3.14+0"

[[deps.Grisu]]
git-tree-sha1 = "53bb909d1151e57e2484c3d1b53e19552b887fb2"
uuid = "42e2da0e-8278-4e71-bc24-59509adca0fe"
version = "1.0.2"

[[deps.HTTP]]
deps = ["Base64", "CodecZlib", "Dates", "IniFile", "Logging", "LoggingExtras", "MbedTLS", "NetworkOptions", "OpenSSL", "Random", "SimpleBufferStream", "Sockets", "URIs", "UUIDs"]
git-tree-sha1 = "e1acc37ed078d99a714ed8376446f92a5535ca65"
uuid = "cd3eb016-35fb-5094-929b-558a96fad6f3"
version = "1.5.5"

[[deps.HarfBuzz_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "Graphite2_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg"]
git-tree-sha1 = "129acf094d168394e80ee1dc4bc06ec835e510a3"
uuid = "2e76f6c2-a576-52d4-95c1-20adfe4de566"
version = "2.8.1+1"

[[deps.IniFile]]
git-tree-sha1 = "f550e6e32074c939295eb5ea6de31849ac2c9625"
uuid = "83e8ac13-25f8-5344-8a64-a9f2b223428f"
version = "0.5.1"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[deps.InverseFunctions]]
deps = ["Test"]
git-tree-sha1 = "49510dfcb407e572524ba94aeae2fced1f3feb0f"
uuid = "3587e190-3f89-42d0-90ee-14403ec27112"
version = "0.1.8"

[[deps.InvertedIndices]]
git-tree-sha1 = "82aec7a3dd64f4d9584659dc0b62ef7db2ef3e19"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.2.0"

[[deps.IrrationalConstants]]
git-tree-sha1 = "7fd44fd4ff43fc60815f8e764c0f352b83c49151"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.1.1"

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[deps.JLFzf]]
deps = ["Pipe", "REPL", "Random", "fzf_jll"]
git-tree-sha1 = "f377670cda23b6b7c1c0b3893e37451c5c1a2185"
uuid = "1019f520-868f-41f5-a6de-eb00f4b6a39c"
version = "0.1.5"

[[deps.JLLWrappers]]
deps = ["Preferences"]
git-tree-sha1 = "abc9885a7ca2052a736a600f7fa66209f96506e1"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.4.1"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "3c837543ddb02250ef42f4738347454f95079d4e"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.3"

[[deps.JpegTurbo_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b53380851c6e6664204efb2e62cd24fa5c47e4ba"
uuid = "aacddb02-875f-59d6-b918-886e6ef4fbf8"
version = "2.1.2+0"

[[deps.LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "f6250b16881adf048549549fba48b1161acdac8c"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.1+0"

[[deps.LERC_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "bf36f528eec6634efc60d7ec062008f171071434"
uuid = "88015f11-f218-50d7-93a8-a6af411a945d"
version = "3.0.0+1"

[[deps.LZO_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e5b909bcf985c5e2605737d2ce278ed791b89be6"
uuid = "dd4b983a-f0e5-5f8d-a1b7-129d4a5fb1ac"
version = "2.10.1+0"

[[deps.LaTeXStrings]]
git-tree-sha1 = "f2355693d6778a178ade15952b7ac47a4ff97996"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.3.0"

[[deps.Latexify]]
deps = ["Formatting", "InteractiveUtils", "LaTeXStrings", "MacroTools", "Markdown", "OrderedCollections", "Printf", "Requires"]
git-tree-sha1 = "ab9aa169d2160129beb241cb2750ca499b4e90e9"
uuid = "23fbe1c1-3f47-55db-b15f-69d7ec21a316"
version = "0.15.17"

[[deps.LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"
version = "0.6.3"

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"
version = "7.84.0+0"

[[deps.LibGit2]]
deps = ["Base64", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"
version = "1.10.2+0"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"

[[deps.Libffi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "0b4a5d71f3e5200a7dff793393e09dfc2d874290"
uuid = "e9f186c6-92d2-5b65-8a66-fee21dc1b490"
version = "3.2.2+1"

[[deps.Libgcrypt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgpg_error_jll", "Pkg"]
git-tree-sha1 = "64613c82a59c120435c067c2b809fc61cf5166ae"
uuid = "d4300ac3-e22c-5743-9152-c294e39db1e4"
version = "1.8.7+0"

[[deps.Libglvnd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll", "Xorg_libXext_jll"]
git-tree-sha1 = "6f73d1dd803986947b2c750138528a999a6c7733"
uuid = "7e76a0d4-f3c7-5321-8279-8d96eeed0f29"
version = "1.6.0+0"

[[deps.Libgpg_error_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "c333716e46366857753e273ce6a69ee0945a6db9"
uuid = "7add5ba3-2f88-524e-9cd5-f83b8a55f7b8"
version = "1.42.0+0"

[[deps.Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "c7cb1f5d892775ba13767a87c7ada0b980ea0a71"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.16.1+2"

[[deps.Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "9c30530bf0effd46e15e0fdcf2b8636e78cbbd73"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.35.0+0"

[[deps.Libtiff_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "LERC_jll", "Libdl", "Pkg", "Zlib_jll", "Zstd_jll"]
git-tree-sha1 = "3eb79b0ca5764d4799c06699573fd8f533259713"
uuid = "89763e89-9b03-5906-acba-b20f662cd828"
version = "4.4.0+0"

[[deps.Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "7f3efec06033682db852f8b3bc3c1d2b0a0ab066"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.36.0+0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[deps.LogExpFunctions]]
deps = ["ChainRulesCore", "ChangesOfVariables", "DocStringExtensions", "InverseFunctions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "946607f84feb96220f480e0422d3484c49c00239"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.19"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[deps.LoggingExtras]]
deps = ["Dates", "Logging"]
git-tree-sha1 = "cedb76b37bc5a6c702ade66be44f831fa23c681e"
uuid = "e6f89c97-d47a-5376-807f-9c37f3926c36"
version = "1.0.0"

[[deps.MLStyle]]
git-tree-sha1 = "060ef7956fef2dc06b0e63b294f7dbfbcbdc7ea2"
uuid = "d8e11817-5142-5d16-987a-aa16d5891078"
version = "0.4.16"

[[deps.MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "42324d08725e200c23d4dfb549e0d5d89dede2d2"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.10"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[deps.MbedTLS]]
deps = ["Dates", "MbedTLS_jll", "MozillaCACerts_jll", "Random", "Sockets"]
git-tree-sha1 = "03a9b9718f5682ecb107ac9f7308991db4ce395b"
uuid = "739be429-bea8-5141-9913-cc70e7f3736d"
version = "1.1.7"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"
version = "2.28.0+0"

[[deps.Measures]]
git-tree-sha1 = "c13304c81eec1ed3af7fc20e75fb6b26092a1102"
uuid = "442fdcdd-2543-5da2-b0f3-8c86c306513e"
version = "0.3.2"

[[deps.Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "bf210ce90b6c9eed32d25dbcae1ebc565df2687f"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.0.2"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"
version = "2022.2.1"

[[deps.NaNMath]]
deps = ["OpenLibm_jll"]
git-tree-sha1 = "a7c3d1da1189a1c2fe843a3bfa04d18d20eb3211"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "1.0.1"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.20+0"

[[deps.OpenLibm_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "05823500-19ac-5b8b-9628-191a04bc5112"
version = "0.8.1+0"

[[deps.OpenSSL]]
deps = ["BitFlags", "Dates", "MozillaCACerts_jll", "OpenSSL_jll", "Sockets"]
git-tree-sha1 = "df6830e37943c7aaa10023471ca47fb3065cc3c4"
uuid = "4d8831e6-92b7-49fb-bdf8-b643e874388c"
version = "1.3.2"

[[deps.OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "f6e9dba33f9f2c44e08a020b0caf6903be540004"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "1.1.19+0"

[[deps.OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "13652491f6856acfd2db29360e1bbcd4565d04f1"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.5+0"

[[deps.Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "51a08fb14ec28da2ec7a927c4337e4332c2a4720"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.2+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "85f8e6578bf1f9ee0d11e7bb1b1456435479d47c"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.4.1"

[[deps.PCRE2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "efcefdf7-47ab-520b-bdef-62a2eaa19f15"
version = "10.40.0+0"

[[deps.PGFPlotsX]]
deps = ["ArgCheck", "DataStructures", "Dates", "DefaultApplication", "DocStringExtensions", "MacroTools", "Parameters", "Requires", "Tables"]
git-tree-sha1 = "1d3729f2cd114a8150ce134f697d07f9ef2b9657"
uuid = "8314cec4-20b6-5062-9cdb-752b83310925"
version = "1.5.1"

[[deps.Parameters]]
deps = ["OrderedCollections", "UnPack"]
git-tree-sha1 = "34c0e9ad262e5f7fc75b10a9952ca7692cfc5fbe"
uuid = "d96e819e-fc66-5662-9728-84c9c7592b0a"
version = "0.12.3"

[[deps.Parsers]]
deps = ["Dates", "SnoopPrecompile"]
git-tree-sha1 = "b64719e8b4504983c7fca6cc9db3ebc8acc2a4d6"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.5.1"

[[deps.Pipe]]
git-tree-sha1 = "6842804e7867b115ca9de748a0cf6b364523c16d"
uuid = "b98c9c47-44ae-5843-9183-064241ee97a0"
version = "1.3.0"

[[deps.Pixman_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b4f5d02549a10e20780a24fce72bea96b6329e29"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.40.1+0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"
version = "1.8.0"

[[deps.PlotThemes]]
deps = ["PlotUtils", "Statistics"]
git-tree-sha1 = "1f03a2d339f42dca4a4da149c7e15e9b896ad899"
uuid = "ccf2f8ad-2431-5c83-bf29-c5338b663b6a"
version = "3.1.0"

[[deps.PlotUtils]]
deps = ["ColorSchemes", "Colors", "Dates", "Printf", "Random", "Reexport", "SnoopPrecompile", "Statistics"]
git-tree-sha1 = "21303256d239f6b484977314674aef4bb1fe4420"
uuid = "995b91a9-d308-5afd-9ec6-746e21dbc043"
version = "1.3.1"

[[deps.Plots]]
deps = ["Base64", "Contour", "Dates", "Downloads", "FFMPEG", "FixedPointNumbers", "GR", "JLFzf", "JSON", "LaTeXStrings", "Latexify", "LinearAlgebra", "Measures", "NaNMath", "Pkg", "PlotThemes", "PlotUtils", "Printf", "REPL", "Random", "RecipesBase", "RecipesPipeline", "Reexport", "RelocatableFolders", "Requires", "Scratch", "Showoff", "SnoopPrecompile", "SparseArrays", "Statistics", "StatsBase", "UUIDs", "UnicodeFun", "Unzip"]
git-tree-sha1 = "6a9521b955b816aa500462951aa67f3e4467248a"
uuid = "91a5bcdd-55d7-5caf-9e0b-520d859cae80"
version = "1.36.6"

[[deps.PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "a6062fe4063cdafe78f4a0a81cfffb89721b30e7"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.2"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "47e5f437cc0e7ef2ce8406ce1e7e24d44915f88d"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.3.0"

[[deps.PrettyTables]]
deps = ["Crayons", "Formatting", "LaTeXStrings", "Markdown", "Reexport", "StringManipulation", "Tables"]
git-tree-sha1 = "96f6db03ab535bdb901300f88335257b0018689d"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "2.2.2"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[deps.Qt5Base_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Fontconfig_jll", "Glib_jll", "JLLWrappers", "Libdl", "Libglvnd_jll", "OpenSSL_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libxcb_jll", "Xorg_xcb_util_image_jll", "Xorg_xcb_util_keysyms_jll", "Xorg_xcb_util_renderutil_jll", "Xorg_xcb_util_wm_jll", "Zlib_jll", "xkbcommon_jll"]
git-tree-sha1 = "0c03844e2231e12fda4d0086fd7cbe4098ee8dc5"
uuid = "ea2cea3b-5b76-57ae-a6ef-0a8af62496e1"
version = "5.15.3+2"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[deps.Random]]
deps = ["SHA", "Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[deps.RecipesBase]]
deps = ["SnoopPrecompile"]
git-tree-sha1 = "18c35ed630d7229c5584b945641a73ca83fb5213"
uuid = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
version = "1.3.2"

[[deps.RecipesPipeline]]
deps = ["Dates", "NaNMath", "PlotUtils", "RecipesBase", "SnoopPrecompile"]
git-tree-sha1 = "e974477be88cb5e3040009f3767611bc6357846f"
uuid = "01d81517-befc-4cb6-b9ec-a95719d0359c"
version = "0.6.11"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.RelocatableFolders]]
deps = ["SHA", "Scratch"]
git-tree-sha1 = "90bc7a7c96410424509e4263e277e43250c05691"
uuid = "05181044-ff0b-4ac5-8273-598c1e38db00"
version = "1.0.0"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "838a3a4188e2ded87a4f9f184b4b0d78a1e91cb7"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.0"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.Scratch]]
deps = ["Dates"]
git-tree-sha1 = "f94f779c94e58bf9ea243e77a37e16d9de9126bd"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.1.1"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[deps.Showoff]]
deps = ["Dates", "Grisu"]
git-tree-sha1 = "91eddf657aca81df9ae6ceb20b959ae5653ad1de"
uuid = "992d4aef-0814-514b-bc4d-f2e9a6c4116f"
version = "1.0.3"

[[deps.SimpleBufferStream]]
git-tree-sha1 = "874e8867b33a00e784c8a7e4b60afe9e037b74e1"
uuid = "777ac1f9-54b0-4bf8-805c-2214025038e7"
version = "1.1.0"

[[deps.SnoopPrecompile]]
git-tree-sha1 = "f604441450a3c0569830946e5b33b78c928e1a85"
uuid = "66db9d55-30c0-4569-8b51-7e840670fc0c"
version = "1.0.1"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[deps.SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "a4ada03f999bd01b3a25dcaa30b2d929fe537e00"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.1.0"

[[deps.SparseArrays]]
deps = ["LinearAlgebra", "Random"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"

[[deps.SpecialFunctions]]
deps = ["ChainRulesCore", "IrrationalConstants", "LogExpFunctions", "OpenLibm_jll", "OpenSpecFun_jll"]
git-tree-sha1 = "d75bda01f8c31ebb72df80a46c88b25d1c79c56d"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "2.1.7"

[[deps.Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[deps.StatsAPI]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "f9af7f195fb13589dd2e2d57fdb401717d2eb1f6"
uuid = "82ae8749-77ed-4fe6-ae5f-f523153014b0"
version = "1.5.0"

[[deps.StatsBase]]
deps = ["DataAPI", "DataStructures", "LinearAlgebra", "LogExpFunctions", "Missings", "Printf", "Random", "SortingAlgorithms", "SparseArrays", "Statistics", "StatsAPI"]
git-tree-sha1 = "d1bf48bfcc554a3761a133fe3a9bb01488e06916"
uuid = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
version = "0.33.21"

[[deps.StringManipulation]]
git-tree-sha1 = "46da2434b41f41ac3594ee9816ce5541c6096123"
uuid = "892a3eda-7b42-436c-8928-eab12a02cf0e"
version = "0.3.0"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"
version = "1.0.0"

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[deps.Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "OrderedCollections", "TableTraits", "Test"]
git-tree-sha1 = "c79322d36826aa2f4fd8ecfa96ddb47b174ac78d"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.10.0"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"
version = "1.10.1"

[[deps.TensorCore]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "1feb45f88d133a655e001435632f019a9a1bcdb6"
uuid = "62fd8b95-f654-4bbd-a8a5-9c27f68ccd50"
version = "0.1.1"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[deps.TranscodingStreams]]
deps = ["Random", "Test"]
git-tree-sha1 = "8a75929dcd3c38611db2f8d08546decb514fcadf"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.9.9"

[[deps.URIs]]
git-tree-sha1 = "ac00576f90d8a259f2c9d823e91d1de3fd44d348"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.4.1"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[deps.UnPack]]
git-tree-sha1 = "387c1f73762231e86e0c9c5443ce3b4a0a9a0c2b"
uuid = "3a884ed6-31ef-47d7-9d2a-63182c4928ed"
version = "1.0.2"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[deps.UnicodeFun]]
deps = ["REPL"]
git-tree-sha1 = "53915e50200959667e78a92a418594b428dffddf"
uuid = "1cfade01-22cf-5700-b092-accc4b62d6e1"
version = "0.4.1"

[[deps.Unzip]]
git-tree-sha1 = "ca0969166a028236229f63514992fc073799bb78"
uuid = "41fe7b60-77ed-43a1-b4f0-825fd5a5650d"
version = "0.2.0"

[[deps.Wayland_jll]]
deps = ["Artifacts", "Expat_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "3e61f0b86f90dacb0bc0e73a0c5a83f6a8636e23"
uuid = "a2964d1f-97da-50d4-b82a-358c7fce9d89"
version = "1.19.0+0"

[[deps.Wayland_protocols_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4528479aa01ee1b3b4cd0e6faef0e04cf16466da"
uuid = "2381bf8a-dfd0-557d-9999-79630e7b1b91"
version = "1.25.0+0"

[[deps.XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "58443b63fb7e465a8a7210828c91c08b92132dff"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.9.14+0"

[[deps.XSLT_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgcrypt_jll", "Libgpg_error_jll", "Libiconv_jll", "Pkg", "XML2_jll", "Zlib_jll"]
git-tree-sha1 = "91844873c4085240b95e795f692c4cec4d805f8a"
uuid = "aed1982a-8fda-507f-9586-7b0439959a61"
version = "1.1.34+0"

[[deps.Xorg_libX11_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll", "Xorg_xtrans_jll"]
git-tree-sha1 = "5be649d550f3f4b95308bf0183b82e2582876527"
uuid = "4f6342f7-b3d2-589e-9d20-edeb45f2b2bc"
version = "1.6.9+4"

[[deps.Xorg_libXau_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4e490d5c960c314f33885790ed410ff3a94ce67e"
uuid = "0c0b7dd1-d40b-584c-a123-a41640f87eec"
version = "1.0.9+4"

[[deps.Xorg_libXcursor_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXfixes_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "12e0eb3bc634fa2080c1c37fccf56f7c22989afd"
uuid = "935fb764-8cf2-53bf-bb30-45bb1f8bf724"
version = "1.2.0+4"

[[deps.Xorg_libXdmcp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fe47bd2247248125c428978740e18a681372dd4"
uuid = "a3789734-cfe1-5b06-b2d0-1dd0d9d62d05"
version = "1.1.3+4"

[[deps.Xorg_libXext_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "b7c0aa8c376b31e4852b360222848637f481f8c3"
uuid = "1082639a-0dae-5f34-9b06-72781eeb8cb3"
version = "1.3.4+4"

[[deps.Xorg_libXfixes_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "0e0dc7431e7a0587559f9294aeec269471c991a4"
uuid = "d091e8ba-531a-589c-9de9-94069b037ed8"
version = "5.0.3+4"

[[deps.Xorg_libXi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll", "Xorg_libXfixes_jll"]
git-tree-sha1 = "89b52bc2160aadc84d707093930ef0bffa641246"
uuid = "a51aa0fd-4e3c-5386-b890-e753decda492"
version = "1.7.10+4"

[[deps.Xorg_libXinerama_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll"]
git-tree-sha1 = "26be8b1c342929259317d8b9f7b53bf2bb73b123"
uuid = "d1454406-59df-5ea1-beac-c340f2130bc3"
version = "1.1.4+4"

[[deps.Xorg_libXrandr_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "34cea83cb726fb58f325887bf0612c6b3fb17631"
uuid = "ec84b674-ba8e-5d96-8ba1-2a689ba10484"
version = "1.5.2+4"

[[deps.Xorg_libXrender_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "19560f30fd49f4d4efbe7002a1037f8c43d43b96"
uuid = "ea2f1a96-1ddc-540d-b46f-429655e07cfa"
version = "0.9.10+4"

[[deps.Xorg_libpthread_stubs_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "6783737e45d3c59a4a4c4091f5f88cdcf0908cbb"
uuid = "14d82f49-176c-5ed1-bb49-ad3f5cbd8c74"
version = "0.1.0+3"

[[deps.Xorg_libxcb_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "XSLT_jll", "Xorg_libXau_jll", "Xorg_libXdmcp_jll", "Xorg_libpthread_stubs_jll"]
git-tree-sha1 = "daf17f441228e7a3833846cd048892861cff16d6"
uuid = "c7cfdc94-dc32-55de-ac96-5a1b8d977c5b"
version = "1.13.0+3"

[[deps.Xorg_libxkbfile_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "926af861744212db0eb001d9e40b5d16292080b2"
uuid = "cc61e674-0454-545c-8b26-ed2c68acab7a"
version = "1.1.0+4"

[[deps.Xorg_xcb_util_image_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "0fab0a40349ba1cba2c1da699243396ff8e94b97"
uuid = "12413925-8142-5f55-bb0e-6d7ca50bb09b"
version = "0.4.0+1"

[[deps.Xorg_xcb_util_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll"]
git-tree-sha1 = "e7fd7b2881fa2eaa72717420894d3938177862d1"
uuid = "2def613f-5ad1-5310-b15b-b15d46f528f5"
version = "0.4.0+1"

[[deps.Xorg_xcb_util_keysyms_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "d1151e2c45a544f32441a567d1690e701ec89b00"
uuid = "975044d2-76e6-5fbe-bf08-97ce7c6574c7"
version = "0.4.0+1"

[[deps.Xorg_xcb_util_renderutil_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "dfd7a8f38d4613b6a575253b3174dd991ca6183e"
uuid = "0d47668e-0667-5a69-a72c-f761630bfb7e"
version = "0.3.9+1"

[[deps.Xorg_xcb_util_wm_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xcb_util_jll"]
git-tree-sha1 = "e78d10aab01a4a154142c5006ed44fd9e8e31b67"
uuid = "c22f9ab0-d5fe-5066-847c-f4bb1cd4e361"
version = "0.4.1+1"

[[deps.Xorg_xkbcomp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxkbfile_jll"]
git-tree-sha1 = "4bcbf660f6c2e714f87e960a171b119d06ee163b"
uuid = "35661453-b289-5fab-8a00-3d9160c6a3a4"
version = "1.4.2+4"

[[deps.Xorg_xkeyboard_config_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_xkbcomp_jll"]
git-tree-sha1 = "5c8424f8a67c3f2209646d4425f3d415fee5931d"
uuid = "33bec58e-1273-512f-9401-5d533626f822"
version = "2.27.0+4"

[[deps.Xorg_xtrans_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "79c31e7844f6ecf779705fbc12146eb190b7d845"
uuid = "c5fb5394-a638-5e4d-96e5-b29de1b5cf10"
version = "1.4.0+3"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"
version = "1.2.12+3"

[[deps.Zstd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e45044cd873ded54b6a5bac0eb5c971392cf1927"
uuid = "3161d3a3-bdf6-5164-811a-617609db77b4"
version = "1.5.2+0"

[[deps.fzf_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "868e669ccb12ba16eaf50cb2957ee2ff61261c56"
uuid = "214eeab7-80f7-51ab-84ad-2988db7cef09"
version = "0.29.0+0"

[[deps.libaom_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "3a2ea60308f0996d26f1e5354e10c24e9ef905d4"
uuid = "a4ae2306-e953-59d6-aa16-d00cac43593b"
version = "3.4.0+0"

[[deps.libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "5982a94fcba20f02f42ace44b9894ee2b140fe47"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.1+0"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl", "OpenBLAS_jll"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.1.1+0"

[[deps.libfdk_aac_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "daacc84a041563f965be61859a36e17c4e4fcd55"
uuid = "f638f0a6-7fb0-5443-88ba-1cc74229b280"
version = "2.0.2+0"

[[deps.libpng_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "94d180a6d2b5e55e447e2d27a29ed04fe79eb30c"
uuid = "b53b4c65-9356-5827-b1ea-8c7a1a84506f"
version = "1.6.38+0"

[[deps.libvorbis_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Ogg_jll", "Pkg"]
git-tree-sha1 = "b910cb81ef3fe6e78bf6acee440bda86fd6ae00c"
uuid = "f27f6e37-5d2b-51aa-960f-b287f2bc3b7a"
version = "1.3.7+1"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"
version = "1.48.0+0"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
version = "17.4.0+0"

[[deps.x264_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fea590b89e6ec504593146bf8b988b2c00922b2"
uuid = "1270edf5-f2f9-52d2-97e9-ab00b5d0237a"
version = "2021.5.5+0"

[[deps.x265_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ee567a171cce03570d77ad3a43e90218e38937a9"
uuid = "dfaa095f-4041-5dcd-9319-2fabd8486b76"
version = "3.5.0+0"

[[deps.xkbcommon_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Wayland_jll", "Wayland_protocols_jll", "Xorg_libxcb_jll", "Xorg_xkeyboard_config_jll"]
git-tree-sha1 = "9ebfc140cc56e8c2156a15ceac2f0302e327ac0a"
uuid = "d8fb68d0-12a3-5cfd-a85a-d49703b185fd"
version = "1.4.1+0"
"""

# ╔═╡ Cell order:
# ╟─0b99a294-3b23-4941-922b-aedde02bbe52
# ╟─5b472ec1-268c-4edd-be69-e66f4576362f
# ╟─8f0ad058-2cfb-49e1-b0a4-c3bbef5d5412
# ╠═fb57906c-f966-4c91-a317-acf0a9124570
# ╠═b5bca982-ac99-4cb5-aa7d-37c876508091
# ╠═97255b29-c5ed-4761-9e81-1a7e78953382
# ╠═f9aa330e-4a7f-4fec-87ac-c80a38bad5a3
# ╠═91787e5e-09be-4658-a3ee-15ce4a03848a
# ╠═d5b18c72-4f3c-4980-9f0b-7ae1b215938b
# ╟─2661cbe1-4dff-4d86-9991-15fea3b0545d
# ╠═c6acdd00-cbe0-4765-af97-9ea872482237
# ╠═86344ca7-e4b6-432b-9600-d5b9b715ae36
# ╠═47f7e480-6ab8-4f54-aa69-27fa7d2cbbb3
# ╠═c4bd968d-4b61-4acd-95aa-37400cdd120c
# ╠═7194866c-d84e-4930-9b86-929b2ada03ae
# ╠═5b0bcc21-47ad-435d-9e3d-9504e1019b57
# ╠═67aa1198-045f-44e8-a020-67aacd3e2da6
# ╠═352d3b6f-f659-4cfb-a54d-97d3e9ef9ff7
# ╟─ec27b519-53a1-4816-b8a8-a8f111e6c641
# ╠═e04313c5-2a5a-4a5c-8f6c-59df0a37b2a2
# ╠═e7f8e486-3da3-408e-b56a-b00f0c006b32
# ╠═006e69ba-9d97-4733-b861-9007b9418c81
# ╠═bde4c07c-202f-406f-bb7c-23673cb3daa8
# ╟─2f607612-736e-4fc0-be11-6e4bb93db8f9
# ╠═6dae27af-1942-4a20-9fc0-009ef740120b
# ╠═a012ed55-4e1a-4fc7-8841-3dcea12b7314
# ╠═412480a2-db4c-4cee-985a-401a624d3e7d
# ╠═e6d66b8e-1050-4fa3-a649-cc3eb6444996
# ╠═4f727ffb-1905-4d0f-821a-d1b05315d24d
# ╠═a156c598-ccbe-48dc-99a7-c15daad2fc99
# ╠═5fcaeed3-4efd-46a2-9463-8ae7680ed812
# ╠═e4d817e1-4f8a-442a-9e7f-452deb711d0e
# ╠═2d280950-b546-4e10-870d-2d54cf55dc8b
# ╠═183a5ac7-e66c-44d5-a42d-d5539b14b2b7
# ╠═6d5eb00c-fc9a-4977-b576-4451456b1ca8
# ╠═2b543d00-d48e-4e1d-86b1-6d59d904ae0c
# ╠═c1a2b2ad-2ad0-4a51-acee-ce81e9e1b8ba
# ╠═3172a98f-41e4-45ea-ade6-4f0db8cf5f30
# ╟─9021f259-58cd-4162-9699-c11ea34f4020
# ╠═f6f10ad1-a205-4247-92b0-049e9615883f
# ╠═7af6b9c9-f921-4d68-a591-e8d00b2ca85f
# ╠═782cbf83-134c-4781-a3a9-044e45af41e0
# ╠═9e21d6a4-b088-4afc-bbec-a74f156b2268
# ╠═4bce2a82-8ec8-456b-9de2-fa8eadc8aa1c
# ╟─0e1fde5a-74b7-44a0-a148-40bc07d99a4c
# ╠═66002dbb-80bf-4fc2-92fc-0d6dd06529d6
# ╠═6edfe026-d046-4489-aa91-bb7d7c6f40a0
# ╠═aee2c54b-3558-4d55-bd8d-67ea8eaa0713
# ╠═29f52842-fb37-4e22-9e65-9058ca654bc8
# ╠═6bc2dce2-5202-499d-830b-a9685f0ac96d
# ╠═c9012459-412d-4a8a-81b6-ba83f8a12e74
# ╠═bb4f206a-ab41-4316-907c-652caeafd9a8
# ╠═9a5e2a75-e57b-4ff3-bdd7-ab920674ce94
# ╠═69673877-00ae-41ba-964a-0bd5f5da4f77
# ╠═8fdba246-f0d1-4d20-97a6-81dccccbe7e3
# ╠═884c1bce-b34e-4ee8-bf74-24f848936fbd
# ╠═bed6191b-e976-4dfb-80f6-d13a19055976
# ╠═3e576b6c-70c1-4b13-83aa-a3a7d399d590
# ╠═b22cdad7-aaca-41eb-84b9-b2e0a5fe5eb6
# ╠═423eaaa6-b4ab-47ab-9a57-81207093f1a8
# ╠═352e1139-811b-4cfb-9202-21c008065bfb
# ╠═f486f9e7-75a2-4da8-bab7-46683730b8d3
# ╠═e62da71f-db6d-4e49-8bac-58ae2c48e937
# ╠═f6736f45-7fb8-46bd-8c13-300d9f760666
# ╠═3b909b09-ad81-4908-a2e5-ba25d7ed7d60
# ╟─540ce384-9cf8-444a-9ac5-4160acf98911
# ╠═86907fdb-47ec-40af-8d9f-904ecceeedda
# ╠═64b9d1fd-e15d-4bcf-82fe-25950dfa1055
# ╠═65588be6-082b-4d19-a538-5b59e20e8aa4
# ╠═b4082bd6-ab2b-433a-92b9-5512fa1c41c1
# ╠═52e0cd46-4c68-49ce-b0a3-852e7e95c740
# ╟─1beaf20b-6b9b-4391-bd9e-649de726bea6
# ╠═459fc841-2d51-43bc-9770-3701c58f54dd
# ╟─c76e090f-66d0-4757-8ddf-67de4c4d84e4
# ╠═936fa9f0-df05-4558-902a-790a38b1ce03
# ╠═84972b80-c4c0-44a1-b9d9-c9ce50f48d08
# ╠═254e5f4d-4ce8-4863-aa70-c7c0eb5c0549
# ╠═f3a557d8-2e27-4311-aa7b-279f74d1823f
# ╟─cd6636bc-90fe-48a2-bc43-8fa32320e8a6
# ╠═61d3be91-8768-4aeb-934c-762b8f866d25
# ╠═44cc07f6-fd74-43b2-a48f-0f7a0f8a6027
# ╠═68642794-8a02-487b-a66a-2df520b88860
# ╟─c4d59b37-943b-4d78-a32d-b340c7e1ed46
# ╠═ac5165db-6b1c-417d-bf13-f16a563486e3
# ╟─280c6399-766e-4d0f-b4b0-04b798091c46
# ╠═2eafa736-de41-4273-aa14-c6943633ac8e
# ╠═b377aaee-d8b6-44eb-a3b3-90077e42fb6c
# ╠═86cc8843-2933-4e94-9464-0cdce10364d6
# ╠═427a9574-3429-4c65-9d0c-6c5cbcc8fcb1
# ╠═5142136c-371e-43d3-bb0d-9ddc2bde3e19
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
