### A Pluto.jl notebook ###
# v0.17.2

using Markdown
using InteractiveUtils

# ╔═╡ 253b7667-3988-4dda-8508-817edf4622e4
using PlutoUI

# ╔═╡ 91d3429b-bd55-4d9e-92e2-af61a2668ac5
using Base.Iterators

# ╔═╡ 2f6aafec-5dc9-4cfb-bdc3-2dca328c0e0f
using MLStyle

# ╔═╡ 00b5e199-f69a-4a1d-81f4-61102358e9f5
using Combinatorics

# ╔═╡ 254b7a7d-d446-49f1-9b00-14acfb16561e
md"
# Day 0: Utilities
"

# ╔═╡ 5261b35f-2c2d-4c3c-abe6-e7fae6c23dca
data(day; parser=identity, reader=readlines) =
	[parser(line) for line in reader("inputs/$(day).in")]

# ╔═╡ ed0fb9d9-641f-4f31-92a6-4408a14494a4
find(predicate::Function, xs) = begin
	idx = findfirst(predicate, xs)
	idx !== nothing ? xs[idx] : nothing
end

# ╔═╡ 35f19bf0-c23c-47b1-affa-466fed4c9486
@active Capture{r :: Regex}(x) begin
    res = match(r, x)
    res !== nothing ? Some(res[1]) : nothing
end

# ╔═╡ 4f47b6cc-0672-4ba8-bcc5-cfb96d3cc6b1
struct Mismatch
	actual::Any
	expected::Any
end

# ╔═╡ 90574e72-1941-4991-b2d0-d36d6685a864
struct Unchecked
	value::Any
end

# ╔═╡ 7596ba1a-e442-4cf8-8bbd-a026cfbaa2a6
struct Match
	value::Any
end

# ╔═╡ dc8abb99-0b0d-4fb4-8c11-54e7d873cad6
test(actual, expected) = @match (actual, expected) begin
	(actual, nothing)								=> Unchecked(actual)
	(actual, expected) && if actual == expected end => Match(actual)
	(actual, expected) 								=> Mismatch(actual, expected)
end

# ╔═╡ 93de06ee-1e88-4225-8be7-8e9cc1c2bd1b
check(input, (part, expected)) = Dict(part => test(part(input), expected))

# ╔═╡ 50024a0f-bcf7-4a80-b3f5-236d922d7d04
check(input, part1, part2) = check(input, part1) ∪ check(input, part2)

# ╔═╡ c10a3946-c73c-11eb-03e0-1ffe27ff778b
md"""
# Day 1: Report Repair

<https://adventofcode.com/2020/day/1>

## Part 1

Find the two entries in the input list that sum to 2020. Multiply these together.

## Part 2

Find the three entries in the input list that sum to 2020. Multiply these together.
"""

# ╔═╡ 2816220f-0e6c-43ae-8dff-eb66bcefe7e3
in1 = data(1, parser=x -> parse(Int, x))

# ╔═╡ 27e188f7-7a16-41b0-9576-cc7c0bfde7a4
day1_1(input) = begin
 	(a, b), _ = [(x, y) for x in input, y in input if x + y == 2020]
	a * b
end

# ╔═╡ 9980bf61-28f3-451c-b3e1-c35c41d4fdfa
day1_2(input) = begin
 	(a, b, c), _ = 
		[(x, y, z) for x in input, y in input, z in input if x + y + z == 2020]
	a * b * c
end

# ╔═╡ 55a4a291-5d76-4215-81b9-e7a6f4a68b3d
check(in1, (day1_1, 840324), (day1_2, 170098110))

# ╔═╡ 4eb09210-5516-470f-ac9b-5f9d116c4311
md"""
# Day 2: Password Philosophy

<https://adventofcode.com/2020/day/2>

## Part 1

Given an input line containing a password policy and password, determine if the password is valid.

```
1-3 a: abcde      # valid, contains 1 instance of a
1-3 b: cdefg      # invalid, contains 0 instances of b
2-9 c: ccccccccc  # valid, contains 9 instances of c
```

Count the number of valid passwords in the input file.

## Part 2

Policy is now interpreted differently. `1-3 a` means `a` should either be in position `1` xor position `3`.

```
1-3 a: abcde      # is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg      # is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc  # is invalid: both position 2 and position 9 contain c.
```

Count the number of valid passwords in the input file.
"""

# ╔═╡ b7b39cee-68f1-4618-a3b6-0e5c77bb966e
struct PasswordPolicy
	ch::Char
	range::UnitRange{Int}
end

# ╔═╡ b5c74108-f007-4f8b-b4b4-45c4e5ac0e89
parse_password_policy(line) = let
	i, j, ch, password = match(r"(\d+)-(\d+) (\w): (\w+)", strip(line)).captures
	(PasswordPolicy(only(ch), UnitRange(parse(Int, i), parse(Int, j))), password)
end

# ╔═╡ 8bfdfc80-9f73-4828-a721-010538405c7a
in2 = data(2, parser=parse_password_policy)

# ╔═╡ 3a2a0208-8aba-410c-8dea-d695ac2a0274
day2_1(input) = begin
	is_valid((policy, password)) =
		count(ch -> policy.ch == ch, password) in policy.range
	
	count(is_valid, input)
end

# ╔═╡ 4387bcac-ec3f-4993-9b2a-732eaad2eae1
day2_2(input) = begin
	is_valid((policy, password)) =
		xor(
			password[policy.range.start] == policy.ch,
			password[policy.range.stop] == policy.ch
		)
	
	count(is_valid, input)
end

# ╔═╡ 75d68aed-bb73-4b0b-b5f7-003bbde00b45
check(in2, (day2_1, 546), (day2_2, 275))

# ╔═╡ c531e657-ab6b-4fdf-90b5-2333a500266f
md"""
# Day 3: Toboggan Trajectory

<https://adventofcode.com/2020/day/3>

## Part 1

Input defines a coordinate map of the terrain:

```
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
```

`.` is an open square and `#` is a tree.

If we start at (0, 0) and follow a slope of (3 across, 1 down), how many trees will we encounter?

## Part 2

Now compute the number of trees for each of the following slopes and multiply the counts together:

```
Right 1, down 1.
Right 3, down 1. (This is the slope you already checked.)
Right 5, down 1.
Right 7, down 1.
Right 1, down 2.
```
"""

# ╔═╡ 9e1a80d8-8a27-4577-a384-731abc33e33c
in3 = data(3)

# ╔═╡ 2a96fe34-7a6f-4518-966d-959507fd2bda
function traverse(map, slope)
	_pos, _slope, n = 
		foldl(map[2:end]; init=(1, slope, 0)) do (x, (dx, dy), n), row
			if dy > 1
				(x, (dx, dy - 1), n)
			else
				if row[mod1(x + dx, length(row))] == '#'
					(x + dx, slope, n + 1)
				else
					(x + dx, slope, n)
				end
			end
		end
	
	n
end

# ╔═╡ 49bcd1d7-a067-4d53-ab80-ee5b998116f9
day3_1(input) = traverse(input, (3, 1))

# ╔═╡ e5e13cec-897f-4aa6-8373-e02cd390d5c3
day3_2(input) =
	prod(slope -> traverse(input, slope), [(1,1), (3,1), (5,1), (7,1), (1,2)])

# ╔═╡ 76809bda-a6d2-405d-917b-510b9bd63aa0
check(in3, (day3_1, 148), (day3_2, 727923200))

# ╔═╡ 932791e0-7965-40ed-b0fe-8187d1c9cd01
md"""
# Day 4: Passport Processing

<https://adventofcode.com/2020/day/4>

## Part 1

A passport has the following required fields:

```
byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)
```

The input contains empty line delimited passports, with fields separated by a newline or space, e.g.:

```
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
```

Parse the passports in the input file and determine how many are valid. `cid` is a special case—treat as though it were optional.


## Part 2


```
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
```
"""

# ╔═╡ c83279ef-2895-4ce2-8a20-5d8722d53fc2
in4 = begin
	parse_passport(in) = 
		Dict(c[1] => c[2] for c in eachmatch(r"([a-z]+):([^\s]+)", in))
	
	read_chunks(fname) =
		split(read(fname, String), "\n\n")
	
	data(4; parser=parse_passport, reader=read_chunks)
end

# ╔═╡ ce9654d2-c40f-4772-8ccc-3089dc52bf2e
required_passport_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

# ╔═╡ 2dacdd92-e255-426d-b07c-5e8c41cd3cf4
function day4_1(passports)
	is_valid(passport) =
		⊆(required_passport_fields, keys(passport))

	count(is_valid, passports)
end

# ╔═╡ 9b2d4d8a-94cf-4179-bfaf-c9325fab2122
function day4_2(passports)
	validate_field((name, value)) = @match (name, value) begin
		("byr", x) 					    => parse(Int, x) in 1920:2002
		("iyr", x) 						=> parse(Int, x) in 2010:2020
		("eyr", x) 						=> parse(Int, x) in 2020:2030
		("hgt", Capture{r"(\d+)cm"}(x)) => parse(Int, x) in 150:193
		("hgt", Capture{r"(\d+)in"}(x)) => parse(Int, x) in 59:76
		("hcl", x)						=> occursin(r"^#[a-f0-9]{6}$", x)
		("ecl", x) 						=> x in ["amb", "blu", "brn", "gry", 
												 	"grn", "hzl", "oth"]
		("pid", x)          			=> occursin(r"^[0-9]{9}$", x)
		("cid", x)						=> true
		_   							=> false
	end
	
	is_valid(passport) =
		⊆(required_passport_fields, keys(passport)) &&
			all(validate_field(field) for field in passport)
	
	count(is_valid, passports)
end

# ╔═╡ d11b2142-ebc8-47aa-9700-e926bed79a20
check(in4, (day4_1, 206), (day4_2, 123))

# ╔═╡ 24d37d17-0d4b-449c-b667-d96a5d1daddd
md"""
# Day 5: Binary Boarding

<https://adventofcode.com/2020/day/5>

## Part 1

For example, consider just the first seven characters of FBFBBFFRLR:

```
    Start by considering the whole range, rows 0 through 127.
    F means to take the lower half, keeping rows 0 through 63.
    B means to take the upper half, keeping rows 32 through 63.
    F means to take the lower half, keeping rows 32 through 47.
    B means to take the upper half, keeping rows 40 through 47.
    B keeps rows 44 through 47.
    F keeps rows 44 through 45.
    The final F keeps the lower of the two, row 44.
```

## Part 2
"""

# ╔═╡ bbf829d0-3829-4c18-8ba9-3ffda133e9c0
parse_seat(seat) = let
	translate(seat) =
		join(replace(collect(seat), 'F' => 0, 'B' => 1, 'L' => 0, 'R' => 1), "")
	
	parse(Int, translate(seat), base=2)
end

# ╔═╡ 7e720c38-940f-47eb-b2ff-5d5bbc06d75f
in5 = data(5, parser=parse_seat)

# ╔═╡ 41afbdc3-7af7-4a03-8f4d-33eee430dbf8
day5_1(seats) = 
	maximum(seats)

# ╔═╡ 8cba29b2-ecb7-47ea-9944-e9a5b99cc46e
day5_2(seats) =
	only(s for s in 0:maximum(seats) if s ∉ seats && s - 1 ∈ seats && s + 1 ∈ seats)

# ╔═╡ 1b975427-d34f-404a-851b-6611117f1788
check(in5, (day5_1, 866), (day5_2, 583))

# ╔═╡ 94967137-6f1a-46dd-930f-187543729fc8
md"""
# Day 6: Custom Customs

## Part 1

## Part 2
"""

# ╔═╡ 3dcfe821-11f1-4989-b512-7d7a4aac4be0
in6 :: Vector{Vector{String}} = data(
	6, 
	parser=chunk -> split(chunk, "\n"), 
	reader=fname -> split(strip(read(fname, String)), "\n\n")
)

# ╔═╡ 840fa352-fdc2-49ff-aec9-3bbeccdc0e2d
day6_1(groups) =
	sum(length(reduce(union, group)) for group in groups)

# ╔═╡ 4bf4ff3e-495a-4fd9-af55-5eb07791860b
day6_2(groups) =
	sum(length(reduce(intersect, group)) for group in groups)

# ╔═╡ 74a004a5-882b-49b3-a1e7-16ff6c9ead7e
check(in6, (day6_1, 6778), (day6_2, 3406))

# ╔═╡ d9bfdd60-27ad-4c6a-a6c0-60c3e8f80e19
md"""
# Day 7: Handy Haversacks

## Part 1

## Part 2
"""

# ╔═╡ 6c773f62-b660-4603-ad61-b71ca475d9dc
BagColor = String

# ╔═╡ 05a1c4c3-2512-4f17-b002-31ad03e7a3a1
BagRule = Tuple{BagColor, Vector{Tuple{Int, BagColor}}}

# ╔═╡ 9b557ae9-9526-40e8-87d7-522e692a5576
parse_bag_rule(line::String)::BagRule = let
	head, tail... = eachmatch(r"((?<count>\d) )?(?<color>\w+\s\w+) bags?", line)

	contains = map(m for m in tail if m[:color] != "no other") do match
		(parse(Int, match[:count]), match[:color])
	end

	(head[:color], contains)
end

# ╔═╡ bab5551e-8409-4de9-8a43-3c57a938ab68
containing_color(color::BagColor, rules::Vector{BagRule})::Vector{BagColor} = let
	colors = [c1 for (c1, contains) in rules 
			  	if any(true for (_, c2) in contains if c2 == color)]
	colors ∪ flatten(containing_color(c, rules) for c in colors)
end

# ╔═╡ 896e5a36-bbcc-4710-8791-45c1085878c0
bag_count(color::BagColor, rules::Vector{BagRule})::Int64 = let
	_, contains = first(rule for rule in rules if rule[1] == color)

	sum([count for (count, _) in contains], init=0) + 
		sum([count * bag_count(col, rules) for (count, col) in contains], init=0)
end

# ╔═╡ bea48ce6-4b28-4545-b334-1ed897877c42
in7 = data(7, parser=parse_bag_rule)

# ╔═╡ 1f9789e8-f7ef-46da-bebc-2f74647c941a
day7_1(input) = length(containing_color("shiny gold", in7))

# ╔═╡ 9602a583-fa34-42cc-9992-522d959470d7
day7_2(input) = bag_count("shiny gold", in7)

# ╔═╡ f75ac600-1b85-43e2-9a1d-4b10409a288e
check(in7, (day7_1, 233), (day7_2, 421550))

# ╔═╡ 604bc07d-675f-4001-b9ec-d397c6135771
md"""
# Day 8: Handheld Halting
"""

# ╔═╡ 04ccb4d5-b0af-4cfe-96b5-5abf996f747c
Instruction = Tuple{Symbol, Int}

# ╔═╡ a72e2f18-ef9b-423d-a3e5-86309dd0daf1
parse_inst(line::String)::Instruction =
	@match split(line, " ") begin
		["nop", x] => (:nop, parse(Int, x))
		["acc", x] => (:acc, parse(Int, x))
		["jmp", x] => (:jmp, parse(Int, x))
		_          => throw("parse error")
	end

# ╔═╡ 7615f265-8d2e-47b8-ad34-d16c916c22e2
in8 = data(8, parser=parse_inst)

# ╔═╡ 9e066340-33f5-49f8-a5c0-5a4f4fcae1c6
boot(program::Vector{Instruction}, state::Tuple{Int, Int, Set{Int}}) = let
	pointer, acc, hist = state

	if pointer in hist
		(acc, true)
	elseif pointer > length(program)
		(acc, false)
	else
		@match program[pointer] begin
			(:acc, x) => boot(program, (pointer + 1, acc + x, hist ∪ pointer))
			(:nop, _) => boot(program, (pointer + 1, acc, hist ∪ pointer))
			(:jmp, x) => boot(program, (pointer + x, acc, hist ∪ pointer))
		end
	end
end

# ╔═╡ f48dcc0c-0334-48a8-b2b6-06a683ff9282
boot(program::Vector{Instruction}) = boot(program, (1, 0, Set{Int}()))

# ╔═╡ 2a54ee72-4394-45c5-ad77-395b3de372ac
day8_1(program) = boot(program)[1]

# ╔═╡ cea5b077-41ac-47a2-83b4-60ceb2ebc011
day8_2(program) = let
	substitute(inst) =
		@match inst begin
			(:nop, x) => (:jmp, x)
			(:jmp, x) => (:nop, x)
			other 	  => other
		end
	
	programs = 
		[[(j == i ? substitute(inst) : inst) for (j, inst) in enumerate(program)]
			for i in 1:length(program)]
	
	terminates(program) =
		@match boot(program) begin
			(_, aborted) => !aborted
		end
	
	only(boot(p)[1] for p in programs if terminates(p))
end

# ╔═╡ 7f53f5c2-91c3-4fee-ac52-410f824b0e03
check(in8, (day8_1, 1553), (day8_2, 1877))

# ╔═╡ 13c19f7c-6b19-4167-a56f-d688c03d0a6d
md"""
# Day 9: Encoding Error
"""

# ╔═╡ 393add40-37ed-489d-b327-7e62e45a8a06
in9 = data(9, parser=line -> parse(Int, line))

# ╔═╡ d1f488c1-690c-4786-83a2-4b01634492a1
find_contiguous_seq_totaling(xs, total) =
	first(i:(i+di) for i in 1:length(xs), di in 1:length(xs)
			if i+di <= length(xs) && sum(xs[i:(i+di)]) == total)

# ╔═╡ 041f1e17-6540-431a-bbdb-af0256658752
day9_1(xs; p=25) =
	first(x for (x, i) in zip(xs[p+1:end], countfrom(p+1))
			if !any(y + z == x for y in xs[i-p:i-1], z in xs[i-p:i-1] if y != z))

# ╔═╡ ec6cf695-5d45-4cee-997a-61eed0441eeb
day9_2(xs; total=day9_1(in9)) = begin
	range = find_contiguous_seq_totaling(xs, total)
	
	minimum(xs[range]) + maximum(xs[range])
end

# ╔═╡ bc77d279-6444-4aab-b55c-f4309a021218
check(in9, (day9_1, 556543474), (day9_2, 76096372))

# ╔═╡ 0050f84d-0a94-489f-9da2-4a837b353626
md"""
# Day 10: Adapter Array
"""

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
Combinatorics = "861a8166-3701-5b0c-9a16-15d98fcdc6aa"
MLStyle = "d8e11817-5142-5d16-987a-aa16d5891078"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"

[compat]
Combinatorics = "~1.0.2"
MLStyle = "~0.4.10"
PlutoUI = "~0.7.9"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[Combinatorics]]
git-tree-sha1 = "08c8b6831dc00bfea825826be0bc8336fc369860"
uuid = "861a8166-3701-5b0c-9a16-15d98fcdc6aa"
version = "1.0.2"

[[Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "81690084b6198a2e1da36fcfda16eeca9f9f24e4"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.1"

[[Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[MLStyle]]
git-tree-sha1 = "594e189325f66e23a8818e5beb11c43bb0141bcd"
uuid = "d8e11817-5142-5d16-987a-aa16d5891078"
version = "0.4.10"

[[Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[Parsers]]
deps = ["Dates"]
git-tree-sha1 = "c8abc88faa3f7a3950832ac5d6e690881590d6dc"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "1.1.0"

[[PlutoUI]]
deps = ["Base64", "Dates", "InteractiveUtils", "JSON", "Logging", "Markdown", "Random", "Reexport", "Suppressor"]
git-tree-sha1 = "44e225d5837e2a2345e69a1d1e01ac2443ff9fcb"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.9"

[[Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[Random]]
deps = ["SHA", "Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[Reexport]]
git-tree-sha1 = "5f6c21241f0f655da3952fd60aa18477cf96c220"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.1.0"

[[SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[Suppressor]]
git-tree-sha1 = "a819d77f31f83e5792a76081eee1ea6342ab8787"
uuid = "fd094767-a336-5f1f-9728-57cf17d0bbfb"
version = "0.2.0"

[[Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"
"""

# ╔═╡ Cell order:
# ╟─254b7a7d-d446-49f1-9b00-14acfb16561e
# ╠═253b7667-3988-4dda-8508-817edf4622e4
# ╠═91d3429b-bd55-4d9e-92e2-af61a2668ac5
# ╠═2f6aafec-5dc9-4cfb-bdc3-2dca328c0e0f
# ╠═00b5e199-f69a-4a1d-81f4-61102358e9f5
# ╠═5261b35f-2c2d-4c3c-abe6-e7fae6c23dca
# ╠═ed0fb9d9-641f-4f31-92a6-4408a14494a4
# ╠═35f19bf0-c23c-47b1-affa-466fed4c9486
# ╠═4f47b6cc-0672-4ba8-bcc5-cfb96d3cc6b1
# ╠═90574e72-1941-4991-b2d0-d36d6685a864
# ╠═7596ba1a-e442-4cf8-8bbd-a026cfbaa2a6
# ╠═dc8abb99-0b0d-4fb4-8c11-54e7d873cad6
# ╠═93de06ee-1e88-4225-8be7-8e9cc1c2bd1b
# ╠═50024a0f-bcf7-4a80-b3f5-236d922d7d04
# ╟─c10a3946-c73c-11eb-03e0-1ffe27ff778b
# ╠═2816220f-0e6c-43ae-8dff-eb66bcefe7e3
# ╠═27e188f7-7a16-41b0-9576-cc7c0bfde7a4
# ╠═9980bf61-28f3-451c-b3e1-c35c41d4fdfa
# ╠═55a4a291-5d76-4215-81b9-e7a6f4a68b3d
# ╟─4eb09210-5516-470f-ac9b-5f9d116c4311
# ╠═b7b39cee-68f1-4618-a3b6-0e5c77bb966e
# ╠═b5c74108-f007-4f8b-b4b4-45c4e5ac0e89
# ╠═8bfdfc80-9f73-4828-a721-010538405c7a
# ╠═3a2a0208-8aba-410c-8dea-d695ac2a0274
# ╠═4387bcac-ec3f-4993-9b2a-732eaad2eae1
# ╠═75d68aed-bb73-4b0b-b5f7-003bbde00b45
# ╟─c531e657-ab6b-4fdf-90b5-2333a500266f
# ╠═9e1a80d8-8a27-4577-a384-731abc33e33c
# ╠═2a96fe34-7a6f-4518-966d-959507fd2bda
# ╠═49bcd1d7-a067-4d53-ab80-ee5b998116f9
# ╠═e5e13cec-897f-4aa6-8373-e02cd390d5c3
# ╠═76809bda-a6d2-405d-917b-510b9bd63aa0
# ╟─932791e0-7965-40ed-b0fe-8187d1c9cd01
# ╠═c83279ef-2895-4ce2-8a20-5d8722d53fc2
# ╠═ce9654d2-c40f-4772-8ccc-3089dc52bf2e
# ╠═2dacdd92-e255-426d-b07c-5e8c41cd3cf4
# ╠═9b2d4d8a-94cf-4179-bfaf-c9325fab2122
# ╠═d11b2142-ebc8-47aa-9700-e926bed79a20
# ╟─24d37d17-0d4b-449c-b667-d96a5d1daddd
# ╠═bbf829d0-3829-4c18-8ba9-3ffda133e9c0
# ╠═7e720c38-940f-47eb-b2ff-5d5bbc06d75f
# ╠═41afbdc3-7af7-4a03-8f4d-33eee430dbf8
# ╠═8cba29b2-ecb7-47ea-9944-e9a5b99cc46e
# ╠═1b975427-d34f-404a-851b-6611117f1788
# ╟─94967137-6f1a-46dd-930f-187543729fc8
# ╠═3dcfe821-11f1-4989-b512-7d7a4aac4be0
# ╠═840fa352-fdc2-49ff-aec9-3bbeccdc0e2d
# ╠═4bf4ff3e-495a-4fd9-af55-5eb07791860b
# ╠═74a004a5-882b-49b3-a1e7-16ff6c9ead7e
# ╟─d9bfdd60-27ad-4c6a-a6c0-60c3e8f80e19
# ╠═6c773f62-b660-4603-ad61-b71ca475d9dc
# ╠═05a1c4c3-2512-4f17-b002-31ad03e7a3a1
# ╠═9b557ae9-9526-40e8-87d7-522e692a5576
# ╠═bab5551e-8409-4de9-8a43-3c57a938ab68
# ╠═896e5a36-bbcc-4710-8791-45c1085878c0
# ╠═bea48ce6-4b28-4545-b334-1ed897877c42
# ╠═1f9789e8-f7ef-46da-bebc-2f74647c941a
# ╠═9602a583-fa34-42cc-9992-522d959470d7
# ╠═f75ac600-1b85-43e2-9a1d-4b10409a288e
# ╟─604bc07d-675f-4001-b9ec-d397c6135771
# ╠═04ccb4d5-b0af-4cfe-96b5-5abf996f747c
# ╠═a72e2f18-ef9b-423d-a3e5-86309dd0daf1
# ╠═7615f265-8d2e-47b8-ad34-d16c916c22e2
# ╠═9e066340-33f5-49f8-a5c0-5a4f4fcae1c6
# ╠═f48dcc0c-0334-48a8-b2b6-06a683ff9282
# ╠═2a54ee72-4394-45c5-ad77-395b3de372ac
# ╠═cea5b077-41ac-47a2-83b4-60ceb2ebc011
# ╠═7f53f5c2-91c3-4fee-ac52-410f824b0e03
# ╟─13c19f7c-6b19-4167-a56f-d688c03d0a6d
# ╠═393add40-37ed-489d-b327-7e62e45a8a06
# ╠═d1f488c1-690c-4786-83a2-4b01634492a1
# ╠═041f1e17-6540-431a-bbdb-af0256658752
# ╠═ec6cf695-5d45-4cee-997a-61eed0441eeb
# ╠═bc77d279-6444-4aab-b55c-f4309a021218
# ╟─0050f84d-0a94-489f-9da2-4a837b353626
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
