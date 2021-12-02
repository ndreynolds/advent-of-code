### A Pluto.jl notebook ###
# v0.17.2

using Markdown
using InteractiveUtils

# ╔═╡ 74f8b5f7-2851-4518-94d9-be108bbdb580
using PlutoUI

# ╔═╡ 15efba7e-af12-4ecf-9ae0-ec5c5dcb92ce
using Base.Iterators

# ╔═╡ 37e1c3fc-0c60-419a-b6a7-381c1fa3e1f3
using MLStyle

# ╔═╡ 5c685e48-1ae8-4f0d-8e36-5b3b1347b2c9
md"""
# Day 0: Utilities
"""

# ╔═╡ 2c014b14-2e8e-4a5d-9352-59cb5a92b5ca
data(day; parser=identity, reader=readlines) =
	[parser(line) for line in reader("inputs/$(day).in")]

# ╔═╡ a2bcd2e6-ad94-4721-b7a9-773c02db8b41
struct Mismatch
	actual::Any
	expected::Any
end

# ╔═╡ 6402c7dc-3676-40cc-972c-7162582f0200
struct Unchecked
	value::Any
end

# ╔═╡ 112f8968-696b-4c15-abe1-bc5918c0b22d
struct Match
	value::Any
end

# ╔═╡ 52b4d814-6795-4b8b-8bcb-34f590303af3
test(actual, expected) = @match (actual, expected) begin
	(actual, nothing)								=> Unchecked(actual)
	(actual, expected) && if actual == expected end => Match(actual)
	(actual, expected) 								=> Mismatch(actual, expected)
end

# ╔═╡ 78717ec7-2f14-4ccf-977c-020dc749ae91
check(input, (part, expected)) = Dict(part => test(part(input), expected))

# ╔═╡ 8cd1155f-07d9-4262-ab47-c0c5ae89632e
check(input, part1, part2) = check(input, part1) ∪ check(input, part2)

# ╔═╡ 3524627d-0222-4520-b697-eba6cc68ca33
md"""
# Day 1: Sonar Sweep
"""

# ╔═╡ 0bcd6c42-4b18-482c-aba6-8c82c9ac38ca
in1 = data(1, parser=x -> parse(Int, x))

# ╔═╡ 00ab578d-55e1-4e66-9460-f6b159b6750a
day1_1(depths) = sum(1 for (a, b) in zip(depths, drop(depths, 1)) if a < b)

# ╔═╡ 1dfd5c4b-edf5-44a5-b297-22952d3aca69
day1_2(depths) = sum(1 for (a, b, c, d) in 
						zip(depths, drop(depths, 1), drop(depths, 2), drop(depths, 3))
							if a < d)

# ╔═╡ fbb2a50a-a5b0-4dd2-9267-8fbe0d6afea6
check(in1, (day1_1, 1527), (day1_2, 1575))

# ╔═╡ ef28c2c3-6701-407e-a118-8c2f775e81b0
md"""
# Day 2: Dive!
"""

# ╔═╡ 1927a127-ea10-4918-93be-0ae1b613c583
in2 = data(2, parser=x -> (split(x)[1], parse(Int, split(x)[2])))

# ╔═╡ 2027848e-8f37-48ec-8edc-03f3f87ac76c
adjust_course_v1((horiz, depth), change) =
	@match change begin
		("forward", dx) => (horiz + dx, depth)
		("down", dx) => (horiz, depth + dx)
		("up", dx) => (horiz, depth - dx)
	end

# ╔═╡ 11bc41f1-ef31-49b9-ad4f-e3b6cdd0a7ff
adjust_course_v2((horiz, depth, aim), change) =
	@match change begin
		("forward", dx) => (horiz + dx, depth + dx*aim, aim)
		("down", dx) => (horiz, depth, aim + dx)
		("up", dx) => (horiz, depth, aim - dx)
	end

# ╔═╡ f23b6e6e-c71f-429b-af53-564604221db5
day2_1(changes) = prod(reduce(adjust_course_v1, changes; init=(0, 0)))

# ╔═╡ 176a5baa-6c68-42d7-a692-a8629a2d32ec
day2_2(changes) = prod(reduce(adjust_course_v2, changes; init=(0, 0, 0))[1:end-1])

# ╔═╡ 86f2509b-37ac-4a9a-8d93-b87a17ebc720
check(in2, (day2_1, 1636725), (day2_2, 1872757425))

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
MLStyle = "d8e11817-5142-5d16-987a-aa16d5891078"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"

[compat]
MLStyle = "~0.4.10"
PlutoUI = "~0.7.21"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.7.0"
manifest_format = "2.0"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "abb72771fd8895a7ebd83d5632dc4b989b022b5b"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.1.2"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[deps.Downloads]]
deps = ["ArgTools", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"

[[deps.Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "8d511d5b81240fc8e6802386302675bdf47737b9"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.4"

[[deps.HypertextLiteral]]
git-tree-sha1 = "2b078b5a615c6c0396c77810d92ee8c6f470d238"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.3"

[[deps.IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "f7be53659ab06ddc986428d3a9dcc95f6fa6705a"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.2"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "8076680b162ada2a031f707ac7b4953e30667a37"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.2"

[[deps.LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"

[[deps.LibGit2]]
deps = ["Base64", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[deps.MLStyle]]
git-tree-sha1 = "594e189325f66e23a8818e5beb11c43bb0141bcd"
uuid = "d8e11817-5142-5d16-987a-aa16d5891078"
version = "0.4.10"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[deps.Parsers]]
deps = ["Dates"]
git-tree-sha1 = "ae4bbcadb2906ccc085cf52ac286dc1377dceccc"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.1.2"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "Dates", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "Markdown", "Random", "Reexport", "UUIDs"]
git-tree-sha1 = "b68904528fd538f1cb6a3fbc44d2abdc498f9e8e"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.21"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[deps.Random]]
deps = ["SHA", "Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
"""

# ╔═╡ Cell order:
# ╟─5c685e48-1ae8-4f0d-8e36-5b3b1347b2c9
# ╠═74f8b5f7-2851-4518-94d9-be108bbdb580
# ╠═15efba7e-af12-4ecf-9ae0-ec5c5dcb92ce
# ╠═37e1c3fc-0c60-419a-b6a7-381c1fa3e1f3
# ╠═2c014b14-2e8e-4a5d-9352-59cb5a92b5ca
# ╠═a2bcd2e6-ad94-4721-b7a9-773c02db8b41
# ╠═6402c7dc-3676-40cc-972c-7162582f0200
# ╠═112f8968-696b-4c15-abe1-bc5918c0b22d
# ╠═52b4d814-6795-4b8b-8bcb-34f590303af3
# ╠═78717ec7-2f14-4ccf-977c-020dc749ae91
# ╠═8cd1155f-07d9-4262-ab47-c0c5ae89632e
# ╟─3524627d-0222-4520-b697-eba6cc68ca33
# ╠═0bcd6c42-4b18-482c-aba6-8c82c9ac38ca
# ╠═00ab578d-55e1-4e66-9460-f6b159b6750a
# ╠═1dfd5c4b-edf5-44a5-b297-22952d3aca69
# ╠═fbb2a50a-a5b0-4dd2-9267-8fbe0d6afea6
# ╟─ef28c2c3-6701-407e-a118-8c2f775e81b0
# ╠═1927a127-ea10-4918-93be-0ae1b613c583
# ╠═2027848e-8f37-48ec-8edc-03f3f87ac76c
# ╠═11bc41f1-ef31-49b9-ad4f-e3b6cdd0a7ff
# ╠═f23b6e6e-c71f-429b-af53-564604221db5
# ╠═176a5baa-6c68-42d7-a692-a8629a2d32ec
# ╠═86f2509b-37ac-4a9a-8d93-b87a17ebc720
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
