struct Nodes
    # node => (L, R)
    dict::Dict{String, Tuple{String, String}}
end

function parseNode(str)
    tokens = split(str)
    id = tokens[1]
    l = tokens[3][2:end-1]
    r = tokens[4][1:end-1]
    id, (l, r)
end

function parseInput(input)
    dirs = input[1]
    nodes = [ parseNode(x) for x in input[3:end] ] |> Dict
    (dirs, Nodes(nodes))
end

function solve(dirs::String, nodes::Nodes, start::String, isDone)::Int
    result = 0
    node = start
    for (i, d) in enumerate(Iterators.cycle(dirs))
        if isDone(node) result = i - 1; break end
        dir = d == 'L' ? 1 : 2
        node = nodes.dict[node][dir]
    end
    result
end

function solveA(input)
    start = "AAA"
    dirs, nodes = parseInput(input)
    solve(dirs, nodes, start, ==("ZZZ"))
end

function solveB(input)
    dirs, nodes = parseInput(input)
    ghosts = collect(keys(nodes.dict)) |> xs -> filter(x -> x[3] == 'A', xs)
    paths = []
    for g in ghosts
        n = solve(dirs, nodes, g, endswith("Z"))
        push!(paths, n)
    end
    result = paths[1]
    for p in paths[2:end]
        result = Int(result * p / Int(gcd(p, result)))
    end
    return result
end

function main(file = "day08.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    # 14681
    println("Solving Day08A...")
    println(solveA(input))
    # 14321394058031
    println("Solving Day08B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day08s.txt"
    # file = "day08s2.txt"
    # file = "day08s3.txt"
    file = "day08.txt"
    println("file: $file")
    main(file)
end
