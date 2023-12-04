function parseCard(line)::NamedTuple{(:id, :winning, :my), Tuple{Int, Vector{Int}, Vector{Int}}}
    tokens = split(line)
    id = parse(Int, tokens[2][begin:end-1])
    winning = Iterators.takewhile(!=("|"), tokens[3:end]) |> xs -> Iterators.map(x -> parse(Int, x), xs) |> collect
    my = Iterators.dropwhile(!=("|"), tokens) |> xs -> Iterators.drop(xs, 1) |> xs -> Iterators.map(x -> parse(Int, x), xs) |> collect
    (id = id, winning = winning, my = my)
end

function countMatches(card::NamedTuple{(:id, :winning, :my), Tuple{Int, Vector{Int}, Vector{Int}}})::Int
    winning = Set(card.winning)
    my = Set(card.my)
    cards = intersect(winning, my)
    length(cards)
end

function calculatePoints(card::NamedTuple{(:id, :winning, :my), Tuple{Int, Vector{Int}, Vector{Int}}})::Int
    n = countMatches(card)
    if n == 0
        0
    else
        2^(n - 1)
    end
end

function solveA(input)
    [ parseCard(line) for line in input ] |> xs -> map(calculatePoints, xs) |> sum
end

function solveB(input)
    cards = [ parseCard(line) for line in input ]
    matchesDict = [ (x.id, countMatches(x)) for x in cards ] |> Dict{Int, Int}
    # array of card ids
    game = map(x -> x.id, cards)
    i = 1
    while i < length(game)
        id = game[i]
        n = matchesDict[id]
        # card wins nothing
        if n == 0
            i += 1
        # card wins n matches
        else
            splice!(game, i+1:i, (id+1):(id+n))
            i += 1
        end
    end
    length(game)
end

function main(file = "day04.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    # 26914
    println("Solving Day04A...")
    println(solveA(input))
    # 13080971
    println("Solving Day04B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day04s.txt"
    file = "day04.txt"
    println("repl - file: $file")
    main(file)
end
