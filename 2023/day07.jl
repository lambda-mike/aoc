struct Hand
    hand::Vector{Int}
    bid::Int
    rank::Int
end

function parseCard(c; partB = false)::Int
    if isdigit(c) parse(Int, c)
    elseif c == 'T' 10
    elseif c == 'J' partB ? 1 : 11
    elseif c == 'Q' 12
    elseif c == 'K' 13
    elseif c == 'A' 14
    else throw(DomainError("unknown char: $(c)"))
    end
end

function classify(hand::Vector{Int})::Int
    groups = Dict{Int, Int}()
    for card in hand
        n = get!(groups, card, 0)
        groups[card] = n + 1
    end
    five = count((x) -> last(x) == 5, groups)
    four = count((x) -> last(x) == 4, groups)
    three = count((x) -> last(x) == 3, groups)
    two = count((x) -> last(x) == 2, groups)
    if five == 1 7
    elseif four == 1 6
    elseif three == 1 && two == 1 5
    elseif three == 1 4
    elseif two == 2 3
    elseif two == 1 2
    # high card
    else 1
    end
end

function classifyB(hand::Vector{Int})::Int
    # J not in hand - old rules
    if !(1 in hand) return classify(hand) end
    bestRank = 0
    for replacement in 2:14
        candidateHand = replace(hand, 1 => replacement)
        rank = classify(candidateHand)
        bestRank = max(bestRank, rank)
    end
    bestRank
end

function parseHand(line; partB = false)
    tokens = split(line)
    bid = parse(Int, tokens[2])
    hand = [ parseCard(c; partB = partB) for c in collect(tokens[1]) ]
    rank = partB ? classifyB(hand) : classify(hand)
    Hand(hand, bid, rank)
end

function handLessThan(x::Hand, y::Hand)::Bool
    if x.rank == y.rank x.hand < y.hand
    else x.rank < y.rank
    end
end

function solveA(input)
    maxRank = length(input)
    hands = [ parseHand(s) for s in input ]
    sorted = sort(hands; lt=handLessThan)
    sorted |> enumerate |> collect |> xs -> map(((rank, hand),) -> rank * hand.bid, xs) |> sum
end

function solveB(input)
    maxRank = length(input)
    hands = [ parseHand(s, partB=true) for s in input ]
    sorted = sort(hands; lt=handLessThan)
    sorted |> enumerate |> collect |> xs -> map(((rank, hand),) -> rank * hand.bid, xs) |> sum
end

function main(file = "day07.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    # 250602641
    println("Solving Day07A...")
    println(solveA(input))
    # 251037509
    println("Solving Day07B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day07s.txt"
    file = "day07.txt"
    println("repl - file: $file")
    main(file)
end
