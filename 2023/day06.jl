function countWinningStrategies(recordTime::Int, dist::Int)::Int
    result = 0
    for chargeTime in 1:recordTime
        v = chargeTime
        d = ( recordTime - chargeTime ) * v
        d > dist && (result += 1)
    end
    result
end

function solveA(input)
    time = [ parse(Int, x) for x in split(input[1])[2:end] ]
    dist = [ parse(Int, x) for x in split(input[2])[2:end] ]
    [ countWinningStrategies(t, d) for (t, d) in zip(time, dist) ] |> prod
end

function solveB(input)
    time = split(input[1])[2:end] |> join |> s -> parse(Int, s)
    dist = split(input[2])[2:end] |> join |> s -> parse(Int, s)
    countWinningStrategies(time, dist)
end

function main(file = "day06.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    println("Solving Day06A...")
    println(solveA(input))
    println("Solving Day06B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day06s.txt"
    file = "day06.txt"
    println("repl - file: $file")
    main(file)
end
