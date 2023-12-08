function solveA(input)
    println(input)
    0
end

function solveB(input)
    0
end

function main(file = "day10.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    println("Solving Day10A...")
    println(solveA(input))
    println("Solving Day10B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day10s.txt"
    file = "day10.txt"
    println("file: $file")
    main(file)
end
