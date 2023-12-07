function solveA(input)
    println(input)
    0
end

function solveB(input)
    0
end

function main(file = "day08.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    println("Solving Day08A...")
    println(solveA(input))
    println("Solving Day08B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    file = "day08s.txt"
    # file = "day08.txt"
    println("file: $file")
    main(file)
end
