function solveA(input)
    println(input)
    0
end

function solveB(input)
    0
end

function main(file = "day09.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    println("Solving Day09A...")
    println(solveA(input))
    println("Solving Day09B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day09s.txt"
    file = "day09.txt"
    println("file: $file")
    main(file)
end
