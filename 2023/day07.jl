function solveA(input)
    0
end

function solveB(input)
    0
end

function main(file = "day07.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    println("Solving Day07A...")
    println(solveA(input))
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
