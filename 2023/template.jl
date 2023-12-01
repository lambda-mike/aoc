function solveA(input)
    0
end

function solveB(input)
    0
end

function main(file = "day01.txt")
    println("file $file")
    # lines = eachline(file)
    # input = strip(read(file, String))
    input = ""
    println("Solving Day01A...")
    println(solveA(input))
    println("Solving Day01B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # file = "day01s.txt"
    file = "day01.txt"
    # main("day01.txt")
end
