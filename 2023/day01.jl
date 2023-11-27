function main(file = "day01.txt")
    println("file $file")
    println("Solving Day01A...")
    # println(solveA())
    println("Solving Day01B...")
    # println(solveB())
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # main("day01.txt")
    main("day01s.txt")
end
