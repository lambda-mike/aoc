function parseInstruction(c::Char)::Int
    if c == ')' return -1
    elseif c == '(' return 1
    else throw(DomainError("Unrecognized char:$c"))
    end
end

function solveA(input::AbstractString)
    mapreduce(parseInstruction, +, input; init=0)
end

function solveB(input)
    reachedFloors = Iterators.accumulate((acc, c) -> acc + parseInstruction(c), input; init = 0) |> collect
    findfirst(==(-1) ∘ last, reachedFloors)
end

function main(file = "day01.txt")
    println("file $file")
    input = strip(read(file, String))
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
    # main("day01.txt")
    file = "day01.txt"
    input = strip(read(file, String))
    println(typeof(input))
    for c in collect(input)
        println(typeof(c))
        println("$c", parseInstruction(Char(c)))
    end
end
