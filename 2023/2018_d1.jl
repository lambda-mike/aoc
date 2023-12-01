function solveA(freqs)
    mapreduce(x->parse(Int, x), +, freqs; init=0)
end

function solveB(lines)
    seen = Set()
    current = 0
    freqs = [ parse(Int, x) for x in lines ]
    for freq in Iterators.cycle(freqs)
        current += freq
        if current in seen
            break
        end
        push!(seen, current)
    end
    current
end

function main(file = "day01.txt")
    println("file $file")
    lines = [ x for x in eachline(file) ]
    # lines = eachline(file)
    # println("lines", lines)
    println("Solving Day01A...")
    println(solveA(lines))
    println("Solving Day01B...")
    println(solveB(lines))
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # main("day01.txt")
    main("day01s.txt")
end
