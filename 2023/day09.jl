function parseSeq(l)
    [ parse(Int, x) for x in split(l) ]
end

function extrapolateSeq(seq::Vector{Int})
    isDone(xs) = all(==(0), xs)
    # from bottom to top
    lastDigits = []
    while !isDone(seq)
        pushfirst!(lastDigits, pop!(seq))
        for i in 2:length(seq)
            seq[i-1] = seq[i] - seq[i-1]
        end
        seq[end] = lastDigits[begin] - seq[end]
    end
    sum(lastDigits)
end

function solveA(input)
    seqs = [ parseSeq(seq) for seq in input ]
    [ extrapolateSeq(s) for s in seqs ] |> sum
end

function extrapolateSeqBackwards(seq::Vector{Int})
    isDone(xs) = all(==(0), xs)
    # from bottom to top
    digits = []
    while !isDone(seq)
        pushfirst!(digits, seq[begin])
        for i in 2:length(seq)
            seq[i-1] = seq[i] - seq[i-1]
        end
        seq[end] = digits[begin] - seq[end]
        pop!(seq)
    end
    pushfirst!(digits, 0)
    reduce((acc,x) -> x - acc, digits; init=0)
end

function solveB(input)
    seqs = [ parseSeq(seq) for seq in input ]
    [ extrapolateSeqBackwards(s) for s in seqs ] |> sum
end

function main(file = "day09.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    println("Solving Day09A...")
    # 1995001648
    println(solveA(input))
    # 988
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
