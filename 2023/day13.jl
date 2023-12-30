function parsePattern(lines::Vector{String})
    rows = length(lines)
    cols = length(lines[1])
    pattern = zeros(Bool, rows, cols)
    for (row, line) in enumerate(lines)
        for (col, ch) in enumerate(line)
            if ch == raw"#"[1] pattern[row, col] = 1 end
        end
    end
    pattern
end

function parseInput(input)
    patterns = []
    pattern = String[]
    for (i, l) in enumerate(input)
        if length(l) == 0
            push!(patterns, parsePattern(pattern))
            empty!(pattern)
        else
            push!(pattern, l)
        end
    end
    push!(patterns, parsePattern(pattern))
    patterns
end

function checkReflectionCols(pattern, column)
    _, cols = size(pattern)
    # left right; we have already checked center
    l = column - 1
    r = column + 2
    while 1 <= l && r <= cols
        pattern[:, l] != pattern[:, r] && return false
        l -= 1
        r += 1
    end
    return true
end

function checkReflectionRows(pattern, row)
    rows, _ = size(pattern)
    # top bottom; we have already checked center
    t = row - 1
    b = row + 2
    while 1 <= t && b <= rows
        pattern[t, :] != pattern[b, :] && return false
        t -= 1
        b += 1
    end
    return true
end

function findLineOfRefl(pattern)
    rows, cols = size(pattern)
    for c in 1:cols - 1
        if pattern[:, c] == pattern[:, c + 1]
            checkReflectionCols(pattern, c) && return ('v', c)
        end
    end
    for r in 1:rows - 1
        if pattern[r, :] == pattern[r + 1, :]
            checkReflectionRows(pattern, r) && return ('h', r)
        end
    end
end

function summarize(line::Tuple{Char, Int})::Int
    orientation, index = line
    orientation == 'h' ? index * 100 : index
end

function solveA(patterns)
    [ findLineOfRefl(p) for p in patterns ] |> xs -> map(l -> summarize(l), xs) |> sum
end

function findNewReflectionLine(pat, oldLine)::Tuple{Char, Int}
    rows, cols = size(pat)
    for r in 1:rows
        for c in 1:cols
            old = pat[r, c]
            pat[r, c] = c == 0 ? 1 : 0
            for col in 1:cols - 1
                if pat[:, col] == pat[:, col + 1]
                    if checkReflectionCols(pat, col)
                        newLine = ('v', col)
                        newLine != oldLine && return newLine
                    end
                end
            end
            for row in 1:rows - 1
                if pat[row, :] == pat[row + 1, :]
                    if checkReflectionRows(pat, row)
                        newLine = ('h', row)
                        newLine != oldLine && return newLine
                    end
                end
            end
            pat[r, c] = old
        end
    end
    return oldLine
end

function solveB(patterns)
    [ (p, findLineOfRefl(p)) for p in patterns ] |> xs -> map(((p, l),) -> findNewReflectionLine(p, l), xs) |> xs -> map(l -> summarize(l), xs) |> sum
end

function main(file = "day13.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    patterns = parseInput(input)
    # 36448
    println("Solving Day13A...")
    println(solveA(patterns))
    # 35799
    println("Solving Day13B...")
    println(solveB(patterns))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day13s.txt"
    file = "day13.txt"
    println("file: $file")
    main(file)
end
