@enum Dir begin
    N
    E
    S
    W
end

function parseInput(input::Vector{String})
    platform = fill('.', (length(input), length(input[1])))
    for r in 1:length(input)
        for c in 1:length(input[1])
            platform[r, c] = input[r][c]
        end
    end
    platform
end

function calculateTotalLoad(P)
    rows, cols = size(P)
    total = 0
    for r in 1:rows
        for c in 1:cols
            if P[r, c] == 'O' total += (rows - r + 1) end
        end
    end
    total
end

function detectFirstObstacle(P, r, c, dir::Dir)
    isObstacle(c) = c == 'O' || c in raw"#"
    if dir == N
        path = P[1:r-1, c]
        return findlast(isObstacle, path)
    elseif dir == E
        path = P[r, c+1:end]
        i = findfirst(isObstacle, path)
        return i === nothing ? nothing : c + i
    elseif dir == S
        path = P[r+1:end, c]
        i = findfirst(isObstacle, path)
        return i === nothing ? nothing : r + i
    elseif dir == W
        path = P[r, 1:c-1]
        return findlast(isObstacle, path)
    else
        throw(DomainError("Unknown dir: $dir"))
    end
    return nothing
end

function tiltRocksN!(P)
    rows, cols = size(P)
    for r in 1:rows
        for c in 1:cols
            P[r, c] == '.' && continue
            P[r, c] in raw"#" && continue
            obstacle = detectFirstObstacle(P, r, c, N)
            targetPlace = obstacle === nothing ? 1 : obstacle + 1
            P[r, c] = '.'
            P[targetPlace, c] = 'O'
        end
    end
end

function tiltRocksE!(P)
    rows, cols = size(P)
    for c in cols:-1:1
        for r in 1:rows
            P[r, c] == '.' && continue
            P[r, c] in raw"#" && continue
            obstacle = detectFirstObstacle(P, r, c, E)
            t = obstacle === nothing ? cols : obstacle - 1
            P[r, c] = '.'
            P[r, t] = 'O'
        end
    end
end

function tiltRocksS!(P)
    rows, cols = size(P)
    for r in rows:-1:1
        for c in 1:cols
            P[r, c] == '.' && continue
            P[r, c] in raw"#" && continue
            obstacle = detectFirstObstacle(P, r, c, S)
            t = obstacle === nothing ? rows : obstacle - 1
            P[r, c] = '.'
            P[t, c] = 'O'
        end
    end
end

function tiltRocksW!(P)
    rows, cols = size(P)
    for c in 1:cols
        for r in 1:rows
            P[r, c] == '.' && continue
            P[r, c] in raw"#" && continue
            obstacle = detectFirstObstacle(P, r, c, W)
            t = obstacle === nothing ? 1 : obstacle + 1
            P[r, c] = '.'
            P[r, t] = 'O'
        end
    end
end

function cycle!(P)
    tiltRocksN!(P)
    tiltRocksW!(P)
    tiltRocksS!(P)
    tiltRocksE!(P)
end

function solveA(platform)
    P = copy(platform)
    tiltRocksN!(P)
    calculateTotalLoad(P)
end

function solveB(platform)
    P = copy(platform)
    # n = 3
    n = 1000000000
    cache = Dict{String, Int}([(join(P), 0)])
    cycleStart = 0
    nextCycleStart = 0
    for i in 1:n
        cycle!(P)
        h = join(P)
        if haskey(cache, h)
            cycleStart = cache[h]
            nextCycleStart = i
            break
        end
        cache[h] = i
    end
    cycleLen = nextCycleStart - cycleStart
    lastIncompleteCycleLen = (n - cycleStart) % cycleLen
    nCacheIndex = cycleStart + lastIncompleteCycleLen
    nthPlatform = findfirst(x -> x == nCacheIndex, cache)
    NP = copy(P)
    @assert length(nthPlatform) == size(P, 1) * size(P, 2) "wrong dim"
    for i in 1:length(nthPlatform)
        col = div((i - 1), size(NP, 1)) + 1
        row = ((i - 1) % size(NP, 2)) + 1
        NP[row, col] = nthPlatform[i]
    end
    calculateTotalLoad(NP)
end

function main(file = "day14.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    platform = parseInput(input)
    # 108889
    println("Solving Day14A...")
    println(solveA(platform))
    # 104671
    println("Solving Day14B...")
    println(solveB(platform))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day14s.txt"
    file = "day14.txt"
    println("file: $file")
    main(file)
end
