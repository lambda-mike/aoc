function inferStartPipe(input, start)::Char
    x, y = start
    cn = y > 1 ? input[y-1][x] : nothing
    cs = y < length(input) ? input[y+1][x] : nothing
    ce = x < length(input[1]) ? input[y][x+1] : nothing
    cw = x > 1 ? input[y][x-1] : nothing
    sPipes = ['|', '7', 'F']
    nPipes = ['|', 'L', 'J']
    ePipes = ['-', 'L', 'F']
    wPipes = ['-', '7', 'J']
    if cn in sPipes && cs in nPipes '|'
    elseif cw in ePipes && ce in wPipes '-'
    elseif cn in sPipes && ce in wPipes 'L'
    elseif cn in sPipes && cw in ePipes 'J'
    elseif cs in nPipes && cw in ePipes '7'
    elseif cs in nPipes && ce in wPipes 'F'
    else throw(DomainError("Could not infer pipe for pos $(start)"))
    end
end

# x L R y T B (increase values to right and to the bottom)
function getStartPoints(start::Tuple{Int, Int}, pipe::Char)
    sx, sy = start
    if pipe === '|' return (sx, sy+1), (sx, sy-1)
    elseif pipe === '-' return ((sx-1, sy), (sx+1, sy))
    elseif pipe === 'L' return ((sx, sy-1), (sx+1, sy))
    elseif pipe === 'J' return ((sx, sy-1), (sx-1, sy))
    elseif pipe === '7' return ((sx, sy+1), (sx-1, sy))
    elseif pipe === 'F' return ((sx, sy+1), (sx+1, sy))
    else ((0, 0), (0, 0))
    end
end

function nextPipePos(input, pos, prevPos)
    x, y = pos
    px, py = prevPos
    pipe = input[y][x]
    if pipe === '|' py < y ? (x, y+1) : (x, y-1)
    elseif pipe === '-' px < x ? (x+1, y) : (x-1, y)
    elseif pipe === 'L' py < y ? (x+1, y) : (x, y-1)
    elseif pipe === 'J' py < y ? (x-1, y) : (x, y-1)
    elseif pipe === '7' py > y ? (x-1, y) : (x, y+1)
    elseif pipe === 'F' py > y ? (x+1, y) : (x, y+1)
    else throw(DomainError("we hit wrong pipe $(pipe) pos ($(x), $(y)) prev $(prevPos)"))
    end
end

function solveA(input)
    sy = findfirst(x -> findfirst('S', x) !== nothing, input)
    sx = findfirst('S', input[sy])
    start = (sx, sy)
    startPipe = inferStartPipe(input, start)
    a, b = getStartPoints(start, startPipe)
    distA = 1
    distB = 1
    prevA, prevB = start, start
    while a !== b && prevA !== b
        newPrevA = a
        newPrevB = b
        a = nextPipePos(input, a, prevA)
        b = nextPipePos(input, b, prevB)
        prevA = newPrevA
        prevB = newPrevB
        distA += 1
        distB += 1
    end
    distB # both are equal
end

function getNeighbours(pipe, pos, prev, right)::Vector{Tuple{Int, Int}}
    x, y = pos
    px, py = prev
    if right
        if pipe === '|' py < y ? [ (x-1, y) ] : [ (x+1, y) ]
        elseif pipe === '-' px < x ? [ (x, y+1) ] : [ (x, y-1) ]
        elseif pipe === 'L' py < y ? [ (x, y+1), (x-1, y) ] : []
        elseif pipe === 'J' px < x ? [ (x+1, y) , (x, y+1) ] : []
        elseif pipe === '7' py > y ? [ (x, y-1) , (x+1, y) ] : []
        elseif pipe === 'F' px > x ? [ (x-1, y) , (x, y-1) ] : []
        else throw(DomainError("getNeighbours we hit wrong pipe $(pipe) pos ($(x), $(y)) prev $(prev)"))
        end
    else throw(DomainError("left side not implemented yet"))
    end
end

# TODO make it work for both L and R
function traverseNeighbours(input, pos, prev, path, inside, startPipe)
    visited = Set{Tuple{Int, Int}}()
    # for now R only
    maxX = length(input[1])
    maxY = length(input)
    # filter out neighbours out of boundries when starting traversal
    pipe = input[pos[2]][pos[1]]
    neighbours = getNeighbours(pipe === 'S' ? startPipe : pipe, pos, prev, true) |> filter(t -> 1 <= t[1] <= maxX && 1 <= t[2] <= maxY)
    queue = Vector{Tuple{Int, Int}}(neighbours)
    while length(queue) > 0
        tile = pop!(queue)
        # do not process tiles on path or already checked to be inside
        if tile in path || tile in inside push!(visited, tile) end
        if tile in visited continue end
        if 1 < tile[1] < maxX && 1 < tile[2] < maxY
            push!(visited, tile)
            push!(inside, tile)
            tx, ty = tile
            nextTiles = [ (tx-1, ty), (tx+1, ty), (tx, ty-1), (tx, ty+1) ] |> filter(t -> !(t in visited || t in path || t in inside))
            push!(queue, nextTiles...)
        else
            # we detected we are on the wrong side of loop
            println("wrong side of the loop!")
            return false
        end
    end
    return true
end

function solveB(input)
    sy = findfirst(x -> findfirst('S', x) !== nothing, input)
    sx = findfirst('S', input[sy])
    start = (sx, sy)
    startPipe = inferStartPipe(input, start)
    a, b = getStartPoints(start, startPipe)
    prevA, prevB = start, start
    path = Set{Tuple{Int, Int}}([ start, a, b ])
    # populate path so we know which tiles are part of path
    while a !== b && prevA !== b
        newPrevA = a
        newPrevB = b
        a = nextPipePos(input, a, prevA)
        b = nextPipePos(input, b, prevB)
        push!(path, a)
        push!(path, b)
        prevA = newPrevA
        prevB = newPrevB
    end
    startPoints = getStartPoints(start, startPipe)
    inside = Set{Tuple{Int, Int}}()
    for startPoint in startPoints
        empty!(inside)
        pos = startPoint
        prev = start
        # traverse whole path, identify all inner tiles
        traverseNeighbours(input, start, prev, path, inside, startPipe)
        while pos != start
            newPrev = pos
            correctSide = traverseNeighbours(input, pos, prev, path, inside, startPipe)
            if !correctSide
                break
            end
            pos = nextPipePos(input, pos, prev)
            prev = newPrev
        end
        if pos === start break end
    end
    length(inside)
end

function main(file = "day10.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    # 7102
    println("Solving Day10A...")
    println(solveA(input))
    # 363
    println("Solving Day10B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day10s.txt"
    # file = "day10s2.txt"
    # file = "day10b1.txt"
    # file = "day10b2.txt"
    # file = "day10b3.txt"
    file = "day10.txt"
    println("file: $file")
    main(file)
end
