struct Mapping
    src::Int64
    dst::Int64
    len::Int64
end

struct Almanac
    seeds::Vector{Int64}
    seeds2soil::Vector{Mapping}
    soil2fert::Vector{Mapping}
    fert2water::Vector{Mapping}
    water2light::Vector{Mapping}
    light2temp::Vector{Mapping}
    temp2hum::Vector{Mapping}
    hum2loc::Vector{Mapping}
end

function parseMapping(line)
    dst, src, len = split(line)
    Mapping(parse(Int, dst), parse(Int, src), parse(Int, len))
end

function parseDict(section)::Vector{Mapping}
    [  parseMapping(l) for l in split(section, "\n")[2:end] ]
end

function parseInput(input)
    sections = split(input, "\n\n")
    seeds = map(x -> parse(Int64, x), split(sections[1])[2:end])
    seeds2soil = parseDict(sections[2])
    soil2fert = parseDict(sections[3])
    fert2water = parseDict(sections[4])
    water2light = parseDict(sections[5])
    light2temp = parseDict(sections[6])
    temp2hum = parseDict(sections[7])
    hum2loc = parseDict(sections[8])

    # println(sections[2])
    Almanac(seeds, seeds2soil, soil2fert, fert2water, water2light, light2temp, temp2hum, hum2loc)
end

function solveA(input)
    almanac = parseInput(input)
    println(almanac)
    0
end

function solveB(input)
    0
end

function main(file = "day05.txt")
    # input = collect(eachline(file))
    input = strip(read(file, String))
    println("Solving Day05A...")
    println(solveA(input))
    println("Solving Day05B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    file = "day05s.txt"
    # file = "day05.txt"
    println("repl - file: $file")
    main(file)
end
