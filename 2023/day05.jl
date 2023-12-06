struct Mapping
    dst::Int64
    src::Int64
    len::Int64
    delta::Int64
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
    dst, src, len = map(x -> parse(Int, x), split(line))
    Mapping(dst, src, len, dst - src)
end

function parseDict(section)::Vector{Mapping}
    [ parseMapping(l) for l in split(section, "\n")[2:end] ] |> xs -> sort(xs, by=((; src),) -> src, rev=true)
end

function parseInput(input)
    sections = split(input, "\n\n")
    seeds = map(x -> parse(Int64, x), split(sections[1])[2:end]) |> xs -> sort(xs; rev=true)
    seeds2soil = parseDict(sections[2])
    soil2fert = parseDict(sections[3])
    fert2water = parseDict(sections[4])
    water2light = parseDict(sections[5])
    light2temp = parseDict(sections[6])
    temp2hum = parseDict(sections[7])
    hum2loc = parseDict(sections[8])
    Almanac(seeds, seeds2soil, soil2fert, fert2water, water2light, light2temp, temp2hum, hum2loc)
end

function findDestination(mappings::Vector{Mapping}, start::Int)::Int
    # println("dbg findDestination ", mappings, " ", start)
    # i = findfirst(x -> 0 <= start - x.src <= x.len - 1, mappings)
    # i = searchsortedfirst(mappings, true; by=(x -> 0 <= start - x.src <= x.len - 1))
    # i = searchsortedfirst(mappings, ((x=4),), by=((; x),) -> x)
    i = searchsortedfirst(mappings, start; lt=(s, (;src,)) -> s < src, rev=true)
    # TODO implement binary search myself
    # Any source numbers that aren't mapped correspond to the same destination number
    # if i == 0 || i > length(mappings)
    # println()
    # println("($(start)): $(i)")
    if i > length(mappings)
        # println("no mapping found ", mappings)
        start
    else
        mapping = mappings[i]
        # println("mapping found: ", mapping)
        delta = start - mapping.src
        if delta <= mapping.len - 1
            # println("src found ", mapping.src, " dst: $(mapping.dst + delta)")
            mapping.dst + delta
        else
            # start not within len from src
            start
        end
    end
end

# find location by going through each map
function traverseSeed(almanac::Almanac, seed::Int64)::Int64
    soil = findDestination(almanac.seeds2soil, seed)
    fert = findDestination(almanac.soil2fert, soil)
    water = findDestination(almanac.fert2water, fert)
    light = findDestination(almanac.water2light, water)
    temp = findDestination(almanac.light2temp, light)
    hum = findDestination(almanac.temp2hum, temp)
    loc = findDestination(almanac.hum2loc, hum)
    loc
end

function solveA(input)
    almanac = parseInput(input)
    # println(almanac)
    [ @time traverseSeed(almanac, seed) for seed in almanac.seeds ] |> minimum
end

function solveB(input)
    almanac = parseInput(input)
    result = typemax(Int)
    seedRanges = Iterators.partition(almanac.seeds, 2)
    for seedRange in seedRanges
        println(seedRange)
        start, len = seedRange
        for seed in start:(start + len - 1)
            loc = traverseSeed(almanac, seed)
            # loc = 3
            result = min(result, loc)
        end
    end
    result
end

function main(file = "day05.txt")
    # input = collect(eachline(file))
    input = strip(read(file, String))
    # 403695602
    println("Solving Day05A...")
    println(solveA(input))
    # 219529182
    println("Solving Day05B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day05s.txt"
    file = "day05.txt"
    println("repl - file: $file")
    main(file)
end
