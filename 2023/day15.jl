abstract type Instruction end

struct Add <: Instruction
    label::String
    len::Int
end

struct Rem <: Instruction
    label::String
end

function calcHash(str::AbstractString)::Int
    result = 0
    for c in str
        result += Int(c)
        result *= 17
        result = result % 256
    end
    result
end

function solveA(input)
    steps = split(input, ",")
    [ calcHash(s) for s in steps ] |> sum
end

function parseStep(str)::Instruction
    if str[end] == '-'
        return Rem(join(str[1:end-1]))
    else
        label = str[1:end - 2]
        len = parse(Int, str[end])
        return Add(join(label), len)
    end
end

function placeLensInBoxes!(hashmap::Dict{String, Int}, instructions::Vector{Instruction}, boxes::Vector{Vector{Add}})
    for instruction in instructions
        n = hashmap[instruction.label]
        box = boxes[n + 1]
        i = findfirst(x -> x.label == instruction.label, box)
        if instruction isa Add
            if i === nothing
                push!(box, instruction)
            else
                box[i] = instruction
            end
        else # Rem
            i !== nothing &&  deleteat!(box, i)
        end
    end
end

calculateFocusingPower(box::Int, slot::Int, len::Int) = box * slot * len

function solveB(input)
    boxesN = 256
    steps = split(input, ",")
    instructions = [ parseStep(s) for s in steps ]
    hashmap = Dict([ (i.label, calcHash(i.label)) for i in instructions ])
    boxes::Vector{Vector{Add}} = repeat([Vector{Add}[]], boxesN)
    placeLensInBoxes!(hashmap, instructions, boxes)
    result = 0
    for b in 1:boxesN
        for (i, lens) in enumerate(boxes[b])
            result += calculateFocusingPower(b, i, lens.len)
        end
    end
    return result
end

function main(file = "day15.txt")
    # input = collect(eachline(file))
    input = strip(read(file, String))
    # 495972
    println("Solving Day15A...")
    println(solveA(input))
    # 245223
    println("Solving Day15B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    # file = "day15s.txt"
    file = "day15.txt"
    println("file: $file")
    main(file)
end
