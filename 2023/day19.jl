@enum Rating X M A S

@enum Op LT GT

struct Part
    x::Int
    m::Int
    a::Int
    s::Int
end

abstract type Target end
struct TargetWorkflow <: Target
    name::String
end
struct TargetAccepted <: Target end
struct TargetRejected <: Target end

abstract type Rule end
struct Rule3 <: Rule
    rating::Rating
    op::Op
    num::Int
    target::Target
end
struct Rule1 <: Rule
    target::Target
end

struct Workflow
    name::String
    rules::Vector{Rule}
end

function parseTarget(s::AbstractString)::Target
    if s == "A" return TargetAccepted()
    elseif s == "R" return TargetRejected()
    else TargetWorkflow(s)
    end
end

function parseOp(op::Char)::Op
    return op == '<' ? LT : GT
end

function parseRating(s::AbstractString)::Rating
    if s == "x" return X
    elseif s == "m" return M
    elseif s == "a" return A
    else return S
    end
end

function parseRule(str::AbstractString)::Rule
    tokens = split(str, [ '<', '>', ':' ])
    if length(tokens) == 1
        return Rule1(parseTarget(tokens[1]))
    else
        rating = parseRating(tokens[1])
        op = parseOp(str[length(tokens[1]) + 1])
        num = parse(Int, tokens[2])
        target = parseTarget(tokens[3])
        return Rule3(rating, op, num, target)
    end
end

function parseInput(input)::Tuple{Vector{Workflow}, Vector{Part}}
    partsStartIndex = 0
    workflows = Workflow[]
    parts = Part[]
    for (i, line) in enumerate(input)
        if length(line) == 0 partsStartIndex = i + 1; break end
        rulesSectionIndex = findfirst('{', line)
        name = line[1:rulesSectionIndex-1]
        rulesSection = line[rulesSectionIndex+1:end-1]
        rules = Rule[]
        for r in split(rulesSection, ",")
            rule = parseRule(r)
            push!(rules, rule)
        end
        push!(workflows, Workflow(name, rules))
    end
    for pStr in input[partsStartIndex:end]
        tokens = split(pStr, [ '{', '}', ',' ])
        x = parse(Int, tokens[2][3:end])
        m = parse(Int, tokens[3][3:end])
        a = parse(Int, tokens[4][3:end])
        s = parse(Int, tokens[5][3:end])
        part = Part(x, m, a, s)
        push!(parts, part)
    end
    (workflows, parts)
end

function getPartRating(part::Part, rating::Rating)::Int
    if rating == X return part.x
    elseif rating == M return part.m
    elseif rating == A return part.a
    else return part.s
    end
end

function sendToWorkflow(workflowsDict::Dict{String, Workflow}, name::String, part::Part)::Union{TargetAccepted, TargetRejected}
    result::Target = TargetWorkflow(name)
    while result != TargetRejected() && result !== TargetAccepted()
        workflow = workflowsDict[result.name]
        for r in workflow.rules
            if r isa Rule1
                result = r.target
            else
                rating = getPartRating(part, r.rating)
                if r.op == LT
                    rating >= r.num && continue
                else
                    rating <= r.num && continue
                end
                result = r.target
            end
            break
        end
    end
    return result
end

function solveA(input)
    workflows, parts = parseInput(input)
    workflowsDict::Dict{String, Workflow} = map(x -> (x.name, x), workflows) |> Dict
    return [ sum([ p.x, p.m, p.a, p.s ]) for p in parts if TargetAccepted() == sendToWorkflow(workflowsDict, "in", p) ] |> x -> sum(x; init = 0)
end

function mergeRule(rule::Rule3, rng::Tuple{Int, Int})::Tuple{Int, Int}
    a, b = rng
    if rule.op == LT
        rule.num > b && return rng
        rule.num in a:b && return (a, rule.num - 1)
        pritnln("mergeRule WRN $rule ($a - $b)")
        return (0, 0)
    else
        rule.num < a && return rng
        rule.num in a:b && return (rule.num + 1, b)
        pritnln("mergeRule WRN $rule ($a - $b)")
        return (0, 0)
    end
end

function mergeRules(acc, rule::Rule3)
    if rule.rating == X return (; acc..., x = mergeRule(rule, acc.x))
    elseif rule.rating == M return (; acc..., m = mergeRule(rule, acc.m))
    elseif rule.rating == A return (; acc..., a = mergeRule(rule, acc.a))
    else return (; acc..., s = mergeRule(rule, acc.s))
    end
end

function reverseRule(rule::Rule3)::Rule3
    if rule.op == LT
        Rule3(rule.rating, GT, rule.num - 1, rule.target)
    else
        Rule3(rule.rating, LT, rule.num + 1, rule.target)
    end
end

function solveB(input)
    workflows, _ = parseInput(input)
    dict::Dict{String, Workflow} = map(x -> (x.name, x), workflows) |> Dict
    # vector of workflow names
    queue = Tuple{String, Vector{Rule}}[ ("in", []) ]
    results = Vector{Rule3}[]
    while length(queue) > 0
        name, sourceRules = pop!(queue)
        # copy of rules accumulated from previous workflows
        rules = copy(sourceRules)
        # all rules accumulated for this workflow
        # next rules are added to previous rules which should already be reversed (LT <-> GT)
        prevRules = Rule3[]
        for r in dict[name].rules
            newRules = [ rules..., prevRules... ]
            if r.target isa TargetAccepted
                if r isa Rule3
                    push!(newRules, r)
                end
                push!(results, newRules)
            end
            if r.target isa TargetWorkflow
                if r isa Rule3
                    push!(newRules, r)
                end
                push!(queue, (r.target.name, newRules))
            end
            r isa Rule3 && push!(prevRules, reverseRule(r))
        end
    end
    combinations = 0
    limit = 4000
    for rules in results
        (; x, m, a, s) = reduce((acc, a) -> mergeRules(acc, a), rules; init = (x = (1, limit), m = (1, limit), a = (1, limit), s = (1, limit)))
        combinations += prod([ z[2] - z[1] + 1 for z in [ x, m, a ,s ]])
    end
    combinations
end

function main(file = "day19.txt")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    # 446935
    println("Solving Day19A...")
    println(solveA(input))
    # 141882534122898
    println("Solving Day19B...")
    println(solveB(input))
end

if !isinteractive()
    main("day19s.txt")
end

function repl()
    # file = "day19s.txt"
    file = "day19.txt"
    println("file: $file")
    main(file)
end
