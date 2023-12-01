function solveA(input)
    result = 0
    for line in input
        digits = filter(x -> !(x in 'a':'z'), line)
        result += parse(Int, "$(digits[begin])$(digits[end])")
    end
    result
end

# 53255 not right
function solveB(input)
    words = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
    result = 0
    for line in input
        digits = Int[]
        index = 1
        while index <= length(line)
            c = line[index]
            if c in '1':'9'
                push!(digits, parse(Int, c))
            else
                for (n, word) in enumerate(words)
                    if startswith(line[index:end], word)
                        push!(digits, n)
                        break
                    end
                end
            end
            index += 1
        end
        num = 10 * digits[begin] + digits[end]
        result += num
    end
    result
end

function main(file = "day01.txt")
    println("file $file")
    input = collect(eachline(file))
    # 53080
    println("Solving Day01A...")
    println(solveA(input))
    # 53268
    println("Solving Day01B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # file = "day01s.txt"
    # file = "day01sb.txt"
    # file = "test.txt"
    file = "day01.txt"
    main(file)
end
