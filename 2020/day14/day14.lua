-- Advent of Code Day14

function setMask(str, mask)
  --print("setMask mask", str)
  local zeros = 0
  local ones = 0
  local i = 1<<35
  for b in string.gmatch(str, ".") do
    if b == "1" then
      ones = ones | i
    elseif b == "0" then
      zeros = zeros | i
    end
    i = i>>1
  end
  mask.zeros = zeros
  mask.ones = ones
  --print("mask set", "0s", mask.zeros, "1s", mask.ones)
end

function applyMask(mask, n)
  --print("applying mask", mask.zeros, mask.ones, n)
  return (n | mask.ones) ~ (n & mask.zeros)
end

function sumMemoryCells(memory)
  local result = 0
  for cell in pairs(memory) do
    --print("memory", a, memory[a])
    result = result + memory[cell]
  end
  return result
end

function solveA(input)
  local memory = {}
  -- we store all 1s and 0s (as 1s) in table
  local mask = { zeros = 0, ones = 0, }
  for line in io.lines(input) do
    for m in string.gmatch(line, "mask = (.+)") do
      setMask(m, mask)
    end
    for a, v in string.gmatch(line, "mem%[(%d+)%] = (%d+)") do
      if a and v then
        local addr = tonumber(a)
        local value = tonumber(v)
        --print("addr:", addr, "value:", value)
        memory[addr] = 0
        memory[addr] = applyMask(mask, value)
      end
    end
  end
  return sumMemoryCells(memory)
end

-- mask = { str = "0X1X...", 1 = A, 2 = B, ... up to N = Z, num = N, base = baseAddr | ones }
-- str is mask read from input as string
-- num is Xs count
-- 1 to num are numbers with 1s in positions of Xs
-- base is address read from input with properly set 1s (from the mask)
function initMask(maskStr, baseAddr)
  -- Remove artifacts from previous interations
  local mask = { str = maskStr }
  --print("initMask", mask.str, baseAddr)
  local xCount = 0
  local b = 1<<35
  local base = baseAddr
  -- every mask contains exactly 36bits
  for char in string.gmatch(mask.str, ".") do
    if char == "X" then
      xCount = xCount + 1
      mask[xCount] = b
      -- remove 1 in base addr if it was present in the place of X
      base = base ~ (base & b)
    elseif char == "1" then
      base = base | b
    end
    b = b>>1
  end
  mask.num = xCount
  mask.base = base
  --print("mask: ", mask.str, mask.num, mask.base, mask[1], mask[2], mask[3])
  return mask
end

-- combination bits define x pos indices (stored in the mask)
function generateAddress(mask, combination)
  local addr = mask.base
  local xPosIndex = 1
  local i = combination
  while i > 0 do
    if 1 == (i & 1) then addr = addr | mask[xPosIndex] end
    i = i>>1
    xPosIndex = xPosIndex + 1
  end
  return addr
end

function solveB(input)
  local memory = {}
  -- store positions of Xs plus their count and base addr
  -- base addr is equal to all Xs equal to 0
  local mask = {}
  for line in io.lines(input) do
    for m in string.gmatch(line, "mask = (.+)") do
      mask = { str = m }
    end
    for a, v in string.gmatch(line, "mem%[(%d+)%] = (%d+)") do
      if a and v then
        local addr = tonumber(a)
        local value = tonumber(v)
        --print(addr, value)
        mask = initMask(mask.str, addr)
        local limit = 2^mask.num
        for i=1,limit do
          local combination = i - 1
          local nextAddr = generateAddress(mask, combination)
          --print(i, "assign", value, "to", nextAddr)
          memory[nextAddr] = value
        end
      end
    end
  end
  return sumMemoryCells(memory)
end

function main()

  local input = "input.txt"
  --local input = "sample.txt"
  --local input = "sample2.txt"

  -- 8332632930672
  print("Solving Day14A...")
  print(solveA(input))

  -- 4753238784664
  print("Solving Day14B...")
  print(solveB(input))
end

main()
