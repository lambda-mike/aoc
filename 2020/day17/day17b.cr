enum Cube
  Inactive
  Active
end

alias Point = {x: Int32, y: Int32, z: Int32, w: Int32}

def parse_cube(c : Char) : Cube | Nil
  case c
  when '.'
    Cube::Inactive
  when '#'
    Cube::Active
  end
end

def parse_puzzle(fileName) : Hash(Point, Cube)
  lines = File.read_lines(fileName)
  cubes = {} of Point => Cube
  lines.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      if cube = parse_cube char
        cubes[{x: x, y: y, z: 0, w: 0}] = cube
      else
        raise "Unknown pocket dimension cube state coming from the input: #{char}"
      end
    end
  end
  cubes
end

def count_active(cubes : Hash(Point, Cube)) : Int32
  cubes.count { |_, cube| cube == Cube::Active }
end

def generate_neighbours(point : Point) : Array(Point)
  neighbours = [] of Point
  (-1..1).each do |x|
    (-1..1).each do |y|
      (-1..1).each do |z|
        (-1..1).each do |w|
          pos = {x: point[:x] + x, y: point[:y] + y, z: point[:z] + z, w: point[:w] + w}
          neighbours << pos unless x == 0 && y == 0 && z == 0 && w == 0
        end
      end
    end
  end
  neighbours
end

def cycle(n : Int32, cubes_input : Hash(Point, Cube)) : Hash(Point, Cube)
  # use two hash maps interchangeably for every iteration: odd and even
  cubes_odd = cubes_input.clone
  cubes_even = cubes_input.clone
  (1..n).each do |i|
    cubes, prev = i % 2 == 0 ? {cubes_even, cubes_odd} : {cubes_odd, cubes_even}
    cubes.clear
    prev.each do |point, cube|
      # do not calculate the same cube again
      unless cubes.has_key?(point)
        neighbours = generate_neighbours(point)
        # TODO instead of counting neighbours loop and break once threshold is crossed > 3;
        # no point counting other neighbours
        case cube
        when Cube::Inactive
          # if cube is inactive, it can become active only when exactly
          # 3 neighbours are active
          active_neighbours = neighbours.count { |pos| prev[pos]? == Cube::Active }
          cubes[point] = Cube::Active if 3 == active_neighbours
        when Cube::Active
          active_neighbours_size = 0
          # if cube is active, check all it's inactive neighbours - whether they need to be active
          neighbours.each do |pos|
            case prev[pos]?
            when Cube::Inactive, nil
              # All neighbours of the inactive cube which is neighbour of
              # the active cube being processed in the main loop
              # TODO extract to a helper fn and re-use in both blocks
              inactive_cube_neighbours = generate_neighbours(pos)
              active_neighbours = inactive_cube_neighbours.count { |p| prev[p]? == Cube::Active }
              cubes[pos] = Cube::Active if 3 == active_neighbours
            else
              active_neighbours_size += 1
            end
          end
          # if cube is active it remains active only if 2 or 3 neighbours are active
          case active_neighbours_size
          when 2, 3
            cubes[point] = Cube::Active
          end
        end
      end
    end
  end
  n % 2 == 0 ? cubes_even : cubes_odd
end

# Count how many cubes are active after cyclesNum cycles
def solve_b(cyclesNum : Int32, input : Hash(Point, Cube)) : Int32
  cubes = cycle(cyclesNum, input)
  count_active cubes
end

cyclesNum = 6
# input = parse_puzzle "sample.txt"
input = parse_puzzle "input.txt"
# 1972
puts "Solving Day17B..."
puts solve_b(cyclesNum, input)
