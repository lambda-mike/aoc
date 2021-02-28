class DAY07
   --
   -- Advent of Code 2020 Day07
   --
   -- To compile type command: se c day07 -o day07
   --
   -- To compile an optimized version type : se c day07 -o day07 -boost -O2
   --

create {ANY}
   main

feature {ANY}
   main
      local
        file_name: STRING
        input: ARRAY[STRING]
        bags: LINKED_HASHED_DICTIONARY[BAG, STRING]
        result_a: INTEGER
        result_b: INTEGER
      do
        --file_name := "sample.txt"
        file_name := "input.txt"
        input := read_file(file_name)
        bags := parse_input(input)
        create solver.init(bags)

        -- 337
        io.put_string("Solving Day07A...%N")
        result_a := solver.solve_a
        io.put_integer(result_a)
        io.put_new_line

        -- 50100
        io.put_string("Solving Day07B...%N")
        result_b := solver.solve_b
        io.put_integer(result_b)
        io.put_new_line

      end

feature {}
  solver: SOLVER

  read_file (name: STRING): ARRAY[STRING]
    local
      file: TEXT_FILE_READ
    do
      create Result.default_create
      create file.connect_to(name)
      if file.is_connected then
        from
        until
          file.end_of_input
        loop
          file.read_line
          if not file.last_string.is_empty then
            Result.add_last(file.last_string.twin)
          end
        end
        file.disconnect
      end
    end

  parse_input (input: ARRAY[STRING]): LINKED_HASHED_DICTIONARY[BAG, STRING]
    local
      bag: BAG
      i: INTEGER
    do
      create Result.make
      from
        i := input.lower
      until
        i > input.upper
      loop
        bag := Void
        create bag.parse(input @ i)
        Result.put(bag, bag.colour)
        i := i + 1
      end
    end

end -- class DAY07
