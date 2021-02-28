class BAG
   -- Bag has a colour and a dictionary of the bags it contains and their count

create {ANY}
  make, parse

feature {ANY}
  colour: STRING
  bags: HASHED_DICTIONARY[INTEGER, STRING]

  make (col: STRING; dict: HASHED_DICTIONARY[INTEGER, STRING])
    do
      colour := col
      bags := dict
    end

  parse (input: STRING)
    local
      stream: STRING_INPUT_STREAM
      no_bags: BOOLEAN
      n: INTEGER
      bag_colour: STRING
    do
      create colour.make_empty
      create bags.make
      create stream.from_string(input)
      -- pattern
      stream.read_word
      colour.append(stream.last_string)
      colour.append(" ")
      -- colour
      stream.read_word
      colour.append(stream.last_string)
      -- "bags"
      stream.read_word
      -- "contain"
      stream.read_word
      from
      until
        no_bags or else stream.end_of_input
      loop
        stream.read_integer
        if not stream.valid_last_integer then
          no_bags := True
        else
          n := stream.last_integer
          create bag_colour.make_empty
          stream.read_word
          bag_colour.append(stream.last_string)
          bag_colour.append(" ")
          stream.read_word
          bag_colour.append(stream.last_string)
          bags.put(n, bag_colour)
          -- bag(s)
          stream.read_word
          if stream.last_string.has('.') then
            no_bags := True
          end
        end
      end
    end

end -- class BAG

