class SOLVER
   -- Solve Day07 part I & II

create {ANY}
  init

feature {ANY}

  init (dict: LINKED_HASHED_DICTIONARY[BAG, STRING])
    do
      bags := dict
    end

  solve_a: INTEGER
    -- calculate how many bags contain target bag (directly or indirectly)
    local
      -- Queue of bags to check
      queue: LINKED_LIST[STRING]
      -- Bags containing target bag (directly or indirectly):
      containing: HASHED_SET[STRING]
      visited: HASHED_SET[STRING]
      -- current bag
      bag: STRING
    do
      create queue.make
      create containing.make
      create visited.make
      from
        init_queue(queue)
      until
        queue.is_empty
      loop
        bag := queue.first
        queue.remove_first
        classify_bag (containing, visited, bag)
      end
      Result := containing.count
    end

  solve_b: INTEGER
    -- count how many bags are inside traget bag in total
    local
      -- Map of bags and their total inner bags count
      bags_totals: HASHED_DICTIONARY[INTEGER, STRING]
      path: LINKED_LIST[STRING]
    do
      create bags_totals.make
      create path.make
      capture_traversal_path (path)
      calculate_totals (path, bags_totals)
      Result := bags_totals @ target
    end

feature {}
  -- Dictionary of all bags
  bags: LINKED_HASHED_DICTIONARY[BAG, STRING]
  target: STRING "shiny gold"

  calculate_totals (path: LINKED_LIST[STRING]; bags_totals: HASHED_DICTIONARY[INTEGER, STRING])
    -- traverse path and populate bags_totals
    local
      bag: STRING
    do
      from
      until
        path.is_empty
      loop
        bag := path.first
        path.remove_first
        if not bags_totals.has(bag) then
          calculate_bag_total (bag, bags_totals)
        end
      end
    end

  calculate_bag_total (bag: STRING; bags_totals: HASHED_DICTIONARY[INTEGER, STRING])
    local
      bag_obj: BAG
      bag_keys: TRAVERSABLE[STRING]
      bag_total: INTEGER
      i: INTEGER
      inner_bag: STRING
      inner_bag_count: INTEGER
      inner_bag_total_children: INTEGER
    do
      bag_obj := bags @ bag
      -- children bags
      bag_keys := bag_obj.bags.keys
      from
        i := bag_keys.lower
      until
        i > bag_keys.upper
      loop
        inner_bag := bag_keys.item(i)
        inner_bag_count := bag_obj.bags @ inner_bag
        inner_bag_total_children := bags_totals @ inner_bag
        -- increase total bags couns by: number of bags with pattern "inner_bag"
        -- multiplied by number of bags in bag "inner_bag" incremented by one
        bag_total := bag_total + inner_bag_count * (1 + inner_bag_total_children)
        i := i + 1
      end
      bags_totals.add(bag_total, bag)
    end

  capture_traversal_path (path: LINKED_LIST[STRING])
    -- traverse tree of target bag
    -- save every consumed node by prepending to path
    local
      -- Queue of bags to check
      queue: LINKED_LIST[STRING]
      bag: STRING
    do
      create queue.make
      from
        queue.add_first(target)
      until
        queue.is_empty
      loop
        bag := queue.first
        queue.remove_first
        path.add_first(bag)
        add_inner_bags_to_queue (queue, bag)
      end
    end

  classify_bag (containing: HASHED_SET[STRING]; visited: HASHED_SET[STRING]; bag: STRING)
    local
      -- Queue of bags to check
      queue: LINKED_LIST[STRING]
      current_bag: STRING
    do
      create queue.make
      from
        queue.add_first(bag)
      until
        queue.is_empty
      loop
        current_bag := queue.first
        queue.remove_first
        if containing.has(current_bag) then
          containing.add(bag)
          queue.clear_count
        end
        if not visited.has(current_bag) then
          if (bags @ current_bag).bags.has(target) then
            containing.add(bag)
            queue.clear_count
          else
            add_inner_bags_to_queue (queue, current_bag)
          end
        end
      end
      visited.add(bag)
    end

  add_inner_bags_to_queue (queue: LINKED_LIST[STRING]; bag: STRING)
    -- add bag's children (bags) to queue (except target)
    local
      bag_obj: BAG
      bag_children: TRAVERSABLE[STRING]
      i: INTEGER
      key: STRING
    do
      bag_obj := bags @ bag
      bag_children := bag_obj.bags.keys
      from
        i := bag_children.upper
      until
        i < bag_children.lower
      loop
        key := bag_children.item(i)
        if not key.is_equal(target) then
          queue.add_first(key)
        end
        i := i - 1
      end
    end

  init_queue (queue: LINKED_LIST[STRING])
    local
      i: INTEGER
      key: STRING
    do
      from
        create key.make_empty
        i := bags.lower
      until
        i > bags.upper
      loop
        key := bags.key(i)
        if not key.is_equal(target) then
          queue.add_last(key)
        end
        i := i + 1
      end
    end

end -- class SOLVER_A
