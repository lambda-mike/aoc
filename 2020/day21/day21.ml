open Base
module StrSet = Set.M (String)

type food = { allergens : StrSet.t; ingredients : StrSet.t }

type input = {
  allAllergens : StrSet.t;
  allIngredients : StrSet.t;
  food : food list;
}

(* mxmxvkd kfcds sqjhc nhms (contains dairy, fish) *)
let parseFood line =
  let separator = "#" in
  let ingredientsStr, allergensStr =
    line
    |> String.substr_replace_all ~pattern:")" ~with_:""
    |> String.substr_replace_all ~pattern:" (contains " ~with_:separator
    |> String.lsplit2_exn ~on:(Char.of_string separator)
  in
  let ingredients =
    ingredientsStr |> String.split ~on:' ' |> Set.of_list (module String)
  in
  let allergens =
    allergensStr
    |> String.substr_replace_all ~pattern:"," ~with_:""
    |> String.split ~on:' '
    |> Set.of_list (module String)
  in
  { ingredients; allergens }

let parseInput fName =
  let lines =
    In_channel.with_open_text fName In_channel.input_all |> String.split_lines
  in
  let acc =
    {
      allAllergens = Set.empty (module String);
      allIngredients = Set.empty (module String);
      food = [];
    }
  in
  lines
  |> List.fold ~init:acc ~f:(fun acc line ->
         let food = parseFood line in
         let newAllergens = Set.union food.allergens acc.allAllergens in
         let newIngredients = Set.union food.ingredients acc.allIngredients in
         (* order of the food does not matter - it will be reverse of what is in
            the file *)
         let newFood = food :: acc.food in
         {
           allAllergens = newAllergens;
           allIngredients = newIngredients;
           food = newFood;
         })

let potentialAllergens input ingredient =
  input.allAllergens
  |> Set.filter ~f:(fun allergen ->
         input.food
         |> List.exists ~f:(fun f ->
                Set.mem f.allergens allergen
                && not (Set.mem f.ingredients ingredient))
         |> not)

let findNonAllergicIngredients input =
  input.allIngredients
  |> Set.filter ~f:(fun i -> potentialAllergens input i |> Set.is_empty)

let solveA input =
  let nonAllergicIngredients = findNonAllergicIngredients input in
  input.food
  |> List.fold ~init:0 ~f:(fun total { ingredients = ins; allergens = _ } ->
         total + Set.count ins ~f:(Set.mem nonAllergicIngredients))

let identifyNextAllergen foods ingredients allergens =
  allergens |> Set.to_list
  |> List.fold ~init:[] ~f:(fun acc allergen ->
         let candidateIngredients =
           foods
           |> List.filter ~f:(fun f -> Set.mem f.allergens allergen)
           |> List.fold ~init:ingredients ~f:(fun ingredients food ->
                  Set.inter ingredients food.ingredients)
         in
         (allergen, candidateIngredients) :: acc)
  |> List.find_map_exn ~f:(fun (allergen, ings) ->
         if
           Set.length ings = 1
           (* It has only one element so we can use Set.choose *)
         then ings |> Set.choose |> Option.map ~f:(fun i -> (allergen, i))
         else None)

let rec identifyAllergens known foods ingredients allergens =
  if Set.is_empty allergens then known
  else
    let allergen, ingredient =
      identifyNextAllergen foods ingredients allergens
    in
    let newKnown = (allergen, ingredient) :: known in
    let newIngredients = Set.remove ingredients ingredient in
    let newAllergens = Set.remove allergens allergen in
    let newFoods =
      foods
      |> List.map ~f:(fun f ->
             { f with ingredients = Set.remove f.ingredients ingredient })
    in
    identifyAllergens newKnown newFoods newIngredients newAllergens

let formatSolution solution =
  solution
  |> List.sort ~compare:(fun (a1, _) (a2, _) -> String.compare a1 a2)
  |> List.map ~f:snd |> String.concat ~sep:","

let solveB input =
  let nonAllergicIngredients = findNonAllergicIngredients input in
  let allergicFoods =
    input.food
    |> List.map ~f:(fun food ->
           {
             food with
             ingredients = Set.diff food.ingredients nonAllergicIngredients;
           })
  in
  let allergicIngredients =
    Set.diff input.allIngredients nonAllergicIngredients
  in
  let solution =
    identifyAllergens [] allergicFoods allergicIngredients input.allAllergens
  in
  solution |> formatSolution

(* main *)
let input = parseInput "input.txt"

(* 1882 *)
let () = Stdio.print_endline "Solving Day21A..."
let () = solveA input |> Int.to_string |> Stdio.print_endline

(* xgtj,ztdctgq,bdnrnx,cdvjp,jdggtft,mdbq,rmd,lgllb *)
let () = Stdio.print_endline "Solving Day21B..."
let () = solveB input |> Stdio.print_endline
