// TODO solveB: parse strings as numbers, remove duplication, create helper fns

// Library functions can be found here:
// https://ats-lang.github.io/DOCUMENT/ATS-Postiats/prelude/HTML/ATSPRE_all_in_one.html

#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
staload
UN = "prelude/SATS/unsafe.sats"

typedef
passdata = @{
  fieldsNum= int,
  optionalFieldPresent= bool
}

datatype parse_mode =
  | ParseField of ()
  | ParseValue of ()

val maxFieldsNum = 8
val optionalField = "cid"

fun
addFields(pd: passdata, n: int): passdata = @{
  fieldsNum= pd.fieldsNum + n,
  optionalFieldPresent= pd.optionalFieldPresent
} : passdata

fun
mergeOptionalFieldStatus(pd: passdata, isPresent: bool): passdata = @{
  fieldsNum= pd.fieldsNum,
  optionalFieldPresent= pd.optionalFieldPresent || isPresent
} : passdata

fun
count_fields{n:nat}
  (line: list_vt (charNZ, n)): int = let
  fun loop
    {n:nat}
    (chars: list_vt(charNZ, n), i: int): int =
    case+ chars of
    | ~list_vt_cons(c, cs) =>
      if (c = ':') then
      // if int_of_char(c) == int_of_char(':') then
        loop(cs, i + 1)
      else
        loop(cs, i)
    | ~list_vt_nil () => i
  in
    loop(line, 0)
end

fun
isPassportFieldsNumValid(pass: passdata): bool = let
  val allFieldsPresent = pass.fieldsNum = maxFieldsNum
  val onlyOptionalFieldMissing =
    pass.fieldsNum = maxFieldsNum - 1 && ~pass.optionalFieldPresent
in allFieldsPresent || onlyOptionalFieldMissing
end

// strstr is searching for the given word in string and returning index >=0 if found
fun
solveA(fileRef: FILEref): int = let
  val passportData = @{
    fieldsNum= 0,
    optionalFieldPresent= false
    } : passdata
in read_lines(passportData, 0)
end where { // end of solveA
fun read_lines(passport: passdata, validPassports: int): int = let
in
  if fileref_isnot_eof(fileRef) then let
    val line = fileref_get_line_string(fileRef)
    val str = $UN.strptr2string(line)
    val lineLength = string_length(str)
    val isOptionalFieldPresent = strstr(str, optionalField) >= 0
    val fieldsNum = count_fields(string_explode(str))
    val () = strptr_free(line)
  in if lineLength = 0 then let
      // passport validated increment counters
      val emptyPass = @{ fieldsNum=0, optionalFieldPresent= false }: passdata
      val newValidPassports = if isPassportFieldsNumValid(passport)
        then succ(validPassports)
        else validPassports
    in read_lines(emptyPass, newValidPassports)
    end else let
      val updatedPass = addFields(passport, fieldsNum)
      val updatedPass = mergeOptionalFieldStatus(updatedPass, isOptionalFieldPresent)
    in read_lines(updatedPass, validPassports)
    end // if lineLength
  end else // if fileref_isnot_eof
    validPassports
end // end of read_line
} // end of solveA where

// ******
// SolveB
// ******

// four digits; at least 1920 and at most 2002.
fun validateByr{n:nat}(value: string(n)): bool =
let
  val chars = string_explode(value)
  val charsLst = list_of_list_vt(chars)
in case+ charsLst of
| cons('1', cons('9', cons(c, cons(d, nil()))))
  when isdigit(c) && c >= '2' && isdigit(d) => true
| cons('2', cons('0', cons('0', cons(d, nil())))) when (d='0') || (d='1') || (d='2') => true
| _ => false
end
// validateByr tests
val () = {
  val () = assertloc(~validateByr("1919"))
  val () = assertloc(validateByr("1920"))
  val () = assertloc(validateByr("1921"))
  val () = assertloc(validateByr("1999"))
  val () = assertloc(validateByr("2000"))
  val () = assertloc(validateByr("2001"))
  val () = assertloc(validateByr("2002"))
  val () = assertloc(~validateByr("2003"))
}

// four digits; at least 2010 and at most 2020.
fun validateIyr{n:nat}(value: string(n)): bool =
let
  val chars = string_explode(value)
  val charsLst = list_of_list_vt(chars)
in case+ charsLst of
| cons('2', cons('0', cons('2', cons('0', nil())))) => true
| cons('2', cons('0', cons('1', cons(d, nil())))) when isdigit(d) => true
| _ => false
end
// validateIyr tests
val () = {
  val () = assertloc(~validateIyr("1919"))
  val () = assertloc(~validateIyr("2009"))
  val () = assertloc(validateIyr("2010"))
  val () = assertloc(validateIyr("2011"))
  val () = assertloc(validateIyr("2019"))
  val () = assertloc(validateIyr("2020"))
  val () = assertloc(~validateIyr("2021"))
}

// four digits; at least 2020 and at most 2030.
fun validateEyr{n:nat}(value: string(n)): bool =
let
  val chars = string_explode(value)
  val charsLst = list_of_list_vt(chars)
in case+ charsLst of
| cons('2', cons('0', cons('3', cons('0', nil())))) => true
| cons('2', cons('0', cons('2', cons(d, nil())))) when isdigit(d) => true
| _ => false
end
// validateEyr tests
val () = {
  val () = assertloc(~validateEyr("1919"))
  val () = assertloc(~validateEyr("2019"))
  val () = assertloc(validateEyr("2020"))
  val () = assertloc(validateEyr("2021"))
  val () = assertloc(validateEyr("2029"))
  val () = assertloc(validateEyr("2030"))
  val () = assertloc(~validateEyr("2031"))
}

// hgt (Height) - a number followed by either cm or in:
// If cm, the number must be at least 150 and at most 193.
// If in, the number must be at least 59 and at most 76.
fun validateHgt{n:nat}(value: string(n)): bool =
let
  val chars = string_explode(value)
  val charsLst = list_of_list_vt(chars)
in case+ charsLst of
| cons('1', cons(b, cons(c, cons('c', cons('m', nil()))))) when b >= '5' && b <= '8' && isdigit(c) => true
| cons('1', cons('9', cons(c, cons('c', cons('m', nil()))))) when c >= '0' && c <= '3' => true
| cons('5', cons('9', cons('i', cons('n', nil())))) => true
| cons('6', cons(b, cons('i', cons('n', nil())))) when isdigit(b) => true
| cons('7', cons(b, cons('i', cons('n', nil())))) when b >= '0' && b <= '6' => true
| _ => false
end
// validateHgt tests
val () = {
  val () = assertloc(~validateHgt("1e9cm"))
  val () = assertloc(~validateHgt("149c"))
  val () = assertloc(~validateHgt("149cm"))
  val () = assertloc(validateHgt("150cm"))
  val () = assertloc(validateHgt("151cm"))
  val () = assertloc(validateHgt("164cm"))
  val () = assertloc(validateHgt("178cm"))
  val () = assertloc(validateHgt("183cm"))
  val () = assertloc(validateHgt("189cm"))
  val () = assertloc(validateHgt("190cm"))
  val () = assertloc(validateHgt("191cm"))
  val () = assertloc(validateHgt("192cm"))
  val () = assertloc(validateHgt("193cm"))
  val () = assertloc(~validateHgt("194cm"))
  // in
  val () = assertloc(~validateHgt("58in"))
  val () = assertloc(validateHgt("59in"))
  val () = assertloc(validateHgt("60in"))
  val () = assertloc(validateHgt("61in"))
  val () = assertloc(validateHgt("69in"))
  val () = assertloc(validateHgt("70in"))
  val () = assertloc(validateHgt("71in"))
  val () = assertloc(validateHgt("76in"))
  val () = assertloc(~validateHgt("77in"))
}

// four digits; at least 2020 and at most 2030.
fun validateHcl{n:nat}(value: string(n)): bool =
let
  val chars = string_explode(value)
  val charsLst = list_of_list_vt(chars)
in case+ charsLst of
| cons('#', cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, nil()))))))) => isxdigit(a) && isxdigit(b) && isxdigit(c) && isxdigit(d) && isxdigit(e) && isxdigit(f)
| _ => false
end
// validateHcl tests
val () = {
  val () = assertloc(~validateHcl("1919"))
  val () = assertloc(~validateHcl("#"))
  val () = assertloc(~validateHcl("#astas"))
  val () = assertloc(~validateHcl("#12345z"))
  val () = assertloc(~validateHcl("#2-3450"))
  val () = assertloc(validateHcl("#213450"))
  val () = assertloc(validateHcl("#abcdef"))
  val () = assertloc(~validateHcl("#abcdeg"))
  val () = assertloc(~validateHcl("#Abcdeg"))
}

// exactly one of: amb blu brn gry grn hzl oth.
fun validateEcl{n:nat}(value: string(n)): bool =
case+ value of
| "amb" => true
| "blu" => true
| "brn" => true
| "gry" => true
| "grn" => true
| "hzl" => true
| "oth" => true
| _ => false
// validateEcl tests
val () = {
  val () = assertloc(~validateEcl("asrtarst"))
  val () = assertloc(validateEcl("amb"))
  val () = assertloc(validateEcl("blu"))
  val () = assertloc(validateEcl("brn"))
  val () = assertloc(validateEcl("gry"))
  val () = assertloc(validateEcl("grn"))
  val () = assertloc(validateEcl("hzl"))
  val () = assertloc(validateEcl("oth"))
  val () = assertloc(~validateEcl("uth"))
}

// a nine-digit number, including leading zeroes.
fun validatePid{n:nat}(value: string(n)): bool =
let
  val chars = string_explode(value)
  val charsLst = list_of_list_vt(chars)
in case+ charsLst of
| cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, cons(g, cons(h, cons(i, nil()))))))))) =>
  isdigit(a) && isdigit(b) && isdigit(c) &&
  isdigit(d) && isdigit(e) && isdigit(f) &&
  isdigit(g) && isdigit(h) && isdigit(i)
| _ => false
end
// validatePid tests
val () = {
  val () = assertloc(~validatePid("1919"))
  val () = assertloc(~validatePid("19122233"))
  val () = assertloc(~validatePid("19122233a"))
  val () = assertloc(validatePid("191222330"))
  val () = assertloc(validatePid("191282330"))
  val () = assertloc(validatePid("091282330"))
  val () = assertloc(validatePid("000000000"))
  val () = assertloc(validatePid("000000001"))
  val () = assertloc(validatePid("000800001"))
  val () = assertloc(validatePid("912384298"))
  val () = assertloc(~validatePid("7912384298"))
}

fun validateField
  {l,m:nat}
  (rfield: list(charNZ, l)): list(charNZ, m) -<cloref1> bool =
  lam rvalue => let
    val fieldStr = string_make_rlist(rfield)
    val valueStr = string_make_rlist(rvalue)
    val field = strnptr2string(fieldStr)
    val value = strnptr2string(valueStr)
  in case+ field of
  | "byr" => validateByr(value)
  | "iyr" => validateIyr(value)
  | "eyr" => validateEyr(value)
  | "hgt" => validateHgt(value)
  | "hcl" => validateHcl(value)
  | "ecl" => validateEcl(value)
  | "pid" => validatePid(value)
  | "cid" => true
  | _     => let val () = println! ("unknown") in false end
  end

fun allFieldsAreValid{n:nat}(passLine: string(n)): bool = let
  val mode = ParseField()
  val field = nil()
  val value = nil()
  val valid = true
in loop(mode, field, value, valid, string_explode(passLine))
end where { // end of allFieldsAreValid
fun loop
    {l,m,n:nat}
    (mode: parse_mode,
    field: list(charNZ, l),
    value: list(charNZ, m),
    valid: bool,
    chars: list_vt (charNZ, n)
    ): bool =
    if ~valid then let
      val () = free(chars)
    in false
    end
    else
      case+ chars of
      | ~list_vt_cons(':', cs) =>
        loop(ParseValue(), field, nil(), valid, cs)
      | ~list_vt_cons(' ', cs) => let
         in loop(ParseField(), nil(), value, validateField(field)(value) && valid, cs)
         end
      | ~list_vt_cons(c, cs) => (
        case+ mode of
        | ParseField() => loop(mode, cons(c, field), value, valid, cs)
        | ParseValue() => loop(mode, field, cons(c, value), valid, cs)
        )
      | ~list_vt_nil() =>
        validateField(field)(value) && valid
} // end of loop

fun validatePassport{n:nat}(line: string(n)): bool = let
  val isOptionalFieldPresent = strstr(line, optionalField) >= 0
  val fieldsNum = count_fields(string_explode(line))
  val passdata = @{
    fieldsNum= fieldsNum,
    optionalFieldPresent= isOptionalFieldPresent
  }: passdata
in isPassportFieldsNumValid(passdata) && allFieldsAreValid(line)
end

fun
solveB
(fileRef: FILEref): int = let
in loop(fileref_get_lines_stringlst(fileRef), "", 0)
end where { // end of solveB
fun loop(lines: List0_vt(Strptr1), passportLine: string, validPassports: int): int = let
in case+ lines of
   | ~list_vt_cons(l, ls) =>
     if (length(l) = 0) then let
       val pass = strptr2string(l)
       in if validatePassport(g1ofg0(passportLine)) then
         loop(ls, "", succ(validPassports))
       else
         loop(ls, "", validPassports)
       end
     else let
       val currentLine = copy(l)
       val () = free(l)
       val separator =
         if (length(passportLine)=0) then
           ""
         else
           " "
     in loop(
       ls,
       strptr2string(
         string_append(
           strptr2string(string_append(passportLine, separator)),
           strptr2string(currentLine)
       )),
       validPassports
     )
     end
   | ~list_vt_nil () => validPassports
end // end of loop
} // end of solveB where

implement
main0 () = {
  // val fileName = "sample.txt"
  val fileName = "input.txt"
  val file = fileref_open_exn(fileName, file_mode_r)
  // 222
  val () = println! ("Solving Day04A...")
  val () = println! (solveA(file))
  val () = fileref_close(file)
  val file = fileref_open_exn(fileName, file_mode_r)
  // 140
  val () = println! ("Solving Day04B...")
  val () = println! (solveB(file))
  val () = fileref_close(file)
}
