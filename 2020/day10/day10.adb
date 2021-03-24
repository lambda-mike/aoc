with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day10 is

  package Integer_Sets is new Ada.Containers.Ordered_Sets
    (Element_Type => Integer);

  use Integer_Sets;
  subtype Long is Long_Integer;

  File_Name : constant String := "input.txt";
  --File_Name : constant String := "sample.txt";
  --File_Name : constant String := "sample2.txt";

  File : File_Type;
  Plugs : Set;
  Result_A : Integer;
  Result_B : Long;

  procedure Read_Input is
  begin
    Open(File => File,
         Mode => In_File,
         Name => File_Name);

    while not End_Of_File(File) loop
      declare
        Line: String := Get_Line(File);
      begin
        Plugs.Include(Integer'Value(Line));
      end;
    end loop;
    --Put_Line("Plugs " & Count_Type'Image(Plugs.Length));

    Close(File);
  end Read_Input;

  function Solve_A return Integer is
    Diff_1 : Natural := 0;
    Diff_3 : Natural := 0;
    Prev_Plug : Natural := 0;
  begin
    for Plug of Plugs loop
      case Plug - Prev_Plug is
          when 1 =>
            Diff_1 := Diff_1 + 1;
          when 3 =>
            Diff_3 := Diff_3 + 1;
          when others =>
            null;
      end case;
      Prev_Plug := Plug;
    end loop;
    --Put_Line("1 " & Integer'Image(Diff_1) & " 3 " & Integer'Image(Diff_3));

    -- Target device is always 3 jolts higher than the last adapter
    Diff_3 := Diff_3 + 1;
    return Diff_1 * Diff_3;
  end Solve_A;

  function Solve_B return Long is
    Combinations : Long := 1;
    -- How many ones in a row
    Ones : Natural := 0;
    Prev_Plug : Natural := 0;

    function Calculate_Combinations return Long is
    begin
      case Ones is
        when 0 | 1 =>
          -- No number can be deleted - one path
          return Combinations;
        when 2 =>
          -- One number can be deleted - 2 paths
          return Combinations * 2;
        when 3 =>
          -- Two nubmers can be deleted - 4 paths
          return Combinations * 4;
        when 4 =>
          -- Three nubmers can be deleted - 7 paths
          return Combinations * 7;
        when others =>
          return Combinations;
      end case;
    end Calculate_Combinations;

  begin
    for Plug of Plugs loop
      case Plug - Prev_Plug is
          when 1 =>
            Ones := Ones + 1;
          when 3 =>
            Combinations := Calculate_Combinations;
            Ones := 0;
          when others =>
            null;
      end case;
      Prev_Plug := Plug;
    end loop;
    -- Target device is always 3 jolts higher than the last adapter
    return Calculate_Combinations;
  end Solve_B;


begin
  Read_Input;

  -- 2376
  Put_Line("Solving Day10A...");
  Result_A := Solve_A;
  Put_Line(Integer'Image(Result_A));

  -- 129586085429248
  Put_Line("Solving Day10B...");
  Result_B := Solve_B;
  Put_Line(Long'Image(Result_B));
end Day10;
