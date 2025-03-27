package Var is

type OptionType is (Option1, Option2);

type varRec (Option : OptionType := Option2) is record -- default value, allows mutable
   A : Integer;

   case Option is
      when Option1 =>
         O1 : String (1..20);
      when Option2 =>
         O2 : Integer;
   end case;
end record;

myVar : varRec; -- mutable
myVar2 : varRec (Option => Option1); -- immutable
myVar3 : varRec := (Option => Option2, A => 3, O2 => 2);

-- Access - typ wskaznikowy (Pool ~ heap, general, anonymous ~ do procedur np)

type varAcc is Access varRec;

type vector is array (Integer range <>) of Integer;

myAcc : varAcc;
myAcc2 : Access varRec;

procedure Modify;

end Var;