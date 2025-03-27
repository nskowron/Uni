with Ada.Text_IO;

procedure IsWhat is
   package IO renames Ada.Text_IO;
   Str : String(1..100);
   Last : Natural;
begin
   IO.Put_Line ("The ");
   IO.Get_Line (Str, Last);

   if Str (1..Last) = "body"  then
      IO.Put_Line ("is round.");
   else
      IO.Put_Line ("is not round.");
   end if;
end IsWhat;   