with Ada.Text_IO; use Ada.Text_IO;

procedure beepboop is
   Str : String (1..100);
   Last : Natural;
begin
   Get_Line(Str, Last);

   if Str (1..Last) = "beep" then
      Put_Line("boop");
   elsif Str (1..Last) = "boop" then
      Put_Line("beep");
   else 
      Put_Line("boepp??");
   end if;
end beepboop;