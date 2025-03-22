with Ada.Text_IO; use Ada.Text_IO; -- allows not to use Ada.Text_IO.Put_Line
-- adb - body
-- ads - spec

procedure Main is

   procedure Hello is
   begin
      Put_Line("Hello ");
   end Hello;

   procedure World is
      package IO renames Ada.Text_IO;
   begin
      IO.Put_Line("World");
   end World;

begin
   Hello;
   World;

end Main;