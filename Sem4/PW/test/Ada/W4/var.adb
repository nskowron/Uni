package body Var is

procedure Modify is
begin
   myAcc.all := (Option => Option1, A => 10, O1 => "11111222223333344444");
end Modify;

end Var;