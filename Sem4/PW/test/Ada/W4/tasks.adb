-- master - glowny task, czeka na wszystkie inne taski

-- taski gadaja ze soba przez entry
procedure Main is
   task Single;
   task body Single is
   begin
      null;
   end Single;

   -- co eksportuje task
   -- jak mozna sie z nim komunikowac
   task type myType is
      entry Ins (Item : Integer);
   end myType;

   -- kod taska, co on robi (kinda jak procedura)
   task body myType is
      -- lokalne zmienne przed begin - deklaracja
      Whatevs : Integer;
   begin
      -- definicja entry i wywolanie
      accept Ins (Item : in Integer) do
         Whatevs := Item;
      end Ins;
   end myType;

begin
   null;

   -- select accept (..do..) or accept (...)
   -- select when [wartownik] accept (...) - akceptujemy tylko gdy wartownik pozwoli
   -- w go po selekcie mozna zrobic maybe funkcje ktora zwraca channel (dla arg Bool np nil - kanal z ktorego nigdy nic nie plynie)

end Main;