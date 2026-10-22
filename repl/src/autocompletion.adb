with Ada.Strings.Bounded;

package body Autocompletion is

   package Command_String is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => 10);

   Commands : constant array (1 .. 6) of Command_String.Bounded_String :=
     (1 => Command_String.To_Bounded_String (":exit"),
      2 => Command_String.To_Bounded_String (":dict"),
      3 => Command_String.To_Bounded_String (":show"),
      4 => Command_String.To_Bounded_String (":clear"),
      5 => Command_String.To_Bounded_String (":render"),
      6 => Command_String.To_Bounded_String (":help"));

   function Start_With (S : String; P : String) return Boolean
   is
     (P'Length <= S'Length and then S (S'First .. S'First + P'Length - 1) = P);

   function My_Completion_Entry_Func
     (Text : String; State : Integer) return String
   is
   begin
      if State = 0 then
         for Cmd of Commands loop
            if Text'Length > 0 and then 
              Start_With (Command_String.To_String (Cmd), Text) then
               return Command_String.To_String (Cmd);
            end if;
         end loop;
         return "";

      else
         return "";
      end if;
   end My_Completion_Entry_Func;

   function My_Completer_Function
     (Full_Line   : String;
      Text        : String;
      Start, Last : Integer) return GNATCOLL.Readline.Possible_Completions
   is
   begin
      return GNATCOLL.Readline.Completion_Matches
        (Text, My_Completion_Entry_Func'Access);
   end My_Completer_Function;

end Autocompletion;
