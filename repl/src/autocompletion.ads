with GNATCOLL.Readline;

package Autocompletion is

   function My_Completion_Entry_Func
     (Text : String; State : Integer) return String;

   function My_Completer_Function
      (Full_Line   : String;
       Text        : String;
       Start, Last : Integer) return GNATCOLL.Readline.Possible_Completions;

end Autocompletion;
