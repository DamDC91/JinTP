with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with GNATCOLL.JSON;
with Jintp;
with Jintp.Loader.Json;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with GNATCOLL.Readline;
with Autocompletion;
with Ada.Strings.Unbounded.Text_IO;

procedure Jintp_Repl is
   Values        : Jintp.Dictionary;
   Json_Value    : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.JSON_Null;
   Template_Name : constant String := "/tmp/jintp_repl_input.tmp";
   Exit_Repl     : Boolean := False;

   procedure Read_Json_Data is
      Json_Open  : Integer := 0;
      Json_Close : Integer := 0;
      Json_Data  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         exit when Json_Open /= 0 and then (Json_Open = Json_Close);
         declare
            Line : constant String  := GNATCOLL.Readline.Get_Line ("DICT > ");
         begin
            if Line = ":quit" or else Line = ":exit" then
               Exit_Repl := True;
               return;
            end if;
            Json_Open := Json_Open + Ada.Strings.Fixed.Count (Line, "{");
            Json_Close := Json_Close + Ada.Strings.Fixed.Count (Line, "}");
            Ada.Strings.Unbounded.Append (Json_Data, Line);
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Message (E));
         end;
      end loop;
      if Json_Open /= 0 and then (Json_Open = Json_Close) then
         declare
            Read_Result : constant GNATCOLL.JSON.Read_Result :=
              GNATCOLL.JSON.Read (Json_Data);
         begin
            if not Read_Result.Success then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  GNATCOLL.JSON.Format_Parsing_Error (Read_Result.Error));
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               return;
            end if;
            Jintp.Loader.Json.Load (Values, Read_Result.Value);
            Json_Value := Read_Result.Value;
         end;
      end if;
   end Read_Json_Data;

   procedure Show_Help is
   begin
      Ada.Text_IO.Put_Line ("Available commands:");
      Ada.Text_IO.Put_Line (":exit to exit the REPL");
      Ada.Text_IO.Put_Line (":dict to create (or overwrite) a dictionnary using JSON syntax");
      Ada.Text_IO.Put_Line (":show to show dictionnary and template");
      Ada.Text_IO.Put_Line (":clear to erase the template");
      Ada.Text_IO.Put_Line (":render to render the template");
      Ada.Text_IO.Put_Line (":help to show the available commands");
   end Show_Help;

   File             : Ada.Text_IO.File_Type;
   Template_Content : Ada.Strings.Unbounded.Unbounded_String;
begin

   Ada.Text_IO.Put_Line ("Welcome to JinTP REPL.");
   Ada.Text_IO.Put_Line ("Type :help and hit Enter for help");

   GNATCOLL.Readline.Initialize
     ("Jintp REPL", "/tmp/jintp_repl_history.txt",
      Autocompletion.My_Completer_Function'Access);

   if Ada.Command_Line.Argument_Count = 1 then
      declare
         Read_Result : constant GNATCOLL.JSON.Read_Result :=
           GNATCOLL.JSON.Read_File (Ada.Command_Line.Argument (1));
      begin
         if not Read_Result.Success then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               GNATCOLL.JSON.Format_Parsing_Error (Read_Result.Error));
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
         Jintp.Loader.Json.Load (Values, Read_Result.Value);
      end;
   end if;

   while not Exit_Repl loop
      declare
         Line       : constant String  :=
           GNATCOLL.Readline.Get_Line ("REPL > ");
         Trimed_Line : constant String :=
           Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both);
      begin
         if Trimed_Line = ":exit" then
            Exit_Repl := True;

         elsif Trimed_Line = ":dict" then
            Read_Json_Data;

         elsif Trimed_Line = ":show" then

            Ada.Text_IO.Put_Line ("Dictionary:");
            Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Json_Value, False));
            Ada.Text_IO.Put_Line ("Template:");
            Ada.Strings.Unbounded.Text_IO.Put_Line (Template_Content);

         elsif Trimed_Line = ":render" then
            Ada.Text_IO.Create
              (File, Ada.Text_IO.Out_File, Template_Name);
            Ada.Strings.Unbounded.Text_IO.Put (File, Template_Content);
            Ada.Text_IO.Close (File);
            Ada.Text_IO.Put (Jintp.Render (Template_Name, Values));

         elsif Trimed_Line = ":clear" then
            Template_Content := Ada.Strings.Unbounded.Null_Unbounded_String;

         elsif Trimed_Line = ":help" then
            Show_Help;

         else
            Ada.Strings.Unbounded.Append (Template_Content, Line & ASCII.LF);
         end if;

      end;
   end loop;
exception
   when Ada.Text_IO.End_Error =>
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
      raise;
end Jintp_Repl;
