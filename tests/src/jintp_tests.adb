with Ada.Text_IO;
with Jintp;
with GNATCOLL.JSON;
with Jintp.Loader.Json;
with Jintp.Loader.Xml;
with DOM.Readers;
with Input_Sources.File;
with Sax.Readers;
with AUnit.Assertions;
with AUnit.Test_Suites;
with AUnit.Options;
with AUnit.Test_Results;
with AUnit.Simple_Test_Cases;
with Ada.Directories;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with AUnit.Reporter.Text;
with Ada.Direct_IO;

procedure Jintp_Tests is
   use type AUnit.Status;

   Test_Cases_Dir : constant String := "test_cases/";

   function Get_File_Content (File_Name : String)
                              return String
   is
      S : constant Ada.Directories.File_Size :=
        Ada.Directories.Size (File_Name);
      subtype Content_Type is String (1 .. Integer (S) - 1);
      Content : Content_Type;

      package IO is new Ada.Direct_IO (Content_Type);
      F : IO.File_Type;
   begin
      IO.Open (File => F,
               Mode => IO.In_File,
               Name => File_Name);
      IO.Read (F, Content);
      IO.Close (F);
      return Content;
   end Get_File_Content;

   type Jintp_Test is new AUnit.Simple_Test_Cases.Test_Case with record
      Dict : Jintp.Dictionary;
      Name : AUnit.Message_String;
   end record;

   overriding
   function Name (T : Jintp_Test) return AUnit.Message_String;

   overriding
   procedure Set_Up (T : in out Jintp_Test);

   overriding
   procedure Run_Test (T : in out Jintp_Test);

   overriding
   function Name (T : Jintp_Test) return AUnit.Message_String
   is
   begin
      return T.Name;
   end Name;

   overriding
   procedure Set_Up (T : in out Jintp_Test)
   is
      Directory : constant String := T.Name.all;
      XML_Path : constant String := Test_Cases_Dir & Directory & "/data.xml";
      Json_Path : constant String := Test_Cases_Dir & Directory & "/data.json";
   begin
      if Ada.Directories.Exists (XML_Path) then
         declare
            Input  : Input_Sources.File.File_Input;
            Reader : DOM.Readers.Tree_Reader;
         begin
            Input_Sources.File.Open (XML_Path, Input);
            Sax.Readers.Parse
              (Sax.Readers.Sax_Reader'Class (Reader), Input);
            Input_Sources.File.Close (Input);
            Jintp.Loader.Xml.Load (T.Dict, DOM.Readers.Get_Tree (Reader));
            DOM.Readers.Free (Reader);
         end;
      else
         declare
            Read_Res : constant GNATCOLL.JSON.Read_Result :=
              GNATCOLL.JSON.Read_File (Json_Path);
         begin
            if Read_Res.Success then
               Jintp.Loader.Json.Load (T.Dict, Read_Res.Value);
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  GNATCOLL.JSON.Format_Parsing_Error (Read_Res.Error));
            end if;
         end;
      end if;
   end Set_Up;

   overriding
   procedure Run_Test (T : in out Jintp_Test)
   is
      Test_Dir      : constant String := Test_Cases_Dir & T.Name.all;
      Template_Path : constant String := Test_Dir & "/template.jintp";
      Expected_Error_Path : constant String :=
        Test_Dir & "/expected-error.txt";

      function Render_Template return String is
         function Bracket_Filter
           (Arguments : Jintp.Unbounded_String_Array)
            return Ada.Strings.Unbounded.Unbounded_String
         is
         begin
            return Ada.Strings.Unbounded.To_Unbounded_String
              ("[" & Ada.Strings.Unbounded.To_String (Arguments (1)) & "]");
         end Bracket_Filter;
      begin
         if T.Name.all = "test_custom_environment" then
            declare
               Settings : Jintp.Environment;
            begin
               Jintp.Configure
                 (Settings,
                  Expression_Start => "<<",
                  Expression_End   => ">>",
                  Statement_Start  => "<%",
                  Statement_End    => "%>",
                  Comment_Start    => "<#",
                  Comment_End      => "#>",
                  Max_Cache_Size   => 0,
                  Trim_Blocks      => True,
                  Lstrip_Blocks    => True);
               Jintp.Register_Filter
                 (Settings, Bracket_Filter'Unrestricted_Access, "bracket");
               return Jintp.Render (Template_Path, T.Dict, Settings);
            end;
         end if;
         return Jintp.Render (Template_Path, T.Dict);
      end Render_Template;
   begin
      if Ada.Directories.Exists (Expected_Error_Path) then
         declare
            Expected_Error : constant String :=
              Get_File_Content (Expected_Error_Path);
            Caught_Error   : Boolean := False;
            Error_Message  : Ada.Strings.Unbounded.Unbounded_String;
            Actual         : Ada.Strings.Unbounded.Unbounded_String;
         begin
            begin
               Actual :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Render_Template);
            exception
               when E : others       =>
                  Caught_Error := True;
                  Error_Message :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Ada.Exceptions.Exception_Message (E));
            end;
            AUnit.Assertions.Assert
              (Caught_Error
               and then Ada.Strings.Fixed.Index
                 (Ada.Strings.Unbounded.To_String (Error_Message),
                  Expected_Error) > 0,
               "Wrong error in test " & T.Name.all & ": "
               & Ada.Strings.Unbounded.To_String (Error_Message)
               & (if Caught_Error
                  then ""
                  else "; rendered '"
                    & Ada.Strings.Unbounded.To_String (Actual) & "'"));
         end;
      else
         declare
            Actual : constant String := Render_Template;
            Expected : constant String :=
              Get_File_Content (Test_Dir & "/expected.txt");
         begin
            AUnit.Assertions.Assert (Actual, Expected, "Fail test " & T.Name.all);
         end;
      end if;
   end Run_Test;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      The_Suite : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;

      procedure Create_Test_From_Folder
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         type Jintp_Test_Access is access Jintp_Test;
         Directory : constant String :=
           Ada.Directories.Simple_Name (Directory_Entry);
         T : constant Jintp_Test_Access :=
           new Jintp_Test'(AUnit.Simple_Test_Cases.Test_Case with
                     Name => AUnit.Format (Directory),
                     Dict => <>);
      begin
         AUnit.Test_Suites.Add_Test
           (S => The_Suite,
            T => T);
      end Create_Test_From_Folder;
   begin
      Ada.Directories.Search
        (Directory => Test_Cases_Dir,
         Pattern   => "test_*",
         Filter    => (Ada.Directories.Directory => True, others => False),
         Process   => Create_Test_From_Folder'Access);
      return The_Suite;
   end Suite;

   Result        : AUnit.Test_Results.Result;
   Status        : AUnit.Status;
   Text_Reporter : AUnit.Reporter.Text.Text_Reporter;
begin

   AUnit.Test_Suites.Run
     (Suite   => Suite,
      Options => AUnit.Options.Default_Options,
      R       => Result,
      Outcome => Status);

   AUnit.Reporter.Text.Report (Engine => Text_Reporter,
                               R      => Result);
   if Status = AUnit.Failure then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Jintp_Tests;
