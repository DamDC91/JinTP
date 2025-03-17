with Ada.Text_IO;
with Jintp;
with Test_Utils;
with GNATCOLL.JSON;
with AUnit.Assertions;
with AUnit.Test_Suites;
with AUnit.Options;
with AUnit.Test_Results;
with AUnit.Simple_Test_Cases;
with Ada.Directories;
with AUnit.Reporter.Text;
with Ada.Direct_IO;

procedure Jintp_Tests is

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
      Json_Path : constant String := Test_Cases_Dir & Directory & "/data.json";

      Read_Res : constant GNATCOLL.JSON.Read_Result :=
        GNATCOLL.JSON.Read_File (Json_Path);
   begin
      case Read_Res.Success is
         when True =>
            Test_Utils.Load (T.Dict, Read_Res.Value);

         when False =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  GNATCOLL.JSON.Format_Parsing_Error
                                    (Read_Res.Error));
      end case;
   end Set_Up;

   overriding
   procedure Run_Test (T : in out Jintp_Test)
   is
      Test_Dir      : constant String := Test_Cases_Dir & T.Name.all;
      Template_Path : constant String := Test_Dir & "/template.jintp";
      Actual        : constant String := Jintp.Render (Template_Path, T.Dict);
      Expected      : constant String :=
        Get_File_Content (Test_Dir & "/expected.txt");
   begin
      AUnit.Assertions.Assert (Actual, Expected, "Fail test " & T.Name.all);
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
end Jintp_Tests;
