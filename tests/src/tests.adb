with Ada.Text_IO;
with Jintp;
with Test_Utils;
with Ada.Command_Line;
with GNATCOLL.JSON;

procedure Tests is
   Json_Path : constant String := Ada.Command_Line.Argument (1);
   Template_Path : constant String := Ada.Command_Line.Argument (2);
   Dict : Jintp.Dictionary;
   Read_Res : constant GNATCOLL.JSON.Read_Result :=
     GNATCOLL.JSON.Read_File (Json_Path);
begin
      case Read_Res.Success is
         when True =>
            Test_Utils.Load (Dict, Read_Res.Value);
            Ada.Text_IO.Put_Line (Jintp.Render (Template_Path, Dict));

         when False =>
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                  GNATCOLL.JSON.Format_Parsing_Error (Read_Res.Error));
      end case;
end Tests;
