with Jintp;
with GNATCOLL.JSON;

package Test_Utils is

   procedure Load (Dict   : in out Jintp.Dictionary;
                   Values : GNATCOLL.JSON.JSON_Value);

end Test_Utils;
