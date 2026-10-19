with GNATCOLL.JSON;

package Jintp.Loader is

   Loading_Error  : exception;
   
   procedure Load (Dict        : in out Jintp.Dictionary;
                   JSON_Values : GNATCOLL.JSON.JSON_Value);

end Jintp.Loader;
