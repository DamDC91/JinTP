with GNATCOLL.JSON;

package Jintp.Loader.Json is

   procedure Load
     (Dict        : in out Jintp.Dictionary;
      JSON_Values : GNATCOLL.JSON.JSON_Value);

end Jintp.Loader.Json;
