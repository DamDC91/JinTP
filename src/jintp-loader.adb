package body Jintp.Loader is

   procedure Map_JSON_Object is
     new GNATCOLL.JSON.Gen_Map_JSON_Object (Mapped => Jintp.Dictionary);

   procedure Map (Dict  : in out Jintp.Dictionary;
                  Name  : GNATCOLL.JSON.UTF8_String;
                  Value : GNATCOLL.JSON.JSON_Value);

   procedure Map_Array (List : in out Jintp.List;
                        Value : GNATCOLL.JSON.JSON_Array)
   is
      use GNATCOLL;
   begin
      for V of Value loop
         case JSON.Kind (V) is
            when JSON.JSON_String_Type =>
               Jintp.Append (Container => List,
                             New_Item => String'(JSON.Get (V)));
            when JSON.JSON_Array_Type =>
               declare
                  L : Jintp.List;
               begin
                  Map_Array (List => L,
                             Value => JSON.JSON_Array'(JSON.Get (V)));
                  Jintp.Append (Container =>  List,
                                New_Item  => L);

               end;
            when JSON.JSON_Object_Type =>
               declare
                  D : Jintp.Dictionary;
               begin
                  Map_JSON_Object (Val => V,
                                   CB => Map'Access,
                                   User_Object => D);
                  Jintp.Append (Container => List,
                                New_Item  => D);
               end;
            when JSON.JSON_Int_Type =>
               Jintp.Append (Container => List,
                             New_Item  => Integer'(JSON.Get (V)));
            when JSON.JSON_Boolean_Type =>
               Jintp.Append (Container => List,
                             New_Item  => Boolean'(JSON.Get (V)));
            when JSON.JSON_Float_Type =>
               Jintp.Append (Container => List,
                             New_Item  => Float'(JSON.Get (V)));
            when JSON.JSON_Null_Type =>
               raise Loading_Error with "Unexpected JSON null in JSON list";
         end case;
      end loop;

   end Map_Array;

   procedure Map (Dict  : in out Jintp.Dictionary;
                  Name  : GNATCOLL.JSON.UTF8_String;
                  Value : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL;
   begin
      case JSON.Kind (Value) is

         when JSON.JSON_Null_Type =>
            null;

         when JSON.JSON_Boolean_Type =>
            Jintp.Insert (Container => Dict,
                          Key => Name,
                          New_Item => Boolean'(JSON.Get (Value)));
         when JSON.JSON_Int_Type =>
            Jintp.Insert (Container => Dict,
                          Key => Name,
                          New_Item => Integer'(JSON.Get (Value)));

         when JSON.JSON_Float_Type =>
            Jintp.Insert
              (Container => Dict, Key => Name,
               New_Item  =>
                  Long_Float'(JSON.Get_Long_Float (Value)));
         when JSON.JSON_String_Type =>
            Jintp.Insert (Container => Dict,
                          Key => Name,
                          New_Item => String'(JSON.Get (Value)));
         when JSON.JSON_Array_Type =>
            declare
               L : Jintp.List;
            begin
               Map_Array (List => L,
                          Value => JSON.JSON_Array'(JSON.Get (Value)));
               Jintp.Insert (Container =>  Dict,
                             Key       => Name,
                             New_Item  => L);
            end;
         when JSON.JSON_Object_Type =>
            declare
               D : Jintp.Dictionary;
            begin
               Map_JSON_Object (Val => Value,
                                CB => Map'Access,
                                User_Object => D);
               Jintp.Insert (Container =>  Dict,
                             Key       => Name,
                             New_Item  => D);
            end;
      end case;
   end Map;

   procedure Load (Dict        : in out Jintp.Dictionary;
                   JSON_Values : GNATCOLL.JSON.JSON_Value)
   is
   begin
      Map_JSON_Object (Val => JSON_Values,
                       CB => Map'Access,
                       User_Object => Dict);

   end Load;

end Jintp.Loader;
