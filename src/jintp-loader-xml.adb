with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with DOM.Core.Nodes;

package body Jintp.Loader.Xml is

   type XML_Value_Kind is
     (String_Value, Integer_Value, Float_Value, Boolean_Value,
      Object_Value, List_Value);

   procedure Map_XML_Element
     (Dict : in out Jintp.Dictionary;
      Elem : DOM.Core.Node);

   function XML_Type (Elem : DOM.Core.Node) return XML_Value_Kind;
   function Is_Integer_Text (Text : String) return Boolean;
   function Is_Float_Text (Text : String) return Boolean;
   function XML_Text (Elem : DOM.Core.Node) return String;
   procedure Insert_XML_Element
     (Dict : in out Jintp.Dictionary;
      Key  : String;
      Elem : DOM.Core.Node);
   procedure Append_XML_Element
     (List : in out Jintp.List;
      Elem : DOM.Core.Node);

   function Is_Integer_Text (Text : String) return Boolean is
      First : Integer := Text'First;
   begin
      if Text'Length = 0 then
         return False;
      end if;
      if Text (First) = '+' or else Text (First) = '-' then
         First := First + 1;
      end if;
      if First > Text'Last then
         return False;
      end if;
      for Index in First .. Text'Last loop
         if Text (Index) < '0' or else Text (Index) > '9' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Integer_Text;

   function Is_Float_Text (Text : String) return Boolean is
      Index : Integer := Text'First;
      Digit_Count : Natural := 0;
      Has_Fraction : Boolean := False;
      Has_Exponent : Boolean := False;
   begin
      if Text'Length = 0 then
         return False;
      end if;
      if Text (Index) = '+' or else Text (Index) = '-' then
         Index := Index + 1;
      end if;
      while Index <= Text'Last
        and then Text (Index) >= '0'
        and then Text (Index) <= '9'
      loop
         Digit_Count := Digit_Count + 1;
         Index := Index + 1;
      end loop;
      if Index <= Text'Last and then Text (Index) = '.' then
         Has_Fraction := True;
         Index := Index + 1;
         while Index <= Text'Last
           and then Text (Index) >= '0'
           and then Text (Index) <= '9'
         loop
            Digit_Count := Digit_Count + 1;
            Index := Index + 1;
         end loop;
      end if;
      if Digit_Count = 0 then
         return False;
      end if;
      if Index <= Text'Last
        and then (Text (Index) = 'e' or else Text (Index) = 'E')
      then
         Has_Exponent := True;
         Index := Index + 1;
         if Index <= Text'Last
           and then (Text (Index) = '+' or else Text (Index) = '-')
         then
            Index := Index + 1;
         end if;
         if Index > Text'Last
           or else Text (Index) < '0'
           or else Text (Index) > '9'
         then
            return False;
         end if;
         while Index <= Text'Last
           and then Text (Index) >= '0'
           and then Text (Index) <= '9'
         loop
            Index := Index + 1;
         end loop;
      end if;
      return Index > Text'Last and then (Has_Fraction or Has_Exponent);
   end Is_Float_Text;

   function XML_Type (Elem : DOM.Core.Node) return XML_Value_Kind is
      use DOM.Core;
      use DOM.Core.Nodes;
      Type_Attribute : constant Node :=
        Get_Named_Item (Attributes (Elem), "type");
      Children : constant Node_List := Child_Nodes (Elem);
      First_Name : Ada.Strings.Unbounded.Unbounded_String;
      Element_Count : Natural := 0;
      Same_Name : Boolean := True;
      Text : constant String :=
        Ada.Strings.Fixed.Trim (XML_Text (Elem), Ada.Strings.Both);
   begin
      if Type_Attribute /= null then
         declare
            Type_Name : constant String := Node_Value (Type_Attribute);
         begin
            if Type_Name = "string" then
               return String_Value;
            elsif Type_Name = "integer" then
               return Integer_Value;
            elsif Type_Name = "float" then
               return Float_Value;
            elsif Type_Name = "boolean" then
               return Boolean_Value;
            elsif Type_Name = "object" then
               return Object_Value;
            elsif Type_Name = "list" then
               return List_Value;
            else
               raise Loading_Error with "Unknown XML type: " & Type_Name;
            end if;
         end;
      end if;

      for I in 0 ..
        (if Length (Children) = 0 then 0 else Length (Children) - 1)
      loop
         exit when Length (Children) = 0;
         declare
            Child : constant Node := Item (Children, I);
         begin
            if Node_Type (Child) = Element_Node then
               Element_Count := Element_Count + 1;
               if Ada.Strings.Unbounded.Length (First_Name) = 0 then
                  First_Name :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Node_Name (Child));
               elsif Node_Name (Child) /=
                 Ada.Strings.Unbounded.To_String (First_Name)
               then
                  Same_Name := False;
               end if;
            end if;
         end;
      end loop;

      if Element_Count > 0 then
         if Same_Name
           and then
             (Element_Count > 1
              or else Ada.Strings.Unbounded.To_String (First_Name) = "item")
         then
            return List_Value;
         else
            return Object_Value;
         end if;
      else
         declare
            Normalized_Text : constant String :=
              Ada.Characters.Handling.To_Lower (Text);
         begin
            if Normalized_Text = "true" or else Normalized_Text = "false" then
               return Boolean_Value;
            elsif Is_Integer_Text (Text) then
               return Integer_Value;
            elsif Is_Float_Text (Text) then
               return Float_Value;
            end if;
         end;
         return String_Value;
      end if;
   end XML_Type;

   function XML_Text (Elem : DOM.Core.Node) return String is
      use DOM.Core;
      use DOM.Core.Nodes;
      Children : constant Node_List := Child_Nodes (Elem);
      Result   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 0 ..
        (if Length (Children) = 0 then 0 else Length (Children) - 1)
      loop
         exit when Length (Children) = 0;
         declare
            Child : constant Node := Item (Children, I);
         begin
            if Node_Type (Child) = Text_Node
              or else Node_Type (Child) = Cdata_Section_Node
            then
               Ada.Strings.Unbounded.Append (Result, Node_Value (Child));
            end if;
         end;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end XML_Text;

   procedure Append_XML_Element
     (List : in out Jintp.List;
      Elem : DOM.Core.Node)
   is
      Kind : constant XML_Value_Kind := XML_Type (Elem);
      Text : constant String := XML_Text (Elem);
   begin
      case Kind is
         when Integer_Value =>
            Jintp.Append (List, Integer'Value (Text));
         when Float_Value =>
            Jintp.Append (List, Float'Value (Text));
         when Boolean_Value =>
            Jintp.Append (List, Boolean'Value (Text));
         when Object_Value =>
            declare
               Value : Jintp.Dictionary;
            begin
               Map_XML_Element (Value, Elem);
               Jintp.Append (List, Value);
            end;
         when List_Value =>
            raise Loading_Error with "Nested XML lists are not supported";
         when String_Value =>
            Jintp.Append (List, Text);
      end case;
   exception
      when Constraint_Error =>
         raise Loading_Error with
           "Invalid value for XML type " & XML_Value_Kind'Image (Kind);
   end Append_XML_Element;

   procedure Insert_XML_Element
     (Dict : in out Jintp.Dictionary;
      Key  : String;
      Elem : DOM.Core.Node)
   is
      use DOM.Core;
      use DOM.Core.Nodes;
      Kind : constant XML_Value_Kind := XML_Type (Elem);
      Text : constant String := XML_Text (Elem);
      Children : constant Node_List := Child_Nodes (Elem);
      Has_Elements : Boolean := False;
   begin
      case Kind is
      when List_Value =>
         declare
            Value : Jintp.List;
         begin
            for I in 0 ..
              (if Length (Children) = 0 then 0 else Length (Children) - 1)
            loop
               exit when Length (Children) = 0;
               if Node_Type (Item (Children, I)) = Element_Node then
                  Append_XML_Element (Value, Item (Children, I));
               end if;
            end loop;
            Jintp.Insert (Dict, Key, Value);
         end;
      when Object_Value =>
         declare
            Value : Jintp.Dictionary;
         begin
            Map_XML_Element (Value, Elem);
            Jintp.Insert (Dict, Key, Value);
         end;
      when Integer_Value =>
         Jintp.Insert (Dict, Key, Integer'Value (Text));
      when Float_Value =>
         Jintp.Insert (Dict, Key, Long_Float'Value (Text));
      when Boolean_Value =>
         Jintp.Insert (Dict, Key, Boolean'Value (Text));
      when String_Value =>
         for I in 0 ..
           (if Length (Children) = 0 then 0 else Length (Children) - 1)
         loop
            exit when Length (Children) = 0;
            if Node_Type (Item (Children, I)) = Element_Node then
               Has_Elements := True;
            end if;
         end loop;
         if Has_Elements then
            declare
               Value : Jintp.Dictionary;
            begin
               Map_XML_Element (Value, Elem);
               Jintp.Insert (Dict, Key, Value);
            end;
         else
            Jintp.Insert (Dict, Key, Text);
         end if;
      end case;
   exception
      when Constraint_Error =>
         raise Loading_Error with
           "Invalid value for XML type " & XML_Value_Kind'Image (Kind);
   end Insert_XML_Element;

   procedure Map_XML_Element
     (Dict : in out Jintp.Dictionary;
      Elem : DOM.Core.Node)
   is
      use DOM.Core;
      use DOM.Core.Nodes;
      Children : constant Node_List := Child_Nodes (Elem);
   begin
      declare
         Attrs : constant Named_Node_Map := Attributes (Elem);
      begin
         for I in 0 ..
           (if Length (Attrs) = 0 then 0 else Length (Attrs) - 1)
         loop
            exit when Length (Attrs) = 0;
            declare
               Attr_Node : constant Node := Item (Attrs, I);
            begin
               if Node_Name (Attr_Node) /= "type" then
                  Jintp.Insert
                    (Dict,
                     "@" & Node_Name (Attr_Node),
                     Node_Value (Attr_Node));
               end if;
            end;
         end loop;
      end;

      for I in 0 ..
        (if Length (Children) = 0 then 0 else Length (Children) - 1)
      loop
         exit when Length (Children) = 0;
         declare
            Child : constant Node := Item (Children, I);
         begin
            if Node_Type (Child) = Element_Node then
               Insert_XML_Element (Dict, Node_Name (Child), Child);
            end if;
         end;
      end loop;
   end Map_XML_Element;

   procedure Load (Dict : in out Jintp.Dictionary;
                   XML_Document : DOM.Core.Document)
   is
      use DOM.Core;
      use DOM.Core.Nodes;
      Children : constant Node_List := Child_Nodes (XML_Document);
   begin
      for I in 0 ..
        (if Length (Children) = 0 then 0 else Length (Children) - 1)
      loop
         exit when Length (Children) = 0;
         declare
            Root : constant Node := Item (Children, I);
         begin
            if Node_Type (Root) = Element_Node then
               Map_XML_Element (Dict, Root);
               return;
            end if;
         end;
      end loop;
      raise Loading_Error with "XML document has no document element";
   end Load;

end Jintp.Loader.Xml;
