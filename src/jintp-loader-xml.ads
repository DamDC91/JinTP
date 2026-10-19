with DOM.Core;

package Jintp.Loader.Xml is

   --  Load the children of the document element into Dict. Element names map
   --  to dictionary keys and attributes use an "@" prefix. Text in leaf
   --  elements becomes the value; text mixed with child elements is ignored.
   --  Leaf values infer booleans, integers, and floats when possible; other
   --  leaves are strings. Elements with children infer objects, or lists
   --  when they contain repeated same-name children (or repeated <item>
   --  children). A type attribute can explicitly specify string, integer,
   --  float, boolean, object, or list. Nested lists are not supported.
   procedure Load
     (Dict         : in out Jintp.Dictionary;
      XML_Document : DOM.Core.Document);

end Jintp.Loader.Xml;
