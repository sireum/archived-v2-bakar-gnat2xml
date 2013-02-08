------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                     G N A T 2 X M L . X M L 2 T R E E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2012, AdaCore, Inc.                     --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Warnings (Off, "*internal GNAT unit*");
with System.String_Hash;
pragma Warnings (On, "*internal GNAT unit*");
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Namet; use Namet;

with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;

with Asis.Text; use Asis.Text;

with Formatted_Output; use Formatted_Output;
with Gnat2xml.Ada_Trees; use Gnat2xml.Ada_Trees;

with Gnat2xml.Ada_Trees.Factory;

package body Gnat2xml.Xml2tree is

   use A4g.Queries;

   function Hash is new System.String_Hash.Hash
     (Character, String, Ada.Containers.Hash_Type);

   package Kinds_Mappings is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => DOM_String,
      Element_Type => Opt_ASIS_Elems,
      Hash => Hash,
      Equivalent_Keys => "=");

   Kinds_Mapping : Kinds_Mappings.Map;
   --  Mapping from element names to kinds

   package Lists_Mappings is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => DOM_String,
      Element_Type => Flat_List_Kinds,
      Hash => Hash,
      Equivalent_Keys => "=");

   Lists_Mapping : Lists_Mappings.Map;
   --  Mapping from query names to list kinds

   function Get_Kind (Name : DOM_String) return Opt_ASIS_Elems;
   function Get_List_Kind (Query_Name : DOM_String) return Flat_List_Kinds;

   procedure Elem_List_Iter
     (List : Node_List;
      Action : not null access procedure (Elem : DOM.Core.Element));

   function Count_Subtrees (List : Node_List) return Query_Count;
   --  Counts the number of Elements, not counting any "sloc" elements

   function Only_Child (List : Node_List) return DOM.Core.Element;

   function Doc_To_Ada_Tree (Doc : Document) return Ada_Tree;
   function Node_To_Ada_Tree (N : Node) return Ada_Tree;
   function Node_List_To_Ada_Tree
     (Query_Name : DOM_String; List : Node_List) return Ada_Tree;

   function Count_Subtrees (List : Node_List) return Query_Count is
      Result : Query_Count := 0;

      procedure Incr_Result (Child : DOM.Core.Element);

      procedure Incr_Result (Child : DOM.Core.Element) is
      begin
         if Node_Name (Child) /= "sloc" then
            Result := Result + 1;
         end if;
      end Incr_Result;
   begin
      Elem_List_Iter (List, Incr_Result'Access);

      return Result;
   end Count_Subtrees;

   function Doc_To_Ada_Tree (Doc : Document) return Ada_Tree is
   begin
      return Node_To_Ada_Tree (Get_Element (Doc));
   end Doc_To_Ada_Tree;

   procedure Elem_List_Iter
     (List : Node_List;
      Action : not null access procedure (Elem : DOM.Core.Element)) is
   begin
      for X in 0 .. Length (List) - 1 loop -- Node_List is 0-based
         declare
            Child : Node := Item (List, X);
         begin
            case Child.Node_Type is
               when Element_Node =>
                  Action (Child);

               when Text_Node =>
                  null; -- It should be whitespace; we can ignore it

               when Attribute_Node |
                 Cdata_Section_Node |
                 Entity_Reference_Node |
                 Entity_Node |
                 Processing_Instruction_Node |
                 Comment_Node |
                 Document_Node |
                 Document_Type_Node |
                 Document_Fragment_Node |
                 Notation_Node =>
               raise Program_Error;
            end case;
         end;
      end loop;
   end Elem_List_Iter;

   function Get_Kind (Name : DOM_String) return Opt_ASIS_Elems is
   begin
      return Kinds_Mappings.Element (Kinds_Mapping, To_Lower (Name));
   end Get_Kind;

   function Get_List_Kind (Query_Name : DOM_String) return Flat_List_Kinds is
   begin
      return Lists_Mappings.Element (Lists_Mapping, To_Lower (Query_Name));
   end Get_List_Kind;

   function Node_List_To_Ada_Tree
     (Query_Name : DOM_String; List : Node_List) return Ada_Tree
   is
      Subtree_Count : Query_Count := Count_Subtrees (List);
   begin
      return Result : Ada_Tree :=
        new Ada_Tree_Rec (Get_List_Kind (Query_Name), Subtree_Count)
      do
         declare
            procedure Do_List_Elem (Child : DOM.Core.Element);

            Subtree_Index : Query_Count := 0;

            procedure Do_List_Elem (Child : DOM.Core.Element) is
            begin
               if Node_Name (Child) = "sloc" then
                  raise Program_Error;
               else
                  Subtree_Index := Subtree_Index + 1;
                  Result.Subtrees (Subtree_Index) := Node_To_Ada_Tree (Child);
               end if;
            end Do_List_Elem;
         begin
            Elem_List_Iter (List, Do_List_Elem'Access);
         end;
      end return;
   end Node_List_To_Ada_Tree;

   function Node_To_Ada_Tree (N : Node) return Ada_Tree is
      Name : constant DOM_String := Node_Name (N);
      Kind : constant Ada_Tree_Kind := Get_Kind (Name);
      Children : constant Node_List := Child_Nodes (N);
      Subtree_Count : Query_Count := Count_Subtrees (Children);

      function Sloc (Child : DOM.Core.Element) return Asis.Text.Span;

      function Sloc (Child : DOM.Core.Element) return Asis.Text.Span is
         Child_Attrs : constant Named_Node_Map := Attributes (Child);
         Result : Asis.Text.Span;
      begin
         pragma Assert (Length (Child_Attrs) = 4);
         --  ???Assert Name (Item (Child_Attrs, X)) is correct.
         --  Or maybe we should be doing lookups.
         --  Use an aggregate here?
         Result.First_Line :=
           Line_Number'Value (Value (Item (Child_Attrs, 0)));
         Result.First_Column :=
           Character_Position'Value (Value (Item (Child_Attrs, 1)));
         Result.Last_Line :=
           Line_Number'Value (Value (Item (Child_Attrs, 2)));
         Result.Last_Column :=
           Character_Position'Value (Value (Item (Child_Attrs, 3)));
         return Result;
      end Sloc;

      Attrs : constant Named_Node_Map := Attributes (N);

      --  Start of processing for Node_To_Ada_Tree

   begin
      return Result : Ada_Tree_Base := new Ada_Tree_Rec (Kind, Subtree_Count) do
         declare
            procedure Do_Child (Child : DOM.Core.Element);

            Subtree_Index : Query_Count := 0;

            procedure Do_Child (Child : DOM.Core.Element) is
            begin
               if Node_Name (Child) = "sloc" then
                  Result.Sloc := Sloc (Child);
               else
                  Subtree_Index := Subtree_Index + 1;

                  if Has_Suffix (Node_Name (Child), "_q") then
                     Result.Subtrees (Subtree_Index) :=
                       Node_To_Ada_Tree (Only_Child (Child_Nodes (Child)));
                  elsif Has_Suffix (Node_Name (Child), "_ql") then
                     Result.Subtrees (Subtree_Index) :=
                       Node_List_To_Ada_Tree
                         (Node_Name (Child), Child_Nodes (Child));
                  else
                     raise Program_Error;
                  end if;
               end if;
            end Do_Child;
         begin
            Elem_List_Iter (Children, Do_Child'Access);
         end;

         for At_Index in 0 .. Length (Attrs) - 1 loop
            declare
               Atr : constant Attr := Item (Attrs, At_Index);
               Nm : constant DOM_String := DOM.Core.Attrs.Name (Atr);
               --  ???Why DOM.Core.Attrs?
               Val : constant DOM_String := Value (Atr);
            begin
               if Nm = "unit_kind" then
                  Result.Unit_Kind := Unit_Kinds'Value (Val);
               elsif Nm = "unit_class" then
                  Result.Unit_Class := Unit_Classes'Value (Val);
               elsif Nm = "unit_origin" then
                  Result.Unit_Origin := Unit_Origins'Value (Val);
               elsif Nm = "unit_full_name" then
                  Result.Unit_Full_Name := Name_Find (Val);
               elsif Nm = "def_name" then
                  Result.Def_Name := Name_Find (Val);
               elsif Nm = "source_file" then
                  Result.Source_File := Name_Find (Val);
               elsif Nm = "def" then
                  Result.Def := Name_Find (Val);
               elsif Nm = "type" then
                  if Kind in Def_Names then
                     Result.Decl_Type := Name_Find (Val);
                  else
                     Result.Expr_Type := Name_Find (Val);
                  end if;
               elsif Nm = "ref_name" then
                  Result.Ref_Name := Name_Find (Val);
               elsif Nm = "ref" then
                  Result.Ref := Name_Find (Val);
               elsif Nm = "lit_val" then
                  Result.Lit_Val := Name_Find (Val);
               elsif Nm = "pragma_name" then
                  Result.Pragma_Name := Name_Find (Val);
               elsif Nm = "mode" then
                  Result.Mode := Asis.Mode_Kinds'Value (Val);
               else
                  raise Program_Error;
               end if;
            end;
         end loop;
      end return;
   end Node_To_Ada_Tree;

   function Only_Child (List : Node_List) return DOM.Core.Element is
      Result : DOM.Core.Element;
      Found : Boolean := False;

      procedure Set_Result (Child : DOM.Core.Element);

      procedure Set_Result (Child : DOM.Core.Element) is
      begin
         pragma Assert (not Found);
         Result := Child;
         Found := True;
      end Set_Result;
   begin
      Elem_List_Iter (List, Set_Result'Access);
      pragma Assert (Found);
      return Result;
   end Only_Child;

   function Read_Xml (File_Name : String) return Ada_Tree is
      Input  : File_Input;
      Reader : Tree_Reader;
      Doc    : Document;
   begin
      --  ???Set_Public_Id (Input, "Preferences file");
      Open (File_Name, Input);

      Set_Feature (Reader, Validation_Feature, False);
      Set_Feature (Reader, Namespace_Feature, False);

      Parse (Reader, Input);
      Close (Input);

      Doc := Get_Tree (Reader);

      return Result : constant Ada_Tree := Doc_To_Ada_Tree (Doc) do
         Free (Reader);
      end return;
   end Read_Xml;

   use Kinds_Mappings, Lists_Mappings;

begin
   --  Initialize Kinds_Mapping

   for Kind in Opt_ASIS_Elems loop
      Insert
        (Kinds_Mapping,
         To_Lower (Strip_Article (Kind'Img)),
         Kind);
   end loop;

   --  Initialize Lists_Mapping

   for Q in Structural_Queries loop
      if Query_Result_Types (Q) in Flat_List_Kinds then
         Insert
           (Lists_Mapping,
            To_Lower (Strip_Article (Q'Img)) & "_ql",
            Query_Result_Types (Q));
      end if;
   end loop;

end Gnat2xml.Xml2tree;
