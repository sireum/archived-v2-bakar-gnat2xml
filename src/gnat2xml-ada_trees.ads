------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . A D A _ T R E E S                   --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides a data structure Ada_Tree for representing Ada syntax
--  trees, plus conversion routines for converting an Ada_Tree to textual Ada
--  code.

with Namet; use Namet;

with Asis.Text;

with Strings; use Strings;

package Gnat2xml.Ada_Trees is

   use A4g.Queries;

   type Ada_Tree_Rec;
   type Ada_Tree_Base is access all Ada_Tree_Rec;
   subtype Ada_Tree is Ada_Tree_Base with
     Predicate => Ada_Tree_Rec_OK (Ada_Tree.all);

   type Ada_Tree_Array is array (Query_Index range <>) of Ada_Tree;--??? with
--     Predicate => Ada_Tree_Array'First = 1;

   function Image (X : Query_Count) return String is
      (Image (Integer (X)));

   subtype Ada_Tree_Kind is ASIS_Elems'Base range
     ASIS_Elems'Base'First .. A_Variant_List;

   function Image (Kind : Ada_Tree_Kind) return String is
      (Capitalize (Kind'Img));

   type Ada_Tree_Rec
     (Kind : Ada_Tree_Kind;
      Subtree_Count : Query_Count)
   is
      record
         Sloc : Asis.Text.Span := Asis.Text.Nil_Span;
         Subtrees : Ada_Tree_Array (1 .. Subtree_Count) := (others => <>);

         case Kind is
            when A_Compilation_Unit | Def_Names =>
               Def_Name : Name_Id;

               case Kind is
                  when A_Compilation_Unit =>
                     Unit_Kind      : Unit_Kinds;
                     Unit_Class     : Unit_Classes;
                     Unit_Origin    : Unit_Origins;
                     Unit_Full_Name : Name_Id;
                     Source_File    : Name_Id;

                  when Def_Names =>
                     Def : Name_Id;
                     Decl_Type : Name_Id;
                     --  Type of declared name; corresponds to "type" attribute

                  when others => null;
               end case;

            when Flat_Expression_Kinds =>
               Expr_Type : Name_Id;
               --  Type of expression; also corresponds to "type" attribute

               case Kind is
                  when Usage_Names =>
                     Ref_Name, Ref : Name_Id;

                  when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
                     Lit_Val : Name_Id;

                  when others => null;
               end case;

            when Flat_Pragma_Kinds =>
               Pragma_Name : Name_Id;

            when A_Parameter_Specification | A_Formal_Object_Declaration =>
               Mode : Asis.Mode_Kinds;

            when others => null;
         end case;
      end record;--??? with -- Ada_Tree_Rec
--         Predicate => Ada_Tree_Rec_OK (Ada_Tree_Rec);

   function Ada_Tree_Rec_OK (X : Ada_Tree_Rec) return Boolean is
      (X.Kind in Flat_List_Kinds or else
       X.Subtree_Count = Num_Queries (X.Kind));

   function Empty
     (Kind : Flat_List_Kinds;
      Sloc : Asis.Text.Span := Asis.Text.Nil_Span)
     return Ada_Tree;

   function Nil
     (Sloc : Asis.Text.Span := Asis.Text.Nil_Span) return Ada_Tree
   is (new Ada_Tree_Rec'(Not_An_Element,
                         Subtree_Count => 0,
                         Sloc => Sloc,
                         Subtrees => (1 .. 0 => <>)));

   procedure Tree_To_Ada (Tree : Ada_Tree; Source_Text : String);
   --  Prints Ada "source" code as text on current output.  Source_Text is the
   --  contents of the original Ada source file.

   ----------------

   type Kind_Set is array (Ada_Tree_Kind) of Boolean with Pack => True;

   function Kinds_In_Class
     (Class: Flat_Element_Kinds'Base) return Kind_Set;

   function Cardinality (Kinds : Kind_Set) return Natural;
   --  Number of elements in Kinds

   procedure Put_Kinds (Kinds : Kind_Set);
   --  Print something like "This | That | The_Other" to standard output

   function Kind_In_Class
     (Kind : Opt_ASIS_Elems;
      Class: Flat_Abstract_Classes)
     return Boolean;
   --  True if Kind is in the Class

   function Get (Tree : Ada_Tree; Q : Structural_Queries) return Ada_Tree;
   procedure Set
     (Tree : Ada_Tree; Q : Structural_Queries; Subtree : Ada_Tree);
   --  Getters and setters

   generic
      Query : Structural_Queries;
      type Result_Type is new Ada_Tree;
   function Generic_Getter (Tree : Ada_Tree) return Result_Type;
   --  An instance will return Get (Tree, Query), returning the appropriate
   --  subtype. For example, instantiate like this:
   --     function Discriminant_Part is new Generic_Getter
   --       (Discriminant_Part, Definition_Class);
   --  to get an instance like this:
   --     function Discriminant_Part (Tree : Ada_Tree) return Definition_Class;

   generic
      Query : Structural_Queries;
      type Result_Type is new Ada_Tree;
   procedure Generic_Setter (Tree : Ada_Tree; Subtree : Result_Type);
   --  An instance will do Set (Tree, Query, Subtree).

   type Assoc is
      record
         Query : Structural_Queries;
         Subtree : Ada_Tree;
      end record;

   type Assoc_List is array (Query_Index range <>) of Assoc;

   function Make
     (Kind : Opt_ASIS_Elems;
      Subtrees : Assoc_List := (1 .. 0 => <>);
      Sloc : Asis.Text.Span := Asis.Text.Nil_Span)
     return Ada_Tree with
       Pre => Subtrees'First = 1 and then Subtrees'Last = Num_Queries (Kind);
   --  Make a new Ada_Tree with the given Kind, Subtrees, and Sloc. Other
   --  components (the ones in the variant part) are not filled in.

   function Make_List
     (Kind : Flat_List_Kinds;
      Subtrees : Ada_Tree_Array := (1 .. 0 => <>);
      Sloc : Asis.Text.Span := Asis.Text.Nil_Span)
     return Ada_Tree;
   --  Make a new list with the given Kind, Subtrees, and Sloc.

   function Clone (Tree : Ada_Tree) return Ada_Tree;
   --  Returns a deep copy of Tree

   function Q_Name (Q : Structural_Queries) return String is
      (Capitalize (Strip_Article (Q'Img)));
   --  Name of the Query function in the Factory child package

   function Constructor_Name (Class : Ada_Tree_Kind) return String is
      ((if Class in Boolean_Elems then "Make_" else "") &
        Capitalize (Strip_Article (Class'Img)));
   --  Name of the constructor function in the Factory child package. We
   --  prepend a "Make_" prefix for the booleans, because some of those are
   --  reserved words.

   List_Component_Type : constant array (Flat_List_Kinds) of Classes :=
     --  Mapping from list kinds to their component kinds. For example a
     --  A_Declarative_Item_List is a list of A_Declarative_Item_Class
     --  elements.
     (An_Element_List => An_Element_Class,
      An_Association_List => An_Association_Class,
      A_Component_Clause_List => A_Component_Clause,
      A_Context_Clause_List => A_Context_Clause_Class,
      A_Declaration_List => A_Declaration_Class,
      A_Declarative_Item_List => A_Declarative_Item_Class,
      A_Definition_List => A_Definition_Class,
      A_Discrete_Range_List => A_Discrete_Range_Class,
      A_Discriminant_Association_List => A_Discriminant_Association,
      A_Discriminant_Specification_List => A_Discriminant_Specification,
      A_Defining_Name_List => A_Defining_Name_Class,
      An_Exception_Handler_List => An_Exception_Handler,
      An_Expression_List => An_Expression_Class,
      A_Name_List => A_Name_Class,
      A_Parameter_Specification_List => A_Parameter_Specification,
      A_Path_List => A_Path_Class,
      A_Record_Component_List => A_Record_Component_Class,
      A_Statement_List => A_Statement_Class,
      A_Variant_List => A_Variant);

   --  For debugging:

   procedure Put_Ada_Templates;

end Gnat2xml.Ada_Trees;
