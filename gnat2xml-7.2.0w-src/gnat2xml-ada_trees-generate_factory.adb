------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--   G N A T 2 X M L . A D A _ T R E E S . G E N E R A T E _ F A C T O R Y  --
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

pragma Ada_2012;

--  This program generates the package Gnat2xml.Ada_Trees.Factory, which
--  contains subtypes and constructors for each kind/class of tree, and getters
--  and setters for each query.

with Text_IO;

with Formatted_Output; use Formatted_Output;
with Strings;          use Strings;

procedure Gnat2xml.Ada_Trees.Generate_Factory is

   Factory_Spec, Factory_Body : Text_IO.File_Type;

   function Subtype_Name (Class : Classes) return String;
   function List_Component_Subtype_Name (Kind : Flat_List_Kinds) return String;
   procedure Construct_Constructor (Kind : Opt_ASIS_Elems; Is_Body : Boolean);
   procedure Construct_List_Constructor (Kind : Flat_List_Kinds; Is_Body : Boolean);

   function Subtype_Name (Class : Classes) return String is
   begin
      if Class = Not_An_Element then
         return "Nil_Element";
      else
         return Capitalize (Strip_Article (Class'Img)) &
           (if Class in ASIS_Elems | Flat_List_Kinds then "_Tree" else "");
         --  Some of the ASIS_Elems conflict with query names, so append
         --  "_Tree" to those. ASIS_Elems and lists conflict with constructor
         --  names. This also avoids conflict with reserved words.
      end if;
   end Subtype_Name;

   function List_Component_Subtype_Name (Kind : Flat_List_Kinds) return String is
   begin
      return Subtype_Name (List_Component_Type (Kind));
   end List_Component_Subtype_Name;

   procedure Construct_Constructor
     (Kind : Opt_ASIS_Elems; Is_Body : Boolean)
   is
      Qs : Query_List renames Appropriate_Queries (Kind).all;
      First_Time : Boolean := True;
   begin
      Put ("function \1", Constructor_Name (Kind));
      Put ("\i\n(");

      case Kind is
         when A_Compilation_Unit | Def_Names =>
            Put ("Def_Name : Name_Id;\n");

            case Kind is
               when A_Compilation_Unit =>
                  Put ("Unit_Kind : Unit_Kinds;\n");
                  Put ("Unit_Class : Unit_Classes;\n");
                  Put ("Unit_Origin : Unit_Origins;\n");
                  Put ("Unit_Full_Name : Name_Id;\n");
                  Put ("Source_File : Name_Id;\n");

               when Def_Names =>
                  Put ("Def : Name_Id;\n");
                  Put ("Decl_Type : Name_Id;\n");

               when others => null;
            end case;

         when Flat_Expression_Kinds =>
            Put ("Expr_Type : Name_Id;\n");

            case Kind is
               when Usage_Names =>
                  Put ("Ref_Name : Name_Id;\n");
                  Put ("Ref : Name_Id;\n");

               when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
                  Put ("Lit_Val : Name_Id;\n");

               when others => null;
            end case;

         when Flat_Pragma_Kinds =>
            Put ("Pragma_Name : Name_Id;\n");

         when A_Parameter_Specification | A_Formal_Object_Declaration =>
            Put ("Mode : Asis.Mode_Kinds;\n");

         when others => null;
      end case;

      for Q of Qs loop
         Put ("\1 : \2;\n", Q_Name (Q), Subtype_Name (Query_Result_Types (Q)));
      end loop;

      Put ("Sloc : Asis.Text.Span := Asis.Text.Nil_Span)\n");
      Put ("return \1\o", Subtype_Name (Kind));

      if not Is_Body then
         Put (";\n");
      else
         Put (" is\n");
         Put ("begin\n\i");

         Put ("return Result : constant \1 := Make (\2,\n\i",
              Subtype_Name (Kind),
              Capitalize (Kind'Img));
         Put ("(");

         if Qs'Length = 0 then
            Put ("1 .. 0 => <>");
         else
            if Qs'Length = 1 then
               Put ("1 => ");
            end if;

            for Q of Qs loop
               if First_Time then
                  First_Time := False;
               else
                  Put (", ");
               end if;

               Put ("(Queries.\1, \1\n)", Q_Name (Q));
               --  We need "Queries.", because the parameter name hides the
               --  Query name.
            end loop;
         end if;

         Put ("),\n");
         Put ("Sloc)\n\odo\n\i");

         case Kind is
            when A_Compilation_Unit | Def_Names =>
               Put ("Result.Def_Name := Def_Name;\n");

               case Kind is
                  when A_Compilation_Unit =>
                     Put ("Result.Unit_Kind := Unit_Kind;\n");
                     Put ("Result.Unit_Class := Unit_Class;\n");
                     Put ("Result.Unit_Origin := Unit_Origin;\n");
                     Put ("Result.Unit_Full_Name := Unit_Full_Name;\n");
                     Put ("Result.Source_File := Source_File;\n");

                  when Def_Names =>
                     Put ("Result.Def := Def;\n");
                     Put ("Result.Decl_Type := Decl_Type;\n");

                  when others => null;
               end case;

            when Flat_Expression_Kinds =>
               Put ("Result.Expr_Type := Expr_Type;\n");

               case Kind is
                  when Usage_Names =>
                     Put ("Result.Ref_Name := Ref_Name;\n");
                     Put ("Result.Ref := Ref;\n");

                  when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
                     Put ("Result.Lit_Val := Lit_Val;\n");

                  when others => null;
               end case;

            when Flat_Pragma_Kinds =>
               Put ("Result.Pragma_Name := Pragma_Name;\n");

            when A_Parameter_Specification | A_Formal_Object_Declaration =>
               Put ("Result.Mode := Mode;\n");

            when others =>
               Put ("null;\n");
         end case;

         Put ("\oend return;\n");
         Put ("\oend \1;\n\n", Constructor_Name (Kind));
      end if;
   end Construct_Constructor;

   procedure Construct_List_Constructor
     (Kind : Flat_List_Kinds; Is_Body : Boolean)
   is
      First_Time : Boolean := True;
   begin
      Put ("function \1", Constructor_Name (Kind));
      Put ("\i\n(Subtrees : Ada_Tree_Array := (1 .. 0 => <>);\n");

      Put ("Sloc : Asis.Text.Span := Asis.Text.Nil_Span)\n");
      Put ("return \1\o", Subtype_Name (Kind));

      if not Is_Body then
         Put (";\n");
      else
         Put (" is\n");
         Put ("begin\n\i");

         Put ("return Result : constant \1 := Make_List (\2,\n\i",
              Subtype_Name (Kind),
              Capitalize (Kind'Img));
         Put ("Subtrees, Sloc);\n\o");

         Put ("\oend \1;\n\n", Constructor_Name (Kind));
      end if;
   end Construct_List_Constructor;

   --  Start of processing for Generate_Factory

begin
   Text_IO.Create (Factory_Spec, Name => "gnat2xml-ada_trees-factory.ads");
   Text_IO.Create (Factory_Body, Name => "gnat2xml-ada_trees-factory.adb");

   Text_IO.Set_Output (Factory_Spec);
   Put ("--  Automatically generated by Gnat2xml.Ada_Trees.Generate_Factory.\n\n");
   Text_IO.Set_Output (Factory_Body);
   Put ("--  Automatically generated by Gnat2xml.Ada_Trees.Generate_Factory.\n\n");

   Text_IO.Set_Output (Factory_Spec);
   Put ("package Gnat2xml.Ada_Trees.Factory is\n\n");
   Put ("pragma Style_Checks (""M200""); -- Allow long lines\n\n");
   Text_IO.Set_Output (Factory_Body);
   Put ("package body Gnat2xml.Ada_Trees.Factory is\n\n");
   Put ("pragma Style_Checks (""M200""); -- Allow long lines\n\n");

   Indent;

   Text_IO.Set_Output (Factory_Spec);
   Put ("--  One subtype for each type/class. A concrete type asserts that\n");
   Put ("--  its Kind is equal to the appropriate kind. A list type\n");
   Put ("--  asserts (in addition) that all of its elements are in the\n");
   Put ("--  appropriate class. And an abstract class asserts that its\n");
   Put ("--  Kind is some Kind in the class.\n");
   Put ("--  \1 classes total.\n\n",
        Image
          (Integer'(Flat_Element_Kinds'Pos (Flat_Element_Kinds'Base'Last))));

   Put ("--  Abstract classes\n\n");

   for Class in Flat_Abstract_Classes loop
      Put ("subtype \1 is Ada_Tree with\n" &
           "\iPredicate => -- \2 kinds\n\i",
           Subtype_Name (Class),
           Image (Cardinality (Kinds_In_Class (Class))));

      Put ("\1.Kind in\n\i", Subtype_Name (Class));
      Put_Kinds (Kinds_In_Class (Class));
      Put ("\o");

      Put (";\n\n\o\o");
   end loop;

   Put ("--  Lists\n\n");

   for Class in Flat_List_Kinds loop
      Put ("subtype \1 is Ada_Tree with\n" &
           "\iPredicate =>\n\i", Subtype_Name (Class));

      if False then -- ???
         Put ("\1.Kind = \2 and then\n" &
                "\i(for all Subtree of \1.Subtrees =>\n" &
                  "\iSubtree in \3)\o\o",
              Subtype_Name (Class),
              Capitalize (Class'Img),
              List_Component_Subtype_Name (Class));
      else
         Put ("\1.Kind = \2",
              Subtype_Name (Class),
              Capitalize (Class'Img));
      end if;

      Put (";\n\n\o\o");
   end loop;

   Put ("--  Concrete single kinds\n\n");

   for Class in Opt_ASIS_Elems loop
      Put ("subtype \1 is Ada_Tree with\n" &
           "\iPredicate =>\n\i", Subtype_Name (Class));

      if False then -- ???
         Put ("\1.Kind = \2", Subtype_Name (Class), Capitalize (Class'Img));
      else
         if Class = Not_An_Element then
            Put ("\1.Kind = \2", Subtype_Name (Class), Capitalize (Class'Img));
         else
            Put ("\1.Kind in \2 | Not_An_Element",
                 Subtype_Name (Class), Capitalize (Class'Img));
         end if;
      end if;

      Put (";\n\n\o\o");
   end loop;

   Put ("--  Getters\n\n");
   --  ???Could narrow down the parameter type, too.
   for Q in Structural_Queries loop
      --  No_Query isn't a real query, so skip it.

      if Q not in No_Query then
         Put ("function \1 (Tree : Ada_Tree) return \2 is\n\i" &
              "(Get (Tree, \1));\n\o",
              Q_Name (Q), Subtype_Name (Query_Result_Types (Q)));
      end if;
   end loop;
   Put ("\n");

   Text_IO.Set_Output (Factory_Spec);
   Put ("--  Setters\n\n");
   for Q in Structural_Queries loop
      if Q not in No_Query then
         if False then -- ???
            Put ("procedure Set_\1 is new Generic_Setter\n\i" &
                 "(\1, \2);\n\o",
                 Q_Name (Q), Subtype_Name (Query_Result_Types (Q)));
         else
            Put ("procedure Set_\1 (Tree : Ada_Tree; Subtree: \2);\n",
                 Q_Name (Q), Subtype_Name (Query_Result_Types (Q)));
         end if;
      end if;
   end loop;
   Put ("\n");

   Text_IO.Set_Output (Factory_Body);
   Put ("--  Setters\n\n");
   for Q in Structural_Queries loop
      if Q not in No_Query then
         Put ("procedure Set_\1 (Tree : Ada_Tree; Subtree: \2) is\n" &
              "begin\n" &
              "\iSet (Tree, \1, Subtree);\o\n" &
              "end Set_\1;\n\n",
              Q_Name (Q), Subtype_Name (Query_Result_Types (Q)));
      end if;
   end loop;

   Text_IO.Set_Output (Factory_Spec);
   Put ("--  Constructors\n\n");
   for Kind in Opt_ASIS_Elems loop
      Construct_Constructor (Kind, Is_Body => False);
   end loop;

   for Kind in Flat_List_Kinds loop
      Construct_List_Constructor (Kind, Is_Body => False);
   end loop;

   Text_IO.Set_Output (Factory_Body);
   Put ("--  Constructors\n\n");
   for Kind in Opt_ASIS_Elems loop
      Construct_Constructor (Kind, Is_Body => True);
   end loop;

   for Kind in Flat_List_Kinds loop
      Construct_List_Constructor (Kind, Is_Body => True);
   end loop;

   Outdent;

   Text_IO.Set_Output (Factory_Spec);
   Put ("\nend Gnat2xml.Ada_Trees.Factory;\n");
   Text_IO.Set_Output (Factory_Body);
   Put ("end Gnat2xml.Ada_Trees.Factory;\n");
   Text_IO.Set_Output (Text_IO.Standard_Output);
   Text_IO.Close (Factory_Spec);
   Text_IO.Close (Factory_Body);

   Main_Done := True;
end Gnat2xml.Ada_Trees.Generate_Factory;
