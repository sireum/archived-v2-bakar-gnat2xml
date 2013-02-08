------------------------------------------------------------------------------
--                                                                          --
--                          GNAT2XML COMPONENTS                             --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (C) 2006, McKae Technologies.                   --
--                Copyright (C) 2012, AdaCore, Inc.                         --
--                                                                          --
-- Avatox is free software; you can redistribute it and/or modify it        --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Avatox is distributed in the hope  that it will be useful,      --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- Avatox is based off the Display_Source software distributed as part of   --
-- the ASIS implementation for GNAT, and therefore inherits its GPL         --
-- licensing.  Ada Core Technologies maintains the Display_Source program   --
-- and its copyright is held by the Free Software Foundation.               --
--                                                                          --
-- Avatox is now maintained by McKae Technologies (http://www.mckae.com)    --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;     use Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps;      use Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with Asis.Elements;
with Asis.Text;
with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Exceptions;
with Asis.Expressions;
with Asis.Extensions; use Asis.Extensions;
with Asis.Implementation;
with Asis.Statements;
with Asis.Set_Get;

with Gnatvsn;

with Mckae.Text.Lexicals;
with Mckae.Xml.Ez_Out;

with Strings_Edit.Utf8.Handling;

with Formatted_Output;
with Strings; use Strings;
with Gnat2xml.Command_Line; use Gnat2xml.Command_Line;
with Gnat2xml.Xml_File_Output;  use Gnat2xml.Xml_File_Output;

with Text_Io; use Text_IO;
with A4G.A_Output; use A4G.A_Output; -- ???
with A4G.Int_Knds;
with A4G.Queries;
with A4G.Mapping;

package body Gnat2xml.Xml is

   use Asis;
   --  to make all the literals from Element classification hierarchy
   --  directly visible

   subtype Structural_Queries is A4G.Queries.Structural_Queries;

   use all type Structural_Queries;

   use Mckae.Text;

   Dq_Map : Wide_Character_Set := To_Set ("'""");

   ------------------------------------------------------------------------------

   -- These declarations associate an empty string with a False boolean
   -- attribute value, and the string "true" with a True value. It's done this
   -- way to take advantage of XML EZ Out's default behavior of omitting
   -- attributes that have an empty string as their value.
   type Boolean_Attr_Values is array (Boolean) of Unbounded_String;
   Boolean_Attr_Value : constant Boolean_Attr_Values
     := (False => Null_Unbounded_String,
         True  => To_Unbounded_String("true"));

   -----------------------------------------------------------------------------

   function To_Trimmed_String (W : Wide_String) return Wide_String is
      S : Natural := W'First;
      First_Char  : Wide_Character := W(S);
      L : Natural := W'Last;
      Last_Char   : Wide_Character := W(L);
   begin
      if Is_In (First_Char, Dq_Map) then
         S := S + 1;
      end if;
      if Is_In (Last_Char, Dq_Map) then
         L := L - 1;
      end if;
      return W(S .. L);
   end To_Trimmed_String;

   ------------------------------------------------------------------------------

   function T (S          : String;
               Xml_Casing : Boolean) return String is
   begin
      if True then -- ???
         declare
            Result : constant String :=
              Lexicals.Transform (S,
                                  Lexicals.lower,
                                  Remove_Underscores => False);
         begin
            return Strip_Article (Result);
         end;
      end if;

      if Xml_Casing then
         return Lexicals.Transform (S,
                                    Lexicals.Xml_Common,
                                    Remove_Underscores => True);
      else
         return S;
      end if;
   end T;

   ------------------------------------------------------------------------------

   function Span_To_Compact_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List
   is
      use Ada.Strings;
      use Asis.Text;

   begin
      if Gnat2xml_Options.Compact_XML then
         return
           (0 => "sloc" =
              To_String (Trim (Line_Number_Positive'Wide_Image
                (Element_Span.First_Line), Left)) & ":" &
              To_String (Trim (Character_Position_Positive'Wide_Image
                (Element_Span.First_Column), Left)));
      else
         return No_Attributes;
      end if;
   end Span_To_Compact_Attributes;

   function Span_To_Verbose_Attributes
     (Element_Span : Asis.Text.Span)
      return         Attributes_List
   is
      use Ada.Strings;
      use Asis.Text;

   begin
      --??????pragma Assert (not Gnat2xml_Options.Compact_XML);
      return
        (("line" =
           To_String (Trim (Line_Number_Positive'Wide_Image
             (Element_Span.First_Line), Left))),
         ("col" =
            To_String (Trim (Character_Position_Positive'Wide_Image
              (Element_Span.First_Column),
              Left))),
         ("endline" =
            To_String (Trim (Line_Number'Wide_Image
              (Element_Span.Last_Line), Left))),
         ("endcol" =
            To_String (Trim (Character_Position'Wide_Image
              (Element_Span.Last_Column), Left))));
   end Span_To_Verbose_Attributes;

   -----------------------------------------------------------------------------

   function Decl_Of_Def_Id (Def_Id : Defining_Name) return Asis.Declaration is
      Result : Asis.Element := Def_Id;
      use Asis.Elements;
   begin
      loop
         Result := Enclosing_Element (Result);
         if Ekind (Result) in Flat_Declaration_Kinds then
            return Result;
         end if;
      end loop;
   end Decl_Of_Def_Id;

   function Decl_Kind (Def_Id : Defining_Name) return String is
      use Asis.Elements;
      Decl : constant Asis.Declaration := Decl_Of_Def_Id (Def_Id);
      Kind : constant Flat_Declaration_Kinds := Ekind (Decl);
      S1 : constant String := Kind'Img;
      S2 : constant String := T (S1, Xml_Casing => False);
      S3 : constant String := Strip_Suffix (S2, Suffix => "_Declaration");
      S4 : constant String := Strip_Suffix (S3, Suffix => "_Specification");
   begin
      return S4;
   end Decl_Kind;

   -----------------------------------------------------------------------------

   function Enclosing_Def_Id (Def_Id : Defining_Name) return Asis.Element is
      --  Returns the Defining_Name of the innermost enclosing declaration of
      --  Def_Id, or Nil_Element if Def_Id is a root library unit or Standard.
      --  If we have spec/body, we return the first (spec) one.

      use Asis.Elements;
      pragma Assert
        (Is_Identical (Def_Id, Corresponding_First_Definition (Def_Id)));
      Decl : Asis.Declaration := Decl_Of_Def_Id (Def_Id);
   begin
      --  Here, Decl is the Declaration of Def_Id. We loop upwards to find the
      --  innermost enclosing Decl.
      loop
         Decl := Enclosing_Element (Decl);
         case Ekind (Decl) is
            when Flat_Declaration_Kinds =>
               return Corresponding_First_Definition (First_Name (Decl));
            when Not_An_Element =>
               return Nil_Element;
            when others =>
               null;
         end case;
      end loop;
   end Enclosing_Def_Id;

   -----------------------------------------------------------------------------

   function Unique_Id (Def_Id : Defining_Name) return String is
      use Ada.Strings, Asis.Text, Asis.Declarations, Asis.Elements;
      use Strings_Edit.Utf8.Handling;

      D : constant Defining_Name := Corresponding_First_Definition (Def_Id);

      Span : constant Asis.Text.Span := Element_Span (D);
      L : constant Wide_String := Line_Number_Positive'Wide_Image
                                    (Span.First_Line);
      LL : constant Wide_String := Trim (L, Left); -- remove annoying blank
      C : constant Wide_String := Character_Position_Positive'Wide_Image
                                    (Span.First_Column);
      CC : constant Wide_String := Trim (C, Left); -- remove annoying blank

      Unit_Kind : constant Asis.Unit_Kinds :=
        Compilation_Units.Unit_Kind
          (Elements.Enclosing_Compilation_Unit (D));
      U : constant Wide_String :=
        (if Unit_Kind in A_Library_Unit_Body
           then "+"
           else "-");
      --  It is possible to have two declarations in the same scope
      --  that have the same Sloc, if one is in the spec and the other
      --  in the body, so we include a different character for spec vs. body
      --  to handle this unlikely case.

      Simple_Name: constant String :=
        To_Utf8 (Defining_Name_Image (D) & U & LL & ":" & CC);
      Enclosing : constant Asis.Element := Enclosing_Def_Id (D);
   begin
      if Is_Nil (Enclosing) then
         return Simple_Name;
      else
         return Unique_Id (Enclosing) & "/" & Simple_Name;
      end if;
   end Unique_Id;

   function Def_Value (Def_Id : Defining_Name) return String is
   begin
      --  Use To_Utf8 here and elsewhere???
      return "ada://" & Decl_Kind (Def_Id) & "/" & Unique_Id (Def_Id);
   end Def_Value;

   function Def_Name_Value (Def_Id : Defining_Name) return String is
      use Asis.Declarations;
      use Strings_Edit.Utf8.Handling;
   begin
      if Ekind (Def_Id) = A_Defining_Expanded_Name then
         return Def_Name_Value (Defining_Selector (Def_Id));
      else
         return To_Utf8 (Defining_Name_Image (Def_Id));
      end if;
   end Def_Name_Value;

   function Ref_Value (Ref_Id : Asis.Name) return String is
      use Expressions;
   begin
      if Is_Uniquely_Defined (Ref_Id) then
         declare
            Def_Id : Asis.Element := Corresponding_Name_Definition (Ref_Id);
         begin
            case Ekind (Def_Id) is
               when Not_An_Element =>
                  return "implicit declaration";
               when Def_Names =>
                  return Def_Value (Def_Id);
               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         --  It's something like a pragma or attribute name, so there is no
         --  corresponding name definition.
         return "null";
      end if;
   end Ref_Value;

   function Ref_Name_Value (Ref_Id : Asis.Name) return String is
      use Expressions;
      use Strings_Edit.Utf8.Handling;
   begin
      --  If there is a corresponding name definition, we return that name, so
      --  that casing is normalized (if you declare Mumble, and refer to it as
      --  mumble, the ref_name will be Mumble). If the declaration is implicit,
      --  or there is none, we return the name as written.

      if Is_Uniquely_Defined (Ref_Id) then
         declare
            Def_Id : Asis.Element := Corresponding_Name_Definition (Ref_Id);
         begin
            case Ekind (Def_Id) is
               when Not_An_Element =>
                  null;
               when Def_Names =>
                  return Def_Name_Value (Def_Id);
               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         --  It's something like a pragma or attribute name, so there is no
         --  corresponding name definition.
         null;
      end if;

      return To_Utf8 (Name_Image (Ref_Id));
   end Ref_Name_Value;

   -----------------------------------------------------------------------------

   function Type_Value (Elem : Asis.Element) return String is
      use Asis.Elements, Asis.Expressions, Asis.Declarations;
      Kind : constant Opt_ASIS_Elems := Ekind (Elem);
   begin
      case Kind is
         when Not_An_Element =>
            return "null";
            --  We need this in case Kind_Kludge below returned
            --  A_Box_Expression or A_Null_Literal.

         when Def_Names =>
            declare
               Decl : constant Asis.Declaration := Decl_Of_Def_Id (Elem);
            begin
               if Ekind (Decl) in A_Flat_Object_Declaration then
                  declare
                     Ident : Asis.Element := Object_Declaration_View (Decl);
                  begin
                     if Ekind (Ident) in A_Subtype_Indication then
                        Ident := Asis.Definitions.Subtype_Mark (Ident);
                     end if;

                     if Ekind (Ident) in A_Selected_Component then
                        Ident := Selector (Ident);
                     end if;

                     return Ref_Value (Ident);
                  end;

               else
                  return "null";
               end if;
            end;
         when Flat_Expression_Kinds =>
            if Is_True_Expression (Elem) then
               declare
                  Type_Decl : constant Asis.Declaration :=
                    Corresponding_Expression_Type (Elem);
                  use Asis.Elements;
               begin
                  if Is_Nil (Type_Decl) then
                     return "anonymous subtype";
                  elsif Asis.Set_Get.Is_Root_Num_Type (Type_Decl) then
                     --  We put a blank in the names of root and universal
                     --  numeric types (instead of an underscore) to
                     --  distinguish them from a user-defined type with the
                     --  same name.

                     case Root_Type_Kind (Type_Declaration_View (Type_Decl)) is
                        when Not_A_Root_Type_Definition =>
                           raise Program_Error;
                        when A_Root_Integer_Definition =>
                           return "root integer";
                        when A_Root_Real_Definition =>
                           return "root real";
                        when A_Universal_Integer_Definition =>
                           return "universal integer";
                        when A_Universal_Real_Definition =>
                           return "universal real";
                        when A_Universal_Fixed_Definition =>
                           return "universal fixed";
                     end case;

                  else
                     return Def_Value
                       (Corresponding_First_Definition
                          (First_Name (Type_Decl)));
                  end if;
               end;
            else
               return "null";
            end if;
         when others =>
            raise Program_Error;
      end case;
   end Type_Value;

   -----------------------------------------------------------------------------

   Stop_Kinds : array (Opt_ASIS_Elems) of Boolean := (others => False);
   --  For debuggging. E.g., set Stop_Kinds(A_Component_Declaration) := True
   --  in gdb to stop when Pre is passed A_Component_Declaration.
   --  And set a breakpoint on Breakpoint.

   procedure Breakpoint is
   begin
      null;
   end Breakpoint;

   -----------------------------------------------------------------------------

   procedure Process_Unit
     (The_Unit : in     Asis.Compilation_Unit;
      State    : in out Info_Node) is
      --  ???Much of the info in State could be local to here!

      use Strings_Edit.Utf8.Handling, Compilation_Units, Expressions;

      Enclosing_Formal_Subp : Asis.Element := Asis.Nil_Element;
      --  See Kind_Kludge below

      procedure Put_Src_Lines (Up_To : Asis.Text.Line_Number);
         --  Print out source lines up to the current Sloc, which is Up_To.
         --  This is for debugging the XML, and is only done in -q mode.

      procedure Process_Elem
        (Q       : in     Structural_Queries;
         Index   : in     Query_Index;
         Is_List_Element : in Boolean;
         Element : in     Asis.Element);

      procedure Process_List
        (Q       : in     Structural_Queries;
         List    : in     Asis.Element_List);

      procedure Pre
        (Q       : in     Structural_Queries;
         Index   : in     Query_Index;
         Is_List_Element : in Boolean;
         Element : in     Asis.Element)
      is
         use Declarations, Expressions;
         use Strings_Edit.Utf8.Handling;

         function Kind_Kludge return Opt_ASIS_Elems;
         --  This kludge works around the handling of generic formal subprogram
         --  defaults. In ASIS, the Formal_Subprogram_Default query doesn't work
         --  very well. If the default is "is Some_Name" it returns Some_Name, but
         --  if it's "is <>" or "is null", then the version in Declarations raises
         --  an exception, and the version in Extensions returns a
         --  Nil_Element. Neither behavior is very useful, so this function
         --  detects these useless cases, and changes the Kind accordingly.
         --
         --  In almost all cases, this just returns Ekind (Element).

         function Kind_Kludge return Opt_ASIS_Elems is
            Result : Opt_ASIS_Elems := Ekind (Element);
            use Asis.Elements;
         begin
            pragma Assert
              (if Q = Formal_Subprogram_Default then
                 Ekind (Enclosing_Formal_Subp) in
                   A_Formal_Procedure_Declaration | A_Formal_Function_Declaration);

            if Q = Formal_Subprogram_Default then
               if Result = Not_An_Element then
                  --  We can't just look at Enclosing_Element, because it
                  --  doesn't work for Nil.

                  case Asis.Elements.Default_Kind (Enclosing_Formal_Subp) is
                     when Not_A_Default =>
                        --  Can't get here, because the Enclosing_Formal_Subp
                        --  must be an appropriate element for Default_Kind.
                        raise Program_Error;

                     when A_Name_Default =>
                        --  Can't get here, because Result would not be
                        --  Not_An_Element in this case.
                        raise Program_Error;

                     when A_Box_Default =>
                        Result := A_Box_Expression;

                     when A_Null_Default =>
                        Result := A_Null_Literal;

                     when A_Nil_Default =>
                        --  Result = Not_An_Element is correct in this case
                        null;
                  end case;
               end if;

               Enclosing_Formal_Subp := Nil_Element;
            end if;

            return Result;
         end Kind_Kludge;

         Kind : constant Opt_ASIS_Elems := Kind_Kludge;

         Element_Span : constant Asis.Text.Span := Span (Element);

         Def_Name_Attrs : constant Attributes_List :=
           (if Kind in Def_Names
              then +("def_name" = Def_Name_Value (Element))
              else No_Attributes
           );

         Def_Attrs : constant Attributes_List :=
           (if Kind in Def_Names
              then +("def" = Def_Value (Element))
              else No_Attributes
           );

         Ref_Name_Attrs : constant Attributes_List :=
           (if Kind in Flat_Usage_Name_Kinds
              then +("ref_name" = Ref_Name_Value (Element))
              else No_Attributes
           );

         Ref_Attrs : constant Attributes_List :=
           (if Kind in Flat_Usage_Name_Kinds
              then +("ref" = Ref_Value (Element))
              else No_Attributes
           );

         Type_Attrs : constant Attributes_List :=
           (if Kind in Def_Names | Flat_Expression_Kinds
              then +("type" = Type_Value (Element))
              else No_Attributes
           );

         pragma Assert (if Kind in Flat_Pragma_Kinds then
                        Asis.Elements.Pragma_Name_Image (Element) /= "");

         Pragma_Name_Attrs : constant Attributes_List :=
           (if Kind in Flat_Pragma_Kinds
              then +("pragma_name" =
                       To_Utf8 (Asis.Elements.Pragma_Name_Image (Element)))
              else No_Attributes
           );

         Lit_Val_Attrs : constant Attributes_List :=
           (if Kind in An_Integer_Literal | A_Real_Literal | A_String_Literal
              then +("lit_val" = To_Utf8 (Value_Image (Element)))
              else No_Attributes
           );

         use Asis.Elements;

         Mode_Attrs : constant Attributes_List :=
           (if Kind in A_Parameter_Specification | A_Formal_Object_Declaration
              then +("mode" = To_Utf8 (Mode_Kind (Element)'Img))
              else No_Attributes
           );

         Attrs : constant Attributes_List :=
             Span_To_Compact_Attributes (Element_Span) &
             Def_Name_Attrs &
             Def_Attrs &
             Ref_Name_Attrs &
             Ref_Attrs &
             Type_Attrs &
             Pragma_Name_Attrs &
             Lit_Val_Attrs &
             Mode_Attrs;

         --  Start of processing for Pre
      begin
         if Stop_Kinds (Kind) then
            Breakpoint;
         end if;

         if Kind in
           A_Formal_Procedure_Declaration | A_Formal_Function_Declaration
         then
            Enclosing_Formal_Subp := Element;
         end if;

         if Gnat2xml_Options.Compact_XML then
            Put_Src_Lines (Up_To => Element_Span.First_Line);
         end if;

         if not Is_List_Element then
            Start_Element
              (State.XML_File.all,
               T (Structural_Queries'Image (Q) & "_q",
                 State.Xml_Style));
         end if;

         if Is_Leaf (Element) and then Gnat2xml_Options.Compact_XML then
            Output_Tag
              (State.XML_File.all,
               T (ASIS_Elems'Image (Kind),
                  State.Xml_Style),
               Attrs);

         else
            Start_Element
              (State.XML_File.all,
               T (ASIS_Elems'Image (Kind),
                  State.Xml_Style),
               Attrs);
            if not Gnat2xml_Options.Compact_XML then
               Output_Tag
                 (State.XML_File.all,
                  T ("sloc", State.Xml_Style),
                  Span_To_Verbose_Attributes (Element_Span));
            end if;
         end if;
      end Pre;

      procedure Process_Elem
        (Q       : in     Structural_Queries;
         Index   : in     Query_Index;
         Is_List_Element : in Boolean;
         Element : in     Asis.Element) is

         procedure Process_Children;

         procedure Process_Children is
            use A4G.Queries;
            Qs : Query_List renames Appropriate_Queries (Ekind (Element)).all;
         begin
            for Index in Qs'Range loop
               declare
                  Q : constant Structural_Queries := Qs (Index);
                  FE : constant Func_Elem := Get_Func_Elem (Q);
               begin
                  case FE.Query_Kind is
                     when Bug | CU_Query_Kinds =>
                        raise Program_Error;

                     --  For Boolean_Query, we concoct a dummy element with the
                     --  appropriate kind to pass to Process_Elem.

                     when Boolean_Query =>
                        declare
                           Val : constant Boolean := FE.Func_Boolean (Element);
                           Dummy_Kind : constant Boolean_Elems :=
                             Query_Result_Types (Q);

                           use A4G.Mapping;
                           Dummy_Child : constant Asis.Element :=
                             (if Val
                                then Node_To_Element_New
                                       (Node => Asis.Set_Get.Node (Element),
                                        Internal_Kind => A4G.Int_Knds.Internal_Element_Kinds (Dummy_Kind),
                                        In_Unit => The_Unit)
                                else Nil_Element);
                        begin
                           Process_Elem
                             (Q, Index,
                              Is_List_Element => False,
                              Element => Dummy_Child);
                        end;

                     when Single_Element_Query =>
                        declare
                           Child : constant Asis.Element :=
                             FE.Func_Simple (Element);
                        begin
                           Process_Elem
                             (Q, Index,
                              Is_List_Element => False,
                              Element => Child);
                        end;

                     when Element_List_Query =>
                        declare
                           Child_List : constant Asis.Element_List :=
                             FE.Func_List (Element);
                        begin
                           Process_List (Q, Child_List);
                        end;

                     when Element_List_Query_With_Boolean =>
                        declare
                           Child_List : constant Asis.Element_List :=
                             FE.Func_List_Boolean (Element, FE.Bool);
                        begin
                           Process_List (Q, Child_List);
                        end;
                  end case;
               end;
            end loop;
         end Process_Children;

         Kind : constant Opt_ASIS_Elems := Ekind (Element);
         use Declarations, Expressions;
         use Strings_Edit.Utf8.Handling;

         --  Start of processing for Process_Elem

      begin
         if Debug_Mode then -- ???
            Write_Element (Element);

            if Kind in Def_Names then
              Put_Line ("Defining_Name_Image = " & To_Utf8 (Defining_Name_Image (Element)));
            end if;

            if Kind in Flat_Usage_Name_Kinds then
              Put_Line ("Name_Image = " & To_Utf8 (Name_Image (Element)));
            end if;
         end if;

         Pre (Q, Index, Is_List_Element, Element);

         Process_Children;

         --  If it's a leaf and we're in compact mode, then we didn't do
         --  Start_Element in Pre_Procedure, but put out the whole element in
         --  short form, so in that case we don't want to End_Element here.

         if Is_Leaf (Element) and then Gnat2xml_Options.Compact_XML then
            null;
         else
            End_Element (State.XML_File.all);
         end if;

         if not Is_List_Element then
            End_Element (State.XML_File.all);
         end if;
      end Process_Elem;

      -----------------------------------------------------------------------------

      procedure Process_List
        (Q       : in     Structural_Queries;
         List    : in     Asis.Element_List) is
      begin
         Start_Element
           (State.XML_File.all,
            T (Structural_Queries'Image (Q) & "_ql",
              State.Xml_Style));  --  ???,

         for Index in List'Range loop
            Process_Elem
              (Q, Query_Index (Index),
               Is_List_Element => True,
               Element => List (Index));
            --  ???Reorder params so we don't need so many named notations.
         end loop;

         End_Element (State.XML_File.all);
      end Process_List;

      Name        : constant String :=
        To_String (Unit_Full_Name (The_Unit));
      Src         : constant String :=
        Ada.Directories.Simple_Name (To_String (Text_Name (The_Unit)));
      Unit_Span   : constant Asis.Text.Span :=
        Asis.Text.Compilation_Unit_Span
        (Asis.Elements.Unit_Declaration (The_Unit));
      use Formatted_Output;

      Src_File : aliased Text_IO.File_Type;
      --  Original source file, used to intersperse source text in the compact
      --  XML form.

      Cur_Line : Natural := 0;
      --  Current source file line that has been printed

      procedure Put_Src_Lines (Up_To : Asis.Text.Line_Number) is
      begin
         while not End_Of_File (Src_File) and then Cur_Line < Up_To loop
            Put_Line (State.XML_File.all, Get_Line (Src_File));
            Cur_Line := Cur_Line + 1;
         end loop;
      end Put_Src_Lines;

      --  Start of processing for Process_Unit

   begin
      if Gnat2xml_Options.Compact_XML then
         declare
            Name : constant String := To_String (Text_Name (The_Unit));
         begin
            Put_Line ("Opening source: " & Name);
            Open (Src_File, In_File, Name);
         end;
      end if;

      Start_Element
        (State.XML_File.all,
         T ("compilation_unit",
            State.Xml_Style),
         ("unit_kind" = Capitalize (Unit_Kind (The_Unit)'Img),
          "unit_class" = Capitalize (Unit_Class (The_Unit)'Img),
          "unit_origin" = Capitalize (Unit_Origin (The_Unit)'Img),
          "unit_full_name" = To_Utf8 (Unit_Full_Name (The_Unit)),
          "def_name" = Name,
          "source_file" = Src) &
          Span_To_Compact_Attributes (Unit_Span));
      if not Gnat2xml_Options.Compact_XML then
         Output_Tag
           (State.XML_File.all,
            T ("sloc", State.Xml_Style),
            Span_To_Verbose_Attributes (Unit_Span));
      end if;

      declare
         Cont_Clause_Elements : constant Element_List :=
            Asis.Elements.Context_Clause_Elements
              (Compilation_Unit => The_Unit,
               Include_Pragmas  => True);
         Unit_Element : constant Asis.Element :=
           Asis.Elements.Unit_Declaration (The_Unit);
         Pragmas : constant Element_List :=
            Asis.Extensions.Pragmas_After (Compilation_Unit => The_Unit);
         use A4G.Queries;
      begin
         Process_List
           (Q => Context_Clause_Elements,
            List => Cont_Clause_Elements);

         Process_Elem
           (Q => Unit_Declaration,
            Index => 2,
            Is_List_Element => False,
            Element => Unit_Element);

         Process_List
           (Q => Pragmas_After,
            List => Pragmas);
      end;

      End_Element (State.XML_File.all);

      if Gnat2xml_Options.Compact_XML then
         --  Force out the rest of the lines:
         Put_Src_Lines (Up_To => Natural'Last);
         Close (Src_File);
      end if;

      --  Termination, depending on the application
--???      Terminate_Node (The_Control, The_Node_Information);
   end Process_Unit;

   -----------------------------------------------------------------------------

   procedure Start_Representation
     (State : in out Info_Node)
   is
   begin
      if State.Krunch then
         Current_Format := McKae.XML.EZ_Out.Continuous_Stream;
      end if;

      -- XML container for code representation
      Output_XML_Header (State.XML_File.all);
   end Start_Representation;

end Gnat2xml.Xml;
