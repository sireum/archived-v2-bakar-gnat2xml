------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . A D A _ T R E E S                   --
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

with Ada.Containers.Vectors;
with Ada.Exceptions;

with Snames;

with Generic_Formatted_Output;

separate (Gnat2xml.Ada_Trees)

procedure Tree_To_Ada (Tree : Ada_Tree; Source_Text : String) is

   procedure Put_To_Buffer (C : Character);
   --  Append C to Buffer

   procedure Put_To_Buffer (C : Character) is
   begin
      if (Last_Index (Buffer) = 0 or else Last_Element (Buffer) = NL)
        and then C /= NL
      then
         for J in 1 .. Cur_Indentation loop
            Append (Buffer, ' ');
         end loop;
      end if;

      Append (Buffer, C);

      if Debug_Mode then
         if C = NL then
            Text_IO.Put (Text_IO.Standard_Output, "<NL>");
            Text_IO.New_Line (Text_IO.Standard_Output);
         else
            Text_IO.Put (Text_IO.Standard_Output, C);
         end if;
      end if;
   end Put_To_Buffer;

   package Buffered_Output is new Generic_Formatted_Output
     (Basic_Put_Char => Put_To_Buffer);

   procedure Indent (Amount : Integer);
   procedure Indent (Amount : Integer) is
   begin
      Cur_Indentation := Cur_Indentation + Amount;
   end Indent;

   procedure New_Line;

   procedure New_Line is
   begin
      Buffered_Output.Put ("\n");
   end New_Line;

   procedure Write_Buffer (Buf : String);

   procedure Write_Buffer (Buf : String) is
      NL_Count : Natural := 0;
   begin
      for C of Buf loop
         if C = NL then
            NL_Count := NL_Count + 1;

            if NL_Count > 2 then
               goto Continue;
            end if;
         else
            NL_Count := 0;
         end if;

         Formatted_Output.Put_Char (C);

         <<Continue>>
      end loop;
   end Write_Buffer;

   procedure Subtree_To_Ada (Tree : Ada_Tree);
   --  We recursively walk the tree, and for most nodes, take the template
   --  from Template_Table, and pass it to Interpret_Template. Some nodes
   --  need special casing, and bypass the Template_Table. Subtree_To_Ada is
   --  directly recursive, and also mutually recursive with
   --  Interpret_Template.

   package Tree_Stacks is new Ada.Containers.Vectors
     (Index_Type => Query_Index, Element_Type => Ada_Tree);
   use Tree_Stacks;

   Tree_Stack : Tree_Stacks.Vector;
   --  Stack of trees that we're in the process of traversing. Pushed and
   --  popped at the beginning and end of Subtree_To_Ada.

   function Ancestor_Tree (N : Query_Count) return Ada_Tree is
      (Tree_Stack (Last_Index (Tree_Stack) - N));
   --  Returns the N'th ancestor of the current tree.
   --  Ancestor (0) is the current tree,
   --  Ancestor (1) is the parent of the current tree,
   --  Ancestor (2) is the grandparent of the current tree,
   --  and so on.

   function Parent_Tree return Ada_Tree is (Ancestor_Tree (1));

   Implicit_Null_Statement_Kludge : Boolean := False;
   --  See the comments about labels under "when A_Null_Statement =>" below for
   --  an explanation of this.

   procedure Subtree_To_Ada (Tree : Ada_Tree) is
      use Buffered_Output;

      procedure Subtrees_To_Ada (Tree : Ada_Tree; Pre, Between, Post : Ada_Template);

      procedure Interpret_Template
        (T : Ada_Template := Template_Table (Tree.Kind).all;
         Subtrees : Ada_Tree_Array := Tree.Subtrees;
         Kind : Ada_Tree_Kind := Tree.Kind);
      --  Interpret the template, printing literal characters, and recursively
      --  calling Subtree_To_Ada when the template calls for a subnode. Kind is
      --  for debugging.

      procedure Prefix_Notation_Call
        (Label_Names, Callee, Actuals : Ada_Tree);
      --  This is called for A_Function_Call and A_Procedure_Call_Statement
      --  when the Is_Prefix_Notation subtree is True. Prefix notation calls
      --  have special visibility rules, so we don't want to turn X.F(Y)
      --  into F(X, Y). Label_Names is always empty for function calls.

      procedure Subtrees_To_Ada (Tree : Ada_Tree; Pre, Between, Post : Ada_Template) is
         First_Time : Boolean := True;
      begin
         if Tree.Subtree_Count = 0 then
            return;
         end if;

         Interpret_Template (Pre, Subtrees => (1 .. 0 => <>));

         for Subtree of Tree.Subtrees loop
            if First_Time then
               First_Time := False;
            else
               Interpret_Template (Between, Subtrees => (1 .. 0 => <>));
            end if;

            Subtree_To_Ada (Subtree);
            --  ???Shouldn't this use the entire template?
         end loop;

         Interpret_Template (Post, Subtrees => (1 .. 0 => <>));
      end Subtrees_To_Ada;

      procedure Interpret_Template
        (T : Ada_Template := Template_Table (Tree.Kind).all;
         Subtrees : Ada_Tree_Array := Tree.Subtrees;
         Kind : Ada_Tree_Kind := Tree.Kind)
      is
         J : Positive := T'First;
         subtype Subtrees_Index is Query_Index range 1 .. Subtrees'Last;
         Used : array (Subtrees_Index) of Boolean := (others => False);
         Cur_Subtree_Index : Query_Count := 0;
         Numeric_Arg : Boolean;
         C : Character;
      begin
         while J <= T'Last loop
            Numeric_Arg := False;
            C := T (J);

            case C is
               when '$' =>
                  New_Line;
               when '{' =>
                  pragma Assert (T (J - 1) = '$');
                  Indent (Normal_Indent);
               when '}' =>
                  Indent (- Normal_Indent);

               when '!' | '?' =>
                  if J < T'Last and then T (J + 1) in '1' .. '9' then
                     Numeric_Arg := True;
                     J := J + 1;
                  else
                     Cur_Subtree_Index := Cur_Subtree_Index + 1;
                  end if;
                  declare
                     Subtree_Index : Query_Index;
                  begin
                     if Numeric_Arg then
                        Subtree_Index := Query_Index'Value ((1 => T (J)));
                     else
                        Subtree_Index := Cur_Subtree_Index;
                     end if;
                     if Subtree_Index not in Subtrees_Index then -- ???
                        Put ("Subtree_Index = \1, not in \2..\3 <<\4>>\n",
                             Image (Subtree_Index),
                             Image (Subtrees'First),
                             Image (Subtrees'Last),
                             "???Image (Tr.Kind)");
                     end if;
                     declare
                        Subtree : constant Ada_Tree :=
                          Subtrees (Subtree_Index);
                     begin
                        Used (Subtree_Index) := True;
                        if C = '!' then
                           Subtree_To_Ada (Subtree);
                        else pragma Assert (C = '?');
                           declare
                              function Scan_To_Tilde return Ada_Template;

                              function Scan_To_Tilde return Ada_Template is
                                 First : constant Positive := J + 1;
                              begin
                                 loop
                                    J := J + 1;
                                    exit when T (J) = '~';
                                 end loop;
                                 return T (First .. J - 1);
                              end Scan_To_Tilde;

                              Pre : constant Ada_Template := Scan_To_Tilde;
                              pragma Assert (T (J) = '~');
                              Between : constant Ada_Template := Scan_To_Tilde;
                              pragma Assert (T (J) = '~');
                              Post : constant Ada_Template := Scan_To_Tilde;
                              pragma Assert (T (J) = '~');
                           begin
                              Used (Subtree_Index) := True;
                              case Subtree.Kind is -- ???This is kind of kludgy
                                 when Flat_List_Kinds =>
                                    Append (Tree_Stack, Subtree); -- push
                                    Subtrees_To_Ada (Subtree, Pre, Between, Post);
                                    Delete_Last (Tree_Stack); -- pop

                                 when Not_An_Element =>
                                    null;

                                 when others =>
                                    Interpret_Template (Pre, Subtrees => (1 .. 0 => <>));
                                    if False and then Between /= "" then -- ???
                                       Put ("\1, \2: ???Between = <<\3>>, T = <<\4>>\n",
                                            "???Image (Tr.Kind)",
                                            Image (Subtree.Kind),
                                            String (Between),
                                            String (T));
                                    pragma Assert (Between = "");
                                    end if;
                                    Subtree_To_Ada (Subtree);
                                    Interpret_Template (Post, Subtrees => (1 .. 0 => <>));
                              end case;
                           end;
                        end if;
                     end;
                  end;

               when ';' =>
                  if Implicit_Null_Statement_Kludge then
                     Implicit_Null_Statement_Kludge := False;
                  else
                     Put_Char (C);
                  end if;

               when others =>
                  Put_Char (C);

            end case;

            J := J + 1;
         end loop;

         if Used /= (Subtrees_Index => True) then -- ???
            Put("???Not all used: \1", "???Image (Tr.Kind)");
         end if;
         -- ???pragma Assert (Used = (Subtrees_Index => True));
      end Interpret_Template;

      procedure Prefix_Notation_Call
        (Label_Names, Callee, Actuals : Ada_Tree)
      is

         --  For X.F(Y,Z), which is shorthand for F(X,Y,Z), First is X
         --  and Rest is Y,Z.

         First : constant  Ada_Tree := Actuals.Subtrees (1);

         Rest : constant Ada_Tree :=
           new Ada_Tree_Rec'
             (Kind => An_Association_List,
              Subtree_Count => Actuals.Subtree_Count - 1,
              Sloc => Asis.Text.Nil_Span,
              Subtrees => Actuals.Subtrees (2 .. Actuals.Subtree_Count));
      begin
         if Label_Names.Subtree_Count /= 0 then
            raise Program_Error with "labeled prefix calls not yet implemented";
         end if;
         Subtree_To_Ada (First);
         Put (".");
         Subtree_To_Ada (Callee);
         Subtrees_To_Ada (Rest, Pre => " (", Between => ", ", Post => ")");
      end Prefix_Notation_Call;

      procedure Maybe_Blank_Line;
      procedure Maybe_Blank_Line is
         Insert_Blank_Line_Before : Boolean := False;
      begin
         if Tree.Kind in
           An_Ordinary_Type_Declaration | -- ???(if rec etc)A_Record_Type_Definition A_Derived_Record_Extension_Definition
           A_Task_Type_Declaration |
           A_Protected_Type_Declaration |
           A_Single_Task_Declaration |
           A_Single_Protected_Declaration |
           A_Procedure_Body_Declaration |
           A_Function_Body_Declaration |
           A_Package_Declaration | -- ???(non lib unit)
           A_Package_Body_Declaration |
           A_Task_Body_Declaration |
           A_Protected_Body_Declaration |
           An_Entry_Body_Declaration |
           A_Generic_Procedure_Declaration |
           A_Generic_Function_Declaration |
           A_Generic_Package_Declaration |
           An_Enumeration_Type_Definition |--???(if big)
           A_Loop_Statement |
           A_While_Loop_Statement |
           A_For_Loop_Statement |
           A_Block_Statement |
           An_Extended_Return_Statement |
           An_Accept_Statement |
           A_Selective_Accept_Statement |
           A_Timed_Entry_Call_Statement |
           A_Conditional_Entry_Call_Statement |
           An_Asynchronous_Select_Statement |
           An_If_Path | --???Must look up to If_Statement, and then up to list.
           An_Elsif_Path |
           An_Else_Path |
           A_Case_Path |
           A_Record_Representation_Clause
--           An_Exception_Handler |???
         then
            declare
               Parent : constant Ada_Tree := Parent_Tree;
            begin
               if Parent.Kind in Flat_List_Kinds then
                  if Parent.Subtrees (1) /= Tree then
                     Insert_Blank_Line_Before := True;
                  end if;
               end if;
            end;
         end if;

         if Insert_Blank_Line_Before then
            New_Line;
            New_Line;
         end if;
      end Maybe_Blank_Line;

      use Asis;
      Index : Query_Index := 1;

      --  Start of processing for Subtree_To_Ada

   begin
      Append (Tree_Stack, Tree); -- push

      Maybe_Blank_Line;

      case Tree.Kind is
         when A_Compilation_Unit =>
            Subtrees_To_Ada (Tree.Subtrees (1), Pre => "", Between => ";$", Post => ";$$");
            --  If it's a subunit, we need "separate (Parent.Name)"

            if Tree.Unit_Kind in A_Subunit then
               declare
                  N : constant String :=
                    Get_Name_String (Tree.Unit_Full_Name);
                  Last : Positive := N'Last;
               begin
                  --  Determine parent name by searching for the last '.'

                  while N (Last) /= '.' loop
                     Last := Last - 1;
                  end loop;
                  Last := Last - 1;

                  Put ("separate (\1)", N (1 .. Last));
                  Interpret_Template ("$", Subtrees => (1 .. 0 => <>));
               end;
            end if;

            case Tree.Unit_Class is
               when A_Private_Declaration =>
                  Put ("private ");

               when A_Public_Declaration |
                 A_Public_Body |
                 A_Public_Declaration_And_Body |
                 A_Private_Body |
                 A_Separate_Body =>
                   null;

               when Not_A_Class =>
                  raise Program_Error;
            end case;

            Subtree_To_Ada (Tree.Subtrees (2));
            Put (";");
            Interpret_Template ("$", Subtrees => (1 .. 0 => <>));
            Subtrees_To_Ada
              (Tree.Subtrees (3), Pre => "", Between => ";$", Post => ";$");

         when Def_Names =>
            if Tree.Kind = A_Defining_Expanded_Name then
               Interpret_Template ("!.!");
            else
               Put ("\1", Get_Name_String (Tree.Def_Name));
            end if;

         when Usage_Names =>
            Put ("\1", Get_Name_String (Tree.Ref_Name));

         when An_Integer_Literal | A_Real_Literal | A_String_Literal =>
            Put ("\1", Get_Name_String (Tree.Lit_Val));

         when Flat_Pragma_Kinds =>
            Put ("pragma \1", Get_Name_String (Tree.Pragma_Name));
            Interpret_Template ("? (~, ~)~");
         when A_Null_Statement =>
            --  If a label comes at the end of a statement list, as allowed in
            --  Ada 2012, ASIS inserts an extra implicit null statement to hang
            --  the label off of. We don't want to print that statement,
            --  because it wasn't in the source code. We can detect such
            --  implicit null statements by checking for a nil Sloc. We also
            --  need to suppress the ";" that comes after the implicit 'null',
            --  which is the purpose of Implicit_Null_Statement_Kludge. We set
            --  that flag True here, and the very next template character seen
            --  by Interpret_Template will be that ";", so Interpret_Template
            --  will suppress the ";" and reset Implicit_Null_Statement_Kludge
            --  to False.

            if Tree.Subtrees (1).Subtree_Count /= 0 and then
              Asis.Text.Is_Nil (Tree.Sloc) then
               Interpret_Template ("?<<~~>>$~");
               Implicit_Null_Statement_Kludge := True;

            else
               Interpret_Template;
            end if;

         when An_Ordinary_Type_Declaration =>
            if Tree.Subtrees (3).Kind in
              A_Derived_Record_Extension_Definition |
              A_Record_Type_Definition |
              A_Tagged_Record_Type_Definition
            then
               Interpret_Template ("type !! is !" & Aspects);
               --  Record_Definition will take care of new lines

            else
               Interpret_Template;
            end if;

        when A_Procedure_Call_Statement =>
           if Tree.Subtrees (4).Kind = An_Is_Prefix_Notation then
              Prefix_Notation_Call
                (Label_Names => Tree.Subtrees (1),
                 Callee => Tree.Subtrees (2),
                 Actuals => Tree.Subtrees (3));

           else
              Interpret_Template;
           end if;

        when A_Function_Call =>
           --  Note: Is_Prefix_Notation is for Object.Operation(...)
           --  notation, whereas Is_Prefix_Call is for anything that's not
           --  an operator notation call. Thus Is_Prefix_Call is True for
           --  "&"(X, Y), and False for X&Y.

           if Tree.Subtrees (4).Kind = An_Is_Prefix_Notation then
              Prefix_Notation_Call
                (Label_Names => Empty (A_Defining_Name_List),
                 Callee => Tree.Subtrees (1),
                 Actuals => Tree.Subtrees (2));

           --  Determine whether to use operator notation, like X+Y instead of
           --  "+"(X,Y). We can use operator notation if it's an operator call,
           --  and the argument(s) are in positional notation (not
           --  named). ???We must use operator notation for "/=", to work
           --  around compiler bug. In some cases, "/="(X, Y) doesn't work (on
           --  access types?), so we generate (X /= Y) instead.

           else
              declare
                 Arg1, Arg2 : Ada_Tree_Base;

                 function Is_Positional (Arg : Ada_Tree) return Boolean is
                    (Arg.Subtrees (1).Kind = Not_An_Element);

                 Use_Operator_Notation : constant Boolean :=
                   Tree.Subtrees (3).Kind /= An_Is_Prefix_Call;

                 --  We don't want to translate
                 --  "&" (STRING'("AB"), STRING'("CDEF"))(5) /= CHARACTER'('E') into
                 --  ((STRING'("AB") & STRING'("CDEF")) (5) /= CHARACTER'('E'))
                 --  because an operator-notation call is not a name, and
                 --  therefore cannot be used as the prefix of an indexed
                 --  component.

              begin
                 if Tree.Subtrees (2).Subtree_Count >= 1 then
                    Arg1 := Tree.Subtrees (2).Subtrees (1);
                    if Is_Positional (Arg1) then
                       if Tree.Subtrees (2).Subtree_Count >= 2 then
                          Arg2 := Tree.Subtrees (2).Subtrees (2);
                          if Is_Positional (Arg2) then
                             null;
                          else
                             pragma Assert (not Use_Operator_Notation);
                          end if;
                       end if;
                    else
                       pragma Assert (not Use_Operator_Notation);
                    end if;
                 end if;

                 if Use_Operator_Notation then
                    pragma Assert
                      (Tree.Subtrees (1).Kind in Flat_Operator_Symbol_Kinds);
                    pragma Assert (Tree.Subtrees (2).Subtree_Count in 1 .. 2);

                    declare
                       Q_Op_Sym : constant String :=
                         Get_Name_String (Tree.Subtrees (1).Ref_Name);
                       Op_Sym : constant Name_Id :=
                         Name_Find (Q_Op_Sym (2 .. Q_Op_Sym'Last - 1));
                       --  Strip off quotes
                       Op : constant Ada_Tree := Clone (Tree.Subtrees (1));

                    begin
                       Op.Ref_Name := Op_Sym;

                       --  Unary operator

                       if Tree.Subtrees (2).Subtree_Count = 1 then
                          if
                            Tree.Subtrees (1).Kind in
                              A_Unary_Plus_Operator | A_Unary_Minus_Operator
                          then
                             Interpret_Template ("!!", (Op, Arg1));
                          else
                             Interpret_Template ("! !", (Op, Arg1));
                          end if;

                       --  Binary operator

                       else
                          Interpret_Template ("! ! !", (Arg1, Op, Arg2));
                       end if;
                    end;

                 else
                    Interpret_Template;
                 end if;
              end;
           end if;

         --  For task [type] declarations, use short form if possible

         when A_Task_Type_Declaration =>
            if Tree.Subtrees (4).Subtree_Count = 0 and then
              Tree.Subtrees (5).Subtree_Count = 0
            then
               Interpret_Template ("task type !!" & Aspects & "!!");
            else
               Interpret_Template;
            end if;

         when A_Single_Task_Declaration =>
            if Tree.Subtrees (3).Subtree_Count = 0 and then
              Tree.Subtrees (4).Subtree_Count = 0
            then
               Interpret_Template ("task !" & Aspects & "!!");
            else
               Interpret_Template;
            end if;

         when A_Block_Statement =>
            if Tree.Subtrees (3).Subtree_Count = 0 then
               Interpret_Template ("?<<~~>>$~" &
                                   "?~~ : ~!" &
                                   Handled_Seq ("2"));
            else
               Interpret_Template;
            end if;

         when A_Subtype_Indication =>
            case Tree.Subtrees (4).Kind is
               when A_Range_Attribute_Reference | A_Simple_Expression_Range =>
                  Interpret_Template ("?~~ ~?~~ ~!? range ~~~");
               when others =>
                  Interpret_Template ("?~~ ~?~~ ~!? ~~~");
            end case;

         when A_Constrained_Array_Definition |
              A_Formal_Constrained_Array_Definition =>
            --  ???Following is wrong for multi-dim arrays
            --  (only some indices need "range")
            --  And it doesn't work anyway.
            case Tree.Subtrees (1).Subtrees (1).Kind is
               when A_Range_Attribute_Reference | A_Simple_Expression_Range =>
                  Interpret_Template ("array (?range ~, range ~~) of !");
               when others =>
                  Interpret_Template ("array (?~, ~~) of !");
            end case;

         when An_Extended_Return_Statement =>
            --  If there are no statements or exception handlers, use short form

            if Tree.Subtrees (2).Subtree_Count = 0 and then
               Tree.Subtrees (3).Subtree_Count = 0
            then
               Interpret_Template ("return !!!");
               --  The last "!!" generates nothing, but satisfies the
               --  requirement that we use all the subtrees.
            else
               Interpret_Template;
            end if;

         when An_Accept_Statement =>
            --  If there are no statements or exception handlers, use short form

            if Tree.Subtrees (5).Subtree_Count = 0 and then
               Tree.Subtrees (6).Subtree_Count = 0
            then
               Interpret_Template
                 ("?<<~~>>$~" &
                  "accept !? (~~)~? (~; ~)~!!");
               --  The last "!!" generates nothing, but satisfies the
               --  requirement that we use all the subtrees.
            else
               Interpret_Template;
            end if;

         when A_Qualified_Expression =>
            if Tree.Subtrees (2).Kind in
              A_Record_Aggregate |
              An_Extension_Aggregate |
              A_Positional_Array_Aggregate |
              A_Named_Array_Aggregate
            then
               Interpret_Template ("!'!");
               --  If the thing after the ' is an aggregate, we leave out
               --  the parentheses here, because the aggregate will insert
               --  them. We want T'(X, Y, Z), not T'((X, Y, Z)).
            else
               Interpret_Template;
            end if;

         when A_Record_Aggregate =>
            if Tree.Subtrees (1).Subtree_Count = 0 then
               Interpret_Template ("(null record)!");
            else
               Interpret_Template;
            end if;

         when An_Extension_Aggregate =>
            if Tree.Subtrees (2).Subtree_Count = 0 then
               Interpret_Template ("(! with$" &
                                   "{null record)}!");
            else
               Interpret_Template;
            end if;

         when A_Parameter_Specification | A_Formal_Object_Declaration =>
            Subtrees_To_Ada
              (Tree.Subtrees (Index), Pre => "", Between => ", ", Post => "");
            Put (" : ");

            if Tree.Kind = A_Parameter_Specification then
               --  A_Formal_Object_Declaration doesn't have "aliased"

               Index := Index + 1;

               if Tree.Subtrees (Index).Kind /= Not_An_Element then
                  Subtree_To_Ada (Tree.Subtrees (Index));
                  Put (" ");
               end if;
            end if;

            case Tree.Mode is
               when Not_A_Mode => raise Program_Error;
               when A_Default_In_Mode => null;
               when An_In_Mode => if True then Put ("in "); end if; -- ????
               when An_Out_Mode => Put ("out ");
               when An_In_Out_Mode => Put ("in out ");
            end case;

            Index := Index + 1;

            if Tree.Subtrees (Index).Kind /= Not_An_Element then
               Subtree_To_Ada (Tree.Subtrees (Index));
               Put (" ");
            end if;

            Index := Index + 1;
            Subtree_To_Ada (Tree.Subtrees (Index));

            Index := Index + 1;
            if Tree.Subtrees (Index).Kind /= Not_An_Element then
               Put (" := ");
               Subtree_To_Ada (Tree.Subtrees (Index));
            end if;

         when Flat_List_Kinds =>
            Subtrees_To_Ada (Tree, Pre => "", Between => "$", Post => "");

         when others =>
            if Template_Table (Tree.Kind) = null then
               Put ("null templ:\1", Image (Tree.Kind));
               Subtrees_To_Ada (Tree, Pre => "{", Between => "|", Post => "}");
            else
               Interpret_Template;
            end if;

      end case;

      Delete_Last (Tree_Stack); -- pop
   end Subtree_To_Ada;

begin
   Clear (Buffer);
   pragma Assert (Cur_Indentation = 0);

   Subtree_To_Ada (Tree);

   Write_Buffer (To_String (Buffer));
end Tree_To_Ada;
