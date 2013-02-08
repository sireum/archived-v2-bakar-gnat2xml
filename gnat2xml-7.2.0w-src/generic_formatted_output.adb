------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--              G E N E R I C _ F O R M A T T E D _ O U T P U T             --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded;

with Gnat2xml;
with Strings; use Strings;

package body Generic_Formatted_Output is

   Indentation : Natural := 0;
   Column : Natural := 1;

   procedure Raw_Put_Char (C : Character);
   --  Put the character and adjust Column

   procedure Put (S : String);
   --  Put_Char all the characters

   type Finalization is new Limited_Controlled with null record;
   procedure Finalize (X : in out Finalization);
   The_Finalization : Finalization;
   --  Declare a singleton object to check that the indentation isn't messed up
   --  -- we should end up at zero indentation.

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Finalization) is
   begin
      if Gnat2xml.Main_Done then
         if Indentation /= 0 then
            raise Program_Error with "Indentation should be zero at end.";
         end if;
      end if;
   end Finalize;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (Indentation_Amount : Natural := Default_Indentation_Amount)
   is
   begin
      Indentation := Indentation + Indentation_Amount;
   end Indent;

   -------------
   -- Outdent --
   -------------

   procedure Outdent
     (Indentation_Amount : Natural := Default_Indentation_Amount)
   is
   begin
      Indentation := Indentation - Indentation_Amount;
   end Outdent;

   ---------
   -- Put --
   ---------

   procedure Put (T : Template; X1, X2, X3, X4 : String := "") is
      J : Positive := T'First;
      Used : array (1 .. 4) of Boolean := (others => False);
   begin
      while J <= T'Last loop
         if T (J) = '\' then
            J := J + 1;
            case T (J) is
               when 'n' =>
                  Put_Char (NL);
               when '\' =>
                  Put_Char ('\');
               when 'i' =>
                  Indent;
               when 'o' =>
                  Outdent;
               when '1' =>
                  Used (1) := True;
                  Put (X1);
               when '2' =>
                  Used (2) := True;
                  Put (X2);
               when '3' =>
                  Used (3) := True;
                  Put (X3);
               when '4' =>
                  Used (4) := True;
                  Put (X4);
               when others =>
                  raise Program_Error;
            end case;

         else
            Put_Char (T (J));
         end if;
         J := J + 1;
      end loop;

      if not Used (1) then
         pragma Assert (X1 = "");
      end if;
      if not Used (2) then
         pragma Assert (X2 = "");
      end if;
      if not Used (3) then
         pragma Assert (X3 = "");
      end if;
      if not Used (4) then
         pragma Assert (X4 = "");
      end if;
   end Put;

   procedure Put (S : String) is
   begin
      for J in S'Range loop
         Put_Char (S (J));
      end loop;
   end Put;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (C : Character) is
   begin
      if Column = 1 and then C /= NL then
         for J in 1 .. Indentation mod 60 loop
            --  The "mod 60" is so we don't indent by huge amounts
            Raw_Put_Char (' ');
         end loop;
      end if;
      Raw_Put_Char (C);
   end Put_Char;

   ------------------
   -- Raw_Put_Char --
   ------------------

   procedure Raw_Put_Char (C : Character) is
   begin
      Basic_Put_Char (C);

      if C = NL then
         Column := 1;
      else
         Column := Column + 1;
      end if;
   end Raw_Put_Char;

   ---------------------
   -- Set_Indentation --
   ---------------------

   procedure Set_Indentation (Indentation_Amount : Natural) is
   begin
      Indentation := Indentation_Amount;
   end Set_Indentation;

end Generic_Formatted_Output;
