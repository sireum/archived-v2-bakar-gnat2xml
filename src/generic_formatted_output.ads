------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--              G E N E R I C _ F O R M A T T E D _ O U T P U T             --
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

with Namet;

generic
   with procedure Basic_Put_Char (C : Character);
package Generic_Formatted_Output is

   --  Simple formatted output.

   --  Client passes in Basic_Put_Char, which determines where the output
   --  characters go.

   type Template is new String;
   procedure Put (T : Template; X1, X2, X3, X4 : String := "");
   --  Prints the template as is, except for the following escape
   --  characters:
   --    "\n" is end of line.
   --    "\1" is replaced with X1, and similarly for 2, 3, 4.
   --    "\\" is "\".

   procedure Put_Char (C : Character);
   --  Same as Put ("\1", (1 => C));

   Default_Indentation_Amount : constant Natural := 3;

   procedure Indent
     (Indentation_Amount : Natural := Default_Indentation_Amount);
   procedure Outdent
     (Indentation_Amount : Natural := Default_Indentation_Amount);
   --  Increase/decrease indentation level by given number of spaces

   procedure Set_Indentation (Indentation_Amount : Natural);
   --  Set the indentation level

end Generic_Formatted_Output;
