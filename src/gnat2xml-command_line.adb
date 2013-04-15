------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                 G N A T 2 X M L . C O M M A N D _ L I N E                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                  Copyright (C) 2012-2013, AdaCore, Inc.                  --
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

with Ada.Command_Line; use Ada.Command_Line;

with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;

package body Gnat2xml.Command_Line is

   Options_Set : Boolean := False;
   --  True if Set_Gnat2xml_Options has been called.

   The_Options : aliased Gnat2xml_Options_Type;

   function Gnat2xml_Options return access constant Gnat2xml_Options_Type is
   begin
      pragma Assert (Options_Set);
      return The_Options'Access;
   end Gnat2xml_Options;

   procedure Print_Command_Line is
   begin
      Put ("\1", Command_Name);

      for X in 1 .. Argument_Count loop
         Put (" \1", Argument (X));
      end loop;
      Put ("\n");
   end Print_Command_Line;

   procedure Set_Gnat2xml_Options (Options : Gnat2xml_Options_Type) is
   begin
      pragma Assert (not Options_Set);
      Options_Set := True;
      The_Options := Options;
   end Set_Gnat2xml_Options;

   procedure Gnat2xml_Usage is
      pragma Style_Checks ("M200"); -- Allow long lines

   begin
      Put ("Usage: gnat2xml [options] files\n");
      Put ("\n");
      Put ("""files"" are the Ada source file names.\n");
      Put ("\n");
      Put ("Options:\n");
      Put ("\n");
      --  -d9 -- debug mode (not documented)
      --  -dc -- print gcc commands (not documented)
      Put
        ("    -mdir -- generate one .xml file for each Ada source file, in directory\n");
      Put
        ("             'dir'. (Default is to generate the XML to standard output.)\n");
      Put ("\n");
      Put ("    -I <include-dir>\n");
      Put ("        directories to search for dependencies\n");
      Put ("\n");
      Put
        ("    -q -- debugging version, with interspersed source, and a more\n");
      Put
        ("          compact representation of ""sloc"". Note that this version\n");
      Put ("          currently does NOT validate.\n");
      Put ("\n");
      Put
        ("    -v -- verbose (print out the names of output files as they are generated).\n");
      Put ("\n");
      Put
        ("    -t -- do not delete tree files when done (they are deleted by default).\n");
      Put ("\n");
      Put ("    -h\n");
      Put
        ("    --help -- print this message and quit, ignoring all other options\n");
      Put ("\n");
      Put
        ("    --version -- print version and quit, ignoring all other options\n");
   end Gnat2xml_Usage;

end Gnat2xml.Command_Line;
