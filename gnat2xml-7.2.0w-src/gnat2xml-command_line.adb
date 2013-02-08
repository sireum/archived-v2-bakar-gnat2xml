------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                 G N A T 2 X M L . C O M M A N D _ L I N E                --
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

with Ada.Command_Line; use Ada.Command_Line;
with Text_IO;

with Formatted_Output; use Formatted_Output;
with Strings; use Strings;

package body Gnat2xml.Command_Line is

   Processed : Boolean := False;
   --  True if Process_Gnat2xml_Command_Line has been called.

   procedure Error (T : Template; X1, X2, X3, X4 : String := "");

   The_Gnat2xml_Options : aliased Gnat2xml_Options_Type;

   procedure Error (T : Template; X1, X2, X3, X4 : String := "") is
      Save : constant Text_IO.File_Type := Text_IO.Current_Output;
   begin
      Text_IO.Set_Output (Text_IO.Current_Error);
      Put ("error: ");
      Put (T, X1, X2, X3, X4);
      Print_Command_Line;
      Gnat2xml_Usage;
      raise Command_Line_Error;
   exception
      when others =>
         Text_IO.Set_Output (Save);
         raise;
   end Error;

   function Gnat2xml_Options return access constant Gnat2xml_Options_Type is
   begin
      pragma Assert (Processed);
      return The_Gnat2xml_Options'Access;
   end Gnat2xml_Options;

   procedure Process_Gnat2xml_Command_Line is
      X : Natural := 0;
      Ignore_Unknown_Options : Boolean := False;
   begin
      pragma Assert (not Processed);
      Processed := True;

      if Argument_Count = 0 then
         Error ("missing arguments\n");
      end if;

      --  --ignore-unknown-options means don't give an error for unrecognized
      --  options. It is currently not documented, because it is mainly of use
      --  for debugging the interface between gnatmake/gprbuild and gnat2xml.
      --  We need to process this option first, because it appears on the
      --  gnat2xml after some of the options to be ignored, and we have no
      --  control over that (gnatmake does).

      for X in 1 .. Argument_Count loop
         if Argument (X) = "--ignore-unknown-options" then
            Ignore_Unknown_Options := True;
         end if;
      end loop;

      while X < Argument_Count loop
         X := X + 1;

         declare
            Arg : constant String (1 .. Argument (X)'Length) := Argument (X);
         begin
            if Arg = "" then
               Error ("empty argument\n");
            end if;

            if Arg (1) = '-' then

               if Arg = "-h" or else Arg = "--help" then
                  The_Gnat2xml_Options.Help := True;
                  return;

               elsif Arg = "--version" then
                  The_Gnat2xml_Options.Version := True;
                  return;

               elsif Has_Prefix (Arg, Prefix => "-m") then
                  if Arg'Length < 3 then
                     Error ("missing directory name for -m\n");
                  end if;

                  The_Gnat2xml_Options.Output_Dir :=
                    To_Unbounded_String (Strip_Prefix (Arg, Prefix => "-m"));

               elsif Arg = "-I-" then
                  null; -- ???ignore this one for now

               elsif Arg = "-I" then
                  X := X + 1;
                  if X > Argument_Count then
                     Error ("missing include directory for -I\n");
                  end if;

                  Include (The_Gnat2xml_Options.Include_Directories, To_Unbounded_String (Argument (X)));

               elsif Has_Prefix (Arg, Prefix => "-I") then
                  pragma Assert (Arg'Length >= 3); -- because of previous elsif
                  Include (The_Gnat2xml_Options.Include_Directories, To_Unbounded_String (Strip_Prefix (Arg, Prefix => "-I")));

               elsif Arg = "-q" then
                  The_Gnat2xml_Options.Compact_XML := True;

               elsif Arg = "-g" then
                  The_Gnat2xml_Options.Debug_Mode := True;

               elsif Arg = "-v" then
                  The_Gnat2xml_Options.Verbose_Mode := True;

               elsif Arg = "-t" then
                  The_Gnat2xml_Options.Delete_Trees := False;

               --  Ignore options passed by gnatmake to gcc, so we can run
               --  gnat2xml from gnatmake, pretending gnat2xml is the compiler,
               --  using --GCC=..., as in:
               --
               --     gnatmake -gnatc -gnat2012 *.ad[sb] --GCC="gnat2xml -v -t -mxml"

               elsif
                 Arg = "-c" or else
                 Arg = "-gnatea" or else
                 Arg = "-gnatez" or else
                 Arg = "-gnatc"
               then
                  null;

               --  Similarly for options passed by gprbuild:

               elsif
                 Arg = "-gnatA" or else
                 Has_Prefix (Arg, Prefix => "-gnatec") or else
                 Has_Prefix (Arg, Prefix => "-gnatem")
               then
                  null;

               --  The "-x ada" option passed by gprbuild has an argument:

               elsif Arg = "-x" then
                  X := X + 1;

               elsif not Ignore_Unknown_Options then
                  Error ("'\1': unrecognized option\n", Arg);
               end if;

            else
               Include (The_Gnat2xml_Options.Input_Files, To_Unbounded_String (Arg));
            end if;
         end;
      end loop;

      if Is_Empty (The_Gnat2xml_Options.Input_Files) then
         Error ("no input files\n");
      end if;
   end Process_Gnat2xml_Command_Line;

   procedure Print_Command_Line is
   begin
      Put ("\1", Command_Name);
      for X in 1 .. Argument_Count loop
         Put (" \1", Argument (X));
      end loop;
      Put ("\n");
   end Print_Command_Line;

   procedure Gnat2xml_Usage is
   begin
      Put ("Usage: gnat2xml [options] files\n");
      Put ("\n");
      Put ("""files"" are the Ada source file names.\n");
      Put ("\n");
      Put ("Options:\n");
      Put ("\n");
      Put ("    -mdir -- generate one .xml file for each Ada source file, in directory\n");
      Put ("             'dir'. (Default is to generate the XML to standard output.)\n");
      Put ("\n");
      Put ("    -I <include-dir>\n");
      Put ("        directories to search for dependencies\n");
      Put ("\n");
      Put ("    -q -- debugging version, with interspersed source, and a more\n");
      Put ("          compact representation of ""sloc"". Note that this version\n");
      Put ("          currently does NOT validate.\n");
      --  -g is not documented???
      Put ("\n");
      Put ("    -v -- verbose (print out the names of output files as they are generated).\n");
      Put ("\n");
      Put ("    -t -- do not delete tree files when done (they are deleted by default).\n");
      Put ("\n");
      Put ("    -h\n");
      Put ("    --help -- print this message and quit, ignoring all other options\n");
      Put ("\n");
      Put ("    --version -- print version and quit, ignoring all other options\n");
   end Gnat2xml_Usage;

end Gnat2xml.Command_Line;
