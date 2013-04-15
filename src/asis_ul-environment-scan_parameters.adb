------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
--  A S I S _ U L . E N V I R O N M E N T . S C A N _ P A R A M E T E R S   --
--                                                                          --
--              (adapted for gnatpp from ASIS Utility Library)              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
--                                                                          --
-- GNATPP is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNATPP is  distributed in the  hope that it will  be  useful, but --
-- WITHOUT ANY WARRANTY; without even the implied warranty of  MERCHANTABI- --
-- LITY or  FITNESS  FOR A  PARTICULAR  PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write to the Free Software Foundation,  51 Franklin Street, Fifth Floor, --
-- Boston,                                                                  --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with ASIS_UL.Compiler_Options;
with ASIS_UL.Options;          use ASIS_UL.Options;
with ASIS_UL.Source_Table;

with Gnat2xml.Command_Line; use Gnat2xml.Command_Line;

with ASIS_UL.Formatted_Output; use ASIS_UL.Formatted_Output;

separate (ASIS_UL.Environment)
procedure Scan_Parameters is
   Options : Gnat2xml_Options_Type;
   Initial_Char : Character;
begin
   Warning_Mode := Quiet;
   --  Otherwise an average gnatpp run would generate too much diagnoses about
   --  problems in reformatting

   Process_RTL_Units := True;
   --  gnatpp does not care about this

   Initialize_Option_Scan
     (Stop_At_First_Non_Switch => False,
      Section_Delimiters       => "cargs");

   loop
      Initial_Char := GNAT.Command_Line.Getopt
        (
         "c gnatea gnatez gnatc " & -- ignored switches passed by gnatmake
         "gnatA gnatem! x: " & -- ignored switches passed by gprbuild
         "gnatec! " &
         "I: -RTS= v q d? " &
         "files= " &
         "h -help -version " &
         "m: t"
        );

      --  Disable printing of each option for now

      if False and then (Initial_Char = 'v' or else
        (Options.Verbose_Mode and then Initial_Char /= ASCII.NUL))
      then
         Put ("\1: ""\2"" = ""\3""\n",
              (1 => Initial_Char),
              Full_Switch,
              (if Initial_Char in 'd' | 'f' | 'm' then Parameter else ""));
      end if;

      case Initial_Char is
         when ASCII.NUL =>
            exit when not More_Arguments;

         when 'c' =>
            if Full_Switch = "c" then
               null; -- ignore
            else
               pragma Assert (False);
            end if;

         when 'd' =>
            if Full_Switch = "d" then
               Set_Debug_Options (Parameter);
               Options.Debug_Mode := True;
            else
               pragma Assert (False);
            end if;

         when 'f' =>
            if Full_Switch = "files" then
               Read_Args_From_File (Parameter, Store_With_No_Check => True);
            else
               pragma Assert (False);
            end if;

         when 'g' =>
            if Full_Switch = "gnatec" then
               Store_GNAT_Option_With_Path (Full_Switch, Parameter);
            elsif Full_Switch in
              "gnatea" | "gnatez" | "gnatc" | "gnatA" | "gnatem"
            then
               null; -- ignore
            else
               pragma Assert (False);
            end if;

         when 'h' =>
            if Full_Switch = "h" then
               Gnat2xml_Usage;
               GNAT.OS_Lib.OS_Exit (Status => 0);
            else
               pragma Assert (False);
            end if;

         when 'I' =>
            if Full_Switch = "I" then
               Store_I_Option (Parameter);
            else
               pragma Assert (False);
            end if;

         when 'm' =>
            if Full_Switch = "m" then
               declare
                  Out_Dir : constant String := Full_Name (Parameter);
               begin
                  Options.Output_Dir := To_Unbounded_String (Out_Dir);
               end;
            else
               pragma Assert (False);
            end if;

         when 'q' =>
            if Full_Switch = "q" then
               Options.Compact_XML := True;
               --  ???Quiet_Mode := True;
            else
               pragma Assert (False);
            end if;

         when 't' =>
            if Full_Switch = "t" then
               Options.Delete_Trees := False;
            else
               pragma Assert (False);
            end if;

         when '-' =>
            if Full_Switch = "-RTS" then
               Store_Option ("--RTS=" & Parameter);
            elsif Full_Switch = "-help" then
               Gnat2xml_Usage;
               GNAT.OS_Lib.OS_Exit (Status => 0);
            elsif Full_Switch = "-version" then
               Print_Version_Info (2012);
               GNAT.OS_Lib.OS_Exit (Status => 0);
            else
               pragma Assert (False);
            end if;

         when 'v' =>
            Print_Version_Info (2012);
            if Full_Switch = "v" then
               Options.Verbose_Mode := True;
               Verbose_Mode := True; -- ???
               Print_Command_Line;
            else
               pragma Assert (False);
            end if;

         when 'x' =>
            if Full_Switch = "x" then
               null; -- ignore
            else
               pragma Assert (False);
            end if;

         when others =>
            pragma Assert (False);
      end case;
   end loop;

   Process_cargs_Section (No_Preprocessing => True);

   Set_Gnat2xml_Options (Options);

exception
   when GNAT.Command_Line.Invalid_Switch =>
      Error ("invalid switch : " & Full_Switch);
      Gnat2xml_Usage;
      raise Parameter_Error;

   when GNAT.Command_Line.Invalid_Parameter =>
      Error ("parameter missed for : -" & Full_Switch);
      Gnat2xml_Usage;
      raise Parameter_Error;

end Scan_Parameters;
