------------------------------------------------------------------------------
--                                                                          --
--                            GNATPP COMPONENTS                             --
--                                                                          --
-- A S I S _ U L . E N V I R O N M E N T . C H E C K  _ P A R A M E T E R S --
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
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNATPP is maintained by AdaCore (http://www.adacore.com)                 --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ASIS_UL.Compiler_Options;
with ASIS_UL.Options;          use ASIS_UL.Options;
with ASIS_UL.Source_Table;

with Gnat2xml.Command_Line; use Gnat2xml.Command_Line;

separate (ASIS_UL.Environment)
procedure Check_Parameters is
begin
   --  First, read all the argument files using all available path information
   if ASIS_UL.Options.No_Argument_File_Specified then
      Error ("No input source file set");
      Gnat2xml_Usage;
      raise Parameter_Error;
   end if;

   Read_Args_From_Temp_Storage (Duplication_Report => True);

   Nothing_To_Do := Last_Source < First_SF_Id;

   if Nothing_To_Do then
      Error ("No existing file to process");
      --  All the rest does not make any sense
      return;
   end if;

   Total_Sources      := Natural (Last_Source);
   Sources_Left       := Total_Sources;

   if Last_Source = First_SF_Id then
      Multiple_File_Mode := False;

      --  If we have only one source to reformat, we have to check
      --  the settings of the output file, if it is set

      Progress_Indicator_Mode := False;
      --  We do not need this in case of one file, and we may be in the
      --  mode of outputting the reformatted source into Stdout
   end if;

   if not Verbose_Mode then
      Progress_Indicator_Mode := False;
   end if;

   --  Create output directory if necessary. An empty string means we're piping
   --  to standard output. Otherwise create the directory if it doesn't exist.

   declare
      Output_Dir : constant String := To_String (Gnat2xml_Options.Output_Dir);
   begin
      if Output_Dir /= "" and then not Exists (Output_Dir) then
         Create_Path (Output_Dir);
         if Gnat2xml_Options.Verbose_Mode then
            Put_Line ("Created directory " & Output_Dir);
         end if;
      end if;
   end;

end Check_Parameters;
