------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                    G N A T 2 X M L . X M L 2 G N A T                     --
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

--  This program translates xml files generated by gnat2xml back into Ada for
--  testing purposes. The command line contains a list of the same Ada files
--  passed to gnat2xml. The xml files are assumed to be in an 'xml'
--  subdirectory of the directory in which the Ada source files are. So for
--  example, if the Ada source file is some/dir/mumble.adb, then the xml file
--  is found in some/dir/xml/mumble.adb.xml.

--  Output goes into subdirectories "generated_ada" and "self_rep" of the
--  output directory, which is the current directory by default, but can be
--  overridden with --output-dir=dir on the command line.

with ASIS_UL.Options;

with Ada.Wide_Text_IO; use Ada;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Namet; use Namet;

with ASIS_UL.Formatted_Output;            use ASIS_UL.Formatted_Output;
with Ada_Trees;          use Ada_Trees;
with Gnat2xml.Xml2tree;           use Gnat2xml.Xml2tree;
with Ada_Trees.Self_Rep; use Ada_Trees.Self_Rep;

procedure Gnat2xml.Xml2gnat is

   Verbose_Mode : Boolean renames ASIS_UL.Options.Verbose_Mode;

   Output_Dir : Name_Id := Name_Find (Current_Directory);
   --  Default is current directory; can be overridden by --output-dir=dir
   --  switch. Output goes in subdirectories of Output_Dir.

   Self_Rep_Subdir      : constant String := "self_rep";
   Generated_Ada_Subdir : constant String := "generated_ada";

   procedure Do_Nothing
     (Original_Source_Name,
      Xml_File_Name,
      Self_Rep_Name,
      Regen_Ada_Name,
      Generated_Ada_Name : String) is null;

   procedure Generate_Ada
     (Original_Source_Name,
      Xml_File_Name,
      Self_Rep_Name,
      Regen_Ada_Name,
      Generated_Ada_Name : String);

   procedure Iter_File_Names
     (Action : not null access procedure
        (Original_Source_Name,
         Xml_File_Name,
         Self_Rep_Name,
         Regen_Ada_Name,
         Generated_Ada_Name : String));

   procedure Generate_Ada
     (Original_Source_Name,
      Xml_File_Name,
      Self_Rep_Name,
      Regen_Ada_Name,
      Generated_Ada_Name : String)
   is
      Tree : constant Ada_Tree := Read_Xml (Xml_File_Name);

      Self_Rep_File, Regen_Ada_File, Generated_Ada_File :
        Wide_Text_IO.File_Type;

   begin
      if not Debug_Mode then
         if Verbose_Mode then
            Put ("Creating \1\n", Self_Rep_Name);
         end if;
         Create_Path (Containing_Directory (Self_Rep_Name));
         Wide_Text_IO.Create (Self_Rep_File, Name => Self_Rep_Name);
         Wide_Text_IO.Set_Output (Self_Rep_File);
      end if;

      Put_Self_Rep (Tree);

      if not Debug_Mode then
         Wide_Text_IO.Set_Output (Wide_Text_IO.Standard_Output);
         Wide_Text_IO.Close (Self_Rep_File);
      end if;

      if not Debug_Mode then
         if Verbose_Mode then
            Put ("Creating \1\n", Regen_Ada_Name);
         end if;
         Create_Path (Containing_Directory (Regen_Ada_Name));
         Wide_Text_IO.Create (Regen_Ada_File, Name => Regen_Ada_Name);
         Wide_Text_IO.Set_Output (Regen_Ada_File);
      end if;

      Put ("--  Created by xml2gnat\n\n");
      Put_Regen_Ada (Tree);

      if not Debug_Mode then
         Wide_Text_IO.Set_Output (Wide_Text_IO.Standard_Output);
         Wide_Text_IO.Close (Regen_Ada_File);
      end if;

      if not Debug_Mode then
         if Verbose_Mode then
            Put ("Creating \1\n", Generated_Ada_Name);
         end if;
         Create_Path (Containing_Directory (Generated_Ada_Name));
         Wide_Text_IO.Create (Generated_Ada_File, Name => Generated_Ada_Name);
         Wide_Text_IO.Set_Output (Generated_Ada_File);
      end if;

      if False then -- ???Messes up the diff's.
         Put ("--  Created by xml2gnat\n\n");
      end if;

      declare
         Source_File_Contents : String_Access :=
           Read_File (Original_Source_Name);

      begin
         Tree_To_Ada (Tree, Source_File_Contents.all);
         Free (Source_File_Contents);
      end;

      if not Debug_Mode then
         Wide_Text_IO.Set_Output (Wide_Text_IO.Standard_Output);
         Wide_Text_IO.Close (Generated_Ada_File);
      end if;

      Format_Debug_Output ("Generate_Ada");

      Free_Tree (Tree);
   end Generate_Ada;

   procedure Iter_File_Names
     (Action : not null access procedure
        (Original_Source_Name,
         Xml_File_Name,
         Self_Rep_Name,
         Regen_Ada_Name,
         Generated_Ada_Name : String))
   is
   begin
      for X in 1 .. Argument_Count loop
         if Has_Prefix (Argument (X), Prefix => "-") then
            if Argument (X) = "--debug" then
               Debug_Mode := True;

            elsif Argument (X) = "-v" then
               Verbose_Mode := True;

            elsif Has_Prefix (Argument (X), Prefix => "--output-dir=") then
               Output_Dir :=
                 Name_Find (Full_Name
                   (Strip_Prefix (Argument (X), Prefix => "--output-dir=")));

            else
               raise Program_Error with "unknown switch";
            end if;

         else
            declare
               Original_Source_Name : constant String := Argument (X);
               --  for example, "some/directory/some-package.adb"

               Original_Source_Dir : constant String :=
                 Containing_Directory (Original_Source_Name);
               --  for example, "some/directory"

               Xml_Dir : constant String :=
                 Compose
                   (Containing_Directory => Original_Source_Dir,
                    Name                 => "xml");
               --  for example, "some/directory/xml"

               Xml_File_Name : constant String :=
                 Compose
                   (Containing_Directory => Xml_Dir,
                    Name                 => Simple_Name (Original_Source_Name),
                    Extension            => "xml");
               --  for example, "some/directory/xml/some-package.adb.xml"

               Self_Rep_Name : constant String :=
                 Compose
                   (Containing_Directory =>
                      Compose (Get_Name_String (Output_Dir), Self_Rep_Subdir),
                    Name      => Simple_Name (Original_Source_Name),
                    Extension => "self_rep.ada");
               --  for example, "./self_rep/some-package.adb.self_rep.ada"

               Regen_Ada_Name : constant String :=
                 Compose
                   (Containing_Directory =>
                      Compose (Get_Name_String (Output_Dir), Self_Rep_Subdir),
                    Name      => Simple_Name (Original_Source_Name),
                    Extension => "regen_ada.ada");
               --  for example, "./self_rep/some-package.adb.regen_ada.ada"

               Generated_Ada_Name : constant String :=
                 Compose
                   (Containing_Directory =>
                      Compose
                        (Get_Name_String (Output_Dir),
                         Generated_Ada_Subdir),
                    Name => Simple_Name (Original_Source_Name));
            --  for example, "./generated_ada/some-package.adb"

            begin
               --  We don't want to overwrite the original source code, and
               --  Generated_Ada_Name is the same name. Note that in --debug
               --  mode, we don't write any files.

               if
                 not Debug_Mode
                 and then Generated_Ada_Name = Original_Source_Name
               then
                  raise Program_Error
                    with "output file same as input: " & Generated_Ada_Name;
               end if;

               Action
                 (Original_Source_Name,
                  Xml_File_Name,
                  Self_Rep_Name,
                  Regen_Ada_Name,
                  Generated_Ada_Name);
            end;
         end if;
      end loop;
   end Iter_File_Names;

--  Start of processing for Xml2gnat

begin
   if Argument_Count = 0 then
      raise Program_Error with "missing arguments";
   end if;

   --  Go through all the files doing nothing, first, to check that we're not
   --  overwriting any sources. Also to pick up a "--debug" switch.

   Iter_File_Names (Do_Nothing'Access);

   if Debug_Mode then
      Dbg_Out.Output_Enabled := True;
      Put_Ada_Templates;
   end if;

   --  Now go through again, this time doing the real work

   Iter_File_Names (Generate_Ada'Access);

   Main_Done := True;
end Gnat2xml.Xml2gnat;
