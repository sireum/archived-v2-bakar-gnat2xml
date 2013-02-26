------------------------------------------------------------------------------
--                                                                          --
--                          GNAT2XML COMPONENTS                             --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                    AVATOX (Ada, Via Asis, To Xml)                        --
--                                                                          --
--                                                                          --
--                Copyright (C) 2007, McKae Technologies.                   --
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

with Ada.Calendar.Formatting;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with A4G.Contt;

with Asis;
with Asis.Elements;
with Asis.Exceptions;
with Asis.Compilation_Units;
with Asis.Compilation_Units.Relations;
with Asis.Ada_Environments;
with Asis.Implementation;
with Asis.Set_Get;

--???Can't find "asis_ul.ads": with ASIS_UL.Output;

with Gnat.Regexp;

with Mckae.Environment.Command_Line_Processor;

with Strings_Edit.Utf8.Handling;

--  functionality packages
with Gnat2xml.Command_Line; use Gnat2xml.Command_Line;
with Gnat2xml_Versioning;
with Gnat2xml.Environment;
with Gnat2xml.Xml;
with Gnat2xml.Xsl_Transformation;

procedure Gnat2xml.Gnat2xml is

   package Root renames Standard.Gnat2xml;
   --  Name of root package. Needed because this package hides its parent.

   use Ada;
   Temp_Filename_Prefix : constant String := ".avx";

   function "<" (L, R : Asis.Compilation_Unit) return Boolean is
   begin
      return Asis.Compilation_Units.Text_Name (L)
        < Asis.Compilation_Units.Text_Name (R);
   end "<";

   function "=" (L, R : Asis.Compilation_Unit) return Boolean is
   begin
      return Asis.Compilation_Units.Text_Name (L)
        = Asis.Compilation_Units.Text_Name (R);
   end "=";

   package Comp_Unit_Sets is new Ada.Containers.Ordered_Sets
     (Asis.Compilation_Unit);

   -----------------------------------------------------------------------------

   function Is_Ads (File : String) return Boolean is
   begin
      return File (File'Last - 3 .. File'Last) = ".ads" or else
             File (File'Last - 3 .. File'Last) = ".ADS";
   end Is_Ads;

   -----------------------------------------------------------------------------

   function Is_Adb (File : String) return Boolean is
   begin
      return File (File'Last - 3 .. File'Last) = ".adb" or else
             File (File'Last - 3 .. File'Last) = ".ADB";
   end Is_Adb;

   -----------------------------------------------------------------------------

   function Main_Name (File : String) return Wide_String is  --  ???
   begin
      return Ada.Characters.Conversions.To_Wide_String
              (File (File'First .. File'Last - 4));
   end Main_Name;

   -----------------------------------------------------------------------------

   function Get_Compilation_Unit (Filename       : Unbounded_String;
                                  Avatox_Context : Asis.Context)
                                  return Asis.Compilation_Unit is
      Unite : String := To_String (Filename);
      The_Unit : Asis.Compilation_Unit := Asis.Nil_Compilation_Unit;

   begin
      --  Converting file name in Ada Unit Name
      --  first let's change the '-' in '.' in the filename
      for I in Unite'Range
      loop
         if Unite (I) = '-' then
            Unite (I) := '.';
         end if;
     end loop;

      if Is_Ads (Unite) then
         The_Unit :=  Asis.Compilation_Units.Library_Unit_Declaration
           (Main_Name (Unite), Avatox_Context);
      elsif Is_Adb (Unite) then
         The_Unit :=  Asis.Compilation_Units.Compilation_Unit_Body
           (Main_Name (Unite), Avatox_Context);
      end if;
      return The_Unit;
   end Get_Compilation_Unit;

   -----------------------------------------------------------------------------

   function Create_Unit_Filename (S             : String;
                                  Axf_Directory : String;
                                  Extension     : String;
                                  Is_Filename   : Boolean := True)
                                  return String is
      Unit_Name : String := S;
      Dir_Name  : String := Axf_Directory & ' ';
      Dir_Name_Length : Natural := Dir_Name'Length - 1;
   begin
      if Axf_Directory = "" then
         Dir_Name := ".";
         Dir_Name_Length := Dir_Name'Length;
      end if;
      if not Is_Filename then
         for I in Unit_Name'Range loop
            if Unit_Name (I) = '.' then
               Unit_Name (I) := '-';
            end if;
         end loop;
      end if;
      return Directories.Compose
        (Dir_Name (1 .. Dir_Name_Length), Unit_Name, Extension);
   end Create_Unit_Filename;

   -----------------------------------------------------------------------------

   function Filename_For (Unit          : Asis.Compilation_Unit;
                          Axf_Directory : String;
                          Extension     : String) return String is

      Unit_Text_Name : constant String
        := To_String (Asis.Compilation_Units.Text_Name (Unit));
      Unit_Comp_Name : constant String
        := To_String (Asis.Compilation_Units.Unit_Full_Name (Unit));
   begin
      if Unit_Text_Name = "" then
         pragma Assert (Unit_Comp_Name /= "");
         return Create_Unit_Filename (Unit_Comp_Name,
                                      Axf_Directory,
                                      Extension,
                                      Is_Filename => False);
      else
         return Create_Unit_Filename (Directories.Simple_Name (Unit_Text_Name),
                                      Axf_Directory,
                                      Extension);
      end if;
   end Filename_For;

   -----------------------------------------------------------------------------

   function Check_And_Validate_Axf_Directory (Axf_Directory : String;
                                              Verbose       : Boolean)
                                              return Boolean is
      F : File_Type;
   begin
      -- Does the target AXF directory exist?
      if not Directories.Exists (Axf_Directory) then
         -- Attempt to create it
         Directories.Create_Directory (Axf_Directory);
         if Verbose then
            Put_Line ("Created directory " & Axf_Directory);
         end if;
      end if;

      -- Is the directory writable?
      Create (F, Out_File, Directories.Compose (Axf_Directory, ".#avtxck#"));
      Delete (F);
      return True;
   exception
      when others =>
         Put_Line ("Cannot create/write to AXF directory " & Axf_Directory);
         return False;
   end Check_And_Validate_Axf_Directory;

   -----------------------------------------------------------------------------

   function Supporting_Application_Units
     (Context      : Asis.Context;
      Comp_Unit    : Asis.Compilation_Unit;
      Unit_Breadth : Root.Environment.Unit_Breadths;
      Filters      : Root.Environment.Filtering_Entries)
      return Comp_Unit_Sets.Set is

      use type Asis.Compilation_Unit;

      Comp_Unit_Set : Comp_Unit_Sets.Set := Comp_Unit_Sets.Empty_Set;
      Related_Units : Asis.Relation_Kinds;

      use Root;
      use type Root.Environment.Unit_Breadths;

   begin
      if not Asis.Compilation_Units.Is_Nil (Comp_Unit) then
         if Unit_Breadth = Environment.Single_Unit then
            Comp_Unit_Set := Comp_Unit_Sets.To_Set (Comp_Unit);
         else
            if Unit_Breadth = Environment.Full_Closure then
               Related_Units := Asis.Needed_Units;
            else
               Related_Units := Asis.Supporters;
            end if;

            declare
               use type Asis.Compilation_Unit_List;

               Closure : constant Asis.Compilation_Units.Relations.Relationship :=
                 Asis.Compilation_Units.Relations.Semantic_Dependence_Order
                   ((1 => Comp_Unit), Asis.Nil_Compilation_Unit_List,
                    Context, Related_Units);
               Units : constant Asis.Compilation_Unit_List := Comp_Unit
                 & Closure.Consistent;
               App_Units : Asis.Compilation_Unit_List (Units'Range);
               App_Unit_Count : Asis.Asis_Natural := 0;

               use Asis;
            begin
               for I in Units'Range loop
                  declare
                     Unit_Name : constant String
                       := Ada.Characters.Handling.To_Lower
                         (To_String (Asis.Compilation_Units.Unit_Full_Name (Units (I))));
                  begin
                     -- Take packages standard and system out of the list, since they're
                     -- usually "virtual"
                     if not (Asis.Set_Get.Is_Standard (Units (I))
                             or (Unit_Name = "system"))
                       and Environment.Passes_Filter (Unit_Name, Filters) then
                        App_Unit_Count := App_Unit_Count + 1;
                        App_Units (App_Unit_Count) := Units (I);
                     end if;
                  end;
               end loop;

               for I in 1 .. App_Unit_Count loop
                  Comp_Unit_Set.Include (App_Units (I));
               end loop;
            end;
         end if;
      end if;
      return Comp_Unit_Set;

   exception
      when others =>
         return Comp_Unit_Sets.Empty_Set;
   end Supporting_Application_Units;

   -----------------------------------------------------------------------------

   function Identify_Units
     (Primary_Files : Root.Environment.File_Name_Entries;
      Context       : Asis.Context;
      Unit_Breadth  : Root.Environment.Unit_Breadths;
      Filters       : Root.Environment.Filtering_Entries)
      return Comp_Unit_Sets.Set is

      Comp_Unit_File  : Unbounded_String;
      Comp_Unit       : Asis.Compilation_Unit;
      Remaining_Files : Root.Environment.File_Name_Entries;

      use Root.Environment.File_Name_Handling;
      use Comp_Unit_Sets;

      Result : Comp_Unit_Sets.Set;

   begin
      if Primary_Files = Root.Environment.File_Name_Handling.Empty_Set then
         return Comp_Unit_Sets.Empty_Set;
      else
         Comp_Unit_File := Primary_Files.First_Element;
         Comp_Unit := Get_Compilation_Unit (Comp_Unit_File, Context);

         -- Return the union of this set of supporting compilation units and
         -- that of the rest of the primary unit files
         Result := Supporting_Application_Units
           (Context, Comp_Unit, Unit_Breadth, Filters)
           or Identify_Units (Primary_Files - To_Set (Comp_Unit_File),
                            Context, Unit_Breadth, Filters);
      end if;

      declare
         use Asis.Compilation_Units, Comp_Unit_Sets;

         Corresponding_Specs : Comp_Unit_Sets.Set;
         Comp_Unit_Cursor : Comp_Unit_Sets.Cursor := Result.First;
      begin
         while Comp_Unit_Cursor /= Comp_Unit_Sets.No_Element loop
            if Unit_Class (Element (Comp_Unit_Cursor)) in Asis.A_Public_Body | Asis.A_Private_Body then
               Include
                 (Corresponding_Specs,
                  Corresponding_Declaration (Element (Comp_Unit_Cursor)));
            end if;

            Next(Comp_Unit_Cursor);
         end loop;

         return Result or Corresponding_Specs;
      end;
   end Identify_Units;

   -----------------------------------------------------------------------------

   procedure Process_Xsl_Transformation
     (XML_In_Filename  : String;
      Output_Filename  : String;
      Info_Node        : Xml.Info_Node) is

      use Root.Xsl_Transformation;

      Result         : Transformation_Results;
      Std_Out_Input  : Boolean := Index (Xml_In_Filename, Temp_Filename_Prefix) /= 0;
      Std_Out_Output : Boolean := Index (Output_Filename, Temp_Filename_Prefix) /= 0;

   begin
      Apply_Stylesheet
        (Xml_In_Filename,
         Output_Filename,
         Info_Node.Xsl_Info,
         Result);
      if Info_Node.Verbose then
         Put ("XSLT: ");
         if Std_Out_Input then
            Put ("Avatox stdout");
         else
            Put (Xml_In_Filename);
         end if;
         Put (" -> ");
         if Std_Out_Output then
            Put ("stdout");
         else
            Put (Output_Filename);
         end if;
         Put_Line (" ... " & XSL_Result_String (Result));
      elsif Result /= Success then
         Put_Line ("Failure transforming "
                   & Xml_In_Filename & " -> "
                   & Output_Filename);
      end if;
   end Process_Xsl_Transformation;

   -----------------------------------------------------------------------------

   function Set_To_Comp_Unit_List (Comp_Unit_Set : Comp_Unit_Sets.Set)
                                   return Asis.Compilation_Unit_List is
      Comp_Units       : Asis.Compilation_Unit_List
        (1 .. Natural (Comp_Unit_Set.Length));
      Comp_Unit_Cursor : Comp_Unit_Sets.Cursor := Comp_Unit_Set.First;
      use type Comp_Unit_Sets.Cursor;
   begin
      for I in Comp_Units'Range loop
         Comp_Units (I) := Comp_Unit_Sets.Element (Comp_Unit_Cursor);
         Comp_Unit_Sets.Next(Comp_Unit_Cursor);
      end loop;
      pragma Assert (Comp_Unit_Cursor = Comp_Unit_Sets.No_Element);
      return Comp_Units;
   end Set_To_Comp_Unit_List;

   -----------------------------------------------------------------------------

   procedure Process_Unit
     (Unit_Closure         : in     Root.Environment.Units_Needed;
      Unit_Breadth         : in     Root.Environment.Unit_Breadths;
      Comp_Units           : in     Asis.Compilation_Unit_List;
      Filters              : in     Root.Environment.Filtering_Entries;
      Axf_Directory        : in     String;
      Xml_File             : in out File_Access;
      The_Node_Information : in out Xml.Info_Node;
      Avatox_Context       : in out Asis.Context) is

      use type Asis.Compilation_Unit_List;

      Next_Unit            : Asis.Compilation_Unit;
      Multiple_Files       : constant Boolean := Xml_File = null;
      Xml_Start_Rep        : Boolean := True;
      Xml_End_Rep          : constant Boolean := Multiple_Files;
      XML_Out_File         : aliased File_Type;
      Filename             : Unbounded_String;

      use type Asis.Unit_Kinds;
      use type Asis.Unit_Origins;
      use type Root.Environment.Units_Needed;
      use Root;

   begin
      for I in Comp_Units'Range loop
         Next_Unit := Comp_Units (I);

         if Asis.Compilation_Units.Unit_Kind (Next_Unit)
           /= Asis.A_Configuration_Compilation then
            if (Unit_Closure = Environment.Predefined) or
              (Unit_Closure = Environment.Unit_Only) or
              ((Unit_Closure = Environment.App_Only) and
                 Asis.Compilation_Units.Unit_Origin (Next_Unit)
               = Asis.An_Application_Unit) then
               if Xml_Start_Rep then
                  if Multiple_Files then
                     Filename := To_Unbounded_String
                       (Filename_For (Next_Unit, Axf_Directory, "xml"));
                     Create (Xml_Out_File, Out_File, To_String (Filename));
                     if The_Node_Information.Verbose then
                        Put_Line ("Creating " & To_String (Filename));
                     end if;
                     Xml_File := Xml_Out_File'Unchecked_Access;
                  end if;

                  The_Node_Information.Xml_File := Xml_File;
                  Xml.Start_Representation (The_Node_Information);
                  Xml_Start_Rep := Multiple_Files;
               end if;

               -- Process
               Xml.Process_Unit (Next_Unit, The_Node_Information);

               if Xml_End_Rep then
                  Close (Xml_Out_File);
                  Xml_File := null;
                  if Root.Xsl_Transformation.XSL_Transformation_To_Be_Done
                    (The_Node_Information.Xsl_Info) then
                     Process_Xsl_Transformation
                       (To_String (Filename),
                        Filename_For (Next_Unit, Axf_Directory,
                          Root.Xsl_Transformation.
                            Get_Xsl_Extension (The_Node_Information.Xsl_Info)),
                        The_Node_Information);
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Process_Unit;

   ----------------------------------------------------------------------------

   procedure Dump_Pseudo_Stdout
     (Filename    : in String;
      Delete : in Boolean := False) is

      Stdout_File   : File_Type;
      Final_Newline : Boolean := False;
      The_Line      : String (1 .. 50_000);
      The_Length    : Natural := 0;
    begin
      Open (Stdout_File, In_File, Filename); -- ???input from out file?
      while not End_Of_File (Stdout_File) loop
         Final_Newline := False;
         Get_Line (Stdout_File, The_Line, The_Length);
         Put (The_Line (1 .. The_Length));
         if The_Length < The_Line'Length then
            New_Line;
            Final_Newline := True;
         end if;
      end loop;
      if Final_Newline then
         New_Line;
      end if;
      Close (Stdout_File);

      if Delete then
         Directories.Delete_File (Filename);
      end if;
   exception
      when others =>
         -- Ignore any errors involved with missing files
         null;
   end Dump_Pseudo_Stdout;

   ----------------------------------------------------------------------------

   The_Avatox_Context   : Asis.Context;
   The_Node_Information : Xml.Info_Node;
   --  ???Should recreate The_Node_Information for each file.
   --  It has default values!
   Filename_Specs       : Root.Environment.File_Name_Entries;
   Primary_Files        : Root.Environment.File_Name_Entries;
   Unit_Cursor          : Root.Environment.File_Name_Handling.Cursor;
   Output_Filename      : Unbounded_String;
   Comp_Unit            : Asis.Compilation_Unit;
   Continue             : Boolean := True;
   XML_Out_File         : aliased File_Type;
   XML_File             : File_Access := Standard_Output;
   --  ???Asis_Context_Params  : constant String := "-CA -FM -I. ";
   --  -CA = all tree files
   --  -FS = call gcc unconditionally
   --  -FM = call gcc if tree file doesn't exist
   --  -Tdir = dir to search for trees
   Asis_Context_Params  : constant String := "-CA -FS -I.";

   Asis_Params          : Unbounded_String := To_Unbounded_String (Asis_Context_Params);
   Refed_Units          : Root.Environment.Units_Needed
     := Root.Environment.Unit_Only;
   Unit_Breadth         : Root.Environment.Unit_Breadths
     := Root.Environment.Single_Unit;
   Multiple_Files       : Boolean := False;
   Axf_Directory        : Unbounded_String;
   Filter_Entries       : Root.Environment.Filtering_Entries;
   Temp_Filename        : String := Temp_Filename_Prefix
     & Calendar.Formatting.Image (Calendar.Clock);
   Std_Out              : Boolean := False;

   use Root.Environment.File_Name_Handling;
   use type Ada.Containers.Count_Type;
   use type Root.Environment.Unit_Breadths;
   use type Root.Environment.Units_Needed;

   Dummy_Delete_Trees : Boolean := True;

begin -- Gnat2xml.Gnat2xml
   Process_Gnat2xml_Command_Line;

   if Gnat2xml_Options.Help then
      Gnat2xml_Usage;
      return;
   end if;

   --  ???ASIS_UL.Output.Print_Version_Info (Released_At => 2012);
   Gnat2xml_Versioning.Print_Version_Info
     (Tool_Name => "gnat2xml", First_Release_Year => "2012");

   if Gnat2xml_Options.Version then
      return;
   end if;

   if Gnat2xml_Options.Verbose_Mode then
      Print_Command_Line;
   end if;

   --  ???Mimic the call below to Process_Args_And_Options:

   The_Node_Information :=
     (XML_File => null,
      Krunch => False,
      Xml_Style => False,
      Xsl_Info => <>,
      Last_Element => Asis.Nil_Element,
      Verbose => Gnat2xml_Options.Verbose_Mode);

   Debug_Mode := Gnat2xml_Options.Debug_Mode;
   Filename_Specs := Gnat2xml_Options.Input_Files;

   for Include of Gnat2xml_Options.Include_Directories loop
      Append
        (Source => Asis_Params,
         New_Item => " -I" & To_String (Include));
   end loop;

   Refed_Units := Environment.Unit_Only;
   Unit_Breadth := Environment.Single_Unit;
   Multiple_Files := Length (Gnat2xml_Options.Output_Dir) /= 0;
   Axf_Directory := Gnat2xml_Options.Output_Dir;

   Continue := True;

if False then
   Root.Environment.Process_Args_And_Options (The_Node_Information,
                                               Filename_Specs,
                                               Output_Filename,
Dummy_Delete_Trees,
                                               Asis_Params,
                                               Refed_Units,
                                               Unit_Breadth,
                                               Axf_Directory,
                                               Multiple_Files,
                                               Filter_Entries,
                                               Continue);
end if;

   if not Continue then
      return;
   end if;

   -- Adjust the temporary filename, which is based on the current time
   Overwrite(Temp_Filename, Index(Temp_Filename, " "), "-");

   -- Use a temporary filename when outputting to stdout
   Std_Out := (Output_Filename = Null_Unbounded_String) and not Multiple_Files;

   -- Given the filename specification(s), get the list of primary file names
   -- to process
   Root.Environment.Collect_Primary_Files (Filename_Specs, Primary_Files);

   -- Make sure there's some files to process
   if Primary_Files.Length = 0 then
      Put_Line ("No unit file(s) located");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   --  Initialization of Asis environment.
   Asis.Implementation.Initialize("-asis05");
   Asis.Ada_Environments.Associate
     (The_Context => The_Avatox_Context,
      Name        => "The_Avatox_Context",
      Parameters  => To_Wide_String (To_String (Asis_Params)));

   -- Open the ASIS context and start processing files
   Asis.Ada_Environments.Open (The_Avatox_Context);

   -- belt: added gnatd.V option to allow processing of Loop_Invariants
   A4G.Contt.Set_Extra_Options
     (Asis.Set_Get.Get_Cont_Id (The_Avatox_Context),
      (1 => new String'("-gnat2012"),
       2 => new String'("-gnatd.V")));

   Continue := True;
   Unit_Cursor := Primary_Files.First;
   while Unit_Cursor /= No_Element loop
      Comp_Unit := Get_Compilation_Unit
        (Element (Unit_Cursor), The_Avatox_Context);

      if Asis.Compilation_Units.Is_Nil (Comp_Unit) then
         --  ???Put all these errors last:
         Put_Line (To_String (Element (Unit_Cursor))
                   & ": compilation error");
         Ada.Command_Line.Set_Exit_Status (1);
         Continue := False;
      elsif (Unit_Breadth = Root.Environment.Full_Closure)
        and (not Asis.Compilation_Units.Can_Be_Main_Program (Comp_Unit)) then
         Put_Line ("Closure processing (-c) is only valid for subprograms");
         Put_Line ("that can serve as main program units.");
      elsif False then -- ???Stop if any file gets compilation errors
         -- Indicate that there's at least one file that can be processed
         Continue := True;
      end if;
      Root.Environment.File_Name_Handling.Next (Unit_Cursor);
   end loop;

   if Continue then
      declare
         CUs_To_Process : constant Asis.Compilation_Unit_List
           := Set_To_Comp_Unit_List
             (Identify_Units
                  (Primary_Files, The_Avatox_Context,
                   Unit_Breadth, Filter_Entries));
      begin
         -- Generate the AXF document(s).

         -- ???Disable use of a temp file for Std_Out, to ease debugging.
         -- This breaks the xsl transformation stuff.
         if Std_Out then
            Xml_File := Standard_Output;
            if False then -- ???
               Create (Xml_Out_File, Out_File, Temp_Filename);
            end if;
         elsif not Multiple_Files then
            Create (Xml_Out_File, Out_File, To_String (Output_Filename));
            Xml_File := Xml_Out_File'Unchecked_Access;
         end if;

         if Multiple_Files then
            Xml_File := null;

            if (Axf_Directory /= Null_Unbounded_String) then
               Continue := Check_And_Validate_Axf_Directory
                 (To_String (Axf_Directory),
                 Verbose => The_Node_Information.Verbose);
            end if;
         end if;

         if Continue then
            Process_Unit (Refed_Units, Unit_Breadth, CUs_To_Process,
                          Filter_Entries, To_String (Axf_Directory),
                          Xml_File, The_Node_Information, The_Avatox_Context);
         end if;

         if not Multiple_Files and not Std_Out then
            Close (Xml_Out_File);
         end if;

         if Std_Out then -- ???
            -- If writing was to stdout, dump the contents of the temp file that
            -- was used in its stead.
            declare
               Xsl_Filename : Unbounded_String := To_Unbounded_String
                 (Root.Xsl_Transformation.Get_Output_Filename
                    (The_Node_Information.Xsl_Info));
               Xsl_Stdout   : Boolean := False;
            begin
               if False then -- ???
                  Dump_Pseudo_Stdout(Temp_Filename, Delete => False);
               end if;
               if Root.Xsl_Transformation.XSL_Transformation_To_Be_Done
                 (The_Node_Information.Xsl_Info) then
                  if True then
                     raise Program_Error; -- ???
                  end if;

                  -- If the XSL output is going to stdout, temporarily write it
                  -- to a file, dump that
                  if Length (Xsl_Filename) = 0 then
                     Xsl_Filename := To_Unbounded_String(Temp_Filename & ".axt");
                     Xsl_Stdout   := True;
                  end if;

                  Process_Xsl_Transformation
                    (Temp_Filename,
                     To_String(Xsl_Filename),
                     The_Node_Information);

                  if Xsl_Stdout then
                     Dump_Pseudo_Stdout (To_String (Xsl_Filename),
                                         Delete => True);
                  end if;
               end if;
               if False then -- ???
                  Open (Xml_Out_File, In_File, Temp_Filename);
                  Delete (Xml_Out_File);
               end if;
           end;
         elsif not Multiple_Files then
            if Root.Xsl_Transformation.XSL_Transformation_To_Be_Done
              (The_Node_Information.Xsl_Info) then
               if True then
                  raise Program_Error; -- ???
               end if;

               Process_Xsl_Transformation
                 (To_String (Output_Filename),
                  Root.Xsl_Transformation.Get_Output_Filename
                    (The_Node_Information.Xsl_Info),
                  The_Node_Information);
             end if;
         end if;
      end;
   end if;

   ------------------------------
   --  Closing Asis ....
   Asis.Ada_Environments.Close (The_Avatox_Context);
   Asis.Ada_Environments.Dissociate (The_Avatox_Context);
   Asis.Implementation.Finalize ("");

   if Gnat2xml_Options.Delete_Trees then
      Unit_Cursor := Primary_Files.First;
      while Unit_Cursor /= No_Element loop
         --  let's delete the *.at? and *.ali files
         declare
            To_Erase : String
              := Directories.Simple_Name (To_String (Element (Unit_Cursor)));
            File     : File_Type;
         begin
            if To_Erase (To_Erase'Last - 3 .. To_Erase'Last - 1) = ".ad" or else
              To_Erase (To_Erase'Last - 3 .. To_Erase'Last - 1) = ".AD"
            then
               --  tree file
               To_Erase (To_Erase'Last) := 't';
               Open (File, Out_File, To_Erase);
               Delete (File);

               --  ali file
               To_Erase (To_Erase'Last - 2 .. To_Erase'Last) := "ali";
               Open (File, Out_File, To_Erase);
               Delete (File);

            end if;
         exception
            when Name_Error =>
               -- An .adt file isn't necessarily generated for every compilation
               -- unit, such as when a child package and its parent packages are
               -- being processed.
               null;
         end;
         Root.Environment.File_Name_Handling.Next (Unit_Cursor);
      end loop;
   end if;

   Main_Done := True;

exception

   when Command_Line_Error =>
      Ada.Command_Line.Set_Exit_Status (1);

   when Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit =>
      Put_Line ("The file " & To_String (Element(Unit_Cursor)) &
                 " does not contain any Ada Unit.");
      Root.Environment.Show_Usage;
      Ada.Command_Line.Set_Exit_Status (1);

   when Asis.Exceptions.ASIS_Failed |
        Asis.Exceptions.ASIS_Inappropriate_Element |
        Asis.Exceptions.ASIS_Inappropriate_Context =>
         Put_Line (Ada.Characters.Conversions.To_String
           (Asis.Implementation.Diagnosis));   --  ???
      Ada.Command_Line.Set_Exit_Status (1);

   when Status_Error | Mode_Error | Name_Error | Use_Error =>
      Put_Line ("Error opening/writing to : " &
                To_String (Output_Filename));
      -- ???This is wrong -- we can get here for errors opening the temp file.
      Ada.Command_Line.Set_Exit_Status (1);

   when The_Error : others =>
      Put_Line (Ada.Exceptions.Exception_Information(The_Error));
      Put_Line (Ada.Characters.Conversions.To_String
         (Asis.Implementation.Diagnosis));
      Ada.Command_Line.Set_Exit_Status (1);

end Gnat2xml.Gnat2xml;
