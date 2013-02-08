------------------------------------------------------------------------------
--                                                                          --
--                           AVATOX COMPONENTS                              --
--                                                                          --
--                        VATOX (Via Asis To Xml)                           --
--                                                                          --
--                                                                          --
--                Copyright (c) 2007, McKae Technologies.                   --
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
-- Avatox is maintained by McKae Technologies (http://www.mckae.com)        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Mckae.Xml.Xsl.Transformation;

package body Vatox.Xsl_Transformation is

   -----------------------------------------------------------------------------

   type Transformation_Results_Status_Mappings is array
     (Mckae.Xml.Xsl.Transformation.Application_Statuses) of
     Transformation_Results;

   -- Map the statuses from the utility transformation back to application
   -- ones.  (Yes, I know I could've done the 'Value of the 'Image to convert
   -- from enumeration to another, but that's just cheesy.)
   Results_Mapping : constant Transformation_Results_Status_Mappings :=
     (Mckae.Xml.Xsl.Transformation.Success => Success,
      Mckae.Xml.Xsl.Transformation.Bad_Input_File => Bad_Input_File,
      Mckae.Xml.Xsl.Transformation.Input_Not_Xml => Input_Not_Xml,
      Mckae.Xml.Xsl.Transformation.Bad_Output_File => Bad_Output_File,
      Mckae.Xml.Xsl.Transformation.Save_Failed => Save_Failed,
      Mckae.Xml.Xsl.Transformation.Bad_Xsl_File => Bad_Xsl_File,
      Mckae.Xml.Xsl.Transformation.Xsl_Not_Xsl => Xsl_Not_Xsl,
      Mckae.Xml.Xsl.Transformation.Transformation_Failed => Transformation_Failed
     );

   -----------------------------------------------------------------------------

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Trim;

   -----------------------------------------------------------------------------

   function Is_Xsl_Available return Boolean is
   begin
      return True;
   end Is_Xsl_Available;

   -----------------------------------------------------------------------------

   function XSL_Transformation_To_Be_Done (Xsl_Info : Xsl_Information)
                                           return Boolean is
   begin
      return Is_Xsl_Available and
        (Xsl_Info.Xsl_Filename /= Null_Unbounded_String);
   end Xsl_Transformation_To_Be_Done;

   -----------------------------------------------------------------------------

   function Xsl_Result_String (Result : Transformation_Results) return String is
   begin
      case Result is
         when Success =>
            return "Success";
         when Bad_Input_File =>
            return "Input file cannot be open/read";
         when Input_Not_Xml =>
            return "Input is not XML";
         when Bad_Output_File =>
            return "Output cannot be created/written";
         when Save_Failed =>
            return "Saving transformed document failed";
         when Bad_Xsl_File =>
            return "XSL file cannot be open/read";
         when Xsl_Not_Xsl =>
            return "XSL file does not contain XSL";
         when Transformation_Failed =>
            return "Transformation failed";
      end case;
   end Xsl_Result_String;

   -----------------------------------------------------------------------------

   procedure Set_Xsl_Filename
     (Xsl_Info : in out Xsl_Information;
      -- XSL transformation configuration information

      Filename : in     String
      -- Filename of XSL stylesheet to apply
     ) is
   begin
      Xsl_Info.XSL_Filename := To_Unbounded_String (Filename);
   end Set_Xsl_Filename;

   -----------------------------------------------------------------------------

   procedure Set_Output_Filename
     (Xsl_Info : in out Xsl_Information;
      -- XSL transformation configuration information

      Filename : in     String
      -- Filename into which to write the transformed output
     ) is
   begin
      Xsl_Info.Output_Filename := To_Unbounded_String (Filename);
   end Set_Output_Filename;

   -----------------------------------------------------------------------------

   function Get_Output_Filename
     ( Xsl_Info : Xsl_Information) return String is
   begin
      return To_String (Xsl_Info.Output_Filename);
   end Get_Output_Filename;

   -----------------------------------------------------------------------------

   procedure Set_XSL_Extension
     (Xsl_Info  : in out Xsl_Information;
      -- XSL transformation configuration information

      Extension : in     String
      -- File extension
     ) is
   begin
      Xsl_Info.XSL_Extension := To_Unbounded_String (Extension);
   end Set_XSL_Extension;

   -----------------------------------------------------------------------------

   function Get_XSL_Extension
     (Xsl_Info : Xsl_Information
      -- XSL transformatino configuration information
     ) return String is
   begin
      return To_String(Xsl_Info.Xsl_Extension);
   end Get_Xsl_Extension;

   -----------------------------------------------------------------------------

   procedure Add_Parameter_Pair
     (Xsl_Info         : in out Xsl_Information;
      -- XSL transformation configuration information

      Param_Value_Pair : in     String;
      -- String that is in the "param=value" format.

      Added            :    out Boolean
      -- Whether the parameter was successfully added to the list of parameters
     ) is

      use Ada.Strings.Fixed;

      -- Extract the location of the "=" that should be in the string
      Eq_Pos : constant Natural :=
        Natural'Max (Index (Param_Value_Pair, "="), 1);

      Param : constant String :=
        Trim (Param_Value_Pair (Param_Value_Pair'First .. Eq_Pos - 1));
      Value : constant String :=
        Trim (Param_Value_Pair (Eq_Pos + 1 .. Param_Value_Pair'Last));

   begin
      Added := (Param /= "") and (Value /= "");
      if Added then
         Xsl_Info.Parameters.Include(Param, Value);
      end if;
   end Add_Parameter_Pair;

   -----------------------------------------------------------------------------

   procedure Apply_Stylesheet
     (Input_Filename   : in     String;
      Output_Filename  : in     String;
      Xsl_Info         : in     Xsl_Information;
      Result           :    out Transformation_Results
     )
   is
      use Mckae.Xml.Xsl.Transformation;
      use type Parameter_Lists.Cursor;

      Status : Application_Statuses;

      Param_Index : Parameter_Indices'Base := 0;
      Parameters  : Parameter_Settings
        (1 .. Parameter_Indices (Xsl_Info.Parameters.Length));

      Param_Cursor : Parameter_Lists.Cursor := Xsl_Info.Parameters.First;

   begin
      -- Convert the parmameters to the needed format
      while Param_Cursor /= Parameter_Lists.No_Element loop
         Param_Index := Param_Index + 1;
         Parameters (Param_Index) :=
           (Key   => To_Unbounded_String (Parameter_Lists.Key (Param_Cursor)),
            Value => To_Unbounded_String (Parameter_Lists.Element (Param_Cursor)));
         Parameter_Lists.Next(Param_Cursor);
      end loop;

      -- Perform the transformation
      Apply_XSL (Input_Filename,
                 To_String(Xsl_Info.Xsl_Filename),
                 Output_Filename,
                 Status,
                 Parameters);

      Result := Results_Mapping (Status);
   end Apply_Stylesheet;

end Vatox.Xsl_Transformation;
