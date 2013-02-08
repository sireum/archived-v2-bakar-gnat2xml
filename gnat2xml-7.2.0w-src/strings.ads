------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               S T R I N G S                              --
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
with GNAT.OS_Lib;

package Strings is

   NL : constant Character := ASCII.LF;
   --  Character used to represent new-line.

   function Image (X : Integer) return String;
   --  Return X'Img without the annoying blank.

   function Capitalize (S : String) return String;
   --  Capitalizes the first letter, and all letters following a
   --  non-letter-or-digit. Converts all others to lower case.

   function Escape_String_Literal (S : String) return String;
   --  Double all the double quotes

   function Slide (X : String) return String;
   --  Return X with X'First = 1

   function Has_Prefix (X, Prefix : String) return Boolean;
   --  True if Prefix is at the beginning of X, case insensitive. For example,
   --  Has_Prefix("An_Identifier", Prefix => "an_") is True.

   function Has_Suffix (X, Suffix : String) return Boolean;
   --  True if Suffix is at the end of X, case insensitive

   function Strip_Prefix (X, Prefix : String) return String;
   --  If Prefix is at the beginning of X (case insensitive), strip it off

   function Strip_Suffix (X, Suffix : String) return String;
   --  If Suffix is at the end of X (case insensitive), strip it off

   function Name_Find (S : String) return Namet.Name_Id;
   --  Wrapper for Namet.Name_Find

   function Strip_Article (S : String) return String;
   --  Removes a leading "A_" or "An_" from the string.
   --  Case insensitive.

   function Replace_All (S, From, To : String) return String;
   --  Replaces all occurrences of From in S with To

   procedure Text_IO_Put_Char (C : Character);
   --  Put C to Current_Output. Used to instantiate Formatted_Output.

   use GNAT.OS_Lib;

   function Read_File
     (FD : File_Descriptor) return String_Access;
   function Read_File
     (File_Name : String) return String_Access;
   --  Reads the entire contents of the file

end Strings;
