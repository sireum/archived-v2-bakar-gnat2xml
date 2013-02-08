------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                               S T R I N G S                              --
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
with Text_IO;

package body Strings is

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
      Result : constant String := X'Img;
   begin
      case Result (1) is
         when ' ' =>
            return Slide (Result (2 .. Result'Last));
         when '-' =>
            return Result;
         when others =>
            raise Program_Error;
      end case;
   end Image;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (S : String) return String is
   begin
      return Result : String (S'Range) do
         for X in S'Range loop
            if X = S'First or else
              not (Is_Letter (S (X - 1)) or else
                   Is_Digit (S (X - 1)))
            then
               Result (X) := To_Upper (S (X));
            else
               Result (X) := To_Lower (S (X));
            end if;
         end loop;
      end return;
   end Capitalize;

   ---------------------------
   -- Escape_String_Literal --
   ---------------------------

   function Escape_String_Literal (S : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for C of S loop
         Append (Result, C);
         if C = '"' then
            Append (Result, C);
         end if;
      end loop;

      return To_String (Result);
   end Escape_String_Literal;

   ----------------
   -- Has_Prefix --
   ----------------

   function Has_Prefix (X, Prefix : String) return Boolean is
   begin
      if X'Length >= Prefix'Length then
         declare
            Slice : constant String :=
              To_Lower (X (X'First .. X'First + Prefix'Length - 1));
         begin
            return Slice = To_Lower (Prefix);
         end;
      end if;
      return False;
   end Has_Prefix;

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (X, Suffix : String) return Boolean is
   begin
      if X'Length >= Suffix'Length then
         declare
            Slice : constant String :=
              To_Lower (X (X'Last - Suffix'Length + 1 .. X'Last));
         begin
            return Slice = To_Lower (Suffix);
         end;
      end if;
      return False;
   end Has_Suffix;

   ------------------
   -- Strip_Prefix --
   ------------------

   function Strip_Prefix (X, Prefix : String) return String is
   begin
      if Has_Prefix (X, Prefix) then
         return X (X'First + Prefix'Length .. X'Last);
      end if;

      return X;
   end Strip_Prefix;

   ------------------
   -- Strip_Suffix --
   ------------------

   function Strip_Suffix (X, Suffix : String) return String is
   begin
      if Has_Suffix (X, Suffix) then
         return X (X'First .. X'Last - Suffix'Length);
      end if;

      return X;
   end Strip_Suffix;

   -----------
   -- Slide --
   -----------

   function Slide (X : String) return String is
   begin
      return Result : String (1 .. X'Length) := X;
   end Slide;

   ---------------
   -- Name_Find --
   ---------------

   function Name_Find (S : String) return Namet.Name_Id is
      use Namet;
      Slid : constant String := Slide (S);
   begin
      Name_Len := Slid'Length;
      Name_Buffer (Slid'Range) := Slid;
      return Name_Find;
   end Name_Find;

   -----------------
   -- Replace_All --
   -----------------

   function Replace_All (S, From, To : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      J : Positive := S'First;
   begin
      while J <= S'Last loop
         if J + From'Length - 1 <= S'Last and then
           S (J .. J + From'Length - 1) = From
         then
            Append (Result, To);
            J := J + From'Length;
         else
            Append (Result, S (J));
            J := J + 1;
         end if;
      end loop;

      return To_String (Result);
   end Replace_All;

   -------------------
   -- Strip_Article --
   -------------------

   function Strip_Article (S : String) return String is
   begin
      return Strip_Prefix (Strip_Prefix (S, Prefix => "A_"), Prefix => "AN_");
   end Strip_Article;

   ----------------------
   -- Text_IO_Put_Char --
   ----------------------

   procedure Text_IO_Put_Char (C : Character) is
   begin
      if C = NL then
         Text_IO.New_Line;
      else
         Text_IO.Put (C);
      end if;
   end Text_IO_Put_Char;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (FD : File_Descriptor) return String_Access
   is
      Length      : constant Natural := Natural (File_Length (FD));

      This_Read   : Integer;
      Read_Ptr    : Natural := 1;

      Buffer : String_Access := new String (1 .. Length);
   begin
      loop
         This_Read := Read (FD,
           A => Buffer (Read_Ptr)'Address,
           N => Length + 1 - Read_Ptr);
         Read_Ptr := Read_Ptr + Integer'Max (This_Read, 0);
         exit when This_Read <= 0 or else Read_Ptr = Length + 1;
      end loop;

      if Read_Ptr /= Length + 1 then
         raise Program_Error with "Read_File failed";
      end if;

      return Buffer;
   end Read_File;

   function Read_File
     (File_Name : String) return String_Access is

      FD : constant File_Descriptor := Open_Read (File_Name, Fmode => Text);

   begin
      if FD = Invalid_FD then
         raise Program_Error with "file not found: " & File_Name;
      end if;

      return Result : String_Access := Read_File (FD) do
         Close (FD);
      end return;
   end Read_File;

end Strings;
