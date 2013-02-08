--                                                                    --
--  package Strings_Edit            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:11 25 Jun 2005  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Strings_Edit is
   function GetDigit (Symbol : Character) return Natural is
   begin
      case Symbol is
         when '0' => return 0;
         when '1' => return 1;
         when '2' => return 2;
         when '3' => return 3;
         when '4' => return 4;
         when '5' => return 5;
         when '6' => return 6;
         when '7' => return 7;
         when '8' => return 8;
         when '9' => return 9;
         when 'A' | 'a' => return 10;
         when 'B' | 'b' => return 11;
         when 'C' | 'c' => return 12;
         when 'D' | 'd' => return 13;
         when 'E' | 'e' => return 14;
         when 'F' | 'f' => return 15;
         when others => return 16;
      end case;
   end GetDigit;
--
-- Text_Edit
--
-- This is an internal package containing  implementation  of  all  text
-- editing subprograms.
--
   package Text_Edit is
      function TrimCharacter
               (  Source : in String;
                  Blank  : in Character := ' '
               )  return String;
      function TrimSet
               (  Source : in String;
                  Blanks : in Ada.Strings.Maps.Character_Set
               )  return String;
      procedure GetCharacter
                (  Source  : in String;
                   Pointer : in out Integer;
                   Blank   : in Character := ' '
                );
      procedure GetSet
                (  Source  : in String;
                   Pointer : in out Integer;
                   Blanks  : in Ada.Strings.Maps. Character_Set
                );
      procedure PutString
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : in String;
                   Field       : in Natural := 0;
                   Justify     : in Alignment := Left;
                   Fill        : in Character := ' '
                );
      procedure PutCharacter
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : in Character;
                   Field       : in Natural := 0;
                   Justify     : in Alignment := Left;
                   Fill        : in Character := ' '
                );
   end Text_Edit;
--????   package body Text_Edit is separate;
-- Can't handle subunits yet.

package body Text_Edit is
   function TrimCharacter
            (  Source : in String;
               Blank  : in Character := ' '
            )  return String is
      First : Integer := Source'First;
      Last  : Integer := Source'Last;
   begin
      while First <= Last and then Source (First) = Blank loop
         First := First + 1;
      end loop;
      while First <= Last and then Source (Last) = Blank loop
         Last := Last - 1;
      end loop;
      return Source (First..Last);
   end TrimCharacter;

   function TrimSet
            (  Source : in String;
               Blanks : in Ada.Strings.Maps.Character_Set
            )  return String is
      First : Integer := Source'First;
      Last  : Integer := Source'Last;
   begin
      while (  First <= Last
            and then
               Ada.Strings.Maps.Is_In (Source (First), Blanks)
            )
      loop
         First := First + 1;
      end loop;
      while (  First <= Last
            and then
               Ada.Strings.Maps.Is_In (Source (Last), Blanks)
            )
      loop
         Last := Last - 1;
      end loop;
      return Source (First..Last);
   end TrimSet;

   procedure GetCharacter
             (  Source  : in String;
                Pointer : in out Integer;
                Blank   : in Character := ' '
             )  is
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      for Index in Pointer..Source'Last loop
         exit when Source (Index) /= Blank;
         Pointer := Pointer + 1;
      end loop;
   end GetCharacter;

   procedure GetSet
             (  Source  : in String;
                Pointer : in out Integer;
                Blanks  : in Ada.Strings.Maps.Character_Set
             )  is
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      for Index in Pointer..Source'Last loop
         exit when not Ada.Strings.Maps.Is_In (Source (Index), Blanks);
         Pointer := Pointer + 1;
      end loop;
   end GetSet;

   procedure PutString
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in String;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
      OutField : Natural := Field;
   begin
      if OutField = 0 then
         OutField := Value'Length;
      end if;
      if (  Pointer < Destination'First
         or else
            Pointer + OutField - 1 > Destination'Last
         or else
            OutField < Value'Length
         )
      then
         raise Layout_Error;
      end if;
      if OutField /= 0 then
         if OutField = Value'Length then
            Destination (Pointer..Pointer + OutField - 1) := Value;
         else
            declare
               First : Integer;
               Next  : Integer;
            begin
               case Justify is
                  when Left =>
                     First := Pointer;
                     Next  := First + Value'Length;
                     for Position in Next..Pointer + OutField - 1 loop
                        Destination (Position) := Fill;
                     end loop;
                  when Center =>
                     First := Pointer + (OutField - Value'Length) / 2;
                     Next  := First + Value'Length;
                     for Position in Pointer..First - 1 loop
                        Destination (Position) := Fill;
                     end loop;
                     for Position in Next..Pointer + OutField - 1 loop
                        Destination (Position) := Fill;
                     end loop;
                  when Right =>
                     Next  := Pointer + OutField;
                     First := Next - Value'Length;
                     for Position in Pointer..First - 1 loop
                        Destination (Position) := Fill;
                     end loop;
               end case;
               Destination (First..Next - 1) := Value;
            end;
         end if;
         Pointer := Pointer + OutField;
      end if;
   end PutString;

   procedure PutCharacter
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Character;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
      Text : String (1..1) := (1 => Value);
   begin
      Put (Destination, Pointer, Text, Field, Justify, Fill);
   end PutCharacter;
end Text_Edit;

   function Trim
            (  Source : in String;
               Blank  : in Character := ' '
            )  return String renames Text_Edit.TrimCharacter;
   function Trim
            (  Source : in String;
               Blanks : in Ada.Strings.Maps.Character_Set
            )  return String renames Text_Edit.TrimSet;
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Blank   : in Character := ' '
             )  renames Text_Edit.GetCharacter;
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Blanks  : in Ada.Strings.Maps. Character_Set
             )  renames Text_Edit.GetSet;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in String;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  renames Text_Edit.PutString;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Character;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  renames Text_Edit.PutCharacter;

end Strings_Edit;
