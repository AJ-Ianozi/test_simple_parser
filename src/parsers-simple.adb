with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

package body Parsers.Simple is 

   function "and" (Left, Right : Operations) return Boolean is
   begin
      return True;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      return False;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      return False;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      return Mul;
   end Group_Inverse;

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
   begin
      return
      (  not Is_Alphanumeric (Source (Pointer))
      or else
         not Is_Alphanumeric (Source (Pointer - 1))
      );
   end Check_Matched;

   function Call
            (  Context   : access Simple_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Operation.Operation;
         This.Location  := Operation.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Operation.Location & Link (List));
   end Call;

   function Enclose
            (  Context : access Simple_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Left.Operation;
         This.Location  := Left.Location & Right.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Left.Location & Right.Location & Link (List));
   end Enclose;

   procedure Get_Operand
             (  Context  : in out Simple_Expression;
                Code     : in out Source;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Line    : String renames Get_Line (Code);
      Pointer : Integer := Get_Pointer (Code);
      Value   : Integer;
   begin
      if Is_Decimal_Digit (Line (Pointer)) then
         Get (Line, Pointer, Value);
         Set_Pointer (Code, Pointer);
         Argument.Location := Link (Code);
         Argument.Value := new Literal;
         declare
            Result : Literal renames Literal (Argument.Value.all);
         begin
            Result.Value    := Value;
            Result.Location := Argument.Location;
         end;
         Got_It := True;
      else
         Got_It := False;
      end if;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Too large number at " &  Image (Link (Code))
         );
      when Data_Error =>
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Malformed number at " &  Image (Link (Code))
         );
      when End_Error =>
         Got_It := False;
   end Get_Operand;

   function Evaluate (Item : Mark) return Integer is
   begin
      return 0;
   end Evaluate;

   function Evaluate (Item : Literal) return Integer is
   begin
      return Item.Value;
   end Evaluate;

   function Evaluate (Item : Expression) return Integer is
      Argument : array (Item.Operands'Range) of Integer;
   begin
      for I in Argument'Range loop
        Argument (I) := Item.Operands (I).Evaluate;
      end loop;
      case Item.Operation is
         when Add => return Argument (1) + Argument (2);
         when Sub => return Argument (1) - Argument (2);
         when Mul => return Argument (1) * Argument (2);
         when Div => return Argument (1) / Argument (2);
         when others => return Argument (1);
      end case;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Numeric error at " & Image (Item.Location)
         );
   end Evaluate;

   use type Tokens.Descriptors.Descriptor_Class;
   use Lexers.Lexical_Descriptors.Operation;
   use Lexers.Lexical_Arguments;

begin
   Add_Operator (Infixes,   "+", Add, 1, 1);
   Add_Operator (Infixes,   "-", Sub, 1, 1);
   Add_Operator (Infixes,   "*", Mul, 2, 2);
   Add_Operator (Infixes,   "/", Div, 2, 2);

   Add_Bracket  (Prefixes,  "(", Left_Bracket);
   Add_Bracket  (Postfixes, ")", Right_Bracket);

end Parsers.Simple;

