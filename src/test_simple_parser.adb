with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;
with Parsers.Simple;                 use Parsers.Simple;
with Parsers.String_Source;          use Parsers.String_Source;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Parsers.Generic_Source.Text_IO;

procedure Test_Simple_Parser is
   use Lexers, Tokens;

   package Text_IO is new Code.Text_IO;
   use Text_IO;

   Parser : Simple_Expression;
   Result : Argument_Token;
   Stub   : Node_Ptr;
begin
   loop
      Put ("Expression:");
      declare
         Line : aliased String := Get_Line;
         Code : Source (Line'Access);
      begin
         exit when Line'Length = 0;
         Stub := new Mark; -- Mark the tree stack
         begin
            Parse (Parser, Code, Result);
            Put_Line
            (  Image (Result.Location)
            &  " = "
            &  Image (Result.Value.Evaluate)
            );
         exception
            when Error : Parsers.Syntax_Error =>
               Put_Line ("Error : " & Exception_Message (Error));
         end;
         Free (Stub);      -- Release the stack
      end;
   end loop;
end Test_Simple_Parser;

