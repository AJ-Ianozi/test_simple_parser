with Ada.Unchecked_Deallocation;
with Parsers.String_Source;            use Parsers.String_Source;
with Parsers.Generic_Lexer.Ada_Blanks;
with Parsers.Generic_Token.Segmented_Lexer;
with Stack_Storage;
with Tables.Names;

package Parsers.Simple is
   type Operations is (Add, Sub, Mul, Div, Left_Bracket, Right_Bracket);
   type Priorities is mod 3; -- The levels of association

   function "and" (Left, Right : Operations) return Boolean;
   function Is_Commutative (Left, Right : Operations) return Boolean;
   function Is_Inverse (Operation : Operations) return Boolean;
   function Group_Inverse (Operation : Operations) return Operations;

   Tree_Pool : Stack_Storage.Pool (2048, 128); -- Arena for the tree
      -- Tree nodes
   type Node is abstract tagged limited null record;
   function Evaluate (Item : Node) return Integer is abstract;
   type Node_Ptr is access Node'Class;
   for Node_Ptr'Storage_Pool use Tree_Pool;
   procedure Free is
      new Standard.Ada.Unchecked_Deallocation (Node'Class, Node_Ptr);
      -- Stub of the arena
   type Mark is new Node with null record;
   overriding function Evaluate (Item : Mark) return Integer;
      -- Terminal nodes
   type Literal is new Node with record
      Location : Parsers.String_Source.Location;
      Value    : Integer;
   end record;
   overriding function Evaluate (Item : Literal) return Integer;
      -- Non-terminal nodes
   type Argument_List is array (Positive range <>) of Node_Ptr;
   type Expression (Count : Positive) is new Node with record
      Operation : Operations;
      Location  : Parsers.String_Source.Location;
      Operands  : Argument_List (1..Count);
   end record;
   overriding function Evaluate (Item : Expression) return Integer;

   package Tokens is -- The lexical tokens
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Node_Ptr,
             Priority_Type  => Priorities,
             Sources        => Code
          );
   use Tokens;

   procedure Check_Spelling (Name : String);
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
   package Token_Tables is new Tokens.Vocabulary.Names;
      -- The tables of prefix, infix and postfix operations
   Prefixes  : aliased Token_Tables.Dictionary;
   Infixes   : aliased Token_Tables.Dictionary;
   Postfixes : aliased Token_Tables.Dictionary;

   package Lexers is new Tokens.Segmented_Lexer; -- Table driven lexers
   package Blank_Skipping_Lexers is -- Lexers that skip Ada blanks
      new Lexers.Token_Lexer.Implementation.Ada_Blanks (Lexers.Lexer);

   type Simple_Expression is -- The lexer that uses our tables
      new Blank_Skipping_Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;
   overriding -- Evaluates an operator
      function Call
               (  Context   : access Simple_Expression;
                  Operation : Tokens.Operation_Token;
                  List      : Tokens.Arguments.Frame
               )  return Tokens.Argument_Token;
   overriding -- Evaluates an expression in brackets
      function Enclose
               (  Context : access Simple_Expression;
                  Left    : Tokens.Operation_Token;
                  Right   : Tokens.Operation_Token;
                  List    : Tokens.Arguments.Frame
               )  return Tokens.Argument_Token;
   overriding -- Recognizes an operand (float number)
      procedure Get_Operand
                (  Context  : in out Simple_Expression;
                   Code     : in out Source;
                   Argument : out Tokens.Argument_Token;
                   Got_It   : out Boolean
                );
end Parsers.Simple;
