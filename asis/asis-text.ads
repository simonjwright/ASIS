------------------------------------------------------------------------------
--                                                                          --
--                   ASIS-for-GNAT INTERFACE COMPONENTS                     --
--                                                                          --
--                            A S I S . T E X T                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1995-2012, Free Software Foundation, Inc.       --
--                                                                          --
-- This   specification  is  derived   from  the  Ada   Semantic  Interface --
-- Specification Standard (ISO/IEC 15291) for use with GNAT.  The copyright --
-- notice above, and the license provisions that follow apply solely to the --
--  contents of the part following the private keyword.                     --
--                                                                          --
-- ASIS-for-GNAT is free software; you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. ASIS-for-GNAT is distributed  in the hope  that it will be use- --
-- ful, but WITHOUT ANY WARRANTY; without even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with  ASIS-for-GNAT; see file --
-- COPYING.  If not,  write  to the  Free Software Foundation,  51 Franklin --
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.                         --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- ASIS-for-GNAT was originally developed  by the ASIS-for-GNAT team at the --
-- Software  Engineering  Laboratory  of  the Swiss  Federal  Institute  of --
-- Technology (LGL-EPFL) in Lausanne,  Switzerland, in cooperation with the --
-- Scientific  Research  Computer  Center of  Moscow State University (SRCC --
-- MSU), Russia,  with funding partially provided  by grants from the Swiss --
-- National  Science  Foundation  and  the  Swiss  Academy  of  Engineering --
-- Sciences. ASIS-for-GNAT is now maintained by AdaCore                     --
-- (http://www.adacore.com).                                                --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

------------------------------------------------------------------------------
--  20 package Asis.Text
------------------------------------------------------------------------------
------------------------------------------------------------------------------
package Asis.Text is
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  Asis.Text
--
--  This package encapsulates a set of operations to access the text of ASIS
--  Elements.  It assumes no knowledge of the existence, location, or form of
--  the program text.
--
--  The text of a program consists of the texts of one or more compilations.
--  The text of each compilation is a sequence of separate lexical elements.
--  Each lexical element is either a delimiter, an identifier (which can be a
--  reserved word), a numeric literal, a character literal, a string literal,
--  blank space, or a comment.
--
--  Each ASIS Element has a text image whose value is the series of characters
--  contained by the text span of the Element.  The text span covers all the
--  characters from the first character of the Element through the last
--  character of the Element over some range of lines.
--
--  General Usage Rules:
--
--  Line lists can be indexed to obtain individual lines.  The bounds of each
--  list correspond to the lines with those same numbers from the compilation
--  text.
--
--  Any Asis.Text query may raise ASIS_Failed with a Status of Text_Error if
--  the program text cannot be located or retrieved for any reason such as
--  renaming, deletion, corruption, or moving of the text.
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--  20.1  type Line
------------------------------------------------------------------------------
--  An Ada text line abstraction (a private type).
--
--  Used to represent text fragments from a compilation.
--  ASIS Lines are representations of the compilation text.
--  This shall be supported by all ASIS implementations.
------------------------------------------------------------------------------

   type Line is private;
   Nil_Line  : constant Line;

   function "=" (Left  : Line; Right : Line) return Boolean is abstract;

--  Nil_Line is the value of an uninitialized Line object.
--
------------------------------------------------------------------------------
--  20.2  type Line_Number
------------------------------------------------------------------------------
--  Line_Number
--
--  A numeric subtype that allows each ASIS implementation to place constraints
--  on the upper bound for Line_List elements and compilation unit size.
--
--  The upper bound of Line_Number (Maximum_Line_Number) is the only
--  allowed variation for these declarations.
--
--  Line_Number = 0 is reserved to act as an "invalid" Line_Number value.  No
--  unit text line will ever have a Line_Number of zero.
------------------------------------------------------------------------------
--  Line shall be an undiscriminated private type, or, shall be derived from an
--  undiscriminated private type.  It can be declared as a new type or as a
--  subtype of an existing type.
------------------------------------------------------------------------------

   Maximum_Line_Number : constant ASIS_Natural :=
      Implementation_Defined_Integer_Constant;

   subtype Line_Number is ASIS_Natural range 0 .. Maximum_Line_Number;

------------------------------------------------------------------------------
--  20.3  type Line_Number_Positive
------------------------------------------------------------------------------

   subtype Line_Number_Positive is Line_Number range 1 .. Maximum_Line_Number;

------------------------------------------------------------------------------
--  20.4  type Line_List
------------------------------------------------------------------------------

   type Line_List is array (Line_Number_Positive range <>) of Line;
   Nil_Line_List : constant Line_List;

------------------------------------------------------------------------------
--  20.5  type Character_Position
------------------------------------------------------------------------------
--  Character_Position
--
--  A numeric subtype that allows each ASIS implementation to place constraints
--  on the upper bound for Character_Position and for compilation unit line
--  lengths.
--
--  The upper bound of Character_Position (Maximum_Line_Length) is the
--  only allowed variation for these declarations.
--
--  Character_Position = 0 is reserved to act as an "invalid"
--  Character_Position value.  No unit text line will ever have a character in
--  position zero.
------------------------------------------------------------------------------

   Maximum_Line_Length : constant ASIS_Natural :=
      Implementation_Defined_Integer_Constant;

   subtype Character_Position is ASIS_Natural range 0 .. Maximum_Line_Length;

------------------------------------------------------------------------------
--  20.6  type Character_Position_Positive
------------------------------------------------------------------------------

   subtype Character_Position_Positive is
      Character_Position range 1 .. Maximum_Line_Length;

------------------------------------------------------------------------------
--  20.7  type Span
------------------------------------------------------------------------------
--  Span
--
--  A single text position is identified by a line number and a column number,
--  that represent the text's position within the compilation unit.
--
--  The text of an element can span one or more lines.  The textual Span of an
--  element identifies the lower and upper bound of a span of text positions.
--
--  Spans and positions give client tools the option of accessing compilation
--  unit text through the queries provided by this package, or, to access
--  the text directly through the original compilation unit text file. Type
--  span
--  facilitates the capture of comments before or after an element.
--
--  Note: The original compilation unit text may or may not have existed in a
--  "file", and any such file may or may not still exist. Reference Manual 10.1
--  specifies that the text of a compilation unit is submitted to a compiler.
--  It does not specify that the text is stored in a "file", nor does it
--  specify that the text of a compilation unit has any particular lifetime.
------------------------------------------------------------------------------

   type Span is                                       -- Default is Nil_Span
      record
         First_Line   : Line_Number_Positive        := 1; -- 1..0 - empty
         First_Column : Character_Position_Positive := 1; -- 1..0 - empty
         Last_Line    : Line_Number                 := 0;
         Last_Column  : Character_Position          := 0;
      end record;

   Nil_Span : constant Span := (First_Line   => 1,
                                First_Column => 1,
                                Last_Line    => 0,
                                Last_Column  => 0);

------------------------------------------------------------------------------
--  20.8  function First_Line_Number
------------------------------------------------------------------------------

   function First_Line_Number (Element : Asis.Element) return Line_Number;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns the first line number on which the text of the element resides.
--
--  Returns 0 if not Is_Text_Available(Element).
--
--  --|AN Application Note:
--  --|AN
--  --|AN The line number recorded for a particular element may or may not
--  --|AN match the "true" line number of the program text for that element if
--  --|AN the Ada environment and the local text editors do not agree on the
--  --|AN definition of "line". For example, the Reference Manual states that
--  --|AN any occurrence of an ASCII.Cr character is to be treated as one or
--  --|AN more end-of-line occurrences.  On most Unix systems, the editors do
--  --|AN not treat a carriage return as being an end-of-line character.
--  --|AN
--  --|AN Ada treats all of the following as end-of-line characters: ASCII.Cr,
--  --|AN ASCII.Lf, ASCII.Ff, ASCII.Vt.  It is up to the compilation system to
--  --|AN determine whether sequences of these characters causes one, or more,
--  --|AN end-of-line occurrences.  Be warned, if the Ada environment and the
--  --|AN system editor (or any other line-counting program) do not use the
--  --|AN same end-of-line conventions, then the line numbers reported by ASIS
--  --|AN may not match those reported by those other programs.
--
------------------------------------------------------------------------------
--  20.9  function Last_Line_Number
------------------------------------------------------------------------------

   function Last_Line_Number (Element : Asis.Element) return Line_Number;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns the last line number on which the text of the element resides.
--
--  Returns 0 if not Is_Text_Available(Element).
--
------------------------------------------------------------------------------
--  20.10 function Element_Span
------------------------------------------------------------------------------

   function Element_Span (Element : Asis.Element) return Span;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns the span of the given element.
--
--  Returns a Nil_Span if the text of a Compilation_Unit (Compilation) cannot
--  be located for any reason.
--  --|AN
--  --|AN For this query, Element is only a means to access the
--  --|AN Compilation_Unit (Compilation), the availability of the text of this
--  --|AN Element itself is irrelevant to the result of the query.
--
------------------------------------------------------------------------------
--  20.11 function Compilation_Unit_Span
------------------------------------------------------------------------------

   function Compilation_Unit_Span (Element : Asis.Element) return Span;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns the span of the text comprising the enclosing compilation unit of
--  the given element.
--
--  Returns a Nil_Span if the text of a Compilation_Unit (Compilation) cannot
--  be located for any reason.
--  --|AN
--  --|AN For this query, Element is only a means to access the
--  --|AN Compilation_Unit (Compilation), the availability of the text of this
--  --|AN Element itself is irrelevant to the result of the query.
--
------------------------------------------------------------------------------
--  20.12 function Compilation_Span
------------------------------------------------------------------------------

   function Compilation_Span (Element : Asis.Element) return Span;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns the span of the text comprising the compilation to which the
--  element belongs.  The text span may include one or more compilation units.
--
--  Returns a Nil_Span if not Is_Text_Available(Element).
--
------------------------------------------------------------------------------
--  20.13 function Is_Nil
------------------------------------------------------------------------------

   function Is_Nil (Right : Line) return Boolean;

------------------------------------------------------------------------------
--  Right   - Specifies the line to check
--
--  Returns True if the argument is the Nil_Line.
--
--  A Line from a Line_List obtained from any of the Lines functions
--  will not be Is_Nil even if it has a length of zero.
--
------------------------------------------------------------------------------
--  20.14 function Is_Nil
------------------------------------------------------------------------------

   function Is_Nil (Right : Line_List) return Boolean;

------------------------------------------------------------------------------
--  Right   - Specifies the line list to check
--
--  Returns True if the argument has a 'Length of zero.
--

------------------------------------------------------------------------------
--  20.15 function Is_Nil
------------------------------------------------------------------------------

   function Is_Nil (Right : Span) return Boolean;

------------------------------------------------------------------------------
--  Right   - Specifies the Span to check
--
--  Returns True if the argument has a Nil_Span.
--
------------------------------------------------------------------------------
--  20.16 function Is_Equal
------------------------------------------------------------------------------

   function Is_Equal (Left  : Line; Right : Line) return Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the first of the two lines
--  Right   - Specifies the second of the two lines
--
--  Returns True if the two lines encompass the same text (have the same Span
--  and are from the same compilation).
--
------------------------------------------------------------------------------
--  20.17 function Is_Identical
------------------------------------------------------------------------------

   function Is_Identical (Left  : Line; Right : Line) return Boolean;

------------------------------------------------------------------------------
--  Left    - Specifies the first of the two lines
--  Right   - Specifies the second of the two lines
--
--  Returns True if the two lines encompass the same text (have the same Span
--  and are from the same compilation) and are from the same Context.
--
------------------------------------------------------------------------------
--  20.18 function Length
------------------------------------------------------------------------------

   function Length (The_Line : Line) return Character_Position;

------------------------------------------------------------------------------
--  The_Line    - Specifies the line to query
--
--  Returns the length of the line.
--
--  Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
--
------------------------------------------------------------------------------
--  20.19 function Lines
------------------------------------------------------------------------------

   function Lines (Element : Asis.Element) return Line_List;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns a list of lines covering the span of the given program element.
--
--  Returns a Nil_Span if the text of a Compilation containing a given
--  Element cannot be located for any reason.
--
--  Line lists can be indexed to obtain individual lines.  The bounds of each
--  list correspond to the lines with those same numbers in the compilation
--  text.
--
--  The first Line of the result contains text from the compilation starting at
--  the First_Line/First_Column of Element's Span.  The last Line of the result
--  contains text from the compilation ending at the Last_Line/Last_Column of
--  the Element's Span.  Text before or after those limits is not reflected
--  in the returned list.
--  --|AN
--  --|AN For this query, Element is only a means to access the
--  --|AN Compilation_Unit (Compilation), the availability of the text of this
--  --|AN Element itself is irrelevant to the result of the query.
--
------------------------------------------------------------------------------
--  20.20 function Lines
------------------------------------------------------------------------------

   function Lines
     (Element  : Asis.Element;
      The_Span : Span)
      return     Line_List;

------------------------------------------------------------------------------
--  Element  - Specifies the element to query
--  The_Span - Specifies the textual span to return
--
--  Returns a list of lines covering the given span from the compilation
--  containing the given program element.
--
--  Returns a Nil_Span if the text of a Compilation containing a given
--  Element cannot be located for any reason.
--
--  This operation can be used to access lines from text outside the span of an
--  element, but still within the compilation.  For example, lines containing
--  preceding comments or lines between two elements.
--
--  Line lists can be indexed to obtain individual lines.  The bounds of each
--  list correspond to the lines with those same numbers in the compilation
--  text.
--
--  The first Line of the result contains text from the compilation starting at
--  line Span.First_Line and column Span.First_Column.  The last Line of the
--  result contains text from the compilation ending at line Span.Last_Line and
--  column Span.Last_Column.  Text before or after those limits is not
--  reflected in the returned list.
--
--  Raises ASIS_Inappropriate_Line_Number if Is_Nil (The_Span). If
--  The_Span defines a line whose number is outside the range of text lines
--  that can be accessed through the Element, the implementation is encouraged,
--  but not required to raise ASIS_Inappropriate_Line_Number.
--  --|AN
--  --|AN For this query, Element is only a means to access the
--  --|AN Compilation_Unit (Compilation), the availability of the text of this
--  --|AN Element itself is irrelevant to the result of the query.
--
------------------------------------------------------------------------------
--  20.21 function Lines
------------------------------------------------------------------------------

   function Lines
     (Element    : Asis.Element;
      First_Line : Line_Number_Positive;
      Last_Line  : Line_Number)
      return       Line_List;

------------------------------------------------------------------------------
--  Element     - Specifies the element to query
--  First_Line  - Specifies the first line to return
--  Last_Line   - Specifies the last line to return
--
--  Returns a list of Lines covering the full text for each of the indicated
--  lines from the compilation containing the given element.  This operation
--  can be used to access lines from text outside the span of an element, but
--  still within the compilation.
--
--  Returns a Nil_Span if the text of a Compilation containing a given
--  Element cannot be located for any reason.
--
--  Line lists can be indexed to obtain individual lines.  The bounds of each
--  list correspond to the lines with those same numbers in the compilation
--  text.
--
--  Raises ASIS_Inappropriate_Line_Number if the span is nil. If the span
--  defines a line whose number is outside the range of text lines that can be
--  accessed through the Element, the implementation is encouraged, but not
--  required to raise ASIS_Inappropriate_Line_Number.
--  --|AN
--  --|AN For this query, Element is only a means to access the
--  --|AN Compilation_Unit (Compilation), the availability of the text of this
--  --|AN Element itself is irrelevant to the result of the query.
--
------------------------------------------------------------------------------
--  20.22 function Delimiter_Image
------------------------------------------------------------------------------

   function Delimiter_Image return Wide_String;

------------------------------------------------------------------------------
--  Returns the string used as the delimiter separating individual lines of
--  text within the program text image of an element. It is also used as the
--  delimiter separating individual lines of strings returned by Debug_Image.
--
------------------------------------------------------------------------------
--  20.23 function Element_Image
------------------------------------------------------------------------------

   function Element_Image (Element : Asis.Element) return Program_Text;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns a program text image of the element.  The image of an element can
--  span more than one line, in which case the program text returned by the
--  function Delimiter_Image separates the individual lines.  The bounds on
--  the returned program text value are 1..N, N is as large as necessary.
--
--  Returns a null string if not Is_Text_Available(Element).
--
--  If an Element's Span begins at column position P, the returned program text
--  will be padded at the beginning with P-1 white space characters (ASCII.' '
--  or ASCII.Ht).  The first character of the Element's image will thus begin
--  at character P of the returned program text. Due to the possible presence
--  of ASCII.Ht characters, the "column" position of characters within the
--  image might not be the same as their print-column positions when the image
--  is displayed on a screen or printed.
--
--  NOTE: The image of a large element can exceed the range of Program_Text.
--  In this case, the exception ASIS_Failed is raised with a Status of
--  Capacity_Error. Use the Lines function to operate on the image of large
--  elements.
--
------------------------------------------------------------------------------
--  20.24 function Line_Image
------------------------------------------------------------------------------

   function Line_Image (The_Line : Line) return Program_Text;

------------------------------------------------------------------------------
--  The_Line    - Specifies the line to query
--
--  Returns a program text image of the line.  The image of a single lexical
--  element can be sliced from the returned value using the first and last
--  column character positions from the Span of the Element.  The bounds on the
--  returned program text are 1 .. Length(Line).
--
--  If the Line is the first line from the Lines result for an Element, it can
--  represent only a portion of a line from the original compilation.  If the
--  span began at character position P, the first Line of it's Lines
--  result is padded at the beginning with P-1 white space characters
--  (ASCII.' ' or ASCII.Ht).  The first character of the image will
--  thus begin at character P of the program text for the first Line. Due to
--  the possible presence of ASCII.Ht characters, the "column" position of
--  characters within the image may not be the same as their print-column
--  positions when the image is displayed or printed.
--
--  Similarly, if the Line is the last line from the Lines result for an
--  Element, it may represent only a portion of a line from the original
--  compilation.  The program text image of such a Line is shorter than the
--  line from compilation and will contain only the initial portion of
--  that line.
--
--  Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
--
------------------------------------------------------------------------------
--  20.25 function Non_Comment_Image
------------------------------------------------------------------------------

   function Non_Comment_Image (The_Line : Line) return Program_Text;

------------------------------------------------------------------------------
--  The_Line    - Specifies the line to query
--
--  Returns a program text image of a Line up to, but excluding, any comment
--  appearing in that Line.
--
--  The value returned is the same as that returned by the Image function,
--  except that any hyphens ("--") that start a comment, and any characters
--  that follow those hyphens, are dropped.
--
--  The bounds on the returned program text are 1..N, where N is one less than
--  the column of any hyphens ("--") that start a comment on the line.
--
--  Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
--
------------------------------------------------------------------------------
--  20.26 function Comment_Image
------------------------------------------------------------------------------

   function Comment_Image (The_Line : Line) return Program_Text;

------------------------------------------------------------------------------
--  The_Line    - Specifies the line to query
--
--  Returns a program text image of any comment on that line, excluding any
--  lexical elements preceding the comment.
--
--  The value returned is the same as that returned by the Image function,
--  except that any program text prior to the two adjacent hyphens ("--") which
--  start a comment is replaced by an equal number of spaces.  If the hyphens
--  began in column P of the Line, they will also begin in character position
--  P of the returned program text.
--
--  A null string is returned if the line has no comment.
--
--  The bounds of the program text are 1..N, where N is as large as necessary.
--
--  Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
--
------------------------------------------------------------------------------
--  20.27 function Is_Text_Available
------------------------------------------------------------------------------

   function Is_Text_Available (Element : Asis.Element) return Boolean;

------------------------------------------------------------------------------
--  Element - Specifies the element to query
--
--  Returns True if the implementation can return a valid text image for the
--  given element.
--
--  Returns False for any Element that Is_Nil, Is_Part_Of_Implicit, or
--  Is_Part_Of_Instance.
--
--  Returns False if the text of the element cannot be located for any reason
--  such as renaming, deletion, or moving of text.
--
--  --|IR Implementation Requirements:
--  --|IR
--  --|IR An implementation shall make text available for all explicit
--  --|IR elements.
--
------------------------------------------------------------------------------
--  20.28 function Debug_Image
------------------------------------------------------------------------------

   function Debug_Image (The_Line : Line) return Wide_String;

------------------------------------------------------------------------------
--  The_Line    - Specifies the line to convert
--
--  Returns a string value containing implementation-defined debug
--  information associated with the line.
--
--  The return value uses Asis.Text.Delimiter_Image to separate the lines
--  of multi-line results.  The return value does not end with
--  Asis.Text.Delimiter_Image.
--
--  These values are intended for two purposes.  They are suitable for
--  inclusion in problem reports sent to the ASIS implementor.  They can
--  be presumed to contain information useful when debugging the
--  implementation itself. They are also suitable for use by the ASIS
--  application when printing simple application debugging messages during
--  application development.  They are intended to be, to some worthwhile
--  degree, intelligible to the user.
--
------------------------------------------------------------------------------

private

   --  The content of this private part is specific for the ASIS
   --  implementation for GNAT

------------------------------------------------------------------------------

   type Line is
      record
         Sloc              : Source_Ptr         := No_Location;
         Comment_Sloc      : Source_Ptr         := No_Location;
         Length            : Character_Position := 0;
         Rel_Sloc          : Source_Ptr         := No_Location;
         Enclosing_Unit    : Unit_Id            := Nil_Unit;
         Enclosing_Context : Context_Id         := Non_Associated;
         Enclosing_Tree    : Tree_Id            := Nil_Tree;
         Obtained          : ASIS_OS_Time       := Nil_ASIS_OS_Time;
      end record;

   Nil_Line : constant Line := (Sloc              => No_Location,
                                Comment_Sloc      => No_Location,
                                Length            => 0,
                                Rel_Sloc          => No_Location,
                                Enclosing_Unit    => Nil_Unit,
                                Enclosing_Context => Non_Associated,
                                Enclosing_Tree    => Nil_Tree,
                                Obtained          => Nil_ASIS_OS_Time);

   --  Note, that Line does not have the predefined "=" operation (it is
   --  overridden by an abstract "=". The predefined "=" is explicitly
   --  simulated in the implementation of Is_Nil query, so, if the full
   --  declaration for Line is changed, the body of Is_Nil should be revised.

   -----------------------------
   -- Fields in the Line type --
   -----------------------------

   --  Sloc : Source_Ptr         := No_Location;
   --    indicates the beginning of the corresponding line in the Source
   --    Buffer. If a given Line is the first line covering the Image of
   --    some Element, the position in the Source Buffer pointed by Sloc
   --    may or may not correspond to the beginning of the source text
   --    line
   --
   --  Comment_Sloc - represents the starting Sloc position of a comment
   --    that is a part of the line. Set to 0 if the line does not contain a
   --    a comment. Note, that we assume that in the comment text the bracket
   --    encoding can not be used for wide characters
   --
   --  Length : Character_Position := 0;
   --    represents the length of the Line, excluding any character
   --    signifying end of line (RM95, 2.2(2))
   --    Note, that in the compiler lines of the source code are represented
   --    using the String type, and if the line contains a wide character that
   --    requires more than one one-byte character for its representation, the
   --    length of one-byte character string used to represent wide-character
   --    string from the source code may be bigger then the length of the
   --    original source line (length here is the number of characters). In the
   --    Line type we keep the lenth of the source line counted in source
   --    characters, but not the length of the internal representation in
   --    one-byte characters. Note that this length does not depend on the
   --    encoding method.
   --
   --  Rel_Sloc : Source_Ptr := No_Location;
   --    needed to compare string representing elements of the same
   --    compilation units, but probably obtained from different trees
   --    containing this unit. Obtained in the same way as for Elements -
   --    by subtracting from Sloc field the source location of the
   --    N_Compilation_Unit node
   --
   --  Enclosing_Unit    : Unit_Id    := Nil_Unit;
   --  Enclosing_Context : Context_Id := Non_Associated;
   --    These fields represent the Context in which and the Compilation
   --    Unit from which the given line was obtained, they are needed
   --    for comparing Lines and for tree swapping when obtaining
   --    Line_Image
   --
   --  Enclosing_Tree : Tree_Id := Nil_Tree;
   --    Sloc field may be used for obtaining the image of a Line only in
   --    the tree from which it was obtained. So the Line has to keep
   --    the reference to the tree for tree swapping
   --
   --  Obtained : ASIS_OS_Time := Nil_ASIS_OS_Time;
   --    Lines, as well as Elements, cannot be used after closing the
   --    Context from which they were obtained. We use time comparing
   --    to check the validity of a Line

   --  In the current implementation, ASIS Lines are mapped onto logical
   --  GNAT lines, as defined in the Sinput package.

   --  The child package Asis.Text.Set_Get defines operations for
   --  accessing and updating fields of Lines, for checking the validity
   --  of a Line and for creating the new value of the Line type.
   --  (This package is similar to the Asis.Set_Get package, which
   --  defines similar things for other ASIS abstractions - Element,
   --  Context, Compilation_Unit.)

   Nil_Line_List : constant Line_List (1 .. 0) := (1 .. 0 => Nil_Line);

------------------------------------------------------------------------------

end Asis.Text;
