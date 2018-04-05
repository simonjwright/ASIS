.. _Processing_an_Ada_Library_by_an_ASIS-Based_Tool:

***********************************************
Processing an Ada Library by an ASIS-Based Tool
***********************************************

.. index:: Ada predefined library (processing by an ASIS tool)

When an Ada unit to be processed by some ASIS-based tool makes
use of an Ada library, you need to be aware of the following features
of using Ada libraries with GNAT:

*
  An Ada library is a collection of precompiled Ada components. The sources
  of the Ada components belonging to the library are present,
  but if your program uses some components from a
  library, these components are not recompiled by *gnatmake*
  (except in circumstances described below).
  For example, ``Ada.Text_IO`` is not recompiled
  when you invoke *gnatmake* on a unit that ``with``\ s
  ``Ada.Text_IO``.

*
  According to the GNAT source-based compilation model, the spec of a library
  component is processed when an application unit depending on such a component is
  compiled, but the body of the library component is not processed. As a result,
  if you invoke *gnatmake* to create a set of tree files covering a given
  program, and if this program references an entity from an Ada library, then the
  set of tree files created by such a call will contain only specs, but not
  bodies for library components.

*
  Any GNAT installation contains the GNAT Run-Time Library (RTL) as a
  precompiled Ada library. In some cases, a GNAT installation may contain some
  other libraries (such as Win32Ada Binding on a Windows GNAT
  platform).

  .. index:: Asis.Extensions package

  .. index:: Asis.Compilation_Units.Unit_Origin query

*
  In ASIS-for-GNAT, there is no standard way to define whether a given
  ``Compilation_Unit`` belongs to some precompiled Ada library other than
  the GNAT Run-Time Library (some heuristics may be added to ``Asis.Extensions``).
  ASIS-for-GNAT classifies (by means of the
  ``Asis.Compilation_Units.Unit_Origin`` query)
  a unit as
  ``A_Predefined_Unit``, if it is from the Run-Time Library
  and if it is mentioned in the Ada Reference Manual, Annex A, Paragraph 2
  as an Ada 95 predefined unit;
  a unit is classified as
  ``An_Implementation_Unit`` if is belongs to Run-Time Library but is not mentioned in
  the paragraph just cited.
  Components of Ada libraries other than the Run-Time Library are always classified
  as ``An_Application_Unit``;

*
  It is possible to recompile the components of the Ada libraries used
  by a given program. To do this, you have to invoke ``gnatmake`` for this
  program with the ``-a`` option. If you create a set of
  tree files for your program by invoking *gnatmake* with the ``-a`` option, the
  resulting set of tree files will contain all the units needed by this
  program to make up a complete partition.

Therefore, there are two possibilities for an ASIS-based tool if processing
(or avoiding processing) of Ada libraries is important for
the functionality of the tool:

  .. index:: Asis.Compilation_Units.Is_Body_Required function

  .. index:: Asis.Compilation_Units.Corresponding_Body function

*
  If the tool is not to process components of Ada libraries, then
  a set of tree files for this tool may be created by invoking *gnatmake*
  without the ``-a`` option (this is the usual way of using *gnatmake*).
  When the tool encounters a ``Compilation_Unit`` which represents a spec of some
  library unit, and for which ``Asis.Compilation_Units.Is_Body_Required``
  returns ``True``, but ``Asis.Compilation_Units.Corresponding_Body``
  yields a
  result of ``A_Nonexistent_Body`` kind, then the tool may conclude that
  this library unit belongs to some precompiled Ada library.

*
  If a tool needs to process all the Ada compilation units making up a
  program, then a set of tree files for this program should be created by
  invoking *gnatmake* with the ``-a`` option.

.. index:: Asis.Compilation_units.Unit_Origin

You can use ``Asis.Compilation_units.Unit_Origin``
to filter out Run-Time Library components.
