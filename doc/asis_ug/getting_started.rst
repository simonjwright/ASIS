.. _Getting_Started:

***************
Getting Started
***************

This section outlines the ASIS application development and usage cycle.
We first take a sample problem and present an ASIS application that offers a
solution; then we show how to build the
executable with ASIS-for-GNAT and how to prepare an ASIS 'Context' to be
processed by the program; and finally we show the output produced by our
program when it is applied to itself.

The Problem
===========

.. index:: Spec (definition of term)

We wish to process some set of Ada compilation units as follows:
for every unit, print its full expanded Ada name,
whether this unit is a spec, a
body or a subunit, and whether this unit is a user-defined unit, an Ada predefined
unit or an implementation-specific unit (such as a part of
a Run-Time Library).

.. _An_ASIS_Application_that_Solves_the_Problem:

An ASIS Application that Solves the Problem
===========================================

.. index:: ASIS Example
.. index:: Context type (example)
.. index:: Asis.Implementation.Initialize procedure (example)
.. index:: Asis.Ada_Environments.Associate query (example)
.. index:: Asis.Ada_Environments.Open procedure (example)
.. index:: Compilation_Unit type (example)
.. index:: Asis.Compilation_Units.Unit_Full_Name query (example)
.. index:: Asis.Compilation_Units.Unit_Kind query (example)
.. index:: Asis.Compilation_Units.Unit_Origin query (example)
.. index:: Asis.Ada_Environments.Close procedure (example)
.. index:: Asis.Ada_Environments.Dissociate procedure (example)
.. index:: Asis.Implementation.Finalize procedure (example)
.. index:: Asis.Exceptions.ASIS_Inappropriate_Context exception (example)
.. index:: Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit exception (example)
.. index:: Asis.Exceptions.ASIS_Failed exception (example)
.. index:: Asis.Implementation.Status function (example)

.. code-block:: ada

  with Ada.Wide_Text_IO;        use Ada.Wide_Text_IO;
  with Ada.Characters.Handling; use Ada.Characters.Handling;

  --  ASIS-specific context clauses:
  with Asis;
  with Asis.Implementation;
  with Asis.Ada_Environments;
  with Asis.Compilation_Units;
  with Asis.Exceptions;
  with Asis.Errors;

  procedure Example1 is
     My_Context : Asis.Context;
     --  ASIS Context is an abstraction of an Ada compilation environment,
     --  it defines a set of ASIS Compilation Units available through
     --  ASIS queries

  begin
     --  first, by initializing an ASIS implementation, we make it
     --  ready for work
     Asis.Implementation.Initialize ("-ws");
     --  The "-ws" parameter of the Initialize procedure means
     --  "turn off all the ASIS warnings"

     --  then we define our Context by making an association with
     --  the "physical" environment:
     Asis.Ada_Environments.Associate
      (My_Context, "My Asis Context", "-CA");
     --  "-CA" as a Context parameter means "consider all the tree
     --  files in the current directory"
     --  See ASIS-for-GNAT Reference Manual for the description of the
     --  parameters of the Associate query, see also chapter
     --  "ASIS Context" for the description of different kinds of
     --  ASIS Context in case of ASIS-for-GNAT

     --  by opening a Context we make it ready for processing by ASIS
     --  queries
     Asis.Ada_Environments.Open (My_Context);
     Processing_Units: declare
        Next_Unit : Asis.Compilation_Unit;
        --  ASIS Compilation_Unit is the abstraction to represent Ada
        --  compilation units as described in RM 95

        All_Units : Asis.Compilation_Unit_List :=
        --  ASIS lists are one-dimensional unconstrained arrays.
        --  Therefore, when declaring an object of an ASIS list type,
        --  we have to provide either a constraint or explicit
        --  initialization expression:

           Asis.Compilation_Units.Compilation_Units (My_Context);
        --  The Compilation_Units query returns a list of all the units
        --  contained in an ASIS Context
     begin
        Put_Line
          ("A Context contains the following compilation units:");
        New_Line;
        for I in All_Units'Range loop
           Next_Unit := All_Units (I);
           Put ("   ");

           --  to get a unit name, we just need a Unit_Full_Name
           --  query. ASIS uses Wide_String as a string type,
           --  that is why we are using Ada.Wide_Text_IO

           Put (Asis.Compilation_Units.Unit_Full_Name (Next_Unit));
           --  to get more info about a unit, we ask about unit class
           --  and about unit origin

           case Asis.Compilation_Units.Unit_Kind (Next_Unit) is
              when Asis.A_Library_Unit_Body =>
                 Put (" (body)");
              when Asis.A_Subunit =>
                 Put (" (subunit)");
              when others =>
                 Put (" (spec)");
           end case;

           case Asis.Compilation_Units.Unit_Origin (Next_Unit) is
              when Asis.An_Application_Unit =>
                 Put_Line (" - user-defined unit");
              when Asis.An_Implementation_Unit =>
                 Put_Line (" - implementation-specific unit");
              when Asis.A_Predefined_Unit =>
                 Put_Line (" - Ada predefined unit");
              when Asis.Not_An_Origin =>
                 Put_Line
                   (" - unit does not actually exist in a Context");
           end case;

        end loop;
     end Processing_Units;

     --  Cleaning up: we have to close out the Context, break its
     --  association with the external environment and finalize
     --  our ASIS implementation to release all the resources used:
     Asis.Ada_Environments.Close (My_Context);
     Asis.Ada_Environments.Dissociate (My_Context);
     Asis.Implementation.Finalize;
  exception
     when Asis.Exceptions.ASIS_Inappropriate_Context |
          Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
          Asis.Exceptions.ASIS_Failed =>
        --  we check not for all the ASIS-defined exceptions, but only
        --  those of them which can actually be raised in our ASIS
        --  application.
        --
        --  If an ASIS exception is raised, we output the ASIS error
        --  status and the ASIS diagnosis string:

        Put_Line ("ASIS exception is raised:");
        Put_Line ("ASIS diagnosis is:");
        Put_Line (Asis.Implementation.Diagnosis);
        Put      ("ASIS error status is: ");
        Put_Line
          (Asis.Errors.Error_Kinds'Wide_Image
             (Asis.Implementation.Status));
  end Example1;


Required Sequence of Calls
==========================

An ASIS application must use the following sequence of calls:

.. index:: Asis.Implementation.Initialize procedure

.. index:: Erroneous execution

*
  ``Asis.Implementation.Initialize (...);``

  This initializes the ASIS implementation's internal data structures.
  In general, calling an ASIS
  query is erroneous unless the ``Initialize`` procedure has been invoked.

.. index:: Asis.Implementation.Associate procedure

*
  ``Asis.Ada_Environments.Associate (...);``

  .. index:: Context type

  .. index:: Compilation_Unit type

  This call is the only means to define a value of a variable of the
  ASIS limited private type ``Context``.
  The value represents some specific
  association of the ASIS ``Context`` with the 'external world'. The way
  of making this association and the meaning of the corresponding
  parameters of the ``Associate`` query are implementation-specific,
  but as soon as this association has been made and a ``Context`` variable
  is opened, the ASIS ``Context`` designated by this variable may be
  considered to be a set of ASIS ``Compilation_Unit``\ s
  available through the ASIS queries.

.. index:: Asis.Ada_Environments.Open procedure

*
  ``Asis.Ada_Environments.Open (...);``

  Opening an ASIS ``Context`` variable makes the corresponding ``Context``
  accessible to all ASIS queries.

  .. index:: Compilation_Unit type

  .. index:: Element type

  .. index:: Erroneous execution

  After opening the ``Context``, an ASIS application can start obtaining
  ASIS ``Compilation_Unit``\ s from it, can further analyze ``Compilation_Unit``\ s
  by decomposing them into ASIS ``Element``\ s, etc.
  ASIS relies on the fact that the content of a ``Context`` remains 'frozen'
  as long as the ``Context`` remains open.
  It is erroneous
  to change through some non-ASIS program any data
  structures used by an ASIS implementation to define and implement
  this ``Context`` while the ``Context`` is open.

  .. index:: Compilation_Unit type

  .. index:: Element type

*
  Now all the ASIS queries can be used. It is possible to access ``Compilation_Unit``\ s
  from the ``Context``, to decompose units into syntactic ``Element``\ s,
  to query syntactic and semantic properties of these
  ``Element``\ s and so on.

  .. index:: Ada_Environments.Close procedure

  .. index:: Context type

  .. index:: Compilation_Unit type

  .. index:: Element type

  .. index:: Line type

  .. index:: Erroneous execution

*
  ``Asis.Ada_Environments.Close (...);``

  After closing the ``Context`` it is impossible to retrieve any information
  from it. All the values of the ASIS objects of ``Compilation_Unit``,
  ``Element``
  and ``Line``
  types obtained when this ``Context`` was open become
  obsolete, and it is erroneous
  to use them after the ``Context`` was closed.
  The content of this ``Context`` need not be frozen while
  the ``Context`` remains closed. Note that a closed ``Context`` keeps its
  association with the 'external world' and it may be opened again with
  the same association. Note also that the content (that is, the
  corresponding set of ASIS ``Compilation_Unit``\ s) of the ``Context`` may be
  different from what was in the ``Context`` before, because the external
  world may have changed while the ``Context`` remained closed.

  .. index:: Asis.Ada_Environments.Dissociate procedure

*
  ``Asis.Ada_Environments.Dissociate (...);``

  This query breaks the association between the corresponding ASIS
  ``Context`` and the 'external world', and the corresponding ``Context``
  variable becomes undefined.

  .. index:: Asis.Implementation.Finalize procedure

*
  ``Asis.Implementation.Finalize (...);``

  This releases all the resources used by an ASIS implementation.

.. index:: Context type

.. index:: ASIS-for-GNAT

An application can perform these steps in a loop. It may initialize and
finalize an ASIS implementation several times, it may associate and dissociate
the same ``Context`` several times while an ASIS implementation remains
initialized, and it may open and close the same ``Context`` several times while
the ``Context`` keeps its association with the 'external world'.
An application can have several ASIS ``Context``\ s opened at a time (the upper
limit is implementation-specific), and for each open ``Context``, an application
can process several ``Compilation_Unit``\ s obtained from this ``Context`` at a time
(the upper limit is also implementation-specific). ASIS-for-GNAT
does not
impose any special limitations on the number of ASIS ``Context``\ s and on the
number of the ASIS ``Compilation_Unit``\ s processed at a time, as long as an ASIS
application is within the general resource limitations of the underlying
system.

Building the Executable for an ASIS application
===============================================

.. index:: ASIS-for-GNAT

The rest of this section assumes that you have ASIS-for-GNAT properly
installed as an Ada library.
As for other components of the GNAT technology, the structure of the
ASIS distribution and the ASIS building and installation process is
based on project files. So, the same should be the case for ASIS
application.

For your ASIS application you should create a project file that depends
on the main ASIS project file ``asis.gpr``. Here is the simplest version of
such a project file:


.. code-block:: ada

  with "asis";
  project Example1 is
     for Main use ("example1.adb");
  end Example1;


To get the executable for the ASIS application from
:ref:`An_ASIS_Application_that_Solves_the_Problem` (assuming
that it is located in your current directory as the Ada source file named
:file:`example1.adb`, and the corresponding project file is also located
in the current directory), invoke *gprbuid* as follows:


::

  $ gprbuild example1.gpr


For more details concerning compiling ASIS applications and building
executables for them with ASIS-for-GNAT see
:ref:`Compiling_Binding_and_Linking_Applications_with_ASIS-for-GNAT`.

Preparing Data for an ASIS Application --- Generating Tree Files
=====================================================================

The general ASIS implementation technique is to use some information generated
by the underlying Ada compiler as the basis for retrieving information
from the Ada environment. As a consequence, an ASIS application can process
only legal (compilable) Ada code, and in most of the cases to make a
compilation unit 'visible' for ASIS means to compile this unit (probably
with some ASIS-specific options)

.. index:: Tree file

ASIS-for-GNAT uses *tree output files* (or, in short, *tree files*)
to capture
information about an Ada unit from an Ada environment. A tree file is
generated by GNAT, and it contains a snapshot of the compiler's internal
data structures at the end of the successful compilation of the
corresponding source file.

.. index:: -gnatct option

To create a tree file for a unit contained in some source file, you should
compile this file with the ``-gnatct`` compiler option.
If you want to apply
the program described in section
:ref:`An_ASIS_Application_that_Solves_the_Problem` to itself,
compile the source of this application with the command:


::

  $ gcc -c -gnatct example1.adb


and as a result, GNAT will generate the tree file named :file:`example1.adt` in the current
directory.

For more information on how to generate and deal with tree files, see
:ref:`ASIS_Context`, and :ref:`ASIS_Tutorials`.

Running an ASIS Application
===========================

.. index:: Context type

To complete our example, let's execute our ASIS application. If you have
followed all the steps described in this chapter,
your current directory should contain the executable :file:`example1`
(:file:`example1.exe` on a Windows platform)
and the tree file :file:`example1.adt`.
If we run
our application, it will process an ASIS ``Context`` defined by one tree file
:file:`example1.adt` (for more details about defining an ASIS ``Context`` see
:ref:`ASIS_Context`, and the ASIS-for-GNAT Reference Manual).
The result will be:


.. code-block:: ada

     A Context contains the following compilation units:

        Standard (spec) - Ada predefined unit
        Example1 (body) - user-defined unit
        Ada (spec) - Ada predefined unit
        Ada.Wide_Text_IO (spec) - Ada predefined unit
        Ada.IO_Exceptions (spec) - Ada predefined unit
        Ada.Streams (spec) - Ada predefined unit
        System (spec) - Ada predefined unit
        System.File_Control_Block (spec) - implementation-specific unit
        Interfaces (spec) - Ada predefined unit
        Interfaces.C_Streams (spec) - implementation-specific unit
        System.Parameters (spec) - implementation-specific unit
        System.WCh_Con (spec) - implementation-specific unit
        Ada.Characters (spec) - Ada predefined unit
        Ada.Characters.Handling (spec) - Ada predefined unit
        Asis (spec) - user-defined unit
        A4G (spec) - user-defined unit
        A4G.A_Types (spec) - user-defined unit
        Ada.Characters.Latin_1 (spec) - Ada predefined unit
        GNAT (spec) - implementation-specific unit
        GNAT.OS_Lib (spec) - implementation-specific unit
        GNAT.Strings (spec) - implementation-specific unit
        Unchecked_Deallocation (spec) - Ada predefined unit
        Sinfo (spec) - user-defined unit
        Types (spec) - user-defined unit
        Uintp (spec) - user-defined unit
        Alloc (spec) - user-defined unit
        Table (spec) - user-defined unit
        Urealp (spec) - user-defined unit
        A4G.Int_Knds (spec) - user-defined unit
        Asis.Implementation (spec) - user-defined unit
        Asis.Errors (spec) - user-defined unit
        Asis.Ada_Environments (spec) - user-defined unit
        Asis.Compilation_Units (spec) - user-defined unit
        Asis.Ada_Environments.Containers (spec) - user-defined unit
        Asis.Exceptions (spec) - user-defined unit
        System.Unsigned_Types (spec) - implementation-specific unit


.. index:: Tree file

Note that the tree file
contains the full syntactic and semantic information not only
about the unit compiled by the given call to *gcc*, but also about all
the units upon which this unit depends semantically; that is why you can see
in the output list a number of units which are not mentioned in our example.

In the current version of ASIS-for-GNAT, ASIS implementation components are considered
user-defined, rather than implementation-specific, units.
