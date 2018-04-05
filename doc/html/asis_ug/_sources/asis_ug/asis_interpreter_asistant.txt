*****************************
ASIS Interpreter ``asistant``
*****************************

.. index:: asistant

.. index:: ASIS queries

This chapter describes ``asistant``, an interactive interface to ASIS queries.

``asistant`` Introduction
=========================

The ``asistant`` tool allows you
to use ASIS without building your own ASIS applications. It
provides a simple command language that allows you to define variables of ASIS
types and to assign them values by calling ASIS queries.

This tool may be very useful while you are learning ASIS:
it lets you try different ASIS queries and see the results immediately.
It does not crash when there is an error in calling an ASIS query
(such as passing an inappropriate ``Element``); instead ``asistant`` reports an
error and lets you try again.

You can also use ``asistant`` as a debug and 'ASIS visualization' tool in
an ASIS application project.  If you have problems
finding out which query should be used in a given situation, or why a given
query does not work correctly with a given piece of Ada code, you may use
``asistant`` to reconstruct the situation that causes the problems,
and then experiment with ASIS queries.

.. index:: Script file (for asistant)

Though primarily an interactive tool, ``asistant`` also can interpret
sequences of commands written to a file (called a 'script file'
below). The ``asistant`` tool can also store in a file the log of an interactive
session that can then be reused as a script file.

The full documentation of ``asistant`` may be found in the
``asistant`` Users' Guide (file :file:`asistant.ug` in the ``asistant`` source directory).
Here is a brief overview of ``asistant`` usage.

.. index:: ASIS-for-GNAT

The executable for ``asistant`` is created in the ``asistant``
source directory as a part of the standard procedure of installing
ASIS-for-GNAT as an Ada library (or it is placed in the :file:`GNATPRO/bin`
directory when installing ASIS from the binary distribution). Put this
executable somewhere on your path (unless you have
installed ASIS from the binary distribution, in which case the executable for
``asistant`` has been added to other GNAT executables).
Then type
'``asistant``' to call ``asistant`` in an interactive mode. As a result,
the program will output brief information about itself and then the
``asistant`` prompt '``>``' will appear:


::

  ASIStant - ASIS Tester And iNTerpreter, v1.2
  (C) 1997-2002, Free Software Foundation, Inc.
    Asis Version: ASIS 2.0.R

  >


Now you can input ``asistant`` commands (``asistant`` supports
in its command language the same form of comments as Ada, and names in
``asistant`` are not case-sensitive):


.. code-block:: ada

  >Initialize ("") -- the ASIS Initialize query is called with an
                   -- empty string as a parameter

  >set (Cont) --  the non-initialized variable Cont of the ASIS
              --  Context type is created

  >Associate (Cont, "", "") --  the ASIS Associate query with two empty
                            --  strings as parameters is called for Cont

  >Open (Cont)  --  the ASIS Open query is called for Cont

  >set (C_U, Compilation_Unit_Body ("Test", Cont)) -- the variable C_U
    --  of the ASIS Compilation_Unit type is created and initialized as
    --  the result of the call to the ASIS query Compilation_Unit_Body.
    --  As a result, C_U will represent a compilation unit named "Test"
    --  and contained in the ASIS Context named Cont

  >set (Unit, Unit_Declaration (C_U))  --  the variable Unit of the ASIS
    --  Element type is created and initialized as the result of calling
    --  the ASIS Unit_Declaration query

  >print (Unit) --  as a result of this command, some information about
                --  the current value of Unit will be printed (a user can set
                --  the desired level of detail of this information):

  A_PROCEDURE_BODY_DECLARATION at ( 1 : 1 )-( 9 : 9 )

  --  suppose now, that we do make an error - we call an ASIS query for
  --  an inappropriate element:

  >set (Elem, Assignment_Expression (Unit))

  --  ASIS will raise an exception, asistant will output the ASIS debug
  --  information:

  Exception is raised by ASIS query ASSIGNMENT_EXPRESSION.
  Status : VALUE_ERROR
  Diagnosis :
  Inappropriate Element Kind in Asis.Statements.Assignment_Expression

  --  it does not change any of the existing variables and it prompts
  --  a user again:

  > ...


``asistant`` commands
=====================

.. index:: asistant commands

The list of ``asistant`` commands given in this section is incomplete;
its purpose is only to give a general idea of ``asistant``'s capabilities.
Standard metalanguage is assumed (i.e., '[*construct*]'
denotes an optional instance of '*construct*').



*Help [(name)]*

  .. index:: Help (asistant command)

  Outputs the profile of the ASIS query '``name``'; when called with no argument,
  generates general ``asistant`` help information.


*Set (name)*

  .. index:: Set (asistant command)

  Creates a (non-initialized) variable '``name``' of the ASIS ``Context`` type.


*Set (name, expr)*
  Evaluates the expression '``expr``' (it may be any legal ``asistant``
  expression; a call to some ASIS query is the most common case in practice)
  and creates the variable '``name``' of the type and with the value of
  '``expr``'.


*Print (expr)*

  .. index:: Print (asistant command)

  Evaluates the expression '``expr``' and outputs its value (some information may be
  omitted depending on the level specified by the *PrintDetail* command).


*Run* (:file:`filename`)

  .. index:: Run (asistant command)

  Launches the script from a file :file:`filename`, reading further commands from it.

  .. index:: Script file (for asistant)


*Pause*

  .. index:: Pause (asistant command)

  Pauses the current script and turns ``asistant`` into interactive mode.


*Run*
  Resumes a previously ``Pause``\ d script.


*Browse*

  .. index:: Browse (asistant command)

  Switches ``asistant`` into step-by-step ASIS tree browsing.


*Log* (:file:`filename`)

  .. index:: Log (asistant command)

  Opens the file :file:`filename` for session logging.


*Log*
  Closes the current log file.


*PrintDetail*

  .. index:: PrintDetail (asistant command)

  Toggles whether the *Print* command outputs additional information.


*Quit [(exit-status)]*

  .. index:: Quit (asistant command)

  Quits ``asistant``.

``asistant`` variables
======================

.. index:: asistant variables

The ``asistant`` tool lets you define variables with Ada-style (simple) names.
Variables can be of
any ASIS type and of conventional ``Integer``, ``Boolean`` and ``String`` type.
All the variables are created and assigned dynamically by the ``Set``
command; there are no predefined variables.

There is no type checking in ``asistant``: each call to a ``Set``
command may be considered as creating the first argument from scratch and
initializing it by the value provided by the second argument.

Browsing an ASIS tree
=====================

.. index:: Browser (asistant utility)

You perform ASIS tree browsing by invoking the ``asistant`` service function
``Browse``. This will disable the ``asistant`` command interpreter
and activate the Browser command interpreter. The Browser ``Q`` command
switches back into the ``asistant`` environment by enabling the ``asistant``
command interpreter and disabling the Browser interpreter.

``Browse`` has a single parameter of ``Element`` type, which establishes
where the ASIS tree browsing will begin.
``Browse`` returns a
result of type ``Element``, namely the ``Element`` at which the tree browsing was
stopped. Thus, if you type:


.. code-block:: ada

  > set (e0, Browse (e1))


you will start ASIS tree browsing from ``e1``; when you finish
browsing, ``e0`` will represent the last ``Element`` visited during the
browsing.

If you type:


.. code-block:: ada

  > Browse (e1)


you will be able to browse the ASIS tree, but the last ``Element`` of the
browsing will be discarded.

Browser displays the ASIS ``Element`` it currently points at and expects one of
the following commands:



*U*
  Go one step up the ASIS tree (equivalent to calling the ASIS
  ``Enclosing_Element`` query);

  .. index:: Enclosing_Element query


*D*
  Go one step down the ASIS tree, to the left-most component of the current ``Element``


*N*
  Go to the right sibling (to the next ``Element`` in the ASIS tree hierarchy)


*P*
  Go to the left sibling (to the previous ``Element`` in the ASIS tree hierarchy)


*\\k1k2*
  where ``k1`` is either ``D`` or ``d``, and
  ``k2`` is either ``T`` or ``t``.
  Change the form of displaying the current ``Element``: ``D`` turns ON displaying the
  debug image, ``d`` turns it OFF. ``T`` turns ON displaying the text image, ``t``
  turns it OFF.


*<SPACE><query>*
  Call the <query> for the current ``Element``.


*Q*
  Go back to the ``asistant`` environment; the Browser command interpreter is
  disabled and the ``asistant`` command interpreter is enabled with the
  current ``Element`` returned as a result of the call to ``Browse``.

Browser immediately interprets the keystroke and displays the new current
``Element``. If the message ``"Cannot go in this direction."`` appears, this
means that traversal in this direction from current node is impossible (that
is, the current node is either a terminal ``Element`` and it is not possible to go
down, or it is the leftmost or the rightmost component of some ``Element``, and
it is not possible to go left or right, or it is the top ``Element`` in its
enclosing unit structure and it is not possible to go up).

It is possible to issue some ordinary ASIS queries from inside the Browser
(for example, semantic queries). These queries should accept one parameter of
type ``Element`` and return ``Element`` as a result.

When you press ``<SPACE>``, you are asked to enter the query name. If the
query is legal, the current ``Element`` is replaced by the result of the call to
the given query with the current ``Element`` as a parameter.

Example
=======

.. index:: ASIS Example

Suppose we have an ASIS ``Compilation_Unit`` ``Demo`` in the source file :file:`demo.adb`:


.. code-block:: ada

  procedure Demo is
     function F (I : Integer) return Integer;

     function F (I : Integer) return Integer is
     begin
        return (I + 1);
     end F;

     N : Integer;

  begin
      N := F (3);
  end Demo;


Suppose also that the tree for this source is created in the current directory.
Below is a sequence of ``asistant`` commands which does process this
unit. Explanation is provided via ``asistant`` comments.


.. code-block:: ada

  initialize ("")

  --  Create and open a Context comprising all the tree files
  --  in the current directory:

  Set (Cont)
  Associate (Cont, "", "")
  Open (Cont)

  -- Get a Compilation_Unit (body) named "Demo" from this Context:

  Set (CU, Compilation_Unit_Body ("Demo", Cont))

  --  Go into the unit structure and get to the expression
  --  in the right part of the assignment statements in the unit body:

  Set (Unit, Unit_Declaration (CU))
  Set (Stmts, Body_Statements (Unit, False))
  Set (Stmt, Stmts (1))
  Set (Expr, Assignment_Expression (Stmt))

  -- Output the debug image and the text image of this expression:

  Print (Expr)
  Print (Element_Image (Expr))

  --  This expression is of A_Function_Call kind, so it's possible to ask
  --  for the declaration of the called function:

  Set (Corr_Called_Fun, Corresponding_Called_Function (Expr))

  --  Print the debug and the text image of the declaration of the called
  --  function:

  Print (Corr_Called_Fun)
  Print (Element_Image (Corr_Called_Fun))

  -- Close the asistant session:

  Quit
